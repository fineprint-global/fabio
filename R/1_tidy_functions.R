
# Rename variables in a datatable, drop unspecified ones
dt_rename <- function(x, rename, drop = TRUE) {

  found <- names(x)[names(x) %in% names(rename)]
  not_found <- names(x)[!names(x) %in% names(rename)]

  if(length(not_found) > 0) {
    cat("Renaming. Unspecified columns:\n\t",
        paste0("`", not_found, "`", collapse = ", "), ".\n", sep = "")
    if(drop) {
      cat("Dropping unspecified columns.\n")
      x <- subset(x, select = found)
    }
  }

  names(x) <- c(rename[names(x)])

  x
}

# Replace values where `fun` applies
dt_replace <- function(x, fun = is.na, value = 0,
  cols = seq_len(ncol(x)), verbose = TRUE) {

  n_replaced <- 0
  for(col in cols) {
    fun_applied <- fun(x[[col]])
    if(verbose) {n_replaced <- n_replaced + sum(fun_applied, na.rm = TRUE)}
    set(x, i = which(fun_applied), j = col, value)
  }
  if(verbose) {
    cat("Replaced ", n_replaced, " values where `", deparse(substitute(fun)),
      "` (applies to columns ", paste0("'", cols, "'", collapse = ", "),
      ") with ", value, ".\n", sep = "")
  }
  return(x)
}


# Filter a datatable verbosely
dt_filter <- function(x, subset, select, na.rm = TRUE) {

  # Evaluate subset
  if(missing(subset)) {
    r <- TRUE
  } else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if(!is.logical(r)) {stop("'subset' must evaluate to logical")}
    na_count <- sum(is.na(r))
    # Remove NAs, as we cannot evaluate them
    r <- if(na.rm) {r & !is.na(r)} else {r | is.na(r)}
  }

  if(missing(select)) {
    vars <- seq_len(ncol(x))
  } else {
    nl <- as.list(seq_len(ncol(x)))
    setattr(nl, "names", names(x))
    vars <- eval(substitute(select), nl, parent.frame())
  }
  cat("Removing ", x[!r, .N], " observations via `", deparse(e), "`.\n", sep = "")
  if(na_count > 0) {
    cat(if(na.rm) {"Included"} else {"Excluded"},
    " were a total of ", na_count,
    " NA values that could not be compared.\n", sep = "")
  }

  return(x[r, vars, with = FALSE])
}


# Area adjustments --------------------------------------------------------


# Fix area codes
area_fix <- function(x, regions, col = "area_code") {

  col_name <- gsub("(.*)_code", "\\1", col)

  matched <- match(x[[col]], regions[["code"]])
  if(any(is.na(matched))) {
    na_codes <- unique(x[[col]][is.na(matched)])
    if(all(na_codes >= 5000)) {
      message("Found no match for grouped areas:\n\t",
        paste0(unique(x[[col_name]][is.na(matched)]), " - ",
          na_codes, collapse = ", "),
        ".\n", "")
    } else {
      stop("Found no match for:\n\t",
        paste0(unique(x[[col_name]][is.na(matched)]), " - ",
          na_codes, collapse = ", "),
        ".\n")
    }
  }
  x[[col_name]] <- regions[matched, name]

  return(x)
}


# Kick out area codes, check the name via pattern
area_kick <- function(x, code, col = "area_code", pattern = "*", groups = TRUE) {

  # Vector to use for subsetting
  idx <- x[[col]]
  col_name <- gsub("(.*)_code", "\\1", col)

  if(!missing(code)) {
    n_found <- x[idx == code, .N]
    cat("Found ", n_found, " observations where `",
        col, " == ", code, "`.\n", sep = "")

    if(n_found > 0) {
      # Check names of code
      if(col_name %in% colnames(x)) {
        name <- x[idx == code, col_name, with = FALSE][1][[1]]
        if(pattern != "*" && !grepl(pattern, name)) {
          stop("Pattern not found.\n")
        }
        cat("Removing observations of '", name, "' from the table.\n", sep = "")
      } else {
        message("Column with names not found. Skipping pattern-check.\n")
        cat("Removing observations of area '", code,
          "' from the table.\n", sep = "")
      }
    }
    x <- x[idx != code, ]
    idx <- idx[idx != code]
  }

  # Remove country groups
  if(groups) {
    # To-do: the four three-digit exceptions could be handled cleaner.
    n_groups <- x[idx >= 5000 | idx %in% c(269, 268, 266, 261), .N]
    cat("Found", n_groups, "observations of grouped areas.\n")
    if(n_groups > 0) {
      cat("Removing observations of:\n\t",
        paste0("'", unique(x[[col_name]][idx >= 5000 |
          idx %in% c(269, 268, 266, 261)]), "'", collapse = ", "),
        ".\n", sep = "")
    }
    x <- x[idx < 5000 & ! idx %in% c(269, 268, 266, 261), ]
  }

  return(x)
}


# Merge areas
area_merge <- function(x, orig, dest, col = "area_code", pattern = "*") {

  # Vector to use for subsetting
  idx <- x[[col]]

  n_orig <- x[idx == orig, .N]
  n_dest <- x[idx == dest, .N]

  cat("Found", n_orig, "/", n_dest, "observations of `orig` / `dest`.\n")

  if(n_orig == 0) {return(x)}

  # Check names of origin and destination
  col_name <- gsub("(.*)_code", "\\1", col)
  if(col_name %in% colnames(x)) {
    orig_name <- x[idx == orig, col_name, with = FALSE][1][[1]]
    dest_name <- if(n_dest == 0) {
      if(pattern != "*") {pattern} else {orig_name}
    } else {
      x[idx == dest, col_name, with = FALSE][1][[1]]
    }
    if(pattern != "*" && !all(grepl(pattern, c(orig_name, dest_name)))) {
      stop("Pattern not found in both origin and destination.\n")
    }
    cat("Merging ", orig_name, " into ", dest_name, ".\n", sep = "")
    set(x, which(idx == orig), col_name, dest_name)
  } else {
    message("Column with names not found. Skipping pattern-check.\n")
    cat("Merging area ", orig, " into area ", dest, ".\n", sep = "")
  }
  set(x, which(idx == orig), col, dest)

  return(x)
}


# Apply technical conversion factors to values
tcf_apply <- function(x, na.rm = TRUE, filler = 1L, fun = `/`) {

  n_na <- sum(is.na(x[["tcf"]]))
  if(n_na > 0) {
    cat("No conversion factors found for:\n\t",
        paste0("'", unique(x[is.na(tcf), item]), "'", collapse = ", "),
        ".\n", sep = "")
    if(na.rm) {
      cat("Dropping", n_na, "missing values.\n")
      x <- x[!is.na(tcf), ]
    } else if(!is.null(filler)) {
      cat("Filling ",  n_na, " missing values with ",
          filler, ".\n", sep = "")
      x[is.na(tcf), tcf := filler]
    }
  }
  x[, `:=`(value = fun(value, tcf), tcf = NULL)]

  return(x)
}


# Give preference to a certain flow
flow_pref <- function(x, pref = "Import") {

  x[, id := paste(from_code, to_code, item_code, year, sep = "_")]

  to_kick <- x[imex != pref & id %in% x[imex == pref, id], id]
  cat("Dropping ", length(to_kick), " observations as preference is given to ",
      pref, ".\n", sep = "")

  x <- x[imex == pref | !id %in% to_kick]
  x[, id := NULL]

  return(x)
}


# Recursive sum over vectors with NA, returns NA if all values are NA
na_sum <- function(..., rowwise = TRUE) {
  dots <- list(...)
  if(length(dots) == 1) { # Base
    ifelse(all(is.na(dots[[1]])), NA_real_, sum(dots[[1]], na.rm = TRUE))
  } else { # Recurse
    if(rowwise) {
      x <- do.call(cbind, dots)
      return(apply(x, 1, na_sum))
    }
    return(na_sum(vapply(dots, na_sum, double(1L))))
  }
}


# Vectorised version of gsub
vsub <- function(a, b, x) {
  stopifnot(length(a) == length(b))
  for(i in seq_along(a)) {x <- gsub(a[i], b[i], x)}
  return(x)
}


# Replace RoW values
replace_RoW <- function(x, cols = "area_code", codes) {

  name_cols <- gsub("(.*)_code", "\\1", cols)
  n_replaced <- 0
  for(i in seq_along(cols)) {
    fun_applied <- !x[[cols[i]]] %in% codes
    n_replaced <- n_replaced + sum(fun_applied, na.rm = TRUE)
    set(x, i = which(fun_applied), j = cols[i], 999)
    set(x, i = which(fun_applied), j = name_cols[i], "RoW")
  }
  cat("Aggregated ", n_replaced, " areas in columns ",
    paste0("'", c(cols, name_cols), "'", collapse = ", "),
    " to 999 / RoW.\n", sep = "")
  return(x)
}


# Fill processing from outputs (y) and inputs (z), given TCF (C)
fill_tcf <- function(y, z, C, cap = TRUE) {
  Z <- diag(z)
  X <- C %*% Z # X holds the potential output of every input
  x <- rowSums(X) # x is the potential output
  exists <- x != 0 # exists kicks 0 potential outputs
  if(!any(exists)) {return(rep(NA, length(z)))}
  # P holds implied processing use
  #   X / x is the percentage-split across inputs
  #   y / x is the required percentage of total output demand
  P <- (X[exists, ] / x[exists]) * y[exists] / C[exists,]
  if(class(P)!="numeric") { processing <- colSums(P, na.rm = T)
  } else processing <- tidyr::replace_na(P, 0)
  if(cap) {processing[processing > z] <- z[processing > z]}
  return(processing)
}


# Split processing use over processes
split_tcf <- function(y, z, C, cap = TRUE) {
  Z <- diag(z)
  X <- C %*% Z
  x <- rowSums(X)
  exists <- x != 0 # exists kicks 0 potential outputs
  if(!any(exists)) {return(NA)}
  P <- ((X[exists, ] / x[exists]) * y[exists]) / C[exists,]
  P[is.na(P)] <- 0
  # P <- .sparseDiagonal(sum(exists), y[exists] / x[exists]) %*%
  #   (X[exists, ] / x[exists]) %*% Z
  if(cap) {
    cap <- rep(0, length(z))
    exists_inp <- z != 0
    if(class(P)!="numeric") {
      cap[exists_inp] <- colSums(P)[exists_inp] / z[exists_inp]
    } else {
      cap[exists_inp] <- P[exists_inp] / z[exists_inp]
    }
    cap[cap < 1] <- 1 # Don't want to scale up
    P <- P %*% diag(1 / cap)
  }
  out <- data.table(as.matrix(P))
  colnames(out) <- colnames(C)
  out[, item_code_proc := rownames(C)[exists]]
  out <- melt(out, id.vars = "item_code_proc", variable.name = "item_code",
    variable.factor = FALSE)
  out[, `:=`(item_code_proc = as.integer(item_code_proc),
    item_code = as.integer(item_code))]

  return(out)
}
