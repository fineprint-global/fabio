
dt_rename <- function(x, rename, drop = TRUE) {

  found <- names(x)[names(x) %in% names(rename)]
  not_found <- names(x)[!names(x) %in% names(rename)]

  if(length(not_found) > 0) {
    cat("Unspecified columns:\n\t",
        paste(not_found, collapse = ", "), ".\n", sep = "")
    if(drop) {
      cat("Dropping unspecified columns.\n")
      x <- subset(x, select = found)
    }
  }

  names(x) <- c(rename[names(x)])

  x
}


dt_replace <- function(x, cond = is.na, value = 0) {

  for(col in seq_len(ncol(x))) {
    set(x, i = which(fun(x[[i]])), j = col, value)
  }

}


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
  cat("Removing ", x[!r, .N], " observations via `", deparse(e), "`.\n",
      if(na.rm) {"Included"} else {"Excluded"}, " were a total of ", na_count,
      " NA values.\n", sep = "")
-
  return(x[r, vars, with = FALSE])
}


# Area adjustments --------------------------------------------------------


area_fix <- function(x, regions, col = "area") {

  col_code <- paste0(col, "_code")

  matched <- match(x[[col_code]], regions[["code"]])
  if(any(is.na(matched))) {
    na_codes <- unique(x[[col_code]][is.na(matched)])
    if(all(na_codes >= 5000)) {
      message("Found no match for grouped areas:\n\t",
              paste0(unique(x[[col]][is.na(matched)]), " - ",
                     na_codes, collapse = ", "),
              ".\n", "")
    } else {
      stop("Found no match for:\n\t",
           paste0(unique(x[[col]][is.na(matched)]), " - ",
                  na_codes, collapse = ", "),
           ".\n")
    }
  }
  x[[col]] <- regions[matched, name]

  return(x)
}


area_kick <- function(x, code, col = "area_code", pattern = "*", groups = TRUE) {

  # Vector to use for subsetting
  idx <- x[[col]]

  n_found <- x[idx == code, .N]
  cat("Found", n_found, "observations of `code`.\n")

  if(n_found > 0) {
    # Check names of code
    col_name <- gsub("(.*)_code", "\\1", col)
    if(col_name %in% colnames(x)) {
      name <- x[idx == code, col_name, with = FALSE][1][[1]]
      if(pattern != "*" && !grepl(pattern, name)) {
        stop("Pattern not found.\n")
      }
      cat("Removing observations of ", name, " from the table.\n", sep = "")
    } else {
      message("Column with names not found. Skipping pattern-check.\n")
      cat("Removing observations of area ", code, " from the table.\n", sep = "")
    }
  }

  # Remove country groups
  if(groups) {
    n_groups <- x[idx >= 5000, .N]
    cat("Found", n_groups, "observations of grouped areas.\n")
    if(n_groups > 0) {
      cat("Removing observations of:\n\t",
          paste0(unique(x[area_code >= 5000, area]), collapse = ", "),
          ".\n", sep = "")
    }
    return(x[idx != code & idx < 5000, ])
  }

  return(x[idx != code, ])
}


area_merge <- function(x, orig, dest, col = "area_code", pattern = "*") {

  # Vector to use for subsetting
  idx <- x[[col]]

  cat("Found", x[idx == orig, .N], "/", x[idx == dest, .N],
      "observations of `orig` / `dest`.\n")

  # Check names of origin and destination
  col_name <- gsub("(.*)_code", "\\1", col)
  if(col_name %in% colnames(x)) {
    orig_name <- x[idx == orig, col_name, with = FALSE][1][[1]]
    dest_name <- x[idx == dest, col_name, with = FALSE][1][[1]]
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


tcf_apply <- function(x, na.rm = TRUE, replacement = NULL) {

  n_na <- sum(is.na(x[["tcf"]]))
  if(n_na > 0) {
    cat("No conversion factors found for:\n\t",
        paste0(unique(x[is.na(tcf), item]), collapse = ", "),
        ".\n", sep = "")
    if(na.rm) {
      cat("Dropping", n_na, "missing values.\n")
      x <- x[!is.na(tcf), ]
    } else if(!is.null(replacement)) {
      cat("Filling ",  n_na, " missing values with ",
          replacement, ".\n", sep = "")
      x[is.na(tcf), tcf := replacement]
    }
  }

  return(x)
}
