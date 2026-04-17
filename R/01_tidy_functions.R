
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
    if(all(na_codes >= 420)) {
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
    n_groups <- x[idx >= 5000 | idx %in% c(269, 268, 266, 261, 420), .N]
    cat("Found", n_groups, "observations of grouped areas.\n")
    if(n_groups > 0) {
      cat("Removing observations of:\n\t",
          paste0("'", unique(x[[col_name]][idx >= 5000 |
                                             idx %in% c(269, 268, 266, 261, 420)]), "'", collapse = ", "),
          ".\n", sep = "")
    }
    x <- x[idx < 5000 & ! idx %in% c(269, 268, 266, 261, 420), ]
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
flow_pref <- function(x, pref = "Import", pure = FALSE) {

  x[, id := paste(from_code, to_code, item_code, year, sep = "_")]

  if(pure == TRUE){
    to_kick <- x[imex != pref, id]
  } else {
    to_kick <- x[imex != pref & id %in% x[imex == pref, id], id]
  }
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

# Extension functions --------------
agg_sua_to_cbs <- function(dt, value_col = "value", cnc = conc, itms = items,
                           agg_method = "sum", weight_col = NULL) {
  dt[, item_code_cbs := conc$item_code_cbs[match(item_code, conc$item_code_sua)]]
  
  if (agg_method == "sum") {
    dt <- dt[, .(value = sum(.SD[[value_col]], na.rm = TRUE)),
             by = .(area_code, year, item_code_cbs)]
  } else {
    dt[, total_weight := sum(.SD[[weight_col]], na.rm = TRUE),
       by = .(area_code, year, item_code_cbs)]
    dt[, weight_share := .SD[[weight_col]] / total_weight]
    dt <- dt[, .(value = sum(.SD[[value_col]] * weight_share, na.rm = TRUE)),
             by = .(area_code, year, item_code_cbs)]
  }
  dt[, `:=` (area = regions$name[match(area_code, regions$code)],
             item = items_cbs$item[match(item_code_cbs, items_cbs$item_code)],
             comm_code = items_cbs$comm_code[match(item_code_cbs, items_cbs$item_code)])]
  
  return(dt)
}

format_extension <- function(dt, yrs = years, reg = regions, itms = items,
                             value_col = "value") {
  
  template <- CJ(year = yrs, area_code = reg$code, 
                 comm_code = itms$comm_code)  
  template[, iso3c := reg$iso3c[match(area_code, reg$code)]]  
  result_list <- lapply(yrs, function(yr) {
    
    tmpl_yr <- template[year == yr]
    dt_yr <- dt[year == yr]
    
    vals <- dt_yr[[value_col]]
    
    values <- vals[match(paste(tmpl_yr$area_code, tmpl_yr$comm_code),
                         paste(dt_yr$area_code, dt_yr$comm_code))]
    values[is.na(values)] <- 0
    
    col_names <- paste(tmpl_yr$iso3c, tmpl_yr$comm_code, sep = "_")
    
    matrix(values, nrow = 1, dimnames = list(NULL, col_names))
  })
  
  setNames(result_list, yrs)
}

# function to convert environmental extension back into list (or long table) that is by 
# environmental pressure, not by year
unformat_extension <- function(data, ext_names, long = TRUE) {
  if (long) {
    out <- rbindlist(
      lapply(ext_names, function(p) {
        rbindlist(
          lapply(names(data[[p]]), function(yr) {
            mat <- data[[p]][[yr]]
            data.table(
              year    = as.integer(yr),
              col_key = colnames(mat),
              value   = as.numeric(mat)
            )
          })
        )[, ext := p]
      })
    )
    out[, `:=`(iso3c = substr(col_key, 1, 3), comm_code = substr(col_key, 5, 8))]
    out[, col_key := NULL]
    setcolorder(out, c("year", "iso3c", "comm_code", "ext", "value"))
    return(out)
  } else {
    out <- lapply(ext_names, function(p) {
      rbindlist(
        lapply(names(data[[p]]), function(yr) {
          mat <- data[[p]][[yr]]
          dt <- data.table(
            year      = as.integer(yr),
            col_key   = colnames(mat),
            value     = as.numeric(mat)
          )
          dt[, `:=`(iso3c = substr(col_key, 1, 3), comm_code = substr(col_key, 5, 8))]
          dt[, col_key := NULL]
          setcolorder(dt, c("iso3c", "comm_code", "year", "value"))
          dt
        })
      )
    })
    names(out) <- ext_names
    return(out)
  }
}

set_co2eq <- function(nm, lst, gwp) {
  dt <- copy(lst[[nm]])
  dt[, GWP := gwp[nm]]
  dt[, value := value * GWP]
  dt
}

# This function adds up all co2eq totals from different ghgs
merge_gwp_lists <- function(prefix) {
  
  matching_names <- ls(envir = .GlobalEnv, pattern = paste0("^", prefix, "_"))
  
  dt_list <- list()
  
  for (list_name in matching_names) {
    gwp_list <- get(list_name, envir = .GlobalEnv)
    
    # Find the element whose name ends in one of the gas names (these are the co2eq for each gas)
    gas <- grep(paste0(gases, "$", collapse = "|"), names(gwp_list), value = TRUE)
    
    dt <- gwp_list[[gas]]
    
    if (is.data.table(dt)) {
      dt_list[[list_name]] <- dt
    }
  }
  
  combined <- rbindlist(dt_list, use.names = TRUE)
  gwp_total <- combined[, .(value = sum(value, na.rm = TRUE)), 
                        by = .(area_code, year, item_code)]
  
  return(gwp_total)
}

# this function renames columns from lc-impact and functional diversity biodiversity
# characterization factors from BAMBOO
tidy_cfs <-  function(dt, cols_to_remove = cols_remove, old_nms = old_names, 
                      new_nms = new_names){
  dt[, intersect(cols_to_remove, names(dt)) := NULL]
  setnames(dt, old_nms, new_nms, skip_absent = TRUE)
  
  # deal with Sudan and Netherlands Antilles
  if (!"SSD" %in% dt$iso3c && "SDN" %in% dt$iso3c) {
    sudan_rows <- copy(dt[iso3c == "SDN"])
    sudan_rows[, `:=`(iso3c = "SSD", area = "South Sudan")]
    dt <- rbind(dt, sudan_rows)
  }
  
  if (!"ANT" %in% dt$iso3c && "ATG" %in% dt$iso3c) {
    nant_rows <- copy(dt[iso3c == "ATG"])
    nant_rows[, `:=`(iso3c = "ANT", area = "Netherlands Antilles")]
    dt <- rbind(dt, nant_rows)
  }
  
  dt
}



# this function merges lc-impact pressures and impacts (can be used for lc-impact directly
# and for ecosystem functionality CFs) merges by iso3c for non-climate CFs and 
# adds all climate CFs with cbind
# needs wide tables with pressure types in column names as inputs (1 row for climate,
# 187 rows for non-climate)
merging_pressures_impacts <- function(pressures, dt, climate_dt = NULL) {
  impacts <- lapply(names(pressures), function(nm) {
    cols <- c("iso3c", colnames(dt)[grepl(nm, colnames(dt))])
    merged <- if(length(cols) > 1) {
      merge(pressures[[nm]], dt[, ..cols], by = "iso3c", all.x = TRUE, sort = FALSE)
    } else {
      pressures[[nm]]
    }
    
    if(!is.null(climate_dt) && nm %like% "ghg") {
      climate_cols <- colnames(climate_dt)[grepl(nm, colnames(climate_dt))]
      if(length(climate_cols) > 0) {
        merged <- cbind(merged, climate_dt[, ..climate_cols])
      }
    }
    
    #setnames(merged, names(merged), gsub(paste0(nm, "_"), "", names(merged)))
    merged
  })
  names(impacts) <- names(pressures)
  impacts
}

# this function takes an output list from "merging_pressures_impacts" and multiplies
# pressures with impacts
multiply_pressures_impacts <- function(impacts_list, key_cols) {
  invisible(lapply(names(impacts_list), function(nm) {
    dt <- impacts_list[[nm]]
    cols <- setdiff(colnames(dt), key_cols)
    dt[, (cols) := lapply(.SD, function(x) x * value), .SDcols = cols]
    dt[, value := NULL]
    setnames(dt, cols, gsub("cf_", "", cols))
  }))
}

# this function aggregates impacts from a wide table within the same impact categories
# and realms

aggregate_impact_categories <- function(impacts, impact_categories, realms) {
  invisible(lapply(impact_categories, function(cat) {
    cols <- colnames(impacts)[colnames(impacts) %like% cat]
    if(length(cols) > 0) {
      impacts[, (cat) := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
      impacts[, (cols) := NULL]
    }
  }))
  
  invisible(lapply(realms, function(r) {
    cols <- colnames(impacts)[colnames(impacts) %like% r]
    if(length(cols) > 0) {
      impacts[, (r) := rowSums(.SD, na.rm = TRUE), .SDcols = cols]
    }
  }))
}

# This function compiles the individual extensions into one big one
compile_extension <- function(data, files, yrs = years) {
  result <- lapply(yrs, function(yr) {
    do.call(rbind, lapply(data, function(x) x[[as.character(yr)]]))
  })
  names(result) <- yrs
  row_names <- tools::file_path_sans_ext(basename(files))
  for (yr in as.character(yrs)) {
    rownames(result[[yr]]) <- row_names
  }
  result
}

read_excel_sheets <- function(filename, sheets = NULL) {
  all_sheets <- readxl::excel_sheets(filename)
  
  if (!is.null(sheets)) {
    invalid <- setdiff(sheets, all_sheets)
    if (length(invalid) > 0) {
      stop("The following sheets were not found: ", paste(invalid, collapse = ", "))
    }
    sheets_out <- intersect(all_sheets, sheets)  
  } else {
    sheets_out <- all_sheets
  }
  
  x <- lapply(sheets_out, function(X) readxl::read_excel(filename, sheet = X))
  x <- lapply(x, as.data.table)
  names(x) <- sheets_out
  
  x
}
