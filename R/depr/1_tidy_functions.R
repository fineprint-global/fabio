replace_dt <- function(dt, value = 0, fun = is.na) {

  for(i in seq_len(ncol(dt)))
    set(dt, which(fun(dt[[i]])), i, value)

  0
}


check_code <- function(x) {

  unique(x$Item)
  unique(x$`Item Code`)
}


merge_areas <- function(dt, orig, dest, pattern = "*", col = "area") {

  col_code <- paste0(col, "_code")

  orig_name <- dt[dt[[col_code]] == orig][[col]][1]
  dest_name <- dt[dt[[col_code]] == dest][[col]][1]
  cat("Found", dt[dt[[col_code]] == orig, .N], "observations of orig",
      "and", dt[dt[[col_code]] == dest, .N], "observations of dest.\n")

  if(!all(grepl(pattern, c(orig_name, dest_name))))
    stop("Pattern not found.")

  cat("Merging areas with codes ", orig, " & ", dest, ".\n", sep = "")

  set(dt, which(dt[[col_code]] == orig), col, dest_name)
  set(dt, which(dt[[col_code]] == orig), col_code, dest)

  0
}


rename_cols <- function(x, rename, drop = TRUE) {

  found_cols <- names(x) %in% names(rename)
  cat("Unspecified columns:",
      paste(names(x)[!found_cols], collapse = ", "), ".\n")
  if(drop) {
    cat("Dropping missing columns.\n")
    x <- subset(x, select = found_cols)
  }

  names(x) <- c(rename[names(x)])
  return(x)
}


missing_summary <- function(x, years) {

  missing <- data.frame(years = years)
  for(code in unique(x$area_code)) {
    missing_years <- which(!years %in% unique(x[area_code == code]$year))
    if(length(missing_years > 0))
      missing[missing_years, as.character(code)] <- TRUE
  }
  cat("Missing years found for the following regions:\n",
      paste(regions$name[match(names(missing)[-1], regions$code)],
            colSums(missing[, -1], na.rm = TRUE), collapse = ";\n"),
      ".\n", sep = "")
  cat("\nMissing regions found for the following years:\n",
      paste(years, rowSums(missing[, -1], na.rm = TRUE), collapse = ";\n"),
      ".\n", sep = "")

}
