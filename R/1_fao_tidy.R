
replace_dt <- function(dt, value = 0, fun = is.na) {

  for(i in seq_len(ncol(dt)))
    set(dt, which(fun(dt[[i]])), i, value)

  0
}

merge_areas <- function(dt, orig, dest, pattern = "*") {

  orig_name <- dt[area_code == orig]$area[1]
  dest_name <- dt[area_code == dest]$area[1]
  cat("Found", dt[area_code == orig, .N], "observations of orig",
      "and", dt[area_code == dest, .N], "observations of dest\n")

  if(!all(grepl(pattern, c(orig_name, dest_name))))
    stop("Pattern not found in both countries")

  cat("Merging areas with codes", orig, "&", dest, "\n")

  set(dt, which(dt$area_code == orig), "area", dest_name)
  set(dt, which(dt$area_code == orig), "area_code", dest)

  0
}
