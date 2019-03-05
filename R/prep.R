
#' @title FABIO download
#'
#' @param file Files to download.
#' @param link Link(s) to download x from.
#' @param path Path to download to.
#' @param v Whether to be verbose.
#'
#' @return Vector of \link[utils]{download.file}'s values.
#'
#' @examples
#' \dontrun{
#' fa_dl(
#'   "Production_Crops_E_All_Data_(Normalized).zip",
#'   "http://fenixservices.fao.org/faostat/static/bulkdownloads/",
#'   "data/fao/"
#' )
#' }
fa_dl <- function(
  file, link, path, v = TRUE) {

  dl <- paste0(link, file)
  dest <- paste0(path, file)

  out <- vector("integer", length(file))
  for(i in seq_along(file)) {
    if(!file.exists(dest[i])) {
      out[i] <- download.file(dl[i], dest[i], method = "auto")
    } else if(v) cat("Skipping download, already found:", file[i], "\n")
  }

  out
}

#' @title FABIO extract to RDS
#' @description Extract CSV files from a ZIP archive and convert them to RDS.
#' Handles multiple ZIP files with one CSV each or a single ZIP with multiple
#' CSV files.
#'
#' @param zip ZIP archive to extract from.
#' @param path Path to read from and write to.
#' @param name Name of the RDS file to write.
#' @param extr File to extract from the ZIP archive. If not set and there are
#' multiple files only the first one will be extracted. Set to NULL or an empty
#' string to try extracting all. See \link[utils]{unzip} for further details.
#' @param colClasses List of vectors with classes for \link[data.table]{fread}.
#' @param stack Whether to stack the CSV files via \link[data.table]{rbindlist}.
#' @param rm Whether to remove the extracted CSV files.
#' @param v Whether to be verbose.
#' @param ... Fed into \link[utils]{unzip}.
#'
#' @return Vector with created RDS files.
#'
#' @importFrom data.table fread rbindlist
#'
#' @examples
#' \dontrun{
#' fa_extract(
#'   "GlobalProduction_2018.1.2.zip",
#'   "data/fao/",
#'   "fish_raw",
#'   extr = "TS_FI_PRODUCTION.csv"
#' )
#' }
fa_extract <- function(
  zip, path, name, extr = NULL,
  colClasses = NULL, stack = FALSE,
  rm = TRUE, v = TRUE, ...) {

  dest_zip <- paste0(path, zip)
  dest_rds <- paste0(path, name, ".rds")

  if(length(zip) == 1 && length(extr) > 1 || is.null(extr)) {
    if(v) cat("Extracting multiple files from a single ZIP archive\n")
    csv <- unzip(dest_zip, extr, exdir = path, ...)
  } else {
    if(v) cat("Extracting single files from multiple ZIP archives\n")
    csv <- vector("character", length(zip))
    for(i in seq_along(zip)) {
      if(is.null(extr[i]) || nchar(extr[i]) == 0)
        extr[i] <- unzip(dest_zip[i], list = TRUE)[[1]]
      csv[i] <- unzip(dest_zip[i], extr[i], exdir = path, ...)
    }
  }

  rds <- vector("list", length(csv))
  for(i in seq_along(csv)) {
    cat("Reading:", csv[i], "\n")
    rds[[i]] <- data.table::fread(csv[i], colClasses = colClasses[[i]])
  }

  if(stack) {
    if(v) cat("Stacking CSV files via data.table::rbindlist()")
    saveRDS(data.table::rbindlist(rds), dest_rds)
  } else {
    for(i in seq_along(csv)) saveRDS(rds[[i]], dest_rds[i])
  }

  if(rm) file.remove(csv)

  dest_rds
}
