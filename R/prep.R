
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
#'
#' @param zip ZIP archive to extract from.
#' @param path Path to read from and write to.
#' @param name Name of the RDS file to write.
#' @param extr File to extract from the ZIP archive. If not set and there are
#' multiple files only the first one will be extracted. Set to NULL or an empty
#' string to try extracting all. See \link[utils]{unzip} for further details.
#' @param col_types List of column types fed to \link[readr]{read_csv}.
#' @param col_names Column names fed to \link[readr]{read_csv}.
#' @param rm Whether to remove the extracted CSV files.
#' @param v Whether to be verbose.
#' @param ... Fed into \link[readr]{read_csv}.
#'
#' @return Vector with created RDS files.
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
  col_types = NULL, col_names = TRUE,
  rm = TRUE, v = TRUE) {

  dest_zip <- paste0(path, zip)
  dest_rds <- paste0(path, name, ".rds")

  csv <- vector("character", length(zip))
  for(i in seq_along(zip)) {
    if(is.null(extr[i]) || nchar(extr[i]) == 0)
      extr[i] <- unzip(dest_zip[i], list = TRUE)[[1]]
    csv[i] <- unzip(dest_zip[i], extr[i], exdir = path, overwrite = TRUE)
    if(v) cat("Unzipped file at:", csv[i], "\n")
  }

  rds <- vector("list", length(csv))
  for(i in seq_along(zip)) {
    cat("Reading: ", csv[i], "\n")
    rds[[i]] <- readr::read_csv(csv[i], col_types = col_types[[i]],
                                col_names = col_names[[i]], ...)
    saveRDS(rds[[i]], dest_rds[i])
  }

  if(rm) file.remove(csv)

  dest_rds
}
