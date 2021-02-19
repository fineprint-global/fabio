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
#' @param zip Path to the ZIP archive to extract from and to.
#' @param path_out Path to write RDS files to.
#' @param name Name of the RDS file to write.
#' @param extr File to extract from the ZIP archive. If not set and there are
#' multiple files only the first one will be extracted. Set to NULL or an empty
#' string to try extracting all. See \link[utils]{unzip} for further details.
#' @param col_types List of vectors with classes for \link[data.table]{fread}.
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
#'   "data/fao/GlobalProduction_2018.1.2.zip",
#'   "data/fao/",
#'   "fish_raw",
#'   extr = "TS_FI_PRODUCTION.csv"
#' )
#' }
fa_extract <- function(
  path_in, files, path_out, name, extr = NULL,
  col_types = NULL, stack = FALSE,
  rm = TRUE, v = TRUE, ...) {

  zip = paste0(path_in, files)
  dest_rds <- paste0(path_out, name, ".rds")

  if(length(zip) == 1 && length(extr) > 1 || is.null(extr)) {
    if(v) cat("Extracting multiple files from a single ZIP archive\n")
    csv <- unzip(zip, extr, exdir = gsub("(.*)/", "\\1", path_out))
  } else {
    if(v) cat("Extracting single files from multiple ZIP archives\n")
    csv <- vector("character", length(zip))
    for(i in seq_along(zip)) {
      if(is.na(extr[i]) || nchar(extr[i]) == 0) {
        extr[i] <- unzip(zip[i], list = TRUE)[[1]][1]
      }
      # if(file.info(zip[i])$size > 200000000) {
        csv[i] <- paste0(path_out, ifelse(!is.na(extr[i]), extr[i], sub("zip", "csv", zip[i])))
        if(grepl("\\(|\\)", zip[i])) file.rename(zip[i], gsub("\\(|\\)", "", zip[i]))
        decompress_file(path_out, gsub("\\(|\\)", "", files[i]))
      # } else { csv[i] <- unzip(zip[i], extr[i], exdir = gsub("(.*)/", "\\1", path_out)) }
    }
  }

  rds <- vector("list", length(csv))
  for(i in seq_along(csv)) {
    cat("Reading:", csv[i], "\n")
    rds[[i]] <- data.table::fread(csv[i], colClasses = col_types[[i]])
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


#' @title Extract zipped files >= 4GB
#' @description Extract files >= 4GB from a ZIP archive without truncation.
#'
#' @param directory Path to the folder containing the ZIP archive.
#' @param file File name of the ZIP archive.
#' @param .file_cache Allows to skip uncompression.
#'
#' @return Unzipped content of a ZIP archive.
#'
#' @examples
#' \dontrun{
#' decompress_file(
#'   "./input/fao/",
#'   "GlobalProduction_2018.1.2.zip"
#' )
#' }
#' @source https://stackoverflow.com/questions/42740206/r-possible-truncation-of-4gb-file
decompress_file <- function(directory, file, .file_cache = FALSE) {

  if (.file_cache == TRUE) {
    print("decompression skipped")
  } else {

    # Set working directory for decompression
    # simplifies unzip directory location behavior
    wd <- getwd()
    setwd(directory)

    # Run decompression
    decompression <-
      system2("unzip",
              args = c("-o", # include override flag
                       file),
              stdout = TRUE)

    # uncomment to delete archive once decompressed
    # file.remove(file)

    # Reset working directory
    setwd(wd); rm(wd)

    # Test for success criteria
    # change the search depending on
    # your implementation
    if (grepl("Warning message", tail(decompression, 1))) {
      print(decompression)
    }
  }
}
