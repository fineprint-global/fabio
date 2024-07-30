## reshape food balances to long ##

# this step is only an intermediary necessity, as long as faostat does not provide the normalized bulk version of fbs and sua


# FAO data ----------------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
source("R/01_tidy_functions.R")
path_fao <- "input/fao/"


# Settings ----------------------------------------------------------------

files <- c(
  "fbs_wide" = "FoodBalanceSheets_E_All_Data.zip",
  "sua_wide" = "SUA_Crops_Livestock_E_All_Data.zip"
)

# Files to extract from the ZIP archives
#extr <- c(rep(NA, length(files) - 1), "TS_FI_PRODUCTION.csv")
extr <- rep(NA, length(files))

name <- names(files)

# Links to the files
links <- rep("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
               length(files) )

# Column types to possibly skip some
col_types <- list(
  "fbs_wide" = c("numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "logical", "numeric", "character", "logical", "numeric", "character", "logical", 
                 "numeric", "character", "logical", "numeric", "character", "logical", "numeric", "character", "logical", 
                 "numeric", "character", "logical", "numeric", "character", "logical", "numeric", "character", "logical", 
                 "numeric", "character", "logical", "numeric", "character", "logical", "numeric", "character", "logical", 
                 "numeric", "character", "logical"),
  "sua_wide" = c("numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "character")
)

# update: add read_method as there are some issues in the trad csv file (probably a missing quote somewhere) that fread cannot deal with, but readr::read_csv can.
read_method = files
read_method[] <- "fread"
#read_method[c("trad", "btd_prod")] <- "read_csv"

# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path_fao)


fa_extract(path_in = path_fao, files = files,
           path_out = path_fao, name = name, extr = extr, col_types = col_types, read_method = read_method,
           rm = FALSE)

# Reshape ------------------------------------------------------------------
sua <- readRDS("input/fao/sua_wide.rds")
fbs <- readRDS("input/fao/fbs_wide.rds")

sua <- sua[, !grepl("Y.*F", names(sua)) & !grepl("Y.*N", names(fbs)), with = FALSE ]
fbs <- fbs[, !grepl("Y.*F", names(fbs)) & !grepl("Y.*N", names(fbs)), with = FALSE ]

sua <- melt(sua, id.vars = names(sua)[1:9], variable.name = "Year", value.name = "Value", variable.factor = FALSE)
fbs <- melt(fbs, id.vars = names(fbs)[1:9], variable.name = "Year", value.name = "Value", variable.factor = FALSE)

sua[, Year := as.numeric(substr(Year, 2, nchar(Year)))]
fbs[, Year := as.numeric(substr(Year, 2, nchar(Year)))]

sua <- dt_replace(sua, cols = "Value")
fbs <- dt_replace(fbs, cols = "Value")

# # compare to normalized data until 2019
# sua_norm <- readRDS("~/fabio/input/fao/sua.rds")
# fbs_norm <- readRDS("~/fabio/input/fao/cbs_food_new.rds")
#
# sua <- merge(sua, sua_norm, by = names(sua)[1:10], suffixes = c("","_norm"), all = TRUE)
# fbs <- merge(fbs, fbs_norm, by = names(fbs)[1:10], suffixes = c("","_norm"), all = TRUE)
#
# sua[, diff := Value - Value_norm]
# fbs[, diff := Value - Value_norm]
#
# # values match exactly
# sua[, .(max_diff = max(diff, na.rm = TRUE)), by = Year]
# fbs[, .(max_diff = max(diff, na.rm = TRUE)), by = Year]
# max(sua[is.na(diff) & Year <2020 ]$Value)
# max(sua[is.na(diff) & Year <2020 ]$Value_norm, na.rm = TRUE)
# max(fbs[is.na(diff) & Year <2020 ]$Value)
# max(fbs[is.na(diff) & Year <2020 ]$Value_norm, na.rm = TRUE)
#
# # remove comparison
# sua[, `:=`(Value_norm = NULL, diff = NULL, Flag = NULL, `Year Code` = NULL)]
# fbs[, `:=`(Value_norm = NULL, diff = NULL, Flag = NULL, `Year Code` = NULL)]

# save
sua[, `Year Code` := Year]
fbs[, `Year Code` := Year]

saveRDS(sua, file = "input/fao/sua.rds")
saveRDS(fbs, file = "input/fao/cbs_food_new.rds")
