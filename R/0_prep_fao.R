
# FAO data ----------------------------------------------------------------

library("data.table")
source("R/0_prep_functions.R")
path_fao <- "input/fao/"


# Settings ----------------------------------------------------------------

files <- c(
  "crop_prod" = "Production_Crops_E_All_Data_(Normalized).zip",
  "crop_proc" = "Production_CropsProcessed_E_All_Data_(Normalized).zip",
  "live_prod" = "Production_Livestock_E_All_Data_(Normalized).zip",
  "live_prim" = "Production_LivestockPrimary_E_All_Data_(Normalized).zip",
  "live_proc" = "Production_LivestockProcessed_E_All_Data_(Normalized).zip",
  # "trade_1" = "Trade_Crops_Livestock_E_All_Data_(Normalized).zip",
  "live_trad" = "Trade_LiveAnimals_E_All_Data_(Normalized).zip",
  "btd_prod" = "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
  "cbs_crop" = "CommodityBalances_Crops_E_All_Data_(Normalized).zip",
  "cbs_live" = "CommodityBalances_LivestockFish_E_All_Data_(Normalized).zip",
  "fore_prod" = "Forestry_E_All_Data_(Normalized).zip",
  "fore_trad" = "Forestry_Trade_Flows_E_All_Data_(Normalized).zip",
  "fish_prod" = "GlobalProduction_2019.1.0.zip")

# Files to extract from the ZIP archives
extr <- c(rep(NA, length(files) - 1), "TS_FI_PRODUCTION.csv")

name <- names(files)

# Links to the files
links <- c(rep("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
  length(files) - 1), "http://www.fao.org/fishery/static/Data/")

# Column types to possibly skip some
col_types <- list(
  "crop_prod" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "crop_proc" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "live_prod" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "live_prim" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "live_proc" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "live_trad" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "btd_prod" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "numeric", "character",
    "numeric", "character"),
  "cbs_crop" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "cbs_live" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "fore_prod" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"),
  "fore_trad" = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "character", "numeric", "numeric", "character",
    "numeric", "character"),
  "fish_prod" = c("integer", "integer", "integer", "character", "integer",
    "character", "numeric", "NULL")
)


# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path_fao)

fa_extract(zip = paste0(path_fao, files),
  path_out = path_fao, name = name, extr = extr, col_types = col_types)


# Add primary crop production ---------------------------------------------

# This file is no longer downloadable from the FAO and needs to be requested.

x <- unzip(paste0(path_fao, "Production_Crops_Primary.zip"),
  exdir = gsub("(.*)/", "\\1", path_out))
y <- fread(x,
  colClasses = c("numeric", "character", "numeric", "character", "numeric",
    "character", "numeric", "numeric", "character", "numeric", "character"))
file.remove(x)
saveRDS(y, paste0(path_fao, "crop_prim.rds"))
