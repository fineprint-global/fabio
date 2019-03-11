
# FAO data ----------------------------------------------------------------

library(data.table)
source("R/0_prep_functions.R")
path_fao <- "input/fao/"


# Settings ----------------------------------------------------------------

files <- c(
  "crop_raw" = "Production_Crops_E_All_Data_(Normalized).zip",
  "crop_proc" = "Production_CropsProcessed_E_All_Data_(Normalized).zip",
  "live_raw" = "Production_Livestock_E_All_Data_(Normalized).zip",
  "live_prim" = "Production_LivestockPrimary_E_All_Data_(Normalized).zip",
  "live_proc" = "Production_LivestockProcessed_E_All_Data_(Normalized).zip",
  # "trade_1" = "Trade_Crops_Livestock_E_All_Data_(Normalized).zip",
  # "trade_2" = "Trade_LiveAnimals_E_All_Data_(Normalized).zip",
  "btd_raw" = "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
  "cbs_crop" = "CommodityBalances_Crops_E_All_Data_(Normalized).zip",
  "cbs_live" = "CommodityBalances_LivestockFish_E_All_Data_(Normalized).zip",
  "for_raw" = "Forestry_E_All_Data_(Normalized).zip",
  "for_trad" = "Forestry_Trade_Flows_E_All_Data_(Normalized).zip",
  "fish_raw" = "GlobalProduction_2018.1.2.zip")

# Files to extract from the ZIP archives
extr <- c(rep("", length(files) - 1), "TS_FI_PRODUCTION.csv")

name <- names(files)

# Links to the files
links <- c(rep("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
               length(files) - 1),
           "http://www.fao.org/fishery/static/Data/")

# Column types to possibly skip some
col_types <- list(
  "crop_raw" = c("numeric", "character", "numeric", "character", "numeric",
                 "character", "numeric", "numeric", "character", "numeric",
                 "character"),
  "crop_proc" = c("numeric", "character", "numeric", "character", "numeric",
                  "character", "numeric", "numeric", "character", "numeric",
                  "character"),
  "live_raw" = c("numeric", "character", "numeric", "character", "numeric",
                 "character", "numeric", "numeric", "character", "numeric",
                 "character"),
  "live_prim" = c("numeric", "character", "numeric", "character", "numeric",
                  "character", "numeric", "numeric", "character", "numeric",
                  "character"),
  "live_proc" = c("numeric", "character", "numeric", "character", "numeric",
                  "character", "numeric", "numeric", "character", "numeric",
                  "character"),
  "btd_raw" = c("numeric", "character", "numeric", "character", "numeric",
                "character", "numeric", "character", "numeric", "numeric",
                "character", "numeric", "character"),
  "cbs_crop" = c("numeric", "character", "numeric", "character", "numeric",
                 "character", "numeric", "numeric", "character", "numeric",
                 "character"),
  "cbs_live" = c("numeric", "character", "numeric", "character", "numeric",
                 "character", "numeric", "numeric", "character", "numeric",
                 "character"),
  "for_raw" = c("numeric", "character", "numeric", "character", "numeric",
                "character", "numeric", "numeric", "character", "numeric",
                "character"),
  "for_trad" = c("numeric", "character", "numeric", "character", "numeric",
                 "character", "numeric", "character", "numeric", "numeric",
                 "character", "numeric", "character"),
  "fish_raw" = c("integer", "integer", "integer", "character", "integer",
                 "character", "numeric", "NULL")
)


# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path_fao)

fa_extract(zip = files, path = path_fao, name = name,
           extr = extr, col_types = col_types)
