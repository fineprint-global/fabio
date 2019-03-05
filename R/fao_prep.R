
# FAO data ----------------------------------------------------------------

library(readr) # 1.3.1
source("R/prep.R")
path <- "input/fao/"


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

# Column types to speed up readr::read_csv and skip some
col_types <- c(
  "crop_raw" = "dcdcdcddcdc",
  "crop_proc" = "dcdcdcddcdc",
  "live_raw" = "dcdcdcddcdc",
  "live_prim" = "dcdcdcddcdc",
  "live_proc" = "dcdcdcddcdc",
  "btd_raw" = "dcdcdcdcddcdc",
  "cbs_crop" = "dcdcdcddcdc",
  "cbs_live" = "dcdcdcddcdc",
  "for_raw" = "dcdcdcddcdc",
  "for_trad" = "dcdcdcdcddcdc",
  "fish_raw" = "ffffifd_"
)


# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path)

fa_extract(zip = files, path = path, name = name,
           extr = extr, col_types = col_types)
