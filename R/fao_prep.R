
source("R/prep.R")


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
  "fish_raw" = "GlobalProduction_2018.1.2.zip")

# Files to extract from the ZIP archives
extr <- c(rep("", length(files) - 1), "TS_FI_PRODUCTION.csv")

# Links to the files
links <- c(rep("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
               length(files) - 1),
           "http://www.fao.org/fishery/static/Data/")

# Path to store at
path <- "input/fao/"

# Column types to speed up readr::read_csv and skip some
library(readr)
{col_types <- list(
  "crop_raw" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "crop_proc" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),

  "live_raw" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "live_prim" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "live_proc" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "btd_raw" = cols(
    `Reporter Country Code` = col_double(), `Reporter Countries` = col_character(),
    `Partner Country Code` = col_double(), `Partner Countries` = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "cbs_crop" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "cbs_live" = cols(
    `Area Code` = col_double(), Area = col_character(),
    `Item Code` = col_double(), Item = col_character(),
    `Element Code` = col_double(), Element = col_character(),
    `Year Code` = col_double(), Year = col_double(),
    Unit = col_character(), Value = col_double(),
    Flag = col_character()
  ),
  "fish_raw" = cols(
    COUNTRY = col_factor(), AREA = col_factor(),
    SOURCE = col_factor(), SPECIES = col_factor(),
    YEAR = col_integer(), UNIT = col_factor(),
    QUANTITY = col_double(), SYMBOL = col_skip()
  )
)}



# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path)

fa_extract(zip = files, path = path, name = names(files),
           extr = extr, col_types = col_types)
