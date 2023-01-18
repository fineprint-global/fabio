
# FAO data ----------------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
path_fao <- "input/fao/"


# Settings ----------------------------------------------------------------

files <- c(
  "prod" = "Production_Crops_Livestock_E_All_Data_(Normalized).zip", #"Production_Crops_E_All_Data_(Normalized).zip",
  #"crop_proc" = "Production_CropsProcessed_E_All_Data_(Normalized).zip",
  #"live_prod" = "Production_Livestock_E_All_Data_(Normalized).zip",
  #"live_prim" = "Production_LivestockPrimary_E_All_Data_(Normalized).zip",
  #"live_proc" = "Production_LivestockProcessed_E_All_Data_(Normalized).zip",
  # "trade_1" = "Trade_Crops_Livestock_E_All_Data_(Normalized).zip",
  "trad" = "Trade_CropsLivestock_E_All_Data_(Normalized).zip", #"Trade_LiveAnimals_E_All_Data_(Normalized).zip",
  "btd_prod" = "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
  #"cbs_crop" = "CommodityBalances_Crops_E_All_Data_(Normalized).zip",
  #"cbs_live" = "CommodityBalances_LivestockFish_E_All_Data_(Normalized).zip",
  "cbs_food_new" = "FoodBalanceSheets_E_All_Data_(Normalized).zip",
  "cbs_food_old" = "FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip",
  "cbs_nonfood" = "CommodityBalances_(non-food)_E_All_Data_(Normalized).zip",
  "sua" = "SUA_Crops_Livestock_E_All_Data_(Normalized).zip",
  #"fore_prod" = "Forestry_E_All_Data_(Normalized).zip",
  #"fore_trad" = "Forestry_Trade_Flows_E_All_Data_(Normalized).zip",
  "prices" = "Prices_E_All_Data_(Normalized).zip",
  "fish_prod" = "GlobalProduction_2022.1.1.zip") #"GlobalProduction_2019.1.0.zip")

# Files to extract from the ZIP archives
#extr <- c(rep(NA, length(files) - 1), "TS_FI_PRODUCTION.csv")
extr <- c(rep(NA, length(files) - 1), "Global_production_Quantity.csv")

name <- names(files)

# Links to the files
links <- c(rep("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
  length(files) - 1), "http://www.fao.org/fishery/static/Data/")

# Column types to possibly skip some
col_types <- list(
  "prod" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
             "character", "numeric", "numeric", "character", "numeric", "character"),
  "trad" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
             "character", "numeric", "numeric", "character", "numeric", "character"),
  "btd_prod" = c("numeric", "character", "character", "numeric", "character", "character", "numeric", "character", "character",
                 "numeric", "character", "numeric", "numeric", "character", "numeric", "character"),
  "cbs_food_new" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
                     "character", "numeric", "numeric", "character", "numeric", "character"),
  "cbs_food_old" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
                     "character", "numeric", "numeric", "character", "numeric", "character"),
  "cbs_nonfood"  = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
                     "character", "numeric", "numeric", "character", "numeric", "character"),
  "sua" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
            "character", "numeric", "numeric", "character", "numeric", "character"),
  "prices" = c("numeric", "character", "character", "numeric", "character", "character", "numeric",
               "character", "numeric", "numeric", "numeric", "character", "character", "numeric", "character"),
  "fish_prod" = c("integer", "character", "integer", "character", "character", "integer", "numeric", "NULL")
)

# update: add read_method as there are some issues in the trad csv file (probably a missing quote somewhere) that fread cannot deal with, but readr::read_csv can.
read_method = files
read_method[] <- "fread"
read_method[c("trad", "btd_prod")] <- "read_csv"

# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path_fao)


fa_extract(path_in = path_fao, files = files,
  path_out = path_fao, name = name, extr = extr, col_types = col_types, read_method = read_method,
  rm = FALSE)


# Add primary crop production ---------------------------------------------

# This file is no longer downloadable from the FAO and needs to be requested.
if(!file.exists(paste0(path_fao, "Production_Crops_Primary.zip"))) {
  stop("The file `Production_Crops_Primary.zip` is no longer available",
    "online. Please request the file and provide it to continue.")
}
x <- unzip(paste0(path_fao, "Production_Crops_Primary.zip"),
  exdir = gsub("(.*)/", "\\1", path_fao))
y <- fread(x,
  colClasses = c("character", "character", "numeric", "character", "numeric", "character",
                 "numeric", "character", "numeric", "numeric", "character", "character", "character"))
file.remove(x)
saveRDS(y, paste0(path_fao, "crop_prim_14.rds"))


# add non-public data from 2015 to 2019 (requested from FAO by Finn)
file <- "Production Fodder Crops 2015_2019 internal working system official data only.xlsx"
fodder19 <- as.data.table(openxlsx::read.xlsx(paste0(path_fao, file), cols = 1:9))
saveRDS(fodder19, paste0(path_fao, "crop_prim_19.rds"))


# download fbs-sua concordance into inst
#fa_dl(file = "", link = "https://fenixservices.fao.org/faostat/static/documents/SCL/FBS%20and%20SUA%20list.xlsx", path = "inst/" )
