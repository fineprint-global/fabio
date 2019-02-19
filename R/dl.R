
# Download ----------------------------------------------------------------

# _zips for downloaded zip-files
# _files for the extracted data
#
# Download, unzip and adjust names

# FAO

fao_path <- "input/fao/"

fao_zips <- c(
  "crop_raw" = "Production_Crops_E_All_Data_(Normalized).zip",
  "crop_proc" = "Production_CropsProcessed_E_All_Data_(Normalized).zip",
  "live_raw" = "Production_Livestock_E_All_Data_(Normalized).zip",
  "live_prim" = "Production_LivestockPrimary_E_All_Data_(Normalized).zip",
  "live_proc" = "Production_LivestockProcessed_E_All_Data_(Normalized).zip",
  # "trade_1" = "Trade_Crops_Livestock_E_All_Data_(Normalized).zip",
  # "trade_2" = "Trade_LiveAnimals_E_All_Data_(Normalized).zip",
  "btd_raw" = "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip",
  "cbs_crop" = "CommodityBalances_Crops_E_All_Data_(Normalized).zip",
  "cbs_live" = "CommodityBalances_LivestockFish_E_All_Data_(Normalized).zip")
fao_dl <- paste0("http://fenixservices.fao.org/faostat/static/bulkdownloads/",
                 fao_zips)

for(i in 1:length(fao_dl)) {
  if(!file.exists(paste0(fao_path, fao_zips[i])))
    download.file(fao_dl[i],
                  paste0(fao_path, fao_zips[i]),
                  method = "auto")
}

fao_files <- vector("character", length(fao_zips))
for(i in 1:length(fao_dl)) {
  fao_files[i] <- unzip(paste0(fao_path, fao_zips[i]),
                        exdir = fao_path, overwrite = FALSE)
}
names(fao_files) <- names(fao_zips)

# FAO Fishery

# only "TS_FI_PRODUCTION.csv" will be extracted
fao_fish_zips <- c("fish_raw" = "GlobalProduction_2018.1.2.zip")
fao_fish_dl <- paste0("http://www.fao.org/fishery/static/Data/",
                      fao_fish_zips)

if(!file.exists(paste0(fao_path, fao_fish_zips)))
  download.file(fao_fish_dl,
                paste0(fao_path, fao_fish_zips),
                method = "auto")
fao_fish_files <- unzip(paste0(fao_path, fao_fish_zips),
                        exdir = fao_path, overwrite = FALSE,
                        files = "TS_FI_PRODUCTION.csv")
names(fao_fish_files) <- names(fao_fish_zips)


# Read --------------------------------------------------------------------

library(readr)

col_types <- list(
  "crop_raw" = cols(`Area Code` = col_double(), Area = col_character(),
                    `Item Code` = col_double(), Item = col_character(),
                    `Element Code` = col_double(), Element = col_character(),
                    `Year Code` = col_double(), Year = col_double(),
                    Unit = col_character(), Value = col_double(),
                    Flag = col_character()),
  "crop_proc" = cols(`Area Code` = col_double(), Area = col_character(),
                     `Item Code` = col_double(), Item = col_character(),
                     `Element Code` = col_double(), Element = col_character(),
                     `Year Code` = col_double(), Year = col_double(),
                     Unit = col_character(), Value = col_double(),
                     Flag = col_character()),
  "live_raw" = cols(`Area Code` = col_double(), Area = col_character(),
                    `Item Code` = col_double(), Item = col_character(),
                    `Element Code` = col_double(), Element = col_character(),
                    `Year Code` = col_double(), Year = col_double(),
                    Unit = col_character(), Value = col_double(),
                    Flag = col_character()),
  "live_prim" = cols(`Area Code` = col_double(), Area = col_character(),
                     `Item Code` = col_double(), Item = col_character(),
                     `Element Code` = col_double(), Element = col_character(),
                     `Year Code` = col_double(), Year = col_double(),
                     Unit = col_character(), Value = col_double(),
                     Flag = col_character()),
  "live_proc" = cols(`Area Code` = col_double(), Area = col_character(),
                     `Item Code` = col_double(), Item = col_character(),
                     `Element Code` = col_double(), Element = col_character(),
                     `Year Code` = col_double(), Year = col_double(),
                     Unit = col_character(), Value = col_double(),
                     Flag = col_character()),
  "btd_raw" = cols(`Reporter Country Code` = col_double(),
                   `Reporter Countries` = col_character(),
                   `Partner Country Code` = col_double(),
                   `Partner Countries` = col_character(),
                   `Item Code` = col_double(), Item = col_character(),
                   `Element Code` = col_double(), Element = col_character(),
                   `Year Code` = col_double(), Year = col_double(),
                   Unit = col_character(), Value = col_double(),
                   Flag = col_character()),
  "cbs_crop" = cols(`Area Code` = col_double(), Area = col_character(),
                    `Item Code` = col_double(), Item = col_character(),
                    `Element Code` = col_double(), Element = col_character(),
                    `Year Code` = col_double(), Year = col_double(),
                    Unit = col_character(), Value = col_double(),
                    Flag = col_character()),
  "cbs_live" = cols(`Area Code` = col_double(), Area = col_character(),
                    `Item Code` = col_double(), Item = col_character(),
                    `Element Code` = col_double(), Element = col_character(),
                    `Year Code` = col_double(), Year = col_double(),
                    Unit = col_character(), Value = col_double(),
                    Flag = col_character()),
  "fish_raw" = cols(COUNTRY = col_factor(), AREA = col_factor(),
                    SOURCE = col_factor(), SPECIES = col_factor(),
                    YEAR = col_integer(), UNIT = col_factor(),
                    QUANTITY = col_double(), SYMBOL = col_skip())
)

for(name in names(fao_files))
  assign(name,
         read_csv(fao_files[name], col_types = col_types[[name]]))

assign("fish_raw",
       read_csv(fao_fish_files["fish_raw"], col_types = col_types[["fish_raw"]]))

file.remove(c(fao_files, fao_fish_files))

for(name in c(names(fao_files), "fish_raw"))
  saveRDS(get(name), paste0(fao_path, name, ".rds"))

# regions <- read_csv("input/regions_all.csv")
