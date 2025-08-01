library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")
path_fao <- "input/fao/"
items <- fread("inst/items_full_123.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]


# NPK synthetic fertilizer application-----------------------------------

HFUBC <- fread("./input/extensions/HFUBC_fert_application_by_crop.csv")
HFUBC <- HFUBC[,.(Country, ISO3_code, Year, Crop, Crop_area_k_ha, N_k_t, P2O5_k_t, K2O_k_t )]
HFUBC <- HFUBC[Year %in% c("1990/91","1991/92","1992/93", "1989/90", "1999/2000", "1998/99", "1997-98"), 
               Year := substr(Year, 1, 4)]                         #convert fertilizer years to calendar years where necessary
HFUBC[, `:=` (P_tons = P2O5_k_t* 0.436, K_tons = K2O_k_t * 0.83, N_tons = N_k_t)] #convert to elemental P and K
HFUBC[, `:=` (P2O5_k_t = NULL, N_k_t = NULL, K2O_k_t = NULL)]
HFUBC[, ':=' (N_rate = N_tons/Crop_area_k_ha, P_rate = P_tons/Crop_area_k_ha, 
              K_rate = K_tons/Crop_area_k_ha, year = as.integer(Year))][, `:=` (N_tons =NULL, P_tons = NULL, 
                                                                                K_tons = NULL, Country = NULL,
                                                                                Year = NULL)] #rates per 1000ha
HFUBC[!is.finite(N_rate), N_rate := 0]
HFUBC[!is.finite(P_rate), P_rate := 0]
HFUBC[!is.finite(K_rate), K_rate := 0] 

HFUBC <- HFUBC[ year %in% years,]

HFUBC_full <- CJ(name = regions$name,
                 item = items[group == "Primary crops", item], 
                 year = years)
HFUBC_full[, `:=` (region = regions$region[match(name, regions$name)], 
                   iso3c = regions$iso3c[match(name, regions$name)])]


conc <- fread("./inst/conc_HFUBC_items.csv")

HFUBC_full <- merge(HFUBC_full, conc, by = "item", allow.cartesian =TRUE)
HFUBC_full <- merge(HFUBC_full, HFUBC, by.x = c("iso3c", "year", "crop"), 
                    by.y = c("ISO3_code", "year", "Crop") , all =TRUE)
HFUBC_full <- HFUBC_full[!is.na(item),]

HFUBC_full[, area_share := Crop_area_k_ha/sum(Crop_area_k_ha, na.rm = TRUE), 
           by = .(item, iso3c, year)]
HFUBC_full[, sum_area_k_ha := sum(Crop_area_k_ha, na.rm = TRUE)] # Sum of crop area
HFUBC_full[, area_share := Crop_area_k_ha/sum(Crop_area_k_ha, na.rm = TRUE), 
           by = .(item, iso3c, year)]
HFUBC_full <- HFUBC_full[, .(
  N_rate = sum(N_rate * area_share, na.rm = TRUE) / sum(area_share, na.rm = TRUE) * 1000,  #doesn't group the area shares properly without numerators
  P_rate = sum(P_rate * area_share, na.rm = TRUE) / sum(area_share, na.rm = TRUE) * 1000,  
  K_rate = sum(K_rate * area_share, na.rm = TRUE) / sum(area_share, na.rm = TRUE) * 1000,  
  region = unique(region),  
  name = unique(name),
  area_k_ha = unique(sum_area_k_ha)
), by = .(item, iso3c, year)] #6 crops that are not in conc get lost somewhere -> add them back as NAs
HFUBC_full[, (names(HFUBC_full)) := lapply(.SD, function(x) fifelse(is.nan(x), NA, x))]


HFUBC_avail <- HFUBC_full[!is.na(N_rate) & !is.nan(N_rate)][, region := regions$region[match(iso3c, regions$iso3c)]]

# # NPKGrids data processing -> this takes several hours to run-------------------
# library(terra)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# 
# # Define directories for fertilizer and area datasets
# nc_fert_dir <- "./input/extensions/NPKGrids"
# nc_area_dir <- "./input/extensions/Cropgrids"
# nc_fert_files <- list.files(nc_fert_dir, pattern = "\\.nc$", full.names = TRUE)
# nc_area_files <- list.files(nc_area_dir, pattern = "\\.nc$", full.names = TRUE)
# 
# # # Load global country boundaries (WGS84) with ISO3 codes
# # countries <- ne_countries(scale = "medium", returnclass = "sf")[, c("iso_a3", "geometry")]
# 
# # Initialize empty data.tables to store summary statistics
# 
# crop_summary_N <- data.table()
# harv_summary_N <- data.table()
# 
# crop_summary_P <- data.table()
# harv_summary_P <- data.table()
# 
# crop_summary_K <- data.table()
# harv_summary_K <- data.table()
# 
# crop_summary_area <- data.table()
# harv_summary_area <- data.table()
# 
# countries_in_order <- readRDS("./input/extensions/countries_in_order.RDS") # generated once below
# 
# # Process each pair of NetCDF files (one for fertilizer and one for crop area)
# for (i in seq_along(nc_fert_files)) {
#   nc_fert <- nc_fert_files[i]
#   nc_area <- nc_area_files[i]
# 
#   # Read the fertilizer and area NetCDF files
#   r_fert <- rast(nc_fert)
#   r_area <- rast(nc_area)
# 
#   # Specify bands for fertilizer and area (crop_area and crop_harvest)
#   bands_fert <- c(1, 4, 7)  # Fertilizer bands
#   bands_area <- c(1, 2)     # crop_area and crop_harvest
# 
#   # Helper function to process a band (for both fertilizer and area data)
#   process_band <- function(r, band) {
#     var <- r[[band]]
#     band_name <- names(r)[band]
#     var_dt <- as.data.table(as.data.frame(var, xy = TRUE, na.rm = TRUE))
#     setnames(var_dt, c("lon", "lat", band_name))  # Use band name directly
#     return(var_dt)
#   }
# 
#   # Process fertilizer bands and crop area bands
#   fert_rate_list <- lapply(bands_fert, function(band) process_band(r_fert, band))
#   area_rate_list <- lapply(bands_area, function(band) process_band(r_area, band))
# 
#   # Convert lists to one data.table
#   fert_rate_dt <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE), fert_rate_list)
#   area_rate_dt <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE), area_rate_list)
# 
#   # Combine fertilizer data, crop area data
#   data_crop <- cbind(fert_rate_dt, area_rate_dt[, !"lon", with = FALSE][, !"lat", with = FALSE][,!"harvarea", with = FALSE])
#   data_harv <- cbind(fert_rate_dt, area_rate_dt[, !"lon", with = FALSE][, !"lat", with = FALSE][,!"croparea", with = FALSE])
# 
#   # # The following is only done once to get a dt with countries in the right order
# 
#   # # Convert the combined data.table to an sf object
#   # data <- st_as_sf(combined_data, coords = c("lon", "lat"), crs = 4326)
#   # Assign ISO3 country codes
#   # data <- st_join(data, countries, join = st_intersects) # take countries from here to make a dataset in the right order and then cbind with dataset
#   # setDT(data)
#   # data[, .(iso_a3)]
#   # saveRDS(data, "./input/countries_in_order.RDS" )
# 
#   data_crop <- cbind(countries_in_order, data_crop)
#   data_harv <- cbind(countries_in_order, data_harv)
# 
#   # Remove rows with missing country codes or non-positive areas
# 
#   data_crop_N <- data_crop[!is.na(iso_a3) & croparea > 0 & Nrate != -1]
#   data_harv_N <- data_harv[!is.na(iso_a3) & harvarea > 0 & Nrate != -1]
# 
#   data_crop_P <- data_crop[!is.na(iso_a3) & croparea > 0 & P2O5rate != -1]
#   data_harv_P <- data_harv[!is.na(iso_a3) & harvarea > 0 & P2O5rate != -1]
# 
#   data_crop_K <- data_crop[!is.na(iso_a3) & croparea > 0 & K2Orate != -1]
#   data_harv_K <- data_harv[!is.na(iso_a3) & harvarea > 0 & K2Orate != -1]
# 
#   data_crop_area <- data_crop[!is.na(iso_a3) & croparea >= 0]
#   data_harv_area <- data_harv[!is.na(iso_a3) & harvarea >= 0]
#   
#   # Calculate weighted averages for N
#   # cropland
#   summary_data_crop_N <- data_crop_N[, .(
#     min_N = min(Nrate, na.rm = TRUE),
#     max_N = max(Nrate, na.rm = TRUE),
#     weighted_N = sum(Nrate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   summary_data_crop_N[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   #harv land
#   summary_data_harv_N <- data_harv_N[, .(
#     min_N = min(Nrate, na.rm = TRUE),
#     max_N = max(Nrate, na.rm = TRUE),
#     weighted_N = sum(Nrate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   # Add crop name from file name
#   summary_data_harv_N[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   # Append to the overall summary table
#   crop_summary_N <- rbind(crop_summary_N, summary_data_crop_N, fill = TRUE)
#   harv_summary_N <- rbind(harv_summary_N, summary_data_harv_N, fill = TRUE)
# 
#   # Calculate weighted averages for P
#   #cropland
#   summary_data_crop_P <- data_crop_P[, .(
#     min_P = min(P2O5rate, na.rm = TRUE),
#     max_P = max(P2O5rate, na.rm = TRUE),
#     weighted_P = sum(P2O5rate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   summary_data_crop_P[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   # harvland
#   summary_data_harv_P <- data_harv_P[, .(
#     min_P = min(P2O5rate, na.rm = TRUE),
#     max_P = max(P2O5rate, na.rm = TRUE),
#     weighted_P = sum(P2O5rate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   # Add crop name from file name
#   summary_data_harv_P[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   # Append to the overall summary table
#   crop_summary_P <- rbind(crop_summary_P, summary_data_crop_P, fill = TRUE)
#   harv_summary_P <- rbind(harv_summary_P, summary_data_harv_P, fill = TRUE)
# 
# 
#   # Calculate weighted averages for K
#   # cropland
#   summary_data_crop_K <- data_crop_K[, .(
#     min_K = min(K2Orate, na.rm = TRUE),
#     max_K = max(K2Orate, na.rm = TRUE),
#     weighted_K = sum(K2Orate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   summary_data_crop_K[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   #harvland
#   summary_data_harv_K <- data_harv_K[, .(
#     min_K = min(K2Orate, na.rm = TRUE),
#     max_K = max(K2Orate, na.rm = TRUE),
#     weighted_K = sum(K2Orate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
#   ), by = c("iso_a3")]
#   # Add crop name from file name
#   summary_data_harv_K[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
# 
#   # Append to the overall summary table
#   crop_summary_K <- rbind(crop_summary_K, summary_data_crop_K, fill = TRUE)
#   harv_summary_K <- rbind(harv_summary_K, summary_data_harv_K, fill = TRUE)
#   
#   
#   # Calculate crop areas
#   # cropland
#   summary_data_crop_area <- data_crop_area[, .(
#     crop_area = sum(croparea) 
#   ), by = c("iso_a3")]
#   summary_data_crop_area[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
#   
#   #harvland
#   summary_data_harv_area <- data_harv_area[, .(
#     harv_area = sum(harvarea)
#   ), by = c("iso_a3")]
#   # Add crop name from file name
#   summary_data_harv_area[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
#   
#   # Append to the overall summary table
#   crop_summary_area <- rbind(crop_summary_area, summary_data_crop_area, fill = TRUE)
#   harv_summary_area <- rbind(harv_summary_area, summary_data_harv_area, fill = TRUE)
#   
# }
# saveRDS(crop_summary_N, "./data/P_cropland_application_npkgrids.rds")
# saveRDS(harv_summary_N, "./data/P_harvland_application_npkgrids.rds")
# 
# saveRDS(crop_summary_P, "./data/P_cropland_application_npkgrids.rds")
# saveRDS(harv_summary_P, "./data/P_harvland_application_npkgrids.rds")
# 
# saveRDS(crop_summary_K, "./data/K_cropland_application_npkgrids.rds")
# saveRDS(harv_summary_K, "./data/K_harvland_application_npkgrids.rds")
# 
# saveRDS(crop_summary_area, "./data/cropland_area_cropgrids.rds")
# saveRDS(harv_summary_area, "./data/harvland_area_cropgrids.rds")

# N processing (mass balance)-------------------------------

harv_area <- readRDS("./data/harvland_area_cropgrids.rds")
conc <- fread("./inst/conc_items_NPK_fabio.csv") 
# start with N
N_app <- readRDS("./data/N_cropland_application_npkgrids.rds")
N_app <- merge(N_app, harv_area, by = c("crop", "iso_a3"))
N_app <- merge(N_app, conc, by = "crop", all = TRUE)
N_app[, area_sum := sum(harv_area, na.rm = TRUE), by = .(item, iso_a3)]
N_app[, area_share := harv_area/area_sum]
N_app <- N_app[, .( 
  weighted_N = sum(weighted_N * area_share, na.rm = TRUE)/
    sum(area_share, na.rm = TRUE)), by = .(iso_a3, item)]
N_app <- N_app[iso_a3 != "-99",]

#harmonize HFUBC and NPK -> easy because HFUBC does not have data for 2020
N_app[, year := 2020]
N_app_har <- merge(HFUBC_full[,. (iso3c, name, region, year, item, N_rate, )],
                   N_app, by.x = c("iso3c", "year", "item"), 
                   by.y = c("iso_a3", "year", "item"), all =TRUE)




#get crop nutrient balances from FAO
file <- c("Environment_Cropland_nutrient_budget_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
cnb <- fread(paste0(path_fao,"Environment_Cropland_nutrient_budget_E_All_Data_(Normalized).csv"))
cnb[, `:=` (`Area Code (M49)` = NULL, `Item Code` = NULL, `Element Code`= NULL, `Year Code` = NULL,
            Flag = NULL, Note = NULL)]
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item" = "item",
  "Element" = "element",
  "Year" = "year",
  "Unit" = "unit",
  "Value" = "value")
cnb <- dt_rename(cnb, rename = rename, drop = FALSE)

# distribute BF from cnb with Kevin's table


# distribute AD from cnb by area



# determine uptake with Kevin's code (FAO)


# apply IPCC tier 1