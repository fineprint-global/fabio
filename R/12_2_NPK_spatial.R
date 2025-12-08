# This script aggregates data for FABIO's NP extension from the datasets "cropgrids",
# "npkgrids" and from Lewis' spatial dataset representing IPCC climactic zones

library(data.table)
library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# set system variables
source("R/00_system_variables.R")
path_geo <- "/mnt/nfs_fineprint/tmp/geo_data/"

# get fabio input data
items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

# spatial input data
r_zones <- rast(paste0(path_geo, "IPCC_Climate_Zones_ts_3.25.tif"))
r_country <- rast(paste0(path_geo, "Countries_2018.nc"))  
r_soils <- rast("data/NPK/HWSD2_soiltype.tif")  
country_mapping <- readRDS("data/NPK/country_mapping.rds")

# Define directories for fertilizer and area datasets (173 each) ----------
nc_fert_dir <- paste0(path_geo, "NPKGrids")
nc_area_dir <- paste0(path_geo, "Cropgrids")
nc_fert_files <- list.files(nc_fert_dir, pattern = "\\.nc$", full.names = TRUE)
nc_area_files <- list.files(nc_area_dir, pattern = "\\.nc$", full.names = TRUE)

# Initialize empty data.tables to store summary statistics
crop_summary_N <- data.table()
harv_summary_N <- data.table()

crop_summary_P <- data.table()
harv_summary_P <- data.table()

crop_summary_K <- data.table()
harv_summary_K <- data.table()

crop_summary_area <- data.table()
harv_summary_area <- data.table()

crop_summary_climate_soils <- data.table()
harv_summary_climate_soils <- data.table()



# Process each pair of NetCDF files (one for fertilizer and one for crop area)
for (i in seq_along(nc_fert_files)) {
  nc_fert <- nc_fert_files[i]
  nc_area <- nc_area_files[i]

  # Read the fertilizer and area NetCDF files
  r_fert <- rast(nc_fert)
  r_area <- rast(nc_area)
           

  # Specify bands for fertilizer and area (crop_area and crop_harvest)
  bands_fert <- c(1, 4, 7)  # Fertilizer bands
  bands_area <- c(1, 2)     # crop_area and crop_harvest

  # Choose a reference raster
  ref_raster <- r_fert[[1]]
  
  # Extract and resample each layer individually
  fert_layers <- lapply(bands_fert, function(i) resample(r_fert[[i]], ref_raster))
  area_layers <- lapply(bands_area, function(i) resample(r_area[[i]], ref_raster))
  country_layer <- resample(r_country[[1]], ref_raster, method = "near")
  zone_layer <- resample(r_zones, ref_raster, method = "near")
  soil_layer <- resample(r_soils, ref_raster, method = "near") 
  
  # Flatten the list of layers
  all_layers <- c(fert_layers, area_layers, list(country_layer, zone_layer, soil_layer))
  
  # Combine into a single SpatRaster stack
  r_stack <- rast(all_layers)
  
  # get reference points for merging
  ref_points <- as.points(ref_raster, na.rm = TRUE)
  values_dt <- as.data.table(extract(r_stack, ref_points))
  setnames(values_dt, "label", "climate_zone")
  
  # add country codes
  values_dt[, iso_a3 := country_mapping$iso_a3_eh[match(country, country_mapping$country)]]
  values_dt[, `:=` (ID = NULL, country = NULL)]
  
  # intitalize empty data.table for all crop data
  data_crop <- copy(values_dt)
  data_crop[, harvarea := NULL]
  
  # intitalize empty data.table for all harv data
  data_harv <- copy(values_dt) 
  data_harv[, croparea := NULL]

  # Initialize data.tables foe different nutrients and areas (differentiated 
  # by climate zones and soil types)
  data_crop_N <- data_crop[croparea > 0]
  data_harv_N <- data_harv[harvarea > 0]

  data_crop_P <- data_crop[croparea > 0]
  data_harv_P <- data_harv[harvarea > 0]

  data_crop_K <- data_crop[croparea > 0]
  data_harv_K <- data_harv[harvarea > 0]

  data_crop_area <- data_crop[croparea >= 0]
  data_harv_area <- data_harv[harvarea >= 0]
  
  data_climate_soils_crop <- data_crop[croparea >= 0]
  data_climate_soils_harv <- data_harv[harvarea >= 0]

  
  # Calculate weighted averages for N
  # cropland
  summary_data_crop_N <- data_crop_N[, .(
    min_N = min(Nrate, na.rm = TRUE),
    max_N = max(Nrate, na.rm = TRUE),
    weighted_N = sum(Nrate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  summary_data_crop_N[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_crop_N, iso_a3)
  
  #harv land
  summary_data_harv_N <- data_harv_N[, .(
    min_N = min(Nrate, na.rm = TRUE),
    max_N = max(Nrate, na.rm = TRUE),
    weighted_N = sum(Nrate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  # Add crop name from file name
  summary_data_harv_N[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_harv_N, iso_a3)
  
  # Append to the overall summary table
  crop_summary_N <- rbind(crop_summary_N, summary_data_crop_N, fill = TRUE)
  harv_summary_N <- rbind(harv_summary_N, summary_data_harv_N, fill = TRUE)

  # Calculate weighted averages for P
  #cropland
  summary_data_crop_P <- data_crop_P[, .(
    min_P = min(P2O5rate, na.rm = TRUE),
    max_P = max(P2O5rate, na.rm = TRUE),
    weighted_P = sum(P2O5rate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  summary_data_crop_P[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_crop_P, iso_a3)
  
  # harvland
  summary_data_harv_P <- data_harv_P[, .(
    min_P = min(P2O5rate, na.rm = TRUE),
    max_P = max(P2O5rate, na.rm = TRUE),
    weighted_P = sum(P2O5rate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  # Add crop name from file name
  summary_data_harv_P[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_harv_P, iso_a3)
  
  # Append to the overall summary table
  crop_summary_P <- rbind(crop_summary_P, summary_data_crop_P, fill = TRUE)
  harv_summary_P <- rbind(harv_summary_P, summary_data_harv_P, fill = TRUE)


  # Calculate weighted averages for K
  # cropland
  summary_data_crop_K <- data_crop_K[, .(
    min_K = min(K2Orate, na.rm = TRUE),
    max_K = max(K2Orate, na.rm = TRUE),
    weighted_K = sum(K2Orate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  summary_data_crop_K[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_crop_K, iso_a3)
  
  
  #harvland
  summary_data_harv_K <- data_harv_K[, .(
    min_K = min(K2Orate, na.rm = TRUE),
    max_K = max(K2Orate, na.rm = TRUE),
    weighted_K = sum(K2Orate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  # Add crop name from file name
  summary_data_harv_K[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_harv_K, iso_a3)
  
  # Append to the overall summary table
  crop_summary_K <- rbind(crop_summary_K, summary_data_crop_K, fill = TRUE)
  harv_summary_K <- rbind(harv_summary_K, summary_data_harv_K, fill = TRUE)


  # Calculate crop areas
  # cropland
  summary_data_crop_area <- data_crop_area[, .(
    crop_area = sum(croparea, na.rm = TRUE)
  ), by = c("iso_a3")]
  summary_data_crop_area[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_crop_area, iso_a3)
  
  #harvland
  summary_data_harv_area <- data_harv_area[, .(
    harv_area = sum(harvarea, na.rm = TRUE)
  ), by = c("iso_a3")]
  # Add crop name from file name
  summary_data_harv_area[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_data_harv_area, iso_a3)
  
  # Append to the overall summary table
  crop_summary_area <- rbind(crop_summary_area, summary_data_crop_area, fill = TRUE)
  harv_summary_area <- rbind(harv_summary_area, summary_data_harv_area, fill = TRUE)
  
  
  # Aggregate crop area by climate zone and soil type
  summary_crop_climate <- data_climate_soils_crop[croparea > 0, .(
    crop_area = sum(croparea, na.rm = TRUE)
  ), by = .(iso_a3, climate_zone, HWSD2)]
  # Add crop name from file name
  summary_crop_climate[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_crop_climate, iso_a3)
  
  # Aggregate harvested area by country and climate zone
  summary_harv_climate <- data_climate_soils_harv[harvarea > 0, .(
    harv_area = sum(harvarea, na.rm = TRUE)
  ), by = .(iso_a3, climate_zone, HWSD2)] 
  # Add crop name from file name
  summary_harv_climate[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  setorder(summary_harv_climate, iso_a3)
  
  # Append to the overall summary table
  crop_summary_climate_soils <- rbind(crop_summary_climate_soils, summary_crop_climate, fill = TRUE)
  harv_summary_climate_soils <- rbind(harv_summary_climate_soils, summary_harv_climate, fill = TRUE)

}

# Tidy climate dataset
climate <- copy(crop_summary_climate_soils)
zone_names <- fread("inst/NPK/climate_zones.csv")
conc <-  fread("inst/NPK/conc_NPK_items.csv")

climate[, zone := zone_names$label[match(climate_zone, zone_names$ids)] ]
climate[, item := conc$item[match(crop, conc$crop)]]
climate[, area := regions$name[match(iso_a3, regions$iso3c)]]
climate[, area_code := regions$code[match(iso_a3, regions$iso3c)]]
climate[, item_code := items$item_code[match(item, items$item)]]

climate <- climate[, .(iso3c = iso_a3, item, zone_code = climate_zone, 
                       crop_area_h = sum(crop_area, na.rm =TRUE)),
                   by = .(area, item_code, zone, HWSD2)]
setcolorder(climate, c("iso3c", "area", "item", "item_code",  "zone_code",
                       "zone", "HWSD2", "crop_area_h"))
setorder(climate, iso3c, item, zone_code, HWSD2)

# save
saveRDS(crop_summary_N, "data/NPK/N_cropland_application_npkgrids.rds")
saveRDS(harv_summary_N, "data/NPK/N_harvland_application_npkgrids.rds")

saveRDS(crop_summary_P, "data/NPK/P_cropland_application_npkgrids.rds")
saveRDS(harv_summary_P, "data/NPK/P_harvland_application_npkgrids.rds")

saveRDS(crop_summary_K, "data/NPK/K_cropland_application_npkgrids.rds")
saveRDS(harv_summary_K, "data/NPK/K_harvland_application_npkgrids.rds")

saveRDS(crop_summary_area, "data/NPK/cropland_area_cropgrids.rds")
saveRDS(harv_summary_area, "data/NPK/harvland_area_cropgrids.rds")

saveRDS(climate, "data/NPK/climate_soils_cropland.rds")
saveRDS(harv_summary_climate_soils, "data/NPK/climate_soils_harvland.rds")
