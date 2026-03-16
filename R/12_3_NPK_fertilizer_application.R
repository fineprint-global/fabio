# This script creates a dataset for N and P application by year, country and crop.
# Nutrients are always converted to elemental mass in kg, area is always converted to
# hectares. 

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/03_gap_functions.R")

years_full <- (1978:2022)
regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/sua/items_sua.csv")
conc_NPK <- fread("inst/NPK/conc_NPK_sua.csv")

# Harvested area per crop -----------------------------------------------
# harvested area from FAO production domain is combined with permanent meadows and pastures
# under FAO land use domain

harv_area <- readRDS("data/tidy/prod_trad_full.rds")
harv_area <- harv_area[year %in% years_full & element == "Area harvested" ]
harv_area[, iso3c := regions$iso3c[match(area, regions$name)]] 
harv_area[is.na(iso3c), `:=`(iso3c = "ROW", area = "RoW", area_code = 999)]
harv_area <- harv_area[, .(area = first(area), 
                           ha = sum(value, na.rm = TRUE)), 
                       by = .(iso3c, year, item, item_code)]

#filter for primary products
harv_area <- harv_area[item_code %in% conc_NPK$item_code]
  
harv_area[, area := NULL]


# add grazing area from FAO landuse
land <- readRDS("data/tidy/land_tidy.rds")

# convert to ha
land[, `:=` (value = value * 1000, unit = NULL)]

# filter for grasslands
land <- land[item %in% c("Permanent meadows and pastures",
                         "Temporary meadows and pastures")
             & year %in% years_full]

# aggregate regions not in FABIO to RoW and sum up temporary and permanent
# pastures to estimate grazing area
# --> temporary pastures are explicitly excluded from fodder crop data
land[!iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]  
land <- land[, .(item_code = 2001, item = "Grazing",
                 ha = sum(value, na.rm = TRUE)), 
             by = .(iso3c, year)]

# add grazing area to harvested area of crops
harv_area <- rbind(harv_area, land)

saveRDS(harv_area[year %in% years], "data/NPK/harv_area_sua_incl_grazing.rds")

# Synthetic fertilizer application from HFUBC------------------------
# Starting with hfubc (this dataset has a timeline but relatively few data points)
# can be downloaded from https://datadryad.org/downloads/file_stream/3940355
hfubc <- fread("input/NPK/FUBC_1_to_9_data.csv")
conc <- fread("inst/NPK/conc_HFUBC_sua.csv")

# subset
hfubc <- hfubc[,.(Country, iso3c = ISO3_code, Year, Crop, 
                  Crop_area_k_ha, N_k_t, P2O5_k_t, K2O_k_t )]

#convert fertilizer years to calendar years where necessary
hfubc[Year == "1999/2000", Year := "2000"]
hfubc[Year %in% c("1990/91", "1991/92", "1992/93", "1989/90", "1998/99", "1997-98"),
      Year := paste0("19", substr(Year, nchar(Year) - 1, nchar(Year)))]

# convert fertilizer units from kt of oxidized (N)PK to kg of elemental NPK
hfubc[, `:=` (P = P2O5_k_t* 436000, K = K2O_k_t * 830000, N = N_k_t * 1000000)][
  , `:=` (P2O5_k_t = NULL, N_k_t = NULL, K2O_k_t = NULL)]

# convert to hectares
hfubc[, ha := Crop_area_k_ha * 1000][, Crop_area_k_ha := NULL]
hfubc[, ':=' (N_rate = N/ha, P_rate = P/ha, 
              K_rate = K/ha, year = as.integer(Year))][, `:=` (N =NULL, P = NULL, 
                                                 K = NULL, Country = NULL,
                                                 Year = NULL)] 

# ensure concordance with fabio items
hfubc <- merge(hfubc, conc, by.x = "Crop", by.y = "item_hfubc")
hfubc <- hfubc[item != "",]

# create averages weighted by area for items that are more aggregated in fabio
# than in hfubc
hfubc <- hfubc[, c(
  lapply(.SD, function(x) sum(x * ha, na.rm = TRUE) / sum(ha, na.rm = TRUE)),
  list(ha = fifelse(all(is.na(ha)), NA_real_, sum(ha, na.rm = TRUE)))
), by = .(item_code, iso3c, year), .SDcols = c("N_rate", "P_rate", "K_rate")]

# replace NAN with NAs
hfubc[, (names(hfubc)) := lapply(.SD, function(x) fifelse(is.nan(x), NA, x))]
setorder(hfubc, item_code, iso3c, year)

# Synthetic fertilizer application from NPKGrids -------------------------
app <- readRDS("data/NPK/N_harvland_application_npkgrids.rds")

# add P and convert to elemental P
P_app <- readRDS("data/NPK/P_harvland_application_npkgrids.rds")
numeric_cols <- names(P_app)[sapply(P_app, is.numeric)]
P_app[, (numeric_cols) := lapply(.SD, function(x) x * 0.436), .SDcols = numeric_cols]
app <- merge(app, P_app, by = c("iso_a3", "crop"), all = TRUE)

conc <- fread("inst/NPK/conc_NPK_sua.csv")


# for ensuring concordance, harvested area from the cropgrids dataset is used
harv_area_grids <- readRDS("data/NPK/harvland_area_cropgrids.rds")
harv_area_grids <- harv_area_grids[!is.na(iso_a3)]

app <- merge(app, harv_area_grids, by = c("crop", "iso_a3"), all = TRUE)
app <- merge(app, conc, by.x = "crop",
             by.y = "item_npk", allow.cartesian = TRUE)
app <- app[!is.na(item) & item != ""]

# Aggregating countries not in fabio to RoW
app[, fabio_iso := regions$iso3c[match(iso_a3, regions$iso3c)]]
app[is.na(fabio_iso), iso_a3 := "ROW"]

# weighted averages for conc merging (fabio items/regions are sometimes more aggregated)
app <- app[, .( 
  weighted_N = sum(weighted_N * harv_area, na.rm = TRUE) / sum(harv_area, na.rm = TRUE),
  weighted_P = sum(weighted_P * harv_area, na.rm = TRUE) / sum(harv_area, na.rm = TRUE),
  ha = sum(harv_area, na.rm = TRUE)), 
  by = .(iso_a3, item, item_code)] 


# Harmonizing and gap-filling HFUBC and NPKgrids -------------------------------------------
# Merging
app[, year := 2020]
app <- merge(hfubc[,. (iso3c, year, item_code, N_rate, P_rate)],
                   app[, .(iso_a3, year, item_code, weighted_N, weighted_P)], 
                   by.x = c("iso3c", "year", "item_code"), 
                   by.y = c("iso_a3", "year", "item_code"), all =TRUE)
app[, (names(app)) := lapply(.SD, function(x) fifelse(is.nan(x), NA, x))]


# Reducing HFUBC and NPK columns (years don't overlap)
app[,  `:=`(N_rate = fifelse(is.na(N_rate), weighted_N, N_rate),
            P_rate = fifelse(is.na(P_rate), weighted_P, P_rate)),][
           , `:=` (weighted_N = NULL, weighted_P = NULL)]

# Creating full dt for gap filling
app_full <- CJ(items[item_code %in% conc_NPK$item_code, item_code],
                 regions[, iso3c],
                 years_full)



setnames(app_full, c("item_code", "iso3c", "year"))
app_full <- merge(app_full, app, by = c("iso3c", "year", "item_code"), all.x = TRUE)



# Adding harvested area from FAO / land use extension
app_full <- merge(harv_area[, .(iso3c, year, item_code, ha)], app_full, 
                  by = c("iso3c", "year", "item_code"), all = TRUE)

app_full[, `:=` (country = regions$name[match(iso3c, regions$iso3c)],
                 region = regions$region[match(iso3c, regions$iso3c)],
                 item = items$item[match(item_code, items$item_code)],
                 comm_code = items$comm_code[match(item_code, items$item_code)])]
setcolorder(app_full, c("iso3c", "country", "region", "year", "item", "item_code",
                        "N_rate", "P_rate", "ha"))

#Finding items that have no data anywhere
empty_items <- app_full[
  , .(all_missing = all(is.na(N_rate) & is.na(P_rate))),
  by = .(item, item_code)
][all_missing == TRUE, .(item, item_code)]

app_full <- app_full[!item_code %in% empty_items$item_code]

# Gap filling
# setting NA N_rates to 0 where area is 0 or NA
app_full[is.na(ha), ha := 0]
app_full[, `:=` (N_rate = fifelse(ha == 0, 0, N_rate),
                 P_rate = fifelse(ha == 0, 0, P_rate))]


# Interpolate app rates and area in same country between years with at least two data points
vars <- c("N_rate", "P_rate")
for (i in vars) {
  app_full <- interpolate(i, app_full)
}

# Extrapolate single values to whole timeline where only one value is available
for (i in vars) {
  app_full <- extrapolate(i, app_full)
}


# Fill remaining NAs for N_rate with regional averages (except for grazing where
# this yields data anomalies and a value of 0 is deemed likely)
app_full[, `:=`(N_reg = sum(N_rate * ha / sum(ha, na.rm = TRUE), na.rm = TRUE),
                P_reg = sum(P_rate * ha / sum(ha, na.rm = TRUE), na.rm = TRUE)),
           by = .(year, item, region)]

app_full[item_code != 2001, `:=` (N_rate = fifelse(is.na(N_rate),N_reg, N_rate),
                 P_rate = fifelse(is.na(P_rate),P_reg, P_rate))]
app_full[, `:=` (P_reg = NULL, N_reg = NULL)]
app_full[is.na(N_rate), N_rate := 0]
app_full[is.na(P_rate), N_rate := 0]

# filter for current years
app <- app_full[ year %in% years,]


# tidy 
app <- app[, .(iso3c, country, year, item, item_code, comm_code, ha, 
                             N_rate = round(N_rate, 2),
                         P_rate = round(P_rate, 2))]
app[, `:=` (N_kg = N_rate * ha,
            P_kg = P_rate * ha)]
setcolorder(app, c("iso3c", "country", "year", "item", "item_code", "comm_code", 
                   "N_kg",  "N_rate", "P_kg", "P_rate", "ha"))
setkey(app, iso3c, year, comm_code)

saveRDS(app, "data/NPK/SF_application_sua.rds")


rm(list = ls())
gc()



