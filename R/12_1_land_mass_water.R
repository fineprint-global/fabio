library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

# Prep production, area and livestock data ------------------------
prod_land <- readRDS("data/tidy/prod_trad_full.rds")[year %in% years & element %in%
                                                       c("Area harvested", "Production", "Producing Animals/Slaughtered") &
                                                       item_code %in% items$item_code]

# aggregate regions not in FABIO to RoW
prod_land[!area_code %in% regions$code, `:=` (area = "RoW", area_code = 999)]
prod_land[, comm_code := items$comm_code[match(item_code, items$item_code)]]
prod_land[, `:=` (value = sum(value, na.rm = TRUE)), 
          by = .(year, item_code, comm_code, item, area, area_code, element, unit)]
prod_land <- unique(prod_land)
prod_land[, iso3c := regions$iso3c[match(area_code, regions$code)]]

# Biomass extension -----------------------
# get primary crop production 
biomass <- prod_land[element == "Production" 
                     & item_code %in% items[processed == FALSE & comm_group == "vegetal products", 
                                            item_code]]
biomass[,`:=` (element = NULL)] 

# add grazing production from supply table
sup <- readRDS("data/sup_final.rds")
grass_prod <- sup[item_code == 2001]
grass_prod[is.na(supply), supply := 0]
grass_prod <- grass_prod[, .(year, item_code, area, area_code, item, 
                   unit = "tonnes", value = supply, comm_code)]
grass_prod[, iso3c := regions$iso3c[match(area_code, regions$code)]]

# combine
biomass <- rbind(biomass, grass_prod)

# tidy 
rm(sup)

# Land extensions -----------------------------
## Land - harvested area ----------------------
land_harv <- prod_land[element == "Area harvested"][, element := NULL]

# prep crop area and grassland (only permanent meadows and pastures)
land_tidy <- readRDS("data/tidy/land_tidy.rds")[ year %in% years]
land_tidy[, `:=` (value = value * 1000, unit = "ha", element = NULL, area_code = 
                    as.integer(area_code))]

# aggregate RoW
land_tidy[!area_code %in% regions$code, `:=` (area = "RoW", area_code = 999, iso3c = "ROW")]
land_tidy[, value := sum(value, na.rm = TRUE), by = .(year, area, item_code)]
land_tidy <- unique(land_tidy)

## Land - permanent grass area ---------------
land_grass <- land_tidy[item_code == 6655]
land_grass[, `:=` (item = "Grazing", item_code = 2001,
                   comm_code = items$comm_code[match(2001L, items$item_code)])]

## Land - crop area --------------------------
# get crop to harv factors from CROPGRIDS
conc <- fread("inst/NPK/conc_NPK_sua.csv")
harv_grids <- readRDS("data/NPK/harvland_area_cropgrids.rds")
crop_grids <- readRDS("data/NPK/cropland_area_cropgrids.rds")
crop_harv <- merge(crop_grids, harv_grids, by = c("iso_a3", "crop"))
crop_harv <- crop_harv[!is.na(iso_a3)]
crop_harv[, item_code := conc$item_code[match(crop, conc$item_npk)]]
crop_harv <- crop_harv[!is.na(item_code) & item_code != ""]

# Aggregating countries not in fabio to RoW
crop_harv[, fabio_iso := regions$iso3c[match(iso_a3, regions$iso3c)]]
crop_harv[is.na(fabio_iso), iso_a3 := "ROW"]

# Sum by fabio items
crop_harv <- crop_harv[, .( 
  harv_area = sum(harv_area, na.rm = TRUE),
  crop_area = sum(crop_area, na.rm = TRUE)),
  by = .(iso_a3, item_code)] 

setkey(crop_harv, iso_a3, item_code)

# find conversion factors from harvested area to crop area by crop
crop_harv[, harv_crop_ratio := harv_area / crop_area]
crop_harv[is.nan(harv_crop_ratio), harv_crop_ratio := NA_real_]

crop_harv[, area_code := regions$code[match(iso_a3, regions$iso3c)]]
setnames(crop_harv, "iso_a3", "iso3c")

#tidy
rm(crop_grids, harv_grids)

# create full timeline
land_crop <- CJ(year = years, area_code = regions$code, 
               item_code = items[processed == FALSE & comm_group == "vegetal products", 
                                 item_code])

# add data to full table
land_crop[, type := items$type[match(item_code, items$item_code)]]
land_crop <- merge(land_crop, land_harv[, .(year, item_code, area_code, harv_area = value)],
                   by = c("year", "item_code", "area_code"),
                   all.x = TRUE)
land_crop <- merge(land_crop, crop_harv[, .(area_code, item_code, harv_crop_ratio)], 
                   by = c("area_code", "item_code"),
                   all.x = TRUE)

# assume a harv to crop ratio of 1 where FAO reports data but CROPGRIDS does not
land_crop[is.na(harv_crop_ratio) & harv_area > 0, harv_crop_ratio := 1]

# estimate crop area from harvested area
land_crop[, est_crop_area := harv_area / harv_crop_ratio][, `:=` (harv_crop_ratio = NULL)]
land_crop[is.na(est_crop_area), est_crop_area := 0]
land_crop[, crop_area := NA_real_]

# get total cropland area from FAO 
land_crop_totals <- land_tidy[ item_code %in% c(6630, 6650, 6640, 6633)]
land_crop_totals[, item := tolower(gsub(" ", "_", item))]
land_crop_totals <- dcast(land_crop_totals, 
                          area_code + area + year ~ item, value.var = "value")

# Add totals to full template
land_crop <- merge(land_crop, land_crop_totals[, .(area_code, year, permanent_crops,
                                              temporary_crops, temporary_fallow,
                                              temporary_meadows_and_pastures)], 
                   by = c("area_code", "year"), all.x = TRUE)

# assign area "temporary meadows and pastures" to grazing crop area
land_crop[item_code == 2001, crop_area := temporary_meadows_and_pastures][, temporary_meadows_and_pastures := NULL]

# add up temporary crop and temporary fallows for distributing between annual crops
land_crop[, temporary := na_sum(temporary_crops, temporary_fallow)][, `:=` (
  temporary_crops = NULL, temporary_fallow = NULL)]

# find estimated totals by crop type (grazing is automatically included, because it does 
# not have a type)
land_crop[type == "perennial", est_permanent := sum(est_crop_area, na.rm = TRUE),
          by = .(area_code, year)]
land_crop[type == "annual", est_temporary := sum(est_crop_area, na.rm = TRUE),
          by = .(area_code, year)]

# find scaling factors by type
land_crop[, scale_factor_temp := temporary/est_temporary]
land_crop[, scale_factor_perm := permanent_crops/est_permanent]

scale_factors <- c("scale_factor_perm", "scale_factor_temp")

# scale to FAO estimates (some outliers propagate from FAO data, e.g. djibouti)
land_crop[type == "annual", crop_area := round(est_crop_area * scale_factor_temp)]
land_crop[type == "perennial", crop_area := round(est_crop_area * scale_factor_perm)]

# Note: in 2010 in `Netherlands Antilles` there are no annual crops harvested,
# but a positive estimation of temporary crop area. Here, crop area does not get
# distributed between crops and the data is lost.

land_crop[is.na(crop_area), crop_area := 0]

# cap harv/crop ratios 0.5 and 2 (reporting inconsistencies within FAO)
land_crop <- land_crop[, .(area_code, year, item_code, harv_area, crop_area)]
land_crop[, ratio := harv_area / crop_area]
land_crop[ratio > 2, crop_area := harv_area * 0.5]
land_crop[ratio < 0.5, crop_area := harv_area * 2 ]


# add names and codes
land_crop[, area := regions$name[match(area_code, regions$code)]]
land_crop[, item := items$item[match(item_code, items$item_code)]]
land_crop[, comm_code := items$comm_code[match(item_code, items$item_code)]]

# tidy
land_crop <- land_crop[, .(year, area, area_code, item, item_code, comm_code, value = crop_area)]
rm(land_crop_totals, crop_harv)

# Water extensions ---------------------
## Water - crops -----------------------
fa_dl(file = "",
  link = "https://data.4tu.nl/file/7b45bcc6-686b-404d-a910-13c87156716a/3787e536-c388-4f76-a603-9081d6748588",
  path = "input/water/water_crop.csv"
)

water_crop <- fread("input/water/water_crop.csv")
setnames(water_crop, names(water_crop), as.character(water_crop[4, ]))
water_crop <- water_crop[-(1:4)]

# select and rename relevant cols
water_crop <- water_crop[year %in% years, 
                         .(iso3c = country_iso3,  area_code = country_code, area = country_name,
                           item_code = crop_code, crop_group, item = crop_name, year, 
                           production_t, wfg_m3_t, wfb_cr_m3_t, wfb_i_m3_t,
                           wf_tot_m3_t, irrigated_harvarea_fraction)]

# convert columns to numeric
num_cols <- c("area_code", "item_code", "year", "production_t", "wfg_m3_t", 
              "wfb_cr_m3_t", "wfb_i_m3_t", "wf_tot_m3_t", "irrigated_harvarea_fraction")
water_crop[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# exclude "other grasses for forage", "mixed grasses" (included in grazing in FABIO)
# and "flaxseed, processed but not spun" (not in current FABIO versions)
water_crop <- water_crop[!item_code %in% c(638, 639, 645, 773)]

# deal with Sudan
water_crop[iso3c == "F206", `:=` (iso3c = "SDN", area_code = 276, area = "Sudan")]

# define num_cols for aggregating water footprints
num_cols_prod <- c("production_t", "wfg_m3_t", "wfb_cr_m3_t", 
                   "wfb_i_m3_t", "wf_tot_m3_t", "irrigated_harvarea_fraction")
num_cols_avg <- c("wfg_m3_t", "wfb_cr_m3_t", "wfb_i_m3_t", 
                      "wf_tot_m3_t", "irrigated_harvarea_fraction")

# prepare fodder crops for aggregating to one category
water_crop[crop_group == "Fodder crops", 
           `:=` (item_code = 2000, item = "Fodder crops")]
water_crop[, fodder_prod_share := 
             production_t / sum(production_t, na.rm = TRUE),
           by = .(iso3c, year, item_code)]
water_crop[, (num_cols_avg) := lapply(.SD, function(x) x * fodder_prod_share), 
           .SDcols = num_cols_avg] 
water_crop <- water_crop[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
                             by = .(iso3c, year, item_code), 
                             .SDcols = num_cols_prod]

# aggregate RoW countries in the same manner
water_crop[!iso3c %in% regions$iso3c, iso3c := "ROW" ]
water_crop[, row_prod_share := production_t / 
             sum(production_t, na.rm = TRUE),
           by = .(iso3c, year, item_code)]
water_crop[, (num_cols_avg) := lapply(.SD, function(x) x * row_prod_share), 
           .SDcols = num_cols_avg]


water_crop <- water_crop[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
                         by = .(iso3c, year, item_code), 
                         .SDcols = num_cols_prod]

# Extrapolate to 2023
# average over the last 3 available years
extra <- water_crop[year %in% c(2017:2019)]
extra[, (num_cols_avg) := lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      by = .(iso3c, item_code),
      .SDcols = num_cols_avg]

# delete years and reduce to unique footprints
extra <- unique(extra[, .(iso3c, item_code, wfg_m3_t, wfb_cr_m3_t, wfb_i_m3_t,
                          wf_tot_m3_t, irrigated_harvarea_fraction)])
years_extra <- (2020:max(years))
extra[, column := paste0(iso3c, "_", item_code)]

# create full table
extra_full <- CJ(year = years_extra, column = extra$column)
extra_full[, `:=` (iso3c = substr(column, 1, 3),
                   item_code = as.numeric(sub(".*_", "", column)))][, column := NULL]

# add data
extra <- merge(extra_full, extra, by = c("iso3c", "item_code"), all.x = TRUE)[, column := NULL]
extra <- merge(extra, prod_land[element == "Production", .(year, iso3c, 
                                                           item_code,
                                                           production_t = value)],
               by = c("year", "iso3c", "item_code"), all.x = TRUE)


# add extrapolated rows back to original table
water_crop <- rbind(water_crop, extra, use.names = TRUE)

# calculate totals
num_cols_avg <- setdiff(num_cols_avg, "irrigated_harvarea_fraction")
water_crop[, (num_cols_avg) := lapply(.SD, function(x) x * production_t),
           .SDcols = num_cols_avg]

# rename cols to reflect change in unit
setnames(water_crop, num_cols_avg, new = sub("_t$", "", num_cols_avg))

# add comm and area codes
water_crop[, area_code := regions$code[match(iso3c, regions$iso3c)]]
water_crop[, comm_code := items$comm_code[match(item_code, items$ item_code)]]

#save irrigation fractions for N emission estimations
saveRDS(water_crop, "data/NPK/irrigation.rds")
# optional todo: some small island states are missing from the water data -> gap fill?

water_crop[, irrigated_harvarea_fraction := NULL]

## Water - pastures ---------------------------------
# get grazing area (temporary + permanent meadows and pastures) from above 
water_pasture <- copy(land_grass)
water_pasture[, crop_area := land_crop$value[match(paste(item_code, year, area_code),
                                                       paste(land_crop$item_code,
                                                             land_crop$year,
                                                             land_crop$area_code))]]
water_pasture[, ha := value + crop_area][, `:=` (value = NULL, crop_area = NULL)]

# get pasture intensities, assume all grazing is green water (i.e. no irrigated pastures)
ints <- fread("input/grazing/grazing.csv")[area_code %in% regions$code
  , .(area_code, m3_per_ha)]

# calculate totals
water_pasture[, wfg_m3_ha := ints$m3_per_ha[match(area_code, ints$area_code)]]
water_pasture[, wfg_m3 := wfg_m3_ha * ha][, wfg_m3_ha := NULL ]


## Water - livestock -------------------------
live_ints <- fread("input/water/water_lvst.csv") 

# start with live animal stocks -> these need to be renamed before combining
water_live <- prod_land[element == "Producing Animals/Slaughtered" &
                           item_code %in% live_ints$item_code]

tgt_item <- c(1126, 866, 1016, 976, 1034, 1096, 1140,
              946, 1157, 1150, 1107, 1110, 1057, 1068, 1072, 1079, 1083)
tgt_name <- c("Camels", "Cattle", "Goats", "Sheep", "Swine / pigs",
              "Horses", "Rabbits and hares", "Buffalo", "Other camelids",
              "Other rodents", "Asses", "Mules and hinnies", "Chickens", "Ducks", "Geese",
              "Turkeys", "Other birds")

# Map "Meat, ..." items to SUA items
src_item <- c(1127, 867, 1017, 977, 1035, 1097, 1141,
              947, 1158, 1151, 1108, 1111, 1058, 1069, 1073, 1080, 1089)

conc <- match(water_live$item_code, src_item)
water_live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
water_live[, comm_code := items$comm_code[match(item_code, items$item_code)]]

# match intensities with production
water_live[,`:=` (wfb_m3_unit = live_ints$wfb_m3_unit[match(item_code,
                                                            live_ints$item_code)])]
# calculate totals
water_live[, wfb_m3 := wfb_m3_unit * value]

## Water - meat processing --------------------------
water_meat <- prod_land[element == "Production" &
                          item_code %in% live_ints$item_code]

# match intensities with production
water_meat[,`:=` (wfb_m3_unit = live_ints$wfb_m3_unit[match(item_code,
                                                live_ints$item_code)])]

# calculate totals
water_meat[, wfb_m3 := wfb_m3_unit * value][, `:=` (value = NULL, wfb_m3_unit = NULL)]
rm(src_item, tgt_item, tgt_name)

## Water - blue total -------------------
water_blue_total <- merge(water_crop[, .(area_code, comm_code, year,
                                         wfb_cr_m3, wfb_i_m3)], 
                          water_live[, .(area_code, comm_code, year, wfb_live_m3 = wfb_m3)],
                          by = c("area_code", "comm_code", "year"),
                          all = TRUE)
water_blue_total <- merge(water_blue_total, 
                          water_meat[, .(area_code, comm_code, year, 
                                         wfb_meat_m3 = wfb_m3)],
                          by = c("area_code", "comm_code", "year"),
                          all = TRUE)
water_blue_total[, wfb_m3_total := na_sum(wfb_cr_m3, wfb_i_m3, wfb_live_m3, wfb_meat_m3)]
water_blue_total[, item_code := items$item_code[match(comm_code, items$comm_code)]]

## Tidy all water extensions  ------------------------
water_green <- rbind(water_crop[, .(year, area_code, item_code, comm_code, value = wfg_m3)],
                     water_pasture[, .(year, area_code, item_code, comm_code, value = wfg_m3)])
water_blue_irr <- water_crop[, .(year, area_code, item_code, comm_code, value = wfb_i_m3)]
water_blue_cap_r <- water_crop[, .(year, area_code, item_code, comm_code, value = wfb_cr_m3)]  
water_blue_live <- water_live[, .(year, area_code, item_code, comm_code, value = wfb_m3)]
water_blue_meat <- water_meat[, .(year, area_code, item_code, comm_code, value = wfb_m3)]
water_blue_total <- water_blue_total[, .(year, area_code, item_code, comm_code, value = wfb_m3_total)]
  
rm(water_crop, water_pasture, water_live, water_meat,  extra, extra_full,
    ints, live_ints, grass_prod, prod_land, land_tidy)
                 

# CBS aggregations for alternative version ------------
sua_extensions <- list(biomass, water_green, water_blue_cap_r, water_blue_irr,
                       water_blue_live, water_blue_meat, water_blue_total, 
                       land_grass, land_crop, land_harv)
names(sua_extensions) <- c("biomass", "water_green", "water_blue_cap_r", "water_blue_irr",
                           "water_blue_live", "water_blue_meat", "water_blue_total", 
                           "land_grass", "land_crop", "land_harv")

conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
cbs_extensions <- lapply(sua_extensions, agg_sua_to_cbs)

# Format all -------------------
E_sua <- lapply(sua_extensions, format_extension)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)

# save
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}


