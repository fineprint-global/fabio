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
                     & item_code %in% items[processed == FALSE & comm_group == "crops", 
                                            item_code]]
biomass[,`:=` ( iso3c = regions$iso3c[match(area_code, regions$code)],
                element = NULL)] 

# add grazing production from supply table
sup <- readRDS("data/sup_final.rds")
grass_prod <- sup[item_code==2001]
grass_prod[is.na(production), production := 0]
grass_prod <- grass_prod[, .(year, item_code, area, area_code, item, 
                   unit = "tonnes", value = production, comm_code)]
grass_prod[, iso3c := regions$iso3c[match(area_code, regions$code)]]

# combine
biomass <- rbind(biomass, grass_prod)

# tidy 
rm(sup)


# Land extensions --------------------------
# create harvested area extension
land_harv <- prod_land[element == "Area harvested"][, element := NULL]

# prep crop area and grassland (only permanent meadows and pastures)
land_tidy <- readRDS("data/tidy/land_tidy.rds")[ year %in% years]
land_tidy[, `:=` (value = value * 1000, unit = "ha", element = NULL)]

# aggregate RoW
land_tidy[!area_code %in% regions$code, `:=` (area = "RoW", area_code = 999)]
land_tidy[, value := sum(value, na.rm = TRUE), by = .(year, area, item_code)]
land_tidy <- unique(land_tidy)


# create grassland extension (i.e. permanent meadows and pastures)
land_grass <- land_tidy[item_code == 6655]
land_grass[, `:=` (item = "Grazing", item_code = 2001, comm_code = "c148")]


# create cropland extension
conc <- fread("inst/NPK/conc_NPK_sua.csv")
land_crop <- readxl::read_xlsx("input/NPK/cropgrids_data.xlsx", sheet = 2)
setDT(land_crop)
land_crop[, iso3c := substr(`Country Name`, 1, 3)][, `:=` (`Country Name` = NULL,
                                                           `Country ISO2 code` = NULL)]
land_crop <- melt(land_crop, id.vars = "iso3c", variable.name = "item_npk", 
                       value.name = "crop_area")
land_crop[, `:=` (item = conc$item[match(item_npk, conc$item_npk)],
                  item_code = conc$item_code[match(item_npk, conc$item_npk)])][, item_npk := NULL]

land_crop[, `:=` (crop_area = round(crop_area, 2),
                  year = 2020)]

# get shares of cropland by group
# TODO: create groups for annual crops, etc. (categories from land_crop_totals)
# pdf file saved under BAMBOO -> data

# get total values from FAO 
land_crop_totals <- land_tidy[ item_code %in% c(6620, 6630, 6650, 6640, 6633)]

# create template for whole timeline

# match shares and totals in template


# add comm codes
land[, comm_code := items$comm_code[match(item_code, items$item_code)]]

# Water extension ---------------------
# Crop water footprints --------------
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
                           wf_tot_m3_t)]

# convert columns to numeric
num_cols <- c("area_code", "item_code", "year", "production_t", "wfg_m3_t", 
              "wfb_cr_m3_t", "wfb_i_m3_t", "wf_tot_m3_t")
water_crop[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# exclude "other grasses for forage", "mixed grasses" (included in grazing in FABIO)
# and "flaxseed, processed but not spun" (not in current FABIO versions)
water_crop <- water_crop[!item_code %in% c(638, 639, 645, 773)]

# deal with regions
water_crop[iso3c == "F206", `:=` (iso3c = "SDN", area_code = 276, area = "Sudan")]

# define num_cols for aggregating water footprints
num_cols_prod <- c("production_t", "wfg_m3_t", "wfb_cr_m3_t", 
                   "wfb_i_m3_t", "wf_tot_m3_t")
num_cols_avg <- c("wfg_m3_t", "wfb_cr_m3_t", "wfb_i_m3_t", 
                      "wf_tot_m3_t")

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

# add comm and area codes
water_crop[, area_code := regions$code[match(iso3c, regions$iso3c)]]
water_crop[, comm_code := items$comm_code[match(item_code, items$ item_code)]]

# Extrapolate to 2022
# average over the last three available years
extra <- water_crop[year %in% c(2017:2019)]
extra[, (num_cols_avg) := lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      by = .(iso3c, item_code),
      .SDcols = num_cols_avg]

# change years and update production
year_conc <- data.table(old_year = 2016:2019,
                        new_year = 2020:max(years))
extra[, year := year_conc$new_year[match(year, year_conc$old_year)]]
extra[, production_t := biomass$value[match(paste(year, area_code, item_code),
                                      paste(biomass$year, biomass$area_code, 
                                            biomass$item_code))]]

# add extrapolated rows back to original table
water_crop <- rbind(water_crop, extra)

# calculate totals
water_crop[, (num_cols_avg) := lapply(.SD, function(x) x * production_t),
           .SDcols = num_cols_avg]

# rename cols to reflect change in unit
num_cols_avg <- sub("_t$", "", num_cols_avg)
setnames(water_crop, old = paste0(num_cols_avg, "_t"), new = num_cols_avg)

# optional todo: some small island states are missing from the water data -> gap fill?

# Pasture water footprints --------------------------------
# get grazing area from above
water_pasture <- copy(land_grass) # todo this should be cropland + land_grass

# get pasture intensities, assume all grazing is green water (i.e. no irrigated pastures)
ints <- fread("input/grazing/grazing.csv")[area_code %in% regions$code
  , .(area_code, m3_per_ha)]

# calculate totals
water_pasture[, wfg_m3_ha := ints$m3_per_ha[match(area_code, ints$area_code)]]
water_pasture[, wfg_m3 := wfg_m3_ha * value][, wfg_m3_ha := NULL ]


# Calculate water footprint of livestock and meat processing--------
live_ints <- fread("input/water/water_lvst.csv") 

#TODO keep capillary rise and irrigated as extensions (only for crops)
# plus processing and drinking water

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


# add meat
water_meat <- prod_land[element == "Production" &
                          item_code %in% live_ints$item_code]

water_lvst <- rbind(water_live, water_meat)
water_lvst <- water_lvst[!is.na(item)]

# match intensities with production
water_lvst[,`:=` (wfb_m3_unit = live_ints$blue[match(item_code,
                                                live_ints$item_code)])]

# calculate totals
water_lvst[, wfb_m3 := wfb_m3_unit * value]

#rm(meat, stocks, src_item, tgt_item, tgt_name)

# Combine all water data  ------------------------
water_green <- rbind(water_crop[, .(year, area_code, item_code, comm_code, value = wfg_m3)],
                     water_pasture[, .(year, area_code, item_code, comm_code, value)])
water_blue <- rbind(water_crop[, .(year, area_code, item_code, comm_code, 
                             value = na_sum(wfb_i_m3, wfb_cr_m3))],
                    water_lvst[, .(year, area_code, item_code, comm_code, value= wfb_m3)])

rm(water_crop, water_pasture, water_live, water_meat, water_lvst, extra, 
   year_conc, ints, live_ints, land_grass, grass_prod, prod_land)
                 

# CBS aggregations for alternative version ------------
sua_extensions <- list(biomass, water_green, water_blue, land)
names(sua_extensions) <- c("biomass", "water_green", "water_blue", "land")

conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
cbs_extensions <- lapply(sua_extensions, agg_sua_to_cbs)

# format
E_sua <- lapply(sua_extensions, format_extension)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)

# save
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/E_", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/E_", nm, ".rds"))
}


