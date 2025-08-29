# This script creates a dataset for N and P application by year, country and crop.
# Nutrients are always converted to kg and hectares. 

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/03_gap_functions.R")


years_full <- (1978:2022)

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]


# Harvested area per crop -----------------------------------------------
# harvested area from FAO is combined with grazing area from fabio's land use 
# extension

harv_area <- readRDS("data/tidy/crop_tidy.rds")

harv_area <- harv_area[year %in% years_full & element == "Area harvested" ]
harv_area[, iso3c := regions$iso3c[match(area, regions$name)]] 
harv_area[is.na(iso3c), `:=`(iso3c = "ROW", area = "RoW", area_code = 999)]
harv_area <- harv_area[, .(area = first(area), 
                           ha = sum(value, na.rm = TRUE)), 
                       by = .(iso3c, year, item, item_code)]

# for some small countries, area for sweeteners is reported -> this is a crop product, not primary crop
harv_area <- harv_area[item %in% items[group == "Primary crops", item]] 
harv_area[, area := NULL]

# add grazing area
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2/"
E <- readRDS(file=paste0(input_path,"E.rds"))

grazing <- rbindlist(lapply(names(E), function(year) {
  dt <- E[[year]]
  dt <- dt[item == "Grazing", .(year = as.integer(year), area, grassland)]
}), use.names = TRUE, fill = TRUE)
grazing[, `:=` (iso3c = regions$iso3c[match(area, regions$name)], item = "Grazing",
                item_code = 2001)][
  ,area := NULL]
setnames(grazing, "grassland", "ha")
setcolorder(grazing, colnames(harv_area))
harv_area <- bind_rows(harv_area, grazing)

saveRDS(harv_area[year %in% years], "data/NPK/harv_area_incl_grazing.rds")

# #tests harvested area 
# app <- merge(app, harv_area[, .(iso3c, item, value)], 
#                by.x = c("iso_a3", "item"), by.y = c("iso3c", "item"), 
#                all = TRUE)
# app[, ':='(ha = sum(ha, na.rm = TRUE),
#              value = sum(value, na.rm = TRUE)), by = .(item)]
# app[, diff := ha - value]
# test <- app[, .(item = unique(item))]
# test <- test[!is.na(item)]
# test <- merge(test, unique(app[, .(item, diff)]), by = "item", all.x = TRUE)


# Synthetic fertilizer application from HFUBC------------------------
# Starting with hfubc (this dataset has a timeline but relatively few data points)
hfubc <- fread("./input/extensions/HFUBC_fert_application_by_crop.csv")
conc <- fread("inst/NPK/conc_HFUBC_items.csv")

# subset
hfubc <- hfubc[,.(Country, iso3c = ISO3_code, Year, Crop, Crop_area_k_ha, N_k_t, P2O5_k_t, K2O_k_t )]

#convert fertilizer years to calendar years where necessary
hfubc <- hfubc[Year %in% c("1990/91","1991/92","1992/93", "1989/90", "1999/2000", "1998/99", "1997-98"), 
               Year := substr(Year, 1, 4)] 

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
hfubc <- merge(hfubc, conc, by.x = "Crop", by.y = "crop")
hfubc <- hfubc[item != "",]

# create averages weighted by area for items that are more aggregated in fabio
# than in hfubc
hfubc <- hfubc[, c(
  lapply(.SD, function(x) sum(x * ha, na.rm = TRUE) / sum(ha, na.rm = TRUE)),
  list(ha = fifelse(all(is.na(ha)), NA_real_, sum(ha, na.rm = TRUE)))
), by = .(item_code, iso3c, year), .SDcols = c("N_rate", "P_rate", "K_rate")]

# replace NAN with NAs
hfubc[, (names(hfubc)) := lapply(.SD, function(x) fifelse(is.nan(x), NA, x))]


# Synthetic fertilizer application from NPKGrids -------------------------
app <- readRDS("data/NPK/N_harvland_application_npkgrids.rds")
P_app <- readRDS("data/NPK/P_harvland_application_npkgrids.rds")
app <- merge(app, P_app, by = c("iso_a3", "crop"), all = TRUE)

conc <- fread("inst/NPK/conc_NPK_items.csv")

# for ensuring concordance, harvested area from the cropgrids dataset is used
harv_area_grids <- readRDS("data/NPK/harvland_area_cropgrids.rds")
harv_area_grids <- harv_area_grids[!is.na(iso_a3)]

app <- merge(app, harv_area_grids, by = c("crop", "iso_a3"), all = TRUE)
app <- merge(app, conc, by = "crop", all = TRUE)
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
app[, item := items$item[match(item_code, items$item_code)]]


# Reducing HFUBC and NPK columns (years don't overlap)
app[,  `:=`(N_rate = fifelse(is.na(N_rate), weighted_N, N_rate),
            P_rate = fifelse(is.na(P_rate), weighted_P, P_rate)),][
           , `:=` (weighted_N = NULL, weighted_P = NULL)]

# Creating full dt for gap filling
app_full <- CJ(items[group == "Primary crops", item_code],
                 regions[, iso3c],
                 years_full)
setnames(app_full, c("item_code", "iso3c", "year"))
app_full <- merge(app_full, app, by = c("iso3c", "year", "item_code"), all.x = TRUE)
app_full[, `:=` (country = regions$name[match(iso3c, regions$iso3c)],
                 region = regions$region[match(iso3c, regions$iso3c)],
                 item = items$item[match(item_code, items$item_code)])]


# Adding harvested area from FAO / land use extension
app_full <- merge(harv_area[, .(iso3c, year, item_code, ha)], app_full, 
                  by = c("iso3c", "year", "item_code"), all = TRUE)
setcolorder(app_full, c("iso3c", "country", "region", "year", "item", "item_code",
                        "N_rate", "P_rate", "ha"))

# Gap filling
# setting NA N_rates to 0 where area is 0
app_full[, `:=` (N_rate = fifelse(ha == 0, 0, N_rate),
                 P_rate = fifelse(ha == 0, 0, P_rate))]
app_full[is.na(ha), ha := 0]

# test <- app_full[ha > 0 & N_rate == 0] # values were checked and seem reasonable

# Interpolate app rates and area in same country between years with at least two data points
vars <- c("N_rate", "P_rate")
for (i in vars) {
  app_full <- interpolate(i, app_full)
}

# Extrapolate single values to whole timeline where only one value is available
for (i in vars) {
  app_full <- extrapolate(i, app_full)
}

#repeat step 1 after having gap-filled the area
app_full[ha == 0, `:=` (N_rate = 0, P_rate = 0)] 

# Step 4: fill remaining NAs for N_rate with regional averages, fill NA areas with 0
app_full[, `:=`(N_reg = sum(N_rate * ha / sum(ha, na.rm = TRUE), na.rm = TRUE),
                P_reg = sum(P_rate * ha / sum(ha, na.rm = TRUE), na.rm = TRUE)),
           by = .(year, item, region)]

app_full[, `:=` (N_rate = fifelse(is.na(N_rate),N_reg, N_rate),
                 P_rate = fifelse(is.na(P_rate),P_reg, P_rate))]
app_full[, `:=` (P_reg = NULL, N_reg = NULL)]



# filter for current years
app <- app_full[ year %in% years,]

# tidy
rm(conc, hfubc, app_full, E, grazing, harv_area_grids, years_full)


# # Rescaling to FAO total fertilizer use -------------------------------
# fert <- readRDS("data/tidy/fert_tidy.rds")
# 
# # get total N and P application per country per year
# fert <- fert[item %in% c("Nutrient nitrogen N (total)", "Nutrient phosphate P2O5 (total)"),]
# 
# 
# # widen
# fert <- dcast(fert, iso3c + year + area ~ item, value.var = "value")
# setnames(fert, c("Nutrient nitrogen N (total)", "Nutrient phosphate P2O5 (total)"),
#          c("N_fao_total", "P_fao_total"))
# 
# # convert to elemental P
# fert[, P_fao_total := P_fao_total * 0.436]
# 
# 
# # add to crop-specific application for rescaling
# app <- merge(app, fert[, .(iso3c, year, N_fao_total, P_fao_total)], 
#                     by = c("iso3c", "year"), all.x = TRUE)
# 
# 
# # calculate estimated total application (by country and year, for comparison with FAO)
# app[, `:=` (total_N_est = sum(N_rate * ha, na.rm = TRUE),
#             total_P_est = sum(P_rate * ha, na.rm = TRUE)),
#            by = .(year, iso3c)]
# 
# #calculate fao/estimated ratio (for scaling N rates)
# app[, `:=` (N_ratio = N_fao_total/total_N_est,
#             P_ratio = P_fao_total/total_P_est)]  
# 
# # calculate crop-wise application (for comparison with FAO grazing shares)
# app[, `:=` (crop_N_est = sum(N_rate * ha, na.rm = TRUE),
#             crop_P_est = sum(P_rate * ha, na.rm = TRUE)),
#            by = .(year, iso3c, item)]


# # add N crop/grass shares as estimated by Ludemann et al (2023) to compare with HFUBC data on grazing
# fert_crop_shares <- fread("input/extensions/fert_crop_shares.csv")
# N_crop_shares <- merge(regions[, .(name, iso3c)], fert_crop_shares[, .(Country, N)],  
#                        by.x = "name", by.y = "Country", all = TRUE)
# 
# # set other countries' crop shares to 100
# N_crop_shares[is.na(N), N := 100]
# 
# # filter redundant rows
# N_crop_shares <- N_crop_shares[, .(iso3c = unique(iso3c), N = first(N)), by =iso3c ]
# N_crop_shares[, grass_share := 100 - N]
# 
# # add to app to compare FAO shares with FUBC data
# app <- merge(app_full, N_crop_shares[, .(iso3c, grass_share)], 
#                     by = c("iso3c"), all.x = TRUE)
# app[item == "Grazing", grass_fert := total_fert * (grass_share/100) ]



# #tests 
# 
# # scatterplot total estimated N vs total N from FAO
# library(ggplot2)
# ggplot(app_full, aes(x = total_fert, y = total_N_est)) +
#   geom_point(alpha = 0.5, color = "blue") +  # Scatter points
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 1:1 reference line
#   labs(x = "Total N from FAO", y = "Total Estimated N",
#        title = "Scatterplot of FAO vs Estimated N Application") +
#   theme_minimal()
# 
# correlation <- cor(app_full$total_fert, app_full$total_N_est, use = "complete.obs")
# R_squared <- correlation^2
# 
# 
# #scatterplot grass estimates from FUBC vs grass shares from Ludemann (2023)
# grass_app <- app_full[item == "Grazing"]
# ggplot(grass_app, aes(x = crop_N_est, y = grass_fert)) +
#   geom_point(alpha = 0.5, color = "blue") +  # Scatter points
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 1:1 reference line
#   labs(
#     x = "Grass N Estimated",
#     y = "Grass N from FAO",
#     title = "Scatterplot of Grazing FAO vs Estimated N Application"
#   ) +
#   theme_minimal()
# 
# 
# correlation <- cor(grass_app$crop_N_est, grass_app$grass_fert, use = "complete.obs")
# R_squared <- correlation^2
# 
# 
# app_filtered <- app_full[total_fert < 1e+10, ]
# ggplot(app_filtered, aes(x = total_N_est, y = total_fert)) +
#   geom_point(alpha = 0.5, color = "blue") +  # Scatter points
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 1:1 reference line
#   labs(
#     x = "N Estimated",
#     y = "N from FAO",
#     title = "Scatterplot of FAO vs Estimated N Application"
#   ) +
#   theme_minimal()
# correlation <- cor(app_filtered$total_fert, app_filtered$total_N_est, use = "complete.obs")
# R_squared <- correlation^2

# # Add scaled fert rates
# app[, `:=` (N_rate_scaled = N_rate * N_ratio,
#                  P_rate_scaled = P_rate * P_ratio)]

# # tests
# ggplot(app_full, aes(x = N_rate, y = N_rate_scaled)) +
#   geom_point(alpha = 0.5, color = "blue") +  # Scatter points
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # 1:1 reference line
#   labs(
#     x = "N Estimated",
#     y = "N scaled",
#     title = "Scatterplot scaled vs. unscaled app"
#   ) +
#   theme_minimal()


# # tidy
# app[, `:=` ( crop_N_est = NULL, crop_P_est = NULL,total_N_est = NULL,
#              total_P_est = NULL, N_ratio = NULL, P_ratio = NULL, 
#                    N_fao_total = NULL, P_fao_total = NULL)]

# # cap to N to 700kg/ha -> add rest to grazing 
# # (cap first, then re-estimate total, allocate difference to grazing)
# app[, N_rate_capped := ifelse(N_rate_scaled < 700, N_rate_scaled, 700)]
# app[, overshoot := ifelse(N_rate_scaled > N_rate_capped,
#             (N_rate_scaled - N_rate_capped) * ha, 0)]
# app[, overshoot := sum(overshoot, na.rm = TRUE), by = .(iso3c, year)]
# app[item == "Grazing" & ha > 0, N_rate_capped := N_rate_scaled + 
#              (overshoot/ha)]
# 
# # cap P to 300 kg/ha (only ~ 10 data points)
# app[, P_rate_capped := ifelse(P_rate_scaled < 300, P_rate_scaled, 300)]
# app[, overshoot := ifelse(P_rate_scaled > P_rate_capped,
#                           (P_rate_scaled - P_rate_capped) * ha, 0)]
# app[, overshoot := sum(overshoot, na.rm = TRUE), by = .(iso3c, year)]
# app[item == "Grazing" & ha > 0, P_rate_capped := P_rate_scaled + 
#       (overshoot/ha)]


# tidy and save
app <- app[, .(iso3c, country, year, item, item_code,  ha, 
                             N_rate = round(N_rate, 2),
                         P_rate = round(P_rate, 2))]
app[, `:=` (N_kg = N_rate * ha,
            P_kg = P_rate * ha)]
app[, comm_code := items$comm_code[match(item, items$item)]]
setcolorder(app, c("iso3c", "country", "year", "item", "item_code", "comm_code", "N_kg",  "N_rate", 
                   "P_kg", "P_rate", "ha"))
setkey(app, iso3c, year, comm_code)

saveRDS(app, "data/NPK/SF_application.rds")

rm(list = ls())
gc()



# # Manure distribution -------------------------------------------------------
# # Following approach by Velthof et al. (2009) -> NO LONGER USED, MANURE IS DISTRIBUTED
# BASED ON NUTRIENT DEMAND AND UPTAKE
# 

#
# # The assumptions for fodder and crops are pretty strong, but this only makes up
# # ~ 30% of all manure, while 70% goes to grass (which we know from FAO data)
# manure <- readRDS("data/tidy/manure_tidy.rds")
# 
# # aggregate countries not in fabio to RoW
# manure[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
# manure <- manure[, .(area = unique(area),area_code = unique(area_code),
#                      item_code = unique(item_code), unit = unique(unit),
#                      value = sum(value, na.rm = TRUE)), 
#                  by = .(iso3c, item, year,element)]
# 
# # filter
# manure <- manure[ element %in% c("Manure applied to soils (N content)", 
#                        "Manure left on pasture (N content)")]
# manure[element == "Manure applied to soils (N content)", element := "N_crop"]
# manure[element == "Manure left on pasture (N content)", element := "N_grass"]
# 
# # widen by element
# manure <- dcast(manure, area_code + area + item_code + item +  year +
#                   unit + iso3c ~ element, value.var = "value")
# 
# # For P: get N/P ratios for manure from different animals
# ratios <- fread("inst/NPK/manure_NPK_ratios.csv")
# 
# # Add P to manure
# manure[, P_conv := ratios$P[match(item_code, ratios$item_code)]]
# manure[, `:=` (P_crop = N_crop * P_conv, P_grass = N_grass * P_conv)][, P_conv := NULL]
# 
# 
# # create full table for gap filling
# manure_full <-CJ(items[group == "Primary crops", item],
#                  regions[, iso3c],
#                  years)
# 
# setnames(manure_full, c("item", "iso3c", "year"))
# manure_full[, `:=` (area = regions$name[match(iso3c, regions$iso3c)],
#                     region = regions$region[match(iso3c, regions$iso3c)],
#                     comm_code = items$comm_code[match(item, items$item)])]
# 
# 
# 
# # get grassland manure from all species
# grazing <- copy(manure)
# grazing[, c("N_crop", "P_crop") := NULL]
# grazing <- grazing[, .(N = sum(N_grass, na.rm = TRUE), 
#                        P = sum(P_grass, na.rm = TRUE)),  
#                    by = .(iso3c, year)]
# grazing[, comm_code := "c062"] # TODO replace with item codes
# 
# # add back to manure_full
# manure_full <- merge(manure_full, grazing, by = c("iso3c", "year","comm_code"),
#                      all.x = TRUE)
# 
# rm(grazing)
# 
# # get manure that goes only to fodder crops (all sheep, goats and cattle incl. Buffalo)
# # -> Velthof assumptions + equines and camelids
# 
# fodder <- manure[item_code %in% c(946, 960, 961, 976, 1016, 1096, 1107, 1110, 1760), 
#                  .(N_fodder = sum(N_crop, na.rm = TRUE),
#                    P_fodder = sum(P_crop, na.rm = TRUE)),
#                  by = .(iso3c, year)]
# fodder[, comm_code := "c061"] #TODO replace with item code
# 
# # add back to manure_full
# manure_full <- merge(manure_full, fodder, by = c("iso3c", "year", "comm_code"),
#                      all.x = TRUE)
# manure_full[!is.na(N_fodder), N := N_fodder][, N_fodder := NULL]
# manure_full[!is.na(P_fodder), P := P_fodder][, P_fodder := NULL]
# 
# rm(fodder)
# 
# # Distribute N and P from pigs to fodder and nonfodder crops
# # -> The more intensive the system, the more goes to fodder crops
# intensity <- fread("inst/NPK/intensity_pig_farming.csv")
# intensity[, intensity := ((industrial + 0.5 * intermediate)/
#                             (industrial + intermediate + backyard))]
# intensity[, `:=` (intermediate = NULL, industrial = NULL, backyard = NULL, `region GLEAM` = NULL)]
# 
# 
# # Normalize intensity so that min = 0.25 and max = 0.75
# intensity[, normalized := 0.25 + 0.5 * (intensity - min(intensity)) / 
#             (max(intensity) - min(intensity))]
# 
# # get fodder and crop shares from manure 
# # (no need to adjust for what's already on grasslands, because pigs manure applied to
# # grasslands is always 0 acc. to FAO data)
# split <- manure[item_code %in% c(1049, 1051)]
# split[, region := regions$region[match(area_code, regions$code)]]
# split[, fodder_share := intensity$normalized[match(region,intensity$region)]]
# split[, `:=` (N_fodder = N_crop * fodder_share, P_fodder = P_crop * fodder_share)]
# 
# 
# # add fodder back to manure full
# split_fodder <- split[, .(N_fodder = sum(N_fodder, na.rm = TRUE),
#                           P_fodder = sum(P_fodder, na.rm = TRUE)),
#                           by = .(iso3c, year)]
# split_fodder[, comm_code := "c061"] # TODO
# 
# manure_full <- merge(manure_full, split_fodder, by = c("iso3c", "year", "comm_code"),
#                      all.x = TRUE)
# manure_full[, `:=` (N = na_sum(N, N_fodder),
#                     P = na_sum(P, P_fodder))][, `:=` (N_fodder = NULL, P_fodder = NULL)]
# 
# # reduce non-fodder crops share
# split[, `:=` (N_crop = N_crop * (1 - fodder_share),
#               P_crop = P_crop - (1 - fodder_share))]
# split <- split[, .(N_split = sum(N_crop, na.rm = TRUE),
#                         P_split = sum(P_crop, na.rm = TRUE)),
#                         by = .(iso3c, year)]
# 
# # get manure from poultry birds (all to non-fodder crops)
# crop <- manure[item_code %in% c(1068, 1079, 1053, 1052)]
# crop <- crop[, . (N_crop = sum(N_crop, na.rm = TRUE),
#                   P_crop = sum(P_crop, na.rm = TRUE)),
#              by = .(iso3c, year)]
# 
# # add up pig and poultry manure to get total manure going to nonfodder crops
# crop <- merge(crop, split, by = c("iso3c", "year"), all = TRUE)
# crop[, `:=` (N_crop = na_sum(N_crop, N_split),
#              P_crop = na_sum(P_crop, P_split))][, `:=`(N_split = NULL, P_split = NULL)]
# 
# # distribute crop manure between non-fodder crops according to their manure-intensities
# # 1 get average manure application per country 
# # 2 low gets 10% of average
# # 3 medium gets 75%
# # 4 high gets rest
# 
# # get crop area for determining average crop manure rates
# crop_area <- harv_area[!item %in% c("Grazing", "Fodder crops")
#                        & year %in% years,
#                        .(ha = sum(ha, na.rm = TRUE)),
#                        by = .(iso3c, year)]
# crop <- merge(crop, crop_area, by = c("iso3c", "year"), all.x = TRUE)
# 
# # get average application rates
# crop[, `:=` (N_rate = N_crop/ha,
#              P_rate = P_crop/ha)]
# crop[, `:=` (ha = NULL)]
# 
# # distribute by crop using Velthof's manure use intensities 
# # (own assumptions for crops not in Velthof)
# manure_use <- fread("inst/NPK/manure_use.csv") 
# manure_full <- merge(manure_full, crop, by = c("iso3c", "year"), all.x = TRUE)
# setnames(manure_full, c("N_crop", "P_crop"), c("N_crop_total", "P_crop_total"))
# 
# # add use intensities to manure_full
# manure_full[, use := manure_use$manure[match(comm_code, manure_use$comm_code)]]
# 
# # add back area 
# manure_full[, ha := harv_area$ha[match(paste(iso3c, year, item),
#                                        paste(harv_area$iso3c, harv_area$year, harv_area$item))]]
# 
# # exclude fodder and grazing for the distribution (grazing included in "fodder" use)
# manure_full[is.na(ha) | use == "fodder", ha := 0]
# 
# # set manure rates to 0 where area is 0
# manure_full[use == "fodder" | ha == 0, `:=` (N_rate = 0, P_rate = 0)] 
# 
# # reduce application rates for crops that have low or medium manure use
# manure_full[use == "low", grep("rate$", names(manure_full), value = TRUE) := 
#               lapply(.SD, function(x) x * 0.1), .SDcols = grep("rate$", names(manure_full), value = TRUE)]
# manure_full[use == "medium", grep("rate$", names(manure_full), value = TRUE) := 
#               lapply(.SD, function(x) x * 0.75), .SDcols = grep("rate$", names(manure_full), value = TRUE)]
# 
# 
# # get absolute N and P application 
# #-> this is incomplete, rest is allocated to high manure use crops below
# manure_full[, N_crop := ha * N_rate]
# manure_full[, P_crop := ha * P_rate]
# 
# # add up absolute application by country and group  for medium and low use crops
# # -> anything left over goes to "high" crops according to their area
# manure_full[, `:=` (N_low_plus_med = sum(N_crop[use %in% c("low", "medium")], na.rm = TRUE), 
#                     P_low_plus_med = sum(P_crop[use %in% c("low", "medium")], na.rm = TRUE)),
#             by = .(iso3c, year)]
# 
# # get N and P left for high use crops
# manure_full[, `:=` (N_high = N_crop_total - N_low_plus_med,
#                     P_high = P_crop_total - P_low_plus_med)]
# 
# # get N and P currently allocated to high use crops (numbers will be too low)
# manure_full[, `:=` (N_high_estimated = sum(N_crop[use == "high"], na.rm = TRUE),
#                     P_high_estimated = sum(P_crop[use == "high"], na.rm = TRUE)), 
#             by = .(iso3c, year)]
# 
# # get ratio between currently allocated N and P and left-over N and P
# manure_full[, `:=` (N_ratio = N_high/N_high_estimated,
#                     P_ratio = P_high/P_high_estimated)]
# 
# # allocate left-over N and P to high use crops
# # P
# manure_full[is.finite(N_ratio) & use == "high", N_rate := N_rate * N_ratio]
# manure_full[is.finite(N_ratio) & use == "high", N_crop := N_rate * ha]
# 
# # N
# manure_full[is.finite(P_ratio) & use == "high", P_rate := P_rate * P_ratio]
# manure_full[is.finite(P_ratio) & use == "high", P_crop := P_rate * ha]
# manure_full[, `:=` (N_high = NULL, N_high_estimated = NULL, P_high = NULL, P_high_estimated = NULL)]
# 
# # scale up manure for all crop items in case there are no high use items in a year/country
# # N
# manure_full[, total_N := sum(N_crop, na.rm = TRUE), by = .(iso3c, year)]
# manure_full[!is.finite(N_ratio) & use != "fodder",  # where the ratio is infinite, there are no high-use crops
#             ratio_med_low := total_N/N_low_plus_med] # allocate difference to the rest of the crops
# manure_full[!is.finite(N_ratio) & use != "fodder", 
#             N_rate := N_rate * ratio_med_low]
# manure_full[!is.finite(N_ratio) & use != "fodder", 
#             N_crop := N_rate * ha]
# 
# # P 
# manure_full[, total_P := sum(P_crop, na.rm = TRUE), by = .(iso3c, year)]
# manure_full[!is.finite(P_ratio) & use != "fodder",  # where the ratio is infinite, there are no high-use crops
#             ratio_med_low := total_P/P_low_plus_med] # allocate difference to the rest of the crops
# manure_full[!is.finite(P_ratio) & use != "fodder", 
#             P_rate := P_rate * ratio_med_low]
# manure_full[!is.finite(P_ratio) & use != "fodder", 
#             P_crop := P_rate * ha]
# 
# # reduce columns 
# manure_full[!comm_code %in% c("c061", "c062"), `:=` (N = N_crop, P = P_crop)] # TODO
# 
# # manure_tests <- copy(manure_full) # get for testing 
# manure_full <- manure_full[, .(area, iso3c, year, item, comm_code, N, P, ha)]
# 
# # save 
# saveRDS(manure_full, "data/NPK/manure_full.rds")

# # tests
# manure_tests[iso3c == "AFG" & year == 2010 & !comm_code %in% c("c062") ,
#             sum(N, na.rm = TRUE), by = .(iso3c, year)]
# 
# # compare grazing + fodder crops with rest
# manure_tests <- manure_tests[, .(iso3c, area, year, item, comm_code, N, P, N_crop_total, P_crop_total)]
# 
# N_tests <- manure_tests[, P_crop_total := NULL]
# N_tests[, fodder := N[comm_code == "c061"], by = .(iso3c, year)]
# N_tests[, grazing := N[comm_code == "c062"], by =.(iso3c, year)]
# N_tests[, non_crop := fodder + grazing]
# N_tests[, crop_share := N_crop_total/non_crop]
# 
# N_tests[comm_code == "c061", sum(N, na.rm = TRUE)]
# N_tests[comm_code == "c062", sum(N, na.rm = TRUE)]
# N_tests[!comm_code %in% c("c061", "c062"), sum(N, na.rm = TRUE)]
# 
# # compare grazing with rest -> maybe some countries where it matters more?



 
