# This script calculates a mass balance for Nitrogen and Phosphorous with the 
# goal of obtaining emissions to the ground which are needed in BAMBOO

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")[current==TRUE]
cbs <- F

if(cbs == TRUE){
  items <- fread("inst/items_full.csv")
  app <- readRDS("data/NPK/SF_application_cbs.rds")
  harv_area <- readRDS("data/NPK/harv_area_cbs_incl_grazing.rds")
  }else{
    items <- fread("inst/sua/items_sua.csv")
    app <- readRDS("data/NPK/SF_application_sua.rds")
    harv_area <- readRDS("data/NPK/harv_area_sua_incl_grazing.rds")}


# Removal (N&P) -----------------------------------------------------------------
# Add Removal
# units are kg N|P removal/ton production

# Average nutrient content of crop products
cont <- fread("inst/NPK/nutrient_content_npk.csv")
cont[, `:=` (K = NULL)]

#add dry matter conversion for grazing (from Lee et al., 2018)
cont[, dm_conv := ifelse(item_code == 2001,0.41,1)]

# Average nutrient content of fodder crops
cont_fodder <- fread("inst/NPK/fodder_NP_content.csv")

# bind all NP contents
cont <- rbind(cont, cont_fodder)

# match with cbs items
if(cbs == TRUE) {
  item_conc <- fread("inst/conc_crop-cbs.csv")
  cont <- merge(cont, item_conc[, .(crop_item_code, item_cbs = cbs_item, cbs_item_code)],
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
  cont[item == "grass", `:=`(item_cbs = "Grazing", cbs_item_code = 2001)]}


# create full table from prod_trad_full (this is the same for the cbs and sua version)
cont_full <- readRDS("data/tidy/prod_trad_full.rds")[element == "Production" & year %in% years, 
                       .(area_code, item, item_code, year, production = value)]


# filter for primary crops 
items_sua <- fread("inst/sua/items_sua.csv") # needed separately here
cont_full <- cont_full[item_code %in% items_sua[processed == FALSE & 
                                                  comm_group == "crops", item_code]]

#aggregate countries not in fabio to RoW
cont_full[!area_code %in% regions$code , area := "RoW"]
cont_full <- cont_full[, .(production = sum(production, na.rm = TRUE)),
                       by = .(area_code, item, item_code, year)]


# add fodder production from FAO files
cont_full <- cont_full[item_code != 2000]
fodder_prod <- readRDS("data/tidy/fodder_crop_non_agg_tidy.rds")
fodder_prod <- fodder_prod[element == "Production" & year %in% years, 
                           .(area_code, item, item_code, year, 
                             production = value )]

# add grazing "production" from supply table
grass_prod <- readRDS("data/sup_final.rds")[item_code == 2001, 
                                            .(area_code, item, item_code, year, production)]
cont_full <- rbind(cont_full, grass_prod, fodder_prod)
cont_full <- merge(cont_full, cont[, .(item_code,dm_conv, N, P)], by = c("item_code"), 
                   allow.cartesian = TRUE)
cont_full[, area := regions$name[match(area_code, regions$code)]]
setcolorder(cont_full, "area", before = "area_code")

# save version with all fodder crops
cont_full_fodder <- copy(cont_full)

# aggregate to CBS items (if cbs == T) or only aggregate fodder crops (if cbs = F)
# weighing the nutrient content by production
if(cbs == TRUE){
  cont_full[, item_code_agg := 
              item_conc$cbs_item_code[match(item_code, item_conc$crop_item_code)]]
  }else{
  cont_full[, item_code_agg := ifelse(item_code %in% cont_fodder$item_code, 
                                      2000, item_code)]
}
  
cont_full[item_code %in% c(2001), item_code_agg := 2001]
cont_full[item_code %in% cont_fodder$item_code, item_code_agg := 2000]
cont_full[, total_production := sum(production, na.rm =TRUE), 
            by = .(area, item_code_agg, year)]
cont_full[, prod_share := production/total_production]
cont_full[, N := sum(N * prod_share, na.rm = TRUE),
            by = .(area, year, item_code_agg)]
cont_full[, P := sum (P * prod_share, na.rm = TRUE),
            by = .(area, year, item_code_agg)]
cont_full[, dm_conv := sum(dm_conv * prod_share, na.rm = TRUE),
          by = .(area, year, item_code_agg)]
  # only keep rows with unique aggregated totals, now including weighted averages for nutrient
  # content
cont_full <- unique(cont_full[, .(area, year, item_code = item_code_agg,
                                    production = total_production, N = round(N, 2), 
                                    P = round(P, 2), dm_conv = round(dm_conv, 2))])
cont_full <- cont_full[!item_code %in% c(2543) & !is.na(item_code)]
cont_full[, item := items$item[match(item_code, items$item_code)]]
setcolorder(cont_full, c("area", "year", "item", "item_code", "N", "P","dm_conv",
                         "production"))


# multiply to get total N removal by country, year and crop in kg
cont_full[, `:=` (N_removal = production * N * dm_conv,
                  P_removal = production * P * dm_conv)]

# same for version with all fodder crops
cont_full_fodder[, `:=` (N_removal = production * N * dm_conv,
                  P_removal = production * P * dm_conv)]

# save version with all fodder crops
saveRDS(cont_full_fodder, "data/NPK/np_cont_full_fodder.rds")

# add iso codes
cont_full[, iso3c := regions$iso3c[match(area, regions$name)]]

# tidy
setcolorder(cont_full, "iso3c", before = 1)

rm(cont, cont_fodder, fodder_prod, cont_full_fodder)

# Biological Fixation (N) -----------------------------------------------------
# Factors from Kevin
# For grass fixation, global 20 Tg are distributed by grass area (Reis-Ely et al., 2025) 
# (bf for cereals from Ladha et al., 2016)
# bf for fodder crops from Lassaletta et al (2014)

if(cbs == TRUE){
  bf <- fread("inst/NPK/N_BF_factors_cbs.csv")
}else{
  bf <- fread("inst/NPK/N_BF_factors_sua.csv")
}

# create template for full tables
if(cbs == TRUE){
template_full <- CJ(items[group == "Primary crops", item_code],
              regions[, iso3c],
              years)
}else{
  template_full <- CJ(app[, unique(item_code)],
                regions[, iso3c],
                years)
}
setnames(template_full, c("V1", "V2", "years"), c("item_code", "iso3c", "year"))
template_full[, item := items$item[match(item_code, items$item_code)]]
setcolorder(template_full, c("iso3c", "year" , "item", "item_code"))

bf_full <- copy(template_full)

# add region names, harvested area, Removal 
bf_full[, area := regions$name[match(iso3c, regions$iso3c)]]
bf_full[, region := regions$region[match(iso3c, regions$iso3c)]]
bf_full[, N_yield := cont_full$N_removal[match(paste(iso3c, item_code, year),
                                               paste(cont_full$iso3c,
                                                     cont_full$item_code,
                                                     cont_full$year))]]
bf_full[, ha := harv_area$ha[match(paste(iso3c,item_code,year),
                                   paste(harv_area$iso3c,
                                         harv_area$item_code,
                                         harv_area$year))]]

# add BF rates to full table
bf_full <- merge(bf_full, bf[level == "global", .(item_code, global_rate = bio_fixation)], 
                 by = "item_code", all.x = TRUE)
bf_full <- merge(bf_full, bf[level =="regional", .(item_code, region, regional_rate = bio_fixation)], 
                 by = c("item_code", "region"), all.x = TRUE)
bf_full <- merge(bf_full, bf[level =="country", .(item_code, region, country_rate = bio_fixation)], 
                 by.x = c("item_code", "area"), by.y= c("item_code", "region"), all.x = TRUE)


# use the most local bf rate available
bf_full[!is.na(global_rate), bf_rate := global_rate]
bf_full[!is.na(regional_rate), bf_rate := regional_rate]
bf_full[!is.na(country_rate), bf_rate := country_rate]
bf_full[, `:=` (global_rate = NULL, regional_rate = NULL, country_rate = NULL,
                bf_rate = as.numeric(bf_rate))]


# average soybean rates for imputing RoW (RoW is the only thing missing in the original
# bf rates table as there is no global rate)
soybean_avg <- bf[item_code == 2555, mean(bio_fixation)]
bf_full[is.na(bf_rate) & item_code == 2555, bf_rate := soybean_avg]

# set to 0 where NA
bf_full[is.na(bf_rate), bf_rate := 0]

# multiply with harvested area for rice and sugar cane and with total N yield for
# everything else (done for both the cbs and sua version)
bf_full[item_code %in% c(27, 156, 2536, 2807), 
        `:=` (biological_fixation = ha * bf_rate) ]
bf_full[!item_code %in% c(27, 156, 2536, 2807), 
        `:=` (biological_fixation = (N_yield * bf_rate)/100) ] #bf rate is given in percent not fractions of 1

# distribute global grass fixation according to a countries' share of global grass area
bf_full[, global_grass_fixation := 2e+10]
bf_full[item_code == 2001, total_area := sum(ha, na.rm = TRUE), by = .(year)] 
bf_full[, area_share := ha/total_area]
bf_full[is.na(area_share) |!is.finite(area_share), area_share := 0]
bf_full[item_code == 2001, biological_fixation := area_share * global_grass_fixation]

#clean up
bf_full <- bf_full[, .(area, iso3c, year, item, item_code, biological_fixation)]

# calculate species-specific fodder crop bf
bf_fodder <- readRDS("data/tidy/fodder_crop_non_agg_tidy.rds")

# filter for leguminous fodder species, years and area
bf_fodder <- bf_fodder[item_code %in% bf$item_code &
                             year %in% years &
                             element == "Area harvested"]
bf_fodder[, bf_rate_kg := bf$bio_fixation[match(item_code, bf$item_code)]]
bf_fodder[, bf := value * bf_rate_kg]
bf_fodder <- bf_fodder[, .(bf = sum(bf, na.rm = TRUE)), 
                       by = .(year, area_code)]

# add columns to merge with bf_full
bf_fodder[, `:=` (iso3c = regions$iso3c[match(area_code, regions$code)],
                  item = "Fodder crops", item_code = 2000)]

# add fodder data to other bf data
bf_full[item_code == 2000, biological_fixation := bf_fodder$bf[match(paste(iso3c, item_code, year),
                                                                     paste(bf_fodder$iso3c, 
                                                                           bf_fodder$item_code,
                                                                           bf_fodder$year))]]

#clean up
bf_full[is.na(biological_fixation), biological_fixation := 0]

rm(bf, soybean_avg, grass_prod)



# Manure Application (N&P) ------------------------------------------
# crop-wise, based on assumptions about the relevance of nitrogen demand and
# Removal 
manure_tidy <- readRDS("data/tidy/manure_tidy.rds")

# Exclude regions not in fabio
manure_tidy[, iso3c_fabio := regions$iso3c[match(iso3c, regions$iso3c)]]
manure_tidy[is.na(iso3c_fabio), `:=` (area = "RoW", iso3c = "ROW")]

N_to_P_factors <- fread("inst/NPK/manure_NPK_ratios.csv")

manure <- manure_tidy[element ==  "Manure applied to soils (N content)"]

# add P 
manure[, P_factor := N_to_P_factors$P[match(item_code, N_to_P_factors$item_code)]]
manure[, P := value * P_factor]

# aggregate
manure <- manure[, .(N = sum(value, na.rm = TRUE),
                     P = sum(P, na.rm = TRUE)), by = .(area, iso3c, year)]

# calculate left over nutrient demand as N_removal - sf - bf (assumption: manure distribution
# is decided by N as the limiting nutrient)
manure_demand <- merge(cont_full[, .(iso3c, year, item, item_code, N_removal)],
                       app[, .(iso3c, year, item, item_code,  sf = N_kg)],
                       by = c("iso3c", "year", "item", "item_code"), all.y = TRUE)
manure_demand  <- merge(manure_demand, bf_full[, .(iso3c, item_code, year, biological_fixation)],
                        by = c("iso3c", "year", "item_code"), all.x = TRUE)
manure_demand[, demand := na_sum(N_removal, -sf , -biological_fixation)]

# exclude grass
manure_demand <- manure_demand[item_code != 2001]

# only consider positive demand for manure shares
manure_demand[demand < 0 , demand := 0]
manure_demand[, total_demand := sum(demand, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, demand_share := demand/total_demand]

# add total manure which needs to be distributed
manure_demand[, `:=` (total_N = manure$N[match(paste(iso3c, year),
                                               paste(manure$iso3c, manure$year))],
                      total_P = manure$P[match(paste(iso3c, year),
                                               paste(manure$iso3c, manure$year))])]

# add N Removal shares
manure_demand[, total_removal := sum(N_removal, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, removal_share := N_removal/total_removal][,`:=` (total_demand = NULL, 
                                                                 total_removal = NULL)]

# create weights
# Removal is weighted more heavily as the demand share increases
# this ensures that if there is little N deficit in the country overall,
# not everything gets allocated to one crop (e.g., spices)

manure_demand[, removal_weight := ifelse(demand_share < 0.3, 0, (demand_share - 0.3) / 0.6)]
manure_demand[removal_weight > 1, removal_weight := 1]

# to create one weighted share for each crop, removal and demand are weighted 
# with the dynamic weight created above
manure_demand[, weighted_share := removal_share * removal_weight 
              + demand_share * (1-removal_weight)]

# Because removal and demand shares do not add up to one, the weighted share 
# needs to be normalized between 0 and 1
manure_demand[, total_weighted_share := sum(weighted_share, na.rm = T), by = .(iso3c, year)]
manure_demand[, weighted_share := weighted_share / total_weighted_share]

# Apply weighted shares to distribute manure
manure_demand[, `:=` (N =  weighted_share * total_N,
                      P =  weighted_share * total_P)]  
manure <- manure_demand[, .(iso3c, year, item, item_code, N, P)]

# add grass manure N and P
manure_grass <- manure_tidy[element ==  "Manure left on pasture (N content)"]
manure_grass[, P_factor := N_to_P_factors$P[match(item_code, N_to_P_factors$item_code)]]
manure_grass[, P := value * P_factor]

# aggregate
manure_grass <- manure_grass[, .(N = sum(value, na.rm = TRUE),
                                 P = sum(P, na.rm = TRUE)), by = .(iso3c, year)]
manure_grass[, `:=` (item_code = 2001, item = "Grazing")]
setcolorder(manure_grass, names(manure))
manure <- rbind(manure, manure_grass)

# use full template (no manure data for 2 out of the 187 regions available)
manure_full <- copy(template_full)
manure <- merge(manure_full, manure[, !"item", with = FALSE], all.x = TRUE,
                by = c("iso3c", "year", "item_code"))

# save and tidy
if(cbs == TRUE){
saveRDS(manure, "data/NPK/manure_app_cbs.rds")
}else{
  saveRDS(manure, "data/NPK/manure_app_sua.rds")
}

rm(manure_demand, manure_full, manure_grass)


# Atmospheric deposition (N) ----------------------------------------------------
# get crop nutrient balances
cnb <- readRDS("data/tidy/cnb_tidy.rds")

# distribute AD from cnb by area 
# need to multiply cropland area rates with harvested area -> don't have cropland
# area for the whole timeline

# unit is kg/ha
dep <- app[, .(iso3c, year, item, country, ha)] 
# dep[, total_area := sum(ha, na.rm = TRUE), by = .(iso3c, year)]
# dep[, area_share := ha/total_area]

dep <- merge(cnb[element == "Cropland nitrogen per unit area" & item == "Atmospheric deposition",
                 .(area, year, dep_rate = value)], 
             dep, by.x = c("area", "year"), by.y = c("country", "year"),
             all.y = TRUE)
# dep[, dep := area_share * total_dep * 1000] # total deposition is in t
dep[, dep := dep_rate * ha]
dep <- dep[, .(area, iso3c, year, item, dep)]

rm(cnb)

# Emissions to the air (N) ----------------------------------
# Components for N emissions are:
# (i) Direct N2O emissions; 
# (ii) Indirect N2O emissions; 
# (iii) N2 emissions
# (iv) NH3 emissions


# (i) Direct N2O emissions --------------------------------------
# Components for direct N2O emissions are emissions from
# a) Crop residues
# b) Synthetic fertilizer and manure input to crops
# c) Drained organic soils
# d) Manure deposited on grasslands

# a) Emissions from crop residues
crop_res <- readRDS("data/tidy/crop_emissions_tidy.rds")
crop_res <- crop_res[element %in% c("Burning crop residues (Emissions N2O)",
                                    "Crop residues (Direct emissions N2O)",
                                   "Crop residues (N content)")]
# convert to kg, where necessary
crop_res[unit == "kt", 
         `:=` (value = value * 1e6, unit = "kg")]

#convert to elemental N, where necessary
crop_res[element %in%  c("Burning crop residues (Emissions N2O)",
                          "Crop residues (Direct emissions N2O)"),
          value := value * 28/44]

# exclude totals from crop_res 
crop_res <- crop_res[item != "All Crops"]

# aggregate regions not in fabio to RoW
crop_res[!iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]

# match to sua items (orginal table is for cbs items), all matches are 1:1
if(cbs == FALSE){
  conc <- fread("inst/conc_crop-cbs.csv")[cbs_item_code %in% crop_res$item_code]
  crop_res[, `:=` (item_code_sua = conc$crop_item_code[match(item_code, conc$cbs_item_code)],
                  item_sua = conc$crop_item[match(item_code, conc$cbs_item_code)])]
  crop_res[, `:=` (item_code = NULL, item = NULL)]
  setnames(crop_res, c("item_sua", "item_code_sua"), c("item", "item_code"))
  setcolorder(crop_res, c("iso3c", "area", "year", "item", "item_code", 
                          "element", "value", "unit"))
}

# aggregate (direct) emissions from burning and from mineralization
crop_em <- crop_res[element != "Crop residues (N content)", 
                    .(res_emissions = sum(value, na.rm = TRUE), 
                      item_code = unique(item_code)), 
                     by = .(iso3c, item, year)]

# get full table
crop_em_full <- copy(template_full)
  
# fill in available values 
crop_em_full <- merge(crop_em_full, crop_em[, !"item", with = FALSE], 
                  by = c("iso3c", "year", "item_code"),
                  all.x = TRUE)

# assume emissions from crop residues for crops not in FAO data at 0
crop_em_full[is.na(res_emissions), res_emissions := 0]

rm(crop_em)

# (b) get direct emissions from synthetic fertilizer and manure
# not available from FAO by crop, working with tier 1 emission factors instead
# find fraction of crops that is in wet/dry climates to differentiate emission factors
# Assumption: climate is the same for the crops in every year (okay assumption)

climate <- readRDS("data/NPK/climate_soils_harvland.rds")
zone_names <- fread("inst/NPK/climate_zones.csv")

# match climate zones to items
if(cbs == TRUE){
  conc <-  fread("inst/NPK/conc_NPK_cbs.csv")
}else{
  conc <-  fread("inst/NPK/conc_NPK_sua.csv")
}

climate[, zone := zone_names$label[match(climate_zone, zone_names$ids)] ]
climate[, item := conc$item[match(crop, conc$item_npk)]]
climate[, item_code := items$item_code[match(item, items$item)]]

climate <- climate[, .(item, zone_code = climate_zone, 
                       harv_area_h = sum(harv_area, na.rm =TRUE)),
                   by = .(iso_a3, item_code, zone, HWSD2)]
setnames(climate, "iso_a3", "iso3c")
setcolorder(climate, c("iso3c", "item", "item_code",  "zone_code",
                       "zone", "HWSD2", "harv_area_h"))
setorder(climate, iso3c, item, zone_code, HWSD2)

# find fractions of harvested area that is in wet climates
climate[, total_area := sum(harv_area_h, na.rm =TRUE),
        by = .(iso3c, item)]
climate[, wet_area := sum(harv_area_h[zone %like% "wet" | zone %like% "Wet"], na.rm = TRUE),
        by = .(iso3c, item)]
climate[, wet_fraction := wet_area/total_area]
climate <- unique(climate[, .(iso3c, item, item_code, wet_fraction)])


# get inputs
direct_inputs <- merge( app[, .(iso3c, year, item, item_code, N_synthetic = N_kg)],
                      manure[, .(iso3c, year, item_code, N_man = N)],  
                      by = c("iso3c", "year", "item_code"), all.x = TRUE)

# add continents
direct_inputs[, continent := regions$continent[match(iso3c,regions$iso3c)]]

# add wet fractions
direct_inputs[, wet_fraction := climate$wet_fraction[match(paste(iso3c, item_code),
                                              paste(climate$iso3c, climate$item_code))]]

# multiply direct inputs with EFs
# EF for flooded rice used for Asian rice 
# (assumption: all Asian rice is flooded, other rice is not)
direct_inputs[continent == "ASI" & item_code %in% c(27, 2807), `:=` (n2o_n_syn = N_synthetic * 0.004,
                                                              n2o_n_man = N_man * 0.004)]
direct_inputs[!(continent == "ASI" & item_code %in% c(27, 2807)) &
                !is.na(wet_fraction), 
              `:=` (n2o_n_syn = (N_synthetic * wet_fraction * 0.016 +
                                 N_synthetic * (1- wet_fraction) * 0.005), 
                    n2o_n_man = N_man * wet_fraction * 0.006 +
                      N_man * (1 - wet_fraction) * 0.005)]

# use default values where wet fractions are not available
direct_inputs[!(continent == "ASI" & item_code %in% c(27, 2807)) &
                is.na(wet_fraction), `:=` (n2o_n_syn = N_synthetic * 0.01, 
                    n2o_n_man = N_man * 0.01)]
direct_inputs[, wet_fraction := NULL]

# set grass emissions from manure to 0 -> this will be calculated later
direct_inputs[item_code == 2001, `:=` (n2o_n_syn = 0)]


# # Get FAO total direct N2O emissions from SF and manure for scaling
# crop_direct <- crop_em[element == "Synthetic fertilizers (Direct emissions N2O)"]
# crop_direct <- crop_direct[, .(iso3c, year, n2o_fao_syn = value)]
# manure_direct <- manure_em[element == "Manure applied to soils (Direct emissions N2O)" &
#                              item == "All Animals"]
# manure_direct <- manure_direct[, .(iso3c, year, n2o_fao_man = value)]
# direct_fao <- merge(manure_direct, crop_direct, by = c("iso3c", "year"), all = TRUE)
# 
# # convert to kg
# direct_fao[, (names(direct_fao)[3:4]) := lapply(.SD, function(x) x * 1e6), .SDcols = 3:4]
# 
# # add totals to direct_inputs
# direct_inputs <- merge(direct_inputs, direct_fao, by = c("iso3c", "year"), all.x = TRUE)
# direct_inputs[, `:=` (n2o_n_syn_estimated = sum(n2o_n_syn, na.rm = TRUE),
#                       n2o_n_man_estimated = sum(n2o_n_man, na.rm = TRUE)), 
#                       by = .(iso3c, year)]
# direct_inputs[, `:=` (ratio_syn = n2o_fao_syn/n2o_n_syn_estimated,
#                       ratio_man = n2o_fao_man/n2o_n_man_estimated)]
# 
# 
# # Scale n20 emissions to FAO totals
# direct_inputs[, `:=` (n2o_n_syn_scaled = n2o_n_syn * ratio_syn,
#                       n2o_n_man_scaled = n2o_n_man * ratio_man)]
# direct_inputs[, n2o_direct := na_sum(n2o_n_syn_scaled, n2o_n_man_scaled) ]
# direct_inputs[, `:=` (continent = NULL, n2o_fao_man = NULL, n2o_fao_syn = NULL,
#                       n2o_n_syn_estimated = NULL, n2o_n_man_estimated = NULL,
#                       n2o_n_syn_scaled = NULL, n2o_n_man_scaled = NULL, n2o_n_syn = NULL, 
#                       n2o_n_man = NULL, ratio_syn = NULL, ratio_man = NULL)]

# add emissions from residues
direct_inputs[, n2o_n_res := crop_em_full$res_emissions[match(paste(iso3c, year, item_code),
                                                        paste(crop_em_full$iso3c, crop_em_full$year,
                                                              crop_em_full$item_code))]]

rm(crop_em_full)
# (crop_em_full# (c) Emissions from managed and drained organic soils
# distribute all FAO emissions from drained soils between crops that are on histosols in 2020
# TODO: check how many of these are annual crops

# get total emissions from drained organic soils
drain <- readRDS("data/tidy/drain_emissions_tidy.rds")

# filtering for emissions and excluding totals
drain <- drain[element == "Emissions (N2O)" & item != "Drained organic soils"] 

# convert to kg
drain[, `:=` (value = value * 1e6, unit = "kg")]

# convert to elemental N
drain[, `:=` (value = value * 28/44)]

# make wide to get crop and grass emissions
drain <- dcast(drain[, .(iso3c, year, item, value)], iso3c + year ~ item, 
               value.var = "value")


# get soil types combined with climate zones from HWSD 2.0 
# (previously combined with Cropgrids and 
# aggregated in NPK_spatial)
soils <- readRDS("data/NPK/climate_soils_harvland.rds")

# filter for crops that are on histosols -> will be aggregated later
soils <- soils[HWSD2 == 15][, climate_zone := NULL]

# add crop concordance
if(cbs == TRUE){
  conc <- fread("inst/NPK/conc_NPK_cbs.csv")
}else{
  conc <- fread("inst/NPK/conc_NPK_sua.csv")
}
  
soils <- merge(soils, conc, by.x = "crop", by.y = "item_npk", allow.cartesian = TRUE)
soils <- soils[ item != ""]
setnames(soils, "harv_area", "hist_area")

# add FAO harvested area
soils[, harv_area_fao_total := harv_area$ha[match(paste(iso_a3, item), 
                                                  paste(harv_area$iso3c, 
                                                        harv_area$item))]]
 
# set histosol area to 0 where FAO reports no harvested area (data mismatch)
soils[harv_area_fao_total == 0 | is.na(harv_area_fao_total), hist_area := 0]

# Aggregating countries not in fabio to RoW
soils[, fabio_iso := regions$iso3c[match(iso_a3, regions$iso3c)]]
soils[ , iso3c := ifelse(is.na(fabio_iso),"ROW", fabio_iso)][, iso_a3:= NULL]
soils <- soils[, .(hist_area = sum(hist_area, na.rm = TRUE) ),
                               by = .(iso3c, item)]

# add up total areas by country to get distribution of histosols
soils[, total := sum(hist_area, na.rm = TRUE), by = .(iso3c)]
soils[, share := hist_area/total]

# get full table -> assumption: histosol shares for each crop are the same every year (strong)
drain_full <- copy(template_full)

# add data to full table
drain_full <- merge(drain_full, drain, by = c("iso3c", "year"), all.x = TRUE)

# add grass emissions from drain to grazing
drain_full[ item_code == 2001, n2o_n_drain := `Grassland organic soils`][
  , `Grassland organic soils`:= NULL]

# add histosol shares from soils dataset
drain_full[, emission_share := soils$share[match(paste(iso3c, item),
                                                   paste(soils$iso3c, soils$item))]]

# multiply to obtain emissions by crop
drain_full[ item_code != 2001, n2o_n_drain := `Cropland organic soils` * emission_share][
  ,`Cropland organic soils` := NULL
]
drain_full[, emission_share := NULL]
drain_full[is.na(n2o_n_drain), n2o_n_drain := 0]

rm(drain)

#TODO: compare grassland with cropland emissions and perennial with annual crops


# (iv) Emissions from manure deposited on grasslands 
pasture <- readRDS("data/tidy/manure_tidy.rds")

# Aggregating countries not in fabio to ROW
pasture[ !iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]

# filtering for manure left on pasture
pasture <- pasture[element == "Manure left on pasture (N content)"][, element := NULL]

# multiplying with emission factors by species
pasture[, EFcpp := ifelse(item_code %in% c(960, 961, 1049, 1051, 1052, 1053, 1068, 1079), 
                          TRUE, FALSE)]
pasture[, n2o_prp := ifelse(EFcpp, value * 0.004, value * 0.003)]
pasture <- pasture[, .(n2o_prp = sum(n2o_prp, na.rm = TRUE), unit = unique(unit), 
                             area = unique(area)), 
                         by = .(iso3c, year)]
pasture[, `:=` (unit = NULL, area = NULL)]

# add emissions from manure inputs to grasslands to the other direct inputs
direct_inputs[, n2o_prp := pasture$n2o_prp[match(paste(iso3c, year), paste(pasture$iso3c, pasture$year))]]
direct_inputs[item_code == 2001, n2o_n_man := n2o_prp][, `:=` (n2o_prp = NULL, continent = NULL)]


# -> this should be scaled to FAO already, because they use the same methodology
# -> FAO uses an emission factor of ~2%, so their emissions are an order of magnitude higher
# not sure where they get this from, should be 0.2-0-6% according to IPCC tier 1

# (ii) Indirect N2O emissions -----------------------------------------
# Components of indirect emissions are N2O emissions from
# (a) Volatilized N
# (b) Leaching and runoff


# (a) N2O emissions from volatilized N that gets redeposited and
# amount of N that gets volatilized
# -> NH3 gets calculated here as it is needed for calculating N2O emissions

indirect_vol <- direct_inputs[,.(iso3c, year, item, item_code, N_man, N_synthetic)]
direct_inputs[, ':=' (N_man = NULL, N_synthetic = NULL)]

indirect_vol[, `:=` (frac_man = 0.21, frac_synth = 0.11)]
indirect_vol[, N_vol := na_sum(N_man * frac_man, N_synthetic * frac_synth)]
indirect_vol[, n2o_indirect := N_vol * 0.01]
indirect_vol[, `:=` (N_man = NULL, N_synthetic = NULL, frac_man = NULL, frac_synth = NULL)]
indirect <- indirect_vol[, .(iso3c, year, item, item_code, n2o_indirect)]
volatilization <- indirect_vol[, .(iso3c, year, item, item_code, N_vol)]

rm(indirect_vol)

# (b) N2O emissions from leaching and N fraction that leaches
# -> not deducted in balance!

# get N content of crop residues
crop_res <- crop_res[element == "Crop residues (N content)"]
crop_res[!iso3c %in% regions$iso3c, iso3c == "ROW"]
crop_res <- crop_res[, .(value = sum(value, na.rm = TRUE)),
                       by = .(iso3c, item, item_code, year)]


# # merge with SF and manure application
leach <- merge(app[, .(iso3c, year, item, item_code, sf = N_kg)],
               manure[, .(iso3c, year, item_code, man = N)],
               by = c("iso3c", "year", "item_code"),
               all.x = TRUE)
leach <- merge(leach, crop_res[, .(iso3c, year, item_code, res = value)],
                by = c("iso3c", "year", "item_code"),
                all.x = TRUE)

#get total N input to croplands
leach[, N := na_sum(sf, man, res)][, c("sf", "man", "res") := NULL]

# find fraction of crop that is in wet climate zones -> leaching only occurs here
leach[, wet_fraction := climate$wet_fraction[match(paste(iso3c, item_code),
                                                   paste(climate$iso3c, climate$item_code))]]

# assume countries' average wet fraction for grazing and fodder crops
leach[, ha := harv_area$ha[match(paste(iso3c, item_code, year),
                                        paste(harv_area$iso3c, harv_area$item_code,
                                              harv_area$year))]]
leach[!item_code %in% c(2000, 2001), total_area := sum(ha, na.rm =TRUE), by = .(iso3c, year)]
leach[, avg_wet_fraction :=  sum((wet_fraction * ha) / total_area, na.rm = TRUE),
      by = .(iso3c, year)]
leach[is.na(wet_fraction), wet_fraction := avg_wet_fraction][, `:=` 
                                                             (ha = NULL, 
                                                               avg_wet_fraction = NULL, 
                                                               total_area = NULL)]


# Find fraction that leaches by multiplying IPCC standard leaching with fraction
# that is in wet climates (this assumes that crops are grown in the same climates
# every year -> not super strong)
leach[, frac_leach := 0.24 * wet_fraction][, wet_fraction := NULL]
leach[, n_leach := frac_leach * N]

# -> this is again 1 order of magnitude smaller than the FAO totals

# calculate n2o emissions from leaching by multiplying fraction with emission factor
# from IPCC tier 1 EF == 0.011
leach[, n2o_n_leach := n_leach * 0.011]


# Merge all n2o emissions
n2o_n <- merge(direct_inputs, drain_full[, !"item", with = FALSE], by = c("item_code", "iso3c", "year"))
n2o_n <- merge(n2o_n, indirect[,.(iso3c, year, item_code, n2o_n_vol = n2o_indirect)], 
               by = c("item_code", "iso3c", "year"))
n2o_n <- merge(n2o_n, leach[,.(iso3c, year, item_code, n2o_n_leach)],
                    by = c("item_code", "iso3c", "year"))

# create aggregated total emissions
n2o_n[, n2o_n_total_direct := na_sum(n2o_n_syn, n2o_n_man, n2o_n_res, n2o_n_drain)] 
n2o_n[, n2o_n_total_indirect := na_sum(n2o_n_vol, n2o_n_leach)]
n2o_n[, n2o_n_total := na_sum(n2o_n_total_direct, n2o_n_total_indirect)]

# set NAs to 0 
n2o_n[, (names(.SD)) := lapply(.SD, function(x) { x[is.na(x)] <- 0; x }), .SDcols = is.numeric]

rm(direct_inputs, drain_full,  pasture, soils, zone_names, climate, crop_res)

# -> TODO: check against total_Fao data 

# (iii) N2 emissions  -----------------------------------------
# based on N2O emissions
# -> multiply N2O with 67/21 (conversion from elemental n20 to N2, average soil 
# denitrification rate from Pan et al) -> assumption: n2 to n2o ratio from non- 
# denitrification processes is the same
# since this is primarily used for the balance, n2o emissions from leaching are not included
n2o_n[, n2_n := na_sum(n2o_n_total_direct, n2o_n_vol) * 67/21] 

# (iv) NH3 emissions ----------------------------------
# Calculated under (ii.a) and saved in "volatilization"

# N balance ----------------------------------------------
# (dep + bf + SF + man - removal - direct n20 emissions - NH3 emissions - N2 "emissions" )
n_balance <- merge(n2o_n,
                   app[, .(iso3c, year, item, fa = N_kg)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, manure[, .(item, iso3c, year, man = N)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, bf_full[, .(item, iso3c, year, n_bf = biological_fixation)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, cont_full[, .(item, iso3c, year, rem = N_removal)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, dep[, .(item, iso3c, year, ad = dep)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, volatilization[, .(item, iso3c, year, nh3_n = N_vol)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance[, bal := na_sum(fa, man, n_bf, ad, -n2o_n_total_direct,
                                         -n2_n, -nh3_n, -rem)]
n_balance <- merge(n_balance, leach[, .(iso3c, year, item, n_lr = n_leach)],
                        by = c("iso3c", "year", "item"), all.x = TRUE)



# add harv_area and item_codes for final version
n_balance[, item_code := items$item_code[match(item, items$item)]]
n_balance[, harv_area := harv_area$ha[match(paste(iso3c, year, item),
                                                 paste(harv_area$iso3c,
                                                       harv_area$year,
                                                       harv_area$item))]]

# tidy
setcolorder(n_balance, c("iso3c", "year", "item", "item_code", "fa", "man", "ad",
                              "rem",  "n_bf", "n_lr",  "nh3_n", "n2_n",
                              "n2o_n_syn" , "n2o_n_man", "n2o_n_res",  "n2o_n_drain",           
                              "n2o_n_vol",  "n2o_n_leach",
                              "n2o_n_total_direct", "n2o_n_total_indirect",
                              "n2o_n_total"))

# set NAs to 0
numeric_cols <- names(n_balance)[sapply(n_balance, is.numeric) 
                                      & !names(n_balance) %in% 
                                        c("year", "item_code")]
n_balance[, (numeric_cols) := 
                 lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
               .SDcols = numeric_cols]
n_balance[, (numeric_cols) :=
                 lapply(.SD, function(x) round(x, 2)),
               .SDcols = numeric_cols]

#set emissions to 0 where FAO reports no area (data mismatch issue between
# FAO and NPK data)
n_balance[rem == 0, (setdiff(numeric_cols, harv_area)) := 0] # all cols except harv_area should be 0

# # tests

nrow(n_balance[bal <0])
nrow(n_balance[bal >0])

# n_balance[, gap := round(bal / removal * 100,0)]
# n_balance[!is.finite(gap), gap := 0]

setcolorder(n_balance, "harv_area",  after = "item_code")

# saveRDS(n_balance,"/mnt/nfs_fineprint/tmp/fabio/v3/n_balance.rds")

# P weathering, runoff/erosion, deposition -------------------
P_weathering <- fread("input/NPK/P_weathering_kg.csv")
P_deposition <- fread("input/NPK/P_deposition_kg.csv")
P_runoff <- fread("input/NPK/P_runoff_erosion_kg.csv")

P_list <- list(P_weathering, P_deposition, P_runoff)
names(P_list) <- c("weathering", "deposition", "runoff")

# create empty data.table for filling
P_wdr <- copy(template_full)

# Tidy input data and merge the three files into one data.table
for (i in seq_along(P_list)) {
  
  # Melt (pivot longer) columns 2010–2022
  P_list[[i]] <- melt(
    P_list[[i]],
    measure.vars = as.character(2010:2022),
    variable.name = "year",
    value.name = "value",
    variable.factor = FALSE
  )
  P_list[[i]][, year := as.integer(year)]
  P_list[[i]] <- P_list[[i]][, .(value = sum(value, na.rm = TRUE)), 
                                     by = .(year, fabio_iso3c)]

  setnames(P_list[[i]], "value", names(P_list)[i])
  
  P_wdr <- merge(P_wdr, P_list[[i]], by.x = c("iso3c", "year"),
                by.y = c("fabio_iso3c", "year"), all.x = TRUE)
}

# add harvested area to distribute wdr
P_wdr[, ha := harv_area$ha[match(paste(iso3c, year, item_code),
                           paste(harv_area$iso3c, harv_area$year, harv_area$item_code))]]

# set FAO data to 0 where it is NA (this is how the FAO reports 0s)
P_wdr[is.na(ha), ha := 0]

# get area shares
P_wdr[, area_share := ha/(sum(ha, na.rm = TRUE)), by = .(iso3c, year)]

# distribute weathering, deposition and runoff to crops by area
P_cols <- c("weathering", "deposition", "runoff")
P_wdr[, (P_cols) := lapply(.SD, function (x) x * area_share), .SDcols = P_cols ]
P_wdr[, `:=` (ha = NULL, area_share = NULL)]

rm(P_list, P_cols, P_weathering, P_deposition, P_runoff)

# P_balance ------------------------------------------------
p_balance <- merge(app[, .(iso3c, year, item, item_code, fa = P_kg)],
                   manure[, .(iso3c, year, item, item_code, man = P)],
                   by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_balance <- merge(p_balance, 
                   cont_full[, .(iso3c, year, item, item_code, rem = P_removal)],
                   by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_balance <- merge(p_balance,
                   P_wdr[, .(iso3c, year, item, item_code, ad = deposition, p_wea = weathering,
                             p_re = runoff)],
                   by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_balance[, bal := na_sum(fa, man, ad, p_wea, p_re, -rem)]

# add item_code and harv_area for final version
p_balance[, item_code := items$item_code[match(item, items$item)]]
p_balance[,harv_area  := harv_area$ha[match(paste(iso3c, year, item),
                                                 paste(harv_area$iso3c,
                                                       harv_area$year,
                                                       harv_area$item))]]

#tidy
numeric_cols <- names(p_balance)[sapply(p_balance, is.numeric)]

p_balance[, (numeric_cols) := 
                 lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
               .SDcols = numeric_cols]
p_balance[, (numeric_cols) :=
                 lapply(.SD, function(x) round(x, 2)),
               .SDcols = numeric_cols]

setcolorder(p_balance, "harv_area",  after = "item_code")
# saveRDS(p_balance, "/mnt/nfs_fineprint/tmp/fabio/v3/p_balance.rds")

 # tests
# p_balance[, gap := round(bal / removal * 100,0)]
nrow(p_balance[bal <0])
nrow(p_balance[bal >0])

# p_balance[!is.finite(gap), gap := 0][, gap := NULL]
# numeric_cols <- names(p_balance)[sapply(p_balance, is.numeric)]
# p_balance[, (numeric_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = numeric_cols]

# save
if(cbs == TRUE){
  saveRDS(n_balance, "data/NPK/N_balance_cbs.rds")
  saveRDS(p_balance, "data/NPK/P_balance_cbs.rds")
}else{
  saveRDS(n_balance, "data/NPK/N_balance_sua.rds")
  saveRDS(p_balance, "data/NPK/P_balance_sua.rds")
}

#rm(list = ls())
#gc()
