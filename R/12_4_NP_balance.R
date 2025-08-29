# This script calculates nutrient inputs and outputs that are specific to nitrogen
# and balances them to obtain emissions to the ground

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

app <- readRDS("data/NPK/SF_application.rds")
harv_area <- readRDS("data/NPK/harv_area_incl_grazing.rds")

# Atmospheric deposition ----------------------------------------------------
# get crop nutrient balances
cnb <- readRDS("data/tidy/cnb_tidy.rds")

# distribute AD from cnb by area (assumption: cropland AD is the same as grassland AD (per ha)
# unit is kg/ha
dep <- app[, .(iso3c, year, item, country, ha)] 
dep[, total_area := sum(ha, na.rm = TRUE), by = .(iso3c, year)]
dep[, area_share := ha/total_area]

dep <- merge(cnb[element == "Cropland nitrogen" & item == "Atmospheric Deposition",
                 .(area, year, total_dep = value)], 
             dep, by.x = c("area", "year"), by.y = c("country", "year"))
dep[, dep := area_share * total_dep * 1000] # total deposition is in t
dep <- dep[, .(area, iso3c, year, item, dep)]

# Uptake -----------------------------------------------------------------
# Add uptake
# units are kg N|P removal/ton production

# Average nutrient content of crop products
cont <- fread("inst/NPK/nutrient_content_npk.csv")
cont[, `:=` (K = NULL)]
item_conc <- fread("inst/conc_crop-cbs.csv")

# match with cbs items
cont <- merge(cont, item_conc[, .(crop_item_code, item_cbs = cbs_item, cbs_item_code)],
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
cont[item == "grass", `:=`(item_cbs = "Grazing", cbs_item_code = 2001)]

#assume Grass N removal rate for fodder crops
cont <- rbind(cont, cont[item_code == 2001][, `:=` (
  item_code = 2000, item = "Fodder crops")]) 

# create full table from prod_trad_full 
cont_full <- readRDS("data/tidy/prod_trad_full.rds")[element == "Production" & year %in% years, 
                       .(area, item, item_code, year, production =value)]

# filter for primary crops
items_sua <- fread("inst/sua/items_sua.csv")
cont_full <- cont_full[item_code == 2001 | item_code %in% items_sua[processed == "primary" 
                                                & item_group %in% c("cereals", "roots and tubers",
                                                "sugar crops", "vegetables and melons", "fruits and berries",
                                                "nuts",  "spices", "oil-bearing crops",
                                                "pulses", "stimulants and beverages", "fodder crops",
                                                "other") , item_code_fcl]]


#aggregate countries not in fabio to RoW
cont_full[!area %in% regions$name , area := "RoW"]
cont_full <- cont_full[, .(production = sum(production, na.rm = TRUE)),
                       by = .(area, item, item_code, year)]

# add grazing "production" from supply table
grass_prod <- readRDS("data/sup_final.rds")[item_code == 2001, 
                                            .(area, item, item_code, year, production)]

# combine
cont_full <- rbind(cont_full, grass_prod)

# merge to obtain removal rates and cbs items
cont_full <- merge(cont_full, cont[, .(item_code, item_cbs, cbs_item_code, N, P)],
                   by = c("item_code"), allow.cartesian = TRUE)

# aggregate to cbs items, weighing nutrient contents by production
cont_full[, total_production := sum(production, na.rm =TRUE), 
          by = .(area, item_cbs, year)]
cont_full[, prod_share := production/total_production]
cont_full[, N_cbs := sum(N * prod_share, na.rm = TRUE),
          by = .(area, year, item_cbs)]
cont_full[, P_cbs := sum (P *prod_share, na.rm = TRUE),
          by = .(area, year, item_cbs)]

# only keep rows with unique cbs totals, now including weighted averages for nutrient
# content
cont_full <- unique(cont_full[, .(area, year, item = item_cbs, item_code = cbs_item_code,
                                  production = total_production, N = round(N_cbs, 2), 
                                  P = round(P_cbs, 2))])
cont_full <- cont_full[!item %in% c("","Sweeteners, Other")]


# multiply to get total N removal by country, year and crop in kg
cont_full[, `:=` (N_removal = production * N,
                  P_removal = production * P)]

# add iso codes
cont_full[, iso3c := regions$iso3c[match(area, regions$name)]]

# tidy
setcolorder(cont_full, "iso3c", before = 1)

rm(cont, cnb)
# Biological Fixation -----------------------------------------------------
# Factors from Kevin
# For grass fixation, global 20 Tg are distributed by grass area (Reis-Ely et al., 2025) 
# (bf for cereals from Ladha et al., 2016)
bf <- fread("inst/NPK/N_BF_factors.csv")

# create full table 
bf_full <- CJ(items[group == "Primary crops", item_code],
               regions[, iso3c],
               years)
setnames(bf_full, c("V1", "V2", "years"), c("item_code", "iso3c", "year"))

# add item names, harvested area, uptake and regions
bf_full[, item := items$item[match(item_code, items$item_code)]]
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
# bf rates table)
soybean_avg <- bf[item_code == 2555, mean(bio_fixation)]
bf_full[is.na(bf_rate) & item_code == 2555, bf_rate := soybean_avg]

# set to 0 where NA
bf_full[is.na(bf_rate), bf_rate := 0]

# multiply with harvested area for rice and sugar cane and with total N yield for
# everything else
bf_full[item_code %in% c(2807, 2536), 
        `:=` (biological_fixation = ha * bf_rate) ]
bf_full[!item_code %in% c(2807, 2536), 
        `:=` (biological_fixation = (N_yield * bf_rate)/100) ] #bf rate is given in percent not fractions of 1

# distribute global grass fixation according to a countries' share of global grass area
bf_full[, global_grass_fixation := 2e+10]
bf_full[item_code == 2001, total_area := sum(ha, na.rm = TRUE), by = .(year)] 
bf_full[, area_share := ha/total_area]
bf_full[is.na(area_share) |!is.finite(area_share), area_share := 0]
bf_full[item_code == 2001, biological_fixation := area_share * global_grass_fixation]

#clean up
bf_full <- bf_full[, .(area, iso3c, year, item, item_code, biological_fixation)]
bf_full[is.na(biological_fixation), biological_fixation := 0]

rm(bf, soybean_avg, grass_prod)

# Crop-wise manure application based on nutrient demand --------------------
manure_tidy <- readRDS("data/tidy/manure_tidy.rds") 
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

# add N uptake shares
manure_demand[, total_removal := sum(N_removal, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, removal_share := N_removal/total_removal][,`:=` (total_demand = NULL, 
                                                          total_removal = NULL)]

# create weights
# uptake is weighted more heavily as the demand share increases
# this ensures that if there is little N deficit in the country overall,
# not everything gets allocated to one crop (e.g., spices)
manure_demand[, uptake_weight := ifelse(demand_share < 0.3, 0, demand_share - 0.3) / 0.6]
manure_demand[uptake_weight > 1, uptake_weight := 1]

# to create one weighted share for each crop, removal and demand are weighted 
# with the dynamic weight created above
manure_demand[, weighted_share := removal_share * uptake_weight 
              + demand_share * (1-uptake_weight)]

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

# save and tidy
# saveRDS(manure, "data/NPK/manure_full.rds")
rm(manure_demand, manure_grass)

# N2O emissions (tier 1) ---------------------------------------------------------
# start with direct emissions (direct emissions from crop residues by crop are 
# provided for some crops by the FAO directly, for SF and manure and "other crops",
# only totals are reported)

# 1. start with direct inputs

# i. get direct emissions from crop residues from FAO where available
crop_em <- readRDS("data/tidy/crop_emissions_tidy.rds")
manure_em <- readRDS("data/tidy/manure_emissions_tidy.rds")

crop_res <- crop_em[element %in% c("Burning crop residues (Emissions N2O)",
                                    "Crop residues (Direct emissions N2O)")]
# convert to kg
crop_res[, `:=` (value = value * 1e6, unit = "kg")]

# exclude totals from crop_res 
crop_res <- crop_res[item != "All Crops"]

# aggregate regions not in fabio to RoW
crop_res[!iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]

# # aggregate (direct) emissions from burning and from mineralization
crop_res <- crop_res[, .(res_emissions = sum(value, na.rm = TRUE)), by = .(iso3c, item, year)]

# get full table
crop_res_full <- CJ(items[group == "Primary crops", item],
               regions[, iso3c],
               years)
setnames(crop_res_full, c("item", "iso3c", "year"))

# fill in available values 
crop_res <- merge(crop_res_full, crop_res, by = c("iso3c", "year", "item"),
                  all.x = TRUE)
# assume emissions from crop residues for crops not in FAO data at 0
crop_res[is.na(res_emissions), res_emissions := 0]


# ii. get direct emissions from synthetic fertilizer and manure
# not available from FAO by crop, working with tier 1 emission factors instead
# get inputs
direct_inputs <- merge( app[, .(iso3c, year, item, item_code, N_synthetic = N_kg)],
                      manure[, .(iso3c, year, item, N_man = N)],  
                      by = c("iso3c", "year", "item"), all.x = TRUE)

# add continents
direct_inputs[, continent := regions$continent[match(iso3c,regions$iso3c)]]

# multiply direct inputs with EFs
# EF for flooded rice used for Asian rice 
# (assumption: all Asian rice is flooded, other rice is not)
direct_inputs[continent == "ASI" & item_code == "2807", `:=` (n2o_syn = N_synthetic * 0.004,
                                                              n2o_man = N_man * 0.004)]
direct_inputs[!(continent == "ASI" & item_code == "2807"), `:=` (n2o_syn = N_synthetic * 0.01,
                                                                 n2o_man = N_man * 0.01)]

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
# direct_inputs[, `:=` (n2o_syn_estimated = sum(n2o_syn, na.rm = TRUE),
#                       n2o_man_estimated = sum(n2o_man, na.rm = TRUE)), 
#                       by = .(iso3c, year)]
# direct_inputs[, `:=` (ratio_syn = n2o_fao_syn/n2o_syn_estimated,
#                       ratio_man = n2o_fao_man/n2o_man_estimated)]
# 
# 
# # Scale n20 emissions to FAO totals
# direct_inputs[, `:=` (n2o_syn_scaled = n2o_syn * ratio_syn,
#                       n2o_man_scaled = n2o_man * ratio_man)]
# direct_inputs[, n2o_direct := na_sum(n2o_syn_scaled, n2o_man_scaled) ]
# direct_inputs[, `:=` (continent = NULL, n2o_fao_man = NULL, n2o_fao_syn = NULL,
#                       n2o_syn_estimated = NULL, n2o_man_estimated = NULL,
#                       n2o_syn_scaled = NULL, n2o_man_scaled = NULL, n2o_syn = NULL, 
#                       n2o_man = NULL, ratio_syn = NULL, ratio_man = NULL)]

# add emissions from residues
direct_inputs[, n2o_res := crop_res$res_emissions[match(paste(iso3c, year, item),
                                                        paste(crop_res$iso3c, crop_res$year,
                                                              crop_res$item))]]
direct_inputs[, n2o_direct := na_sum(n2o_syn, n2o_man, n2o_res)][,`:=` 
                                                                 (n2o_res = NULL)]
rm(crop_res, crop_res_full)


# 2. direct emissions from managed and drained soils
# distribute all FAO emissions from drained soils between crops that are on histosols in 2020
# check: how many of these are annual crops?

# get total emissions from drained organic soils
drain <- readRDS("data/tidy/drain_emissions_tidy.rds")

# filtering for emissions and excluding totals
drain <- drain[element == "Emissions (N2O)" & item != "Drained organic soils"] 

# convert to kg
drain[, `:=` (value = value * 1e6, unit = "kg")]

# make wide to get crop and grass emissions
drain <- dcast(drain[, .(iso3c, year, item, value)], iso3c + year ~ item, 
               value.var = "value")


# get soil types from HWSD 2.0 ( previously combined with Cropgrids and 
# aggregated in NPK_spatial)
soils <- readRDS("data/NPK/climate_soils_harvland.rds")

# filter for crops that are on histosols
soils <- soils[HWSD2 == 15][, climate_zone := NULL]

# add crop concordance
conc <- fread("inst/NPK/conc_NPK_items.csv")
soils <- merge(soils, conc, by = "crop", all = TRUE)
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
soils[ , iso_a3 := ifelse(is.na(fabio_iso),"ROW", fabio_iso)]
soils <- soils[, .(hist_area = sum(hist_area, na.rm = TRUE) ),
                               by = .(iso_a3, item)]

# add up total areas by country to get distribution of histosols
soils[, total := sum(hist_area, na.rm = TRUE), by = .(iso_a3)]
soils[, share := hist_area/total]

# get full table -> assumption: histosol shares for each crop are the same every year (strong)
drain_full <- CJ(items[group == "Primary crops", item],
                    regions[, iso3c],
                    years)
setnames(drain_full, c("item", "iso3c", "year"))

# add data to full table
drain_full <- merge(drain_full, drain, by = c("iso3c", "year"), all.x = TRUE)

# add grass emissions from drain to grazing
drain_full[ item == "Grazing", n2o_drain := `Grassland organic soils`][
  , `Grassland organic soils`:= NULL]

# add histosol shares from soils dataset
drain_full[, emission_share := soils$share[match(paste(iso3c, item),
                                                   paste(soils$iso_a3, soils$item))]]

# multiply to obtain emissions by crop
drain_full[ item != "Grazing", n2o_drain := `Cropland organic soils` * emission_share][
  ,`Cropland organic soils` := NULL
]
drain_full[, emission_share := NULL]
drain_full[is.na(n2o_drain), n2o_drain := 0]

rm(drain)

#TODO: compare grassland with cropland emissions and perennial with annual crops


# 3. Direct emissions from manure deposited on grasslands 
pasture <- readRDS("data/tidy/manure_tidy.rds")
pasture <- pasture[element == "Manure left on pasture (N content)"][, element := NULL]
pasture[, EFcpp := ifelse(item_code %in% c(960, 961, 1049, 1051, 1052, 1053, 1068, 1079), 
                          TRUE, FALSE)]
pasture[, n2o_prp := ifelse(EFcpp, value * 0.004, value * 0.003)]
pasture <- pasture[, .(n2o_prp = sum(n2o_prp, na.rm = TRUE), unit = unique(unit), 
                             area = unique(area)), 
                         by = .(iso3c, year)]
pasture[, `:=` (item = "Grazing")]
pasture[, `:=` (unit = NULL, area = NULL)]

# -> this should be scaled to FAO already, because they use the same methodology
# -> TODO double check

# Calculate indirect n2o emissions (from volatilized N that gets redeposited) and
# amount of N that gets volatilized

indirect_vol <- direct_inputs[,.(iso3c, year, item, item_code, N_man, N_synthetic)]
direct_inputs[, ':=' (N_man = NULL, N_synthetic = NULL)]

indirect_vol[, `:=` (frac_man = 0.21, frac_synth = 0.11)]
indirect_vol[, N_vol := na_sum(N_man * frac_man, N_synthetic * frac_synth)]
indirect_vol[, n2o_indirect := N_vol * 0.01]
indirect_vol[, `:=` (N_man = NULL, N_synthetic = NULL, frac_man = NULL, frac_synth = NULL)]
indirect <- indirect_vol[, .(iso3c, year, item, item_code, n2o_indirect)]
volatilization <- indirect_vol[, .(iso3c, year, item, item_code, N_vol)]

# -> this should be scaled to FAO already, since inputs are scaled and all other
# parts of the equation come from the IPCC methodology that the FAO also uses
# -> can only scale this if I also estimate other indirect emissions that are not needed in BAMBOO

rm(indirect_vol)


# Add up all n2o emissions
n2o_n <- merge(direct_inputs, drain_full, by = c("item", "iso3c", "year"))
n2o_n <- merge(n2o_n, pasture, by = c("item", "iso3c", "year"), all.x = TRUE)
n2o_n <- merge(n2o_n, indirect[,.(iso3c, year, item, n2o_indirect)], 
               by = c("item", "iso3c", "year"))
n2o_n[, n2o_n_total := na_sum(n2o_direct, n2o_prp, n2o_indirect, n2o_drain)] 
n2o_n[, (names(.SD)) := lapply(.SD, function(x) { x[is.na(x)] <- 0; x }), .SDcols = is.numeric]

# -> TODO: check against total_Fao data -> should always be a little less

# get N2 emissions based on N20 emissions
# -> multiply n2o with 67/42 (conversion from elemental n20 to N2, average soil 
# denitrification rate from Pan et al) -> assumption: n2 to n2o ratio from non- 
# denitrification processes is the same
n2o_n[, n2_n := n2o_n_total * 67/42] 

# N balance ----------------------------------------------------------
# (dep + bf + SF + man - uptake - n20 emissions - ammonia emissions - N2 "emissions" )
n_balance <- merge(n2o_n[, .(iso3c, year, item, n2o_n = n2o_n_total, n2_n)],
                   app[, .(iso3c, year, item, sf = N_kg)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, manure[, .(item, iso3c, year, manure = N)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, bf_full[, .(item, iso3c, year, bf = biological_fixation)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, cont_full[, .(item, iso3c, year, uptake = N_removal)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, dep[, .(item, iso3c, year, dep)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)
n_balance <- merge(n_balance, volatilization[, .(item, iso3c, year, nh3_n = N_vol)],
                   by = c("iso3c", "year", "item"), all.x = TRUE)

#rm(app, manure, bf_full, cont_full, dep, volatilization, n2o_n)

n_balance[, n_emissions_ground := na_sum(sf, manure, bf, dep, 
                                         -n2o_n, -n2_n, -nh3_n, -uptake)]

# set NAs to 0
numeric_cols <- names(n_balance)[sapply(n_balance, is.numeric) & names(n_balance) != "year"]
n_balance[, (numeric_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = numeric_cols]


# #tests
# n_balance[, gap := round(n_emissions_ground / uptake * 100,0)]
# nrow(n_balance[n_emissions_ground <0])
# nrow(n_balance[n_emissions_ground >0])
# 
# n_balance[!is.finite(gap), gap := 0][, gap := NULL]
# numeric_cols <- names(n_balance)[sapply(n_balance, is.numeric)]
# n_balance[, (numeric_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = numeric_cols]


# P_balance
p_balance <- merge(app[, .(iso3c, year, item, item_code, sf = P_kg)],
                   manure[, .(iso3c, year, item, item_code, man = P)],
                   by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_balance <- merge(p_balance, 
                   cont_full[, .(iso3c, year, item, item_code, uptake = P_removal)],
                   by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_balance[, p_emissions_ground := na_sum(sf, man, -uptake)]

# # tests
# p_balance[, gap := round(emissions_to_ground / P_removal * 100,0)]
# nrow(p_balance[emissions_to_ground <0])
# nrow(p_balance[emissions_to_ground >0])
# 
# p_balance[!is.finite(gap), gap := 0][, gap := NULL]
# numeric_cols <- names(p_balance)[sapply(p_balance, is.numeric)]
# p_balance[, (numeric_cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = numeric_cols]

# save 
saveRDS(n_balance, "data/NPK/N_balance.rds")
saveRDS(p_balance, "data/NPK/P_balance.rds")
