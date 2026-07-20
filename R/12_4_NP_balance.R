# This script creates a comprehensive dataset for all relevant agricultural nutrient
# inputs and outputs

library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/03_gap_functions.R")

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/sua/items_sua.csv")
app <- readRDS("data/NPK/SF_application_sua.rds")
harv_area <- readRDS("data/NPK/harv_area_sua_incl_grazing.rds")

# Prepare -------------------
# create template for full tables
template_full <- CJ(app[, unique(item_code)],
                    regions[, iso3c],
                    years)
setnames(template_full, c("V1", "V2", "years"), c("item_code", "iso3c", "year"))
template_full[, item := items$item[match(item_code, items$item_code)]]
setcolorder(template_full, c("iso3c", "year" , "item", "item_code"))


# Removal (N&P) -----------------------------------------------------------------
# Add Removal
# units are kg N|P removal/ton production

# Average nutrient content of crop and fodder products
cont <- fread("inst/NPK/NP_cont_crops_grass.csv")[, `:=` (K = NULL)]
cont_fodder <- fread("inst/NPK/NP_cont_fodder.csv")

# add dry matter conversion for non-fodder crops and grazing
# TODO: if grazing is reported as fresh weight in sup table, needs to be converted 
# (check GLEAM dm content, should be .23)
cont[, dm_conv := 1]

# combine
cont <- rbind(cont, cont_fodder)

# create full table from prod_trad_full 
cont_full <- readRDS("data/tidy/prod_trad_full.rds")[element == "Production" & year %in% years, 
                       .(area_code, item, item_code, year, production = value)]

# filter for primary crops 
items_sua <- fread("inst/sua/items_sua.csv") 
cont_full <- cont_full[item_code %in% app$item_code]

# aggregate countries not in fabio to RoW
cont_full[!area_code %in% regions$code , area := "RoW"]
cont_full <- cont_full[, .(production = sum(production, na.rm = TRUE)),
                       by = .(area_code, item, item_code, year)]

# exclude aggregated fodder production and add in detailed fodder production
cont_full <- cont_full[item_code != 2000]
fodder_prod <- readRDS("data/tidy/fodder_crop_non_agg_tidy.rds")[year %in% years]
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


# aggregate fodder crop N content with production-weighted average
cont_full[, item_code_agg := ifelse(item_code %in% cont_fodder$item_code, 
                                      2000, item_code)]

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
cont_full[, iso3c := regions$iso3c[match(area, regions$name)]]


# add harvested area to cap grazing nutrient removal at 200 kg N /ha
# GLEAM production estimation yields unrealistically high values in some cases
cont_full[, harv_area := harv_area$ha[match(paste(iso3c, year, item_code),
                                            paste(harv_area$iso3c, harv_area$year,
                                                  harv_area$item_code))]]
cont_full[, rem_rate := N_removal/harv_area]
cont_full[item_code == 2001 & rem_rate > 200, scaling_factor := 200/rem_rate]
cont_full[!is.na(scaling_factor) & is.finite(scaling_factor), 
          `:=` (N_removal = N_removal * scaling_factor,
                P_removal = P_removal * scaling_factor)]
# tidy
cont_full[, `:=` (rem_rate = NULL, scaling_factor = NULL, harv_area = NULL)]
setcolorder(cont_full, "iso3c", before = 1)

rm(cont, cont_fodder, fodder_prod)

# Atmospheric deposition (N) ----------------------------------------------------
# get crop nutrient balances
cnb <- readRDS("data/tidy/cnb_tidy.rds")

# distribute AD from cnb by area 
# need to multiply cropland area rates with harvested area -> don't have cropland
# area for the whole timeline

# unit is kg/ha
dep <- app[, .(iso3c, year, item_code, country, ha)] 
dep <- merge(cnb[element == "Cropland nitrogen per unit area" & item == "Atmospheric deposition",
                 .(area, year, dep_rate = value)], 
             dep, by.x = c("area", "year"), by.y = c("country", "year"),
             all.y = TRUE)
dep[, dep := dep_rate * ha]
dep <- dep[, .(area, iso3c, year, item_code, dep)]

rm(cnb)


# Biological Fixation (N) -----------------------------------------------------
# For grass fixation, global 20 Tg are distributed by grass area (Reis-Ely et al., 2025) 
# bf for cereals from Ladha et al., 2016
# bf for fodder crops from Lassaletta et al (2014)
bf <- fread("inst/NPK/N_BF_factors_sua.csv")
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
bf_full[, bf_rate := as.numeric(fcoalesce(country_rate, regional_rate, global_rate))]
bf_full[, c("global_rate", "regional_rate", "country_rate") := NULL]

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
N_to_P_factors <- fread("inst/NPK/manure_NPK_ratios.csv")

# Exclude regions not in fabio
manure_tidy[, iso3c_fabio := regions$iso3c[match(iso3c, regions$iso3c)]]
manure_tidy[is.na(iso3c_fabio), `:=` (area = "RoW", iso3c = "ROW")]

# Aggregate national manure totals
manure <- manure_tidy[element == "Manure applied to soils (N content)"]
manure[, P_factor := N_to_P_factors$P[match(item_code, N_to_P_factors$item_code)]]
manure[, P := value * P_factor]
manure <- manure[, .(N = sum(value, na.rm = TRUE),
                     P = sum(P, na.rm = TRUE)), by = .(area, iso3c, year)]

# Grass manure (FAOSTAT direct)
manure_grass <- manure_tidy[element == "Manure left on pasture (N content)"]
manure_grass[, P_factor := N_to_P_factors$P[match(item_code, N_to_P_factors$item_code)]]
manure_grass[, P := value * P_factor]
manure_grass <- manure_grass[, .(N = sum(value, na.rm = TRUE),
                                 P = sum(P,     na.rm = TRUE)), by = .(iso3c, year)]
manure_grass[, `:=` (item_code = 2001, item = "Grazing")]

# Manure demand table, determined by other N inputs - outputs
manure_demand <- merge(
  cont_full[, .(iso3c, year, item, item_code, N_removal)],
  app[,       .(iso3c, year, item, item_code, sf = N_kg)],
  by = c("iso3c", "year", "item", "item_code"), all.y = TRUE)
manure_demand <- merge(manure_demand,
                       bf_full[, .(iso3c, item_code, year, biological_fixation)],
                       by = c("iso3c", "year", "item_code"), all.x = TRUE)
manure_demand <- merge(manure_demand,
                       dep[, .(iso3c, item_code, year, dep)],
                       by = c("iso3c", "year", "item_code"), all.x = TRUE)
manure_demand <- manure_demand[is.na(N_removal), N_removal := 0]

# Exclude grass
manure_demand <- manure_demand[item_code != 2001]

# Add harvested area, national totals, shares
manure_demand[, ha := harv_area$ha[match(paste(iso3c, year, item_code),
                                         paste(harv_area$iso3c, harv_area$year,
                                               harv_area$item_code))]]
manure_demand[is.na(ha), ha := 0]
manure_demand[, `:=` (
  total_N      = manure$N[match(paste(iso3c, year), paste(manure$iso3c, manure$year))],
  total_P      = manure$P[match(paste(iso3c, year), paste(manure$iso3c, manure$year))]
)]

# Demand share
manure_demand[, demand := pmax(0, na_sum(N_removal, -sf, -biological_fixation, -dep))]
manure_demand[, total_demand := sum(demand, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, demand_share := demand / total_demand]

# Removal share
manure_demand[, total_removal := sum(N_removal, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, removal_share := N_removal / total_removal]
manure_demand[, `:=` (total_demand = NULL, total_removal = NULL)]

# Area share
manure_demand[, area_share := ha / sum(ha, na.rm = TRUE), by = .(iso3c, year)]

# Distribute manure first by residual demand up until 250 kg/ha, then iteratively by N removal
cap <- 250
manure_demand[, `:=` (
  N_demand = demand_share * total_N,
  P_demand = demand_share * total_P
)]
manure_demand[, N_demand_capped := pmin(N_demand, cap * ha)]
manure_demand[, P_demand_capped := P_demand * (N_demand_capped / fifelse(N_demand > 0, N_demand, 0))]
manure_demand[, total_N_used := sum(N_demand_capped, na.rm = TRUE), by = .(iso3c, year)]
manure_demand[, N_leftover := pmax(0, total_N - total_N_used)]
manure_demand[, P_leftover := pmax(0, total_P - sum(P_demand_capped, na.rm = TRUE)),
              by = .(iso3c, year)]
manure_demand[, N_removal_pass := removal_share * N_leftover]
manure_demand[, N_removal_capped := pmin(N_removal_pass, pmax(0, cap * ha - N_demand_capped))]
manure_demand[, P_removal_capped := fifelse(N_removal_pass > 0,
                                            P_leftover * removal_share *
                                              (N_removal_capped / N_removal_pass),
                                            0)]

#  bridge: seed the iterator from previous result, then keep going 
tol <- 1e-6; max_rounds <- 100L; rnd <- 0L
manure_demand[, `:=`(
  tot_N   = fcoalesce(total_N, 0),
  tot_P   = fcoalesce(total_P, 0),
  cap_N   = cap * ha
)]
# what's allocated so far (NA-safe, and guaranteed <= cap_N)
manure_demand[, N_alloc := pmin(fcoalesce(N_demand_capped + N_removal_capped, 0), cap_N)]

repeat {
  manure_demand[, used         := sum(N_alloc),         by = .(iso3c, year)]
  manure_demand[, leftover     := pmax(0, tot_N - used)]
  manure_demand[, headroom     := pmax(0, cap_N - N_alloc)]
  manure_demand[, grp_headroom := sum(headroom),        by = .(iso3c, year)]
  manure_demand[, grp_done     := leftover <= tol | grp_headroom <= tol]
  
  chk <- unique(manure_demand[, .(iso3c, year, leftover, grp_done)])
  cat(sprintf("round %2d | groups done: %d/%d | undistributed N: %.0f\n",
              rnd, chk[grp_done == TRUE, .N], chk[, .N],
              chk[grp_done == FALSE, sum(leftover)]))
  
  if (manure_demand[, all(grp_done)] || rnd == max_rounds) break
  rnd <- rnd + 1L
  
  manure_demand[, w := fifelse(headroom > 0, fcoalesce(removal_share, 0), 0)]
  manure_demand[, wsum := sum(w), by = .(iso3c, year)]
  manure_demand[wsum <= 0, w := fifelse(headroom > 0, headroom, 0)]
  manure_demand[, wsum := sum(w), by = .(iso3c, year)]
  manure_demand[, N_alloc := pmin(N_alloc + fifelse(wsum > 0, leftover * w / wsum, 0), cap_N)]
}
if (rnd == max_rounds) warning("hit max_rounds before every group converged")

# P rides on N; also produces the final rate
manure_demand[, `:=`(
  P_alloc        = fifelse(tot_N > 0, tot_P * N_alloc / tot_N, 0),
  final_man_rate = fifelse(ha > 0, N_alloc / ha, NA_real_)
)]

# unallocatable residual (only >0 when manure > Σcaps in a group)
manure_demand[, N_residual := pmax(0, tot_N - sum(N_alloc)), by = .(iso3c, year)]

# check how much manure is unallocated
leftover <- unique(manure_demand[N_residual >1e-6, .(iso3c, year, N_residual)])


# tidy
drop_cols <- intersect(c("tot_N","tot_P","cap_N","used","leftover","headroom",
                         "grp_headroom","grp_done","w","wsum"), names(manure_demand))
manure_demand[, (drop_cols) := NULL]
manure_crops <- manure_demand[, .(iso3c, year, N = N_alloc, P = P_alloc, item,
                                  item_code)]
manure <- rbind(manure_crops, manure_grass, use.names = TRUE)

# capped <- manure_demand[N_removal_capped < N_removal_pass]
# capped[, final_man_rate := N_sce1/ha]
# 
# manure_demand[, final_man_rate := N_sce1/ha]

#tidy
rm(manure_crops, manure_demand, manure_grass, manure_tidy, N_to_P_factors, bf_fodder,
   leftover)


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

setcolorder(crop_res, c("iso3c", "area", "year", "item", "item_code", 
                        "element", "value", "unit"))


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
conc <-  fread("inst/NPK/conc_NPK_sua.csv")

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
climate[, wet_area := sum(harv_area_h[!zone %like% "Dry" & !is.na(zone)], na.rm = TRUE),
        by = .(iso3c, item)]
climate[, wet_fraction := wet_area/total_area]
climate <- unique(climate[, .(iso3c, item, item_code, wet_fraction)])


# get irrigation shares for estimating flooded vs non-flooded rice
rice_irr <- readRDS("data/NPK/irrigation.rds")
rice_irr <- rice_irr[item_code == 27]
rice_irr_full <- CJ(year = years, iso3c = regions$iso3c)[, item_code := 27]
rice_irr_full <- rice_irr_full[!(year %in% c(2010, 2011) & iso3c == "SSD")]
rice_irr_full[, irr_fraction := rice_irr$irrigated_harvarea_fraction[match(paste(iso3c, year),
                                                                           paste(rice_irr$iso3c,
                                                                                 rice_irr$year))]]

# Adding harvested area from FAO (rice only) for filling gaps
rice_irr_full <- merge(harv_area[item_code == 27, .(iso3c, year, item_code, ha)],
                       rice_irr_full, by = c("iso3c", "year", "item_code"), all = TRUE)

# Add country/region identifiers
rice_irr_full[, `:=` (country = regions$name[match(iso3c, regions$iso3c)],
                      region  = regions$region[match(iso3c, regions$iso3c)],
                      item    = items$item[match(item_code, items$item_code)])]

setcolorder(rice_irr_full, c("iso3c", "country", "region", "year", "item", "item_code",
                             "irr_fraction", "ha"))

# Gap filling
# setting NA irr_fraction to 0 where area is 0 or NA
rice_irr_full[is.na(ha), `:=` (ha = 0)]
rice_irr_full[, irr_fraction := fifelse(ha == 0, 0, irr_fraction)]

# Interpolate irr_fraction within country between years with at least two data points
rice_irr_full <- interpolate("irr_fraction", rice_irr_full)

# Extrapolate single values to whole timeline where only one value is available
rice_irr_full <- extrapolate("irr_fraction", rice_irr_full)

# Fill remaining NAs with regional average irr_fraction, weighted by harvested area
rice_irr_full[, irr_reg := sum(irr_fraction * ha / sum(ha, na.rm = TRUE), na.rm = TRUE),
              by = .(year, region)]
rice_irr_full[, irr_fraction := fifelse(is.na(irr_fraction), irr_reg, irr_fraction)]
rice_irr_full[, irr_reg := NULL]


# Tidy
setkey(rice_irr_full, iso3c, year)


# get inputs
direct_inputs <- merge(app[, .(iso3c, year, item, item_code, N_synthetic = N_kg)],
        manure[, .(iso3c, year, item_code, N_man = N)],
        by = c("iso3c", "year", "item_code"), all.x = TRUE)

# add wet fractions
direct_inputs[, wet_fraction := climate$wet_fraction[match(paste(iso3c, item_code),
                                                                   paste(climate$iso3c, climate$item_code))]]
  
# add irrigation fractions for rice
direct_inputs[, irr_fraction := rice_irr_full$irr_fraction[match(
    paste(iso3c, year, item_code),
    paste(rice_irr_full$iso3c, rice_irr_full$year, rice_irr_full$item_code)
  )]]
  

# Define emission factors (EF) according to IPCC tier 1 (2019)
EF_1_FR        <- 0.004
EF_1_wet_syn   <- 0.016
EF_1_wet_man   <- 0.006
EF_1_dry       <- 0.005
EF_1_default   <- 0.010
EF_3_cpp       <- 0.004
EF_3_so        <- 0.003
EF_4           <- 0.01
EF_5           <- 0.011
frac_man_vol   <- 0.21
frac_synth_vol <- 0.11
frac_lch       <- 0.24

# multiply direct inputs with EFs
# EF for flooded rice used for fraction of rice that is irrigated 
# (assumption: irrigated rice = flooded rice)
direct_inputs[item_code == 27, `:=`(
  n2o_n_syn = (N_synthetic * EF_1_FR * irr_fraction) + # flooded rice in all climates 
    (N_synthetic * EF_1_wet_syn * (1 - irr_fraction) * wet_fraction) + # non-flooded rice in wet climates
    (N_synthetic * EF_1_dry * (1 - irr_fraction) * (1 - wet_fraction)),# non-flooded rice in dry climates
  n2o_n_man = (N_man * EF_1_FR * irr_fraction) + # flooded rice in all climates 
    (N_man * EF_1_wet_man * wet_fraction * (1 - irr_fraction)) + # non-flooded rice in wet climates
    (N_man * EF_1_dry * (1 - wet_fraction) * (1 - irr_fraction)) # non-flooded rice in dry climates
  )]

direct_inputs[item_code != 27 &
                        !is.na(wet_fraction), 
                      `:=` (n2o_n_syn = (N_synthetic * wet_fraction * EF_1_wet_syn + # all other crops wet climates
                                           N_synthetic * (1- wet_fraction) * EF_1_dry),  # all other crops dry climates
                            n2o_n_man = N_man * wet_fraction * EF_1_wet_man +
                              N_man * (1 - wet_fraction) * EF_1_dry)]
  
  
# use default values where wet fractions are not available
direct_inputs[item_code != 27 &
                        is.na(wet_fraction), `:=` (n2o_n_syn = N_synthetic * EF_1_default, 
                                                   n2o_n_man = N_man * EF_1_default)]
direct_inputs[, `:=` (wet_fraction = NULL, irr_fraction = NULL)]
  
# set grass emissions from manure to 0 -> this will be calculated later
direct_inputs[item_code == 2001, `:=` (n2o_n_man = 0)]
  
# add emissions from residues
direct_inputs <- merge(direct_inputs, crop_em_full, by = c("iso3c", "year", "item_code", "item"),
                               all.x = TRUE)
  

rm(crop_em_full)

# (c) Emissions from managed and drained organic soils
# distribute all FAO emissions from drained soils between crops that are on histosols in 2020

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

# get soil type/climate zone combinations from HWSD 2.0 
# (previously combined with Cropgrids and 
# aggregated in NPK_spatial)
soils <- readRDS("data/NPK/climate_soils_harvland.rds")

# filter for crops that are on histosols -> will be aggregated later
soils <- soils[HWSD2 == 15][, climate_zone := NULL]

# add crop concordance
conc <- fread("inst/NPK/conc_NPK_sua.csv")

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
drain_full[is.na(n2o_n_drain), n2o_n_drain := 0]
drain_full[is.na(emission_share), emission_share := 0]

#save separately for GHG extension
saveRDS(drain_full[, .(iso3c, year, item_code, emission_share)], "data/NPK/drain_shares.rds")

drain_full[, emission_share := NULL]
rm(drain)

# (iv) Emissions from manure deposited on grasslands 
pasture <- readRDS("data/tidy/manure_tidy.rds")

# Aggregating countries not in fabio to ROW
pasture[ !iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]

# filtering for manure left on pasture
pasture <- pasture[element == "Manure left on pasture (N content)"][, element := NULL]

# multiplying with emission factors by species
pasture[, EFcpp := ifelse(item_code %in% c(960, 961, 1049, 1051, 1052, 1053, 1068, 1079), 
                          TRUE, FALSE)]
pasture[, n2o_prp := ifelse(EFcpp, value * EF_3_cpp, value * EF_3_so)]
pasture <- pasture[, .(n2o_prp = sum(n2o_prp, na.rm = TRUE), unit = unique(unit), 
                       area = unique(area)), 
                   by = .(iso3c, year)]
pasture[, `:=` (unit = NULL, area = NULL)]

# add emissions from manure inputs to grasslands to the other direct inputs
direct_inputs[, n2o_prp := pasture$n2o_prp[match(paste(iso3c, year), paste(pasture$iso3c, pasture$year))]]
direct_inputs[item_code == 2001, n2o_n_man := n2o_prp][, `:=` (n2o_prp = NULL)]


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

indirect_vol[, `:=` (frac_man = frac_man_vol, frac_synth = frac_synth_vol)]
indirect_vol[, N_vol := na_sum(N_man * frac_man, N_synthetic * frac_synth)]
indirect_vol[, n2o_indirect := N_vol * EF_4]
indirect_vol[, `:=` (N_man = NULL, N_synthetic = NULL, frac_man = NULL, frac_synth = NULL)]
indirect <- indirect_vol[, .(iso3c, year, item, item_code, n2o_indirect)]
volatilization <- indirect_vol[, .(iso3c, year, item, item_code, N_vol)]

rm(indirect_vol)

# (b) N2O emissions from leaching and N fraction that leaches

# get N content of crop residues
crop_res <- crop_res[element == "Crop residues (N content)"]
crop_res[!iso3c %in% regions$iso3c, iso3c == "ROW"]
crop_res <- crop_res[, .(value = sum(value, na.rm = TRUE)),
                     by = .(iso3c, item, item_code, year)]


# merge with FA and manure application
leach <- merge(app[, .(iso3c, year, item, item_code, sf = N_kg)],
               manure[, .(iso3c, year, item_code, man = N)],
               by = c("iso3c", "year", "item_code"),
               all.x = TRUE)
leach <- merge(leach, crop_res[, .(iso3c, year, item_code, res = value)],
               by = c("iso3c", "year", "item_code"),
               all.x = TRUE)

# calculate total N input to croplands
leach[, N := na_sum(sf, man, res)][, c("sf", "man", "res") := NULL]

# find fraction of crop that is in wet or moist climate zones -> leaching only occurs here
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
leach[, frac_leach := frac_lch * wet_fraction][, wet_fraction := NULL]
leach[, n_leach := frac_leach * N]

# calculate n2o emissions from leaching by multiplying fraction with emission factor
# from IPCC tier 1 EF5 == 0.011
leach[, n2o_n_leach := n_leach * EF_5]

# Merge all n2o emissions
n2o_n <- merge(direct_inputs, drain_full[, !"item", with = FALSE], by = c("item_code", "iso3c", "year"))
n2o_n <- merge(n2o_n, indirect[,.(iso3c, year, item_code, n2o_n_vol = n2o_indirect)], 
               by = c("item_code", "iso3c", "year"))
n2o_n <- merge(n2o_n, leach[,.(iso3c, year, item_code, n2o_n_leach)],
               by = c("item_code", "iso3c", "year"))

setnames(n2o_n, "res_emissions", "n2o_n_res")
# calculate total emissions
n2o_n[, n2o_n_total_direct := na_sum(n2o_n_syn, n2o_n_man, n2o_n_res,
                                     n2o_n_drain)] 
n2o_n[, n2o_n_total_indirect := na_sum(n2o_n_vol, n2o_n_leach)]
n2o_n[, n2o_n_total := na_sum(n2o_n_total_direct, n2o_n_total_indirect)]

# save for GHG extension
saveRDS(n2o_n, "data/NPK/n2o_emis.rds")

# set NAs to 0 
n2o_n[, (names(.SD)) := lapply(.SD, function(x) { x[is.na(x)] <- 0; x }), .SDcols = is.numeric]
rm(direct_inputs, drain_full,  pasture, soils, zone_names, climate, crop_res)

# (iii) N2 emissions  -----------------------------------------
# based on N2O emissions
# -> multiply N2O with 67/21 (conversion from elemental n20 to N2, average soil 
# denitrification rate from Pan et al) -> assumption: n2 to n2o ratio from non- 
# denitrification processes is the same
# since this is primarily used for the balance, n2o emissions from leaching are not included
n2o_n[, n2_n := na_sum(n2o_n_total_direct, n2o_n_vol) * 67/21] 

# (iv) NH3 emissions ----------------------------------
# Calculated under (ii.a) and saved as "volatilization"

# N balance ----------------------------------------------
# (dep + bf + SF + man - removal - direct n20 emissions - NH3 emissions - N2 "emissions" )
n_budget <- merge(n2o_n,
                  app[, .(iso3c, year, item_code, fa = N_kg)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, manure[, .(item_code, iso3c, year, man = N)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, bf_full[, .(item_code, iso3c, year, n_bf = biological_fixation)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, cont_full[, .(item_code, iso3c, year, rem = N_removal)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, dep[, .(item_code, iso3c, year, ad = dep)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, volatilization[, .(item_code, iso3c, year, nh3_n = N_vol)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)
n_budget <- merge(n_budget, leach[, .(iso3c, year, item_code, n_lr = n_leach)],
                  by = c("iso3c", "year", "item_code"), all.x = TRUE)

# add harv_area and item names for final version
n_budget[, item := items$item[match(item_code, items$item_code)]]
n_budget[, harv_area := harv_area$ha[match(paste(iso3c, year, item),
                                           paste(harv_area$iso3c,
                                                 harv_area$year,
                                                 harv_area$item))]]

# tidy
setcolorder(n_budget, c("iso3c", "year", "item", "item_code", "fa", "man", "ad",
                        "rem",  "n_bf", "n_lr",  "nh3_n", "n2_n",
                        "n2o_n_syn" , "n2o_n_man", "n2o_n_res",  "n2o_n_drain",           
                        "n2o_n_vol",  "n2o_n_leach",
                        "n2o_n_total_direct", "n2o_n_total_indirect",
                        "n2o_n_total"))

# set NAs to 0 and round to two digits
numeric_cols <- names(n_budget)[sapply(n_budget, is.numeric) 
                                & !names(n_budget) %in% 
                                  c("year", "item_code")]
n_budget[, (numeric_cols) := 
           lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
         .SDcols = numeric_cols]
n_budget[, (numeric_cols) :=
           lapply(.SD, function(x) round(x, 2)),
         .SDcols = numeric_cols]

#set emissions to 0 where FAO reports no area (data mismatch issue between
# FAO and NPK data)
n_budget[harv_area == 0, (setdiff(numeric_cols, harv_area)) := 0] 
setcolorder(n_budget, "harv_area",  after = "item_code")


# P weathering, runoff/erosion, deposition -------------------
# this data needs to be requested from the authors
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

# delete rows after 2022 (no data for this in this step, will be added below)
P_wdr <- P_wdr[year <= 2022]


# extrapolate using average from last three available years
cols <- c("weathering", "deposition", "runoff")
extra <- P_wdr[year %in% c(2020:2022)]
extra[, (cols) := lapply(.SD, function(x) mean(x, na.rm = TRUE)),
      by = .(iso3c, item_code),
      .SDcols = cols]

# delete years and reduce to unique footprints
extra <- unique(extra[, .(iso3c, item_code, item, weathering, deposition, runoff)])
years_extra <- (2023:max(years))
extra[, column := paste0(iso3c, "_", item_code)]

# create full table
extra_full <- CJ(year = years_extra, column = extra$column)
extra_full[, `:=` (iso3c = substr(column, 1, 3),
                   item_code = as.numeric(sub(".*_", "", column)))][, column := NULL]

# add data
extra <- merge(extra_full, extra, by = c("iso3c", "item_code"), all.x = TRUE)[, column := NULL]


# add extrapolated rows back to original table
P_wdr <- rbind(P_wdr, extra, use.names = TRUE)

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

# p_budget ------------------------------------------------
p_budget <- merge(app[, .(iso3c, year, item, item_code, fa = P_kg)],
                  manure[, .(iso3c, year, item, item_code, man = P)],
                  by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_budget <- merge(p_budget, 
                  cont_full[, .(iso3c, year, item, item_code, rem = P_removal)],
                  by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)
p_budget <- merge(p_budget,
                  P_wdr[, .(iso3c, year, item, item_code, ad = deposition, p_wea = weathering,
                            p_re = runoff)],
                  by = c("iso3c", "year", "item", "item_code"), all.x = TRUE)

# add item_code and harv_area for final version
p_budget[, item_code := items$item_code[match(item, items$item)]]
p_budget[,harv_area  := harv_area$ha[match(paste(iso3c, year, item),
                                           paste(harv_area$iso3c,
                                                 harv_area$year,
                                                 harv_area$item))]]

#tidy

numeric_cols <- setdiff(names(p_budget)[sapply(p_budget, is.numeric)], c("item_code", "year"))
p_budget[, (numeric_cols) := 
           lapply(.SD, function(x) fifelse(is.na(x), 0, x)), 
         .SDcols = numeric_cols]
p_budget[, (numeric_cols) :=
           lapply(.SD, function(x) round(x, 2)),
         .SDcols = numeric_cols]
setcolorder(p_budget, "harv_area",  after = "item_code")

# re-format as FABIO extensions ---------
# reduce to many data.tables with one value column to enable cbs aggregation
# add to lists for aggregations
n_list <- list()
p_list <- list()
key_cols <- c("iso3c", "year", "item", "item_code")

#extract value columns from single data.tables, save back to list
mappings <- rbind(
  data.frame(target = "n_list", nm = "fertilizer",          col = "fa"),
  data.frame(target = "n_list", nm = "manure",               col = "man"),
  data.frame(target = "n_list", nm = "removal",                  col = "rem_harv"),
  data.frame(target = "n_list", nm = "biological_fixation",     col = "n_bf"),
  data.frame(target = "n_list", nm = "atmospheric_deposition",   col = "ad"),
  data.frame(target = "n_list", nm = "nh3_n",                  col = "nh3_n"),
  data.frame(target = "n_list", nm = "leaching_runoff",        col = "n_lr"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(mappings))) {
  lst <- get(mappings$target[i])
  lst[[ mappings$nm[i] ]] <- n_budget[, c(key_cols, value = mappings$col[i]), with = FALSE]
  setnames(lst[[ mappings$nm[i] ]], mappings$col[i], "value")
  assign(mappings$target[i], lst)
  rm(lst)
}

# same for P
mappings <- rbind(
  data.frame(target = "p_list", nm = "fertilizer",          col = "fa"),
  data.frame(target = "p_list", nm = "manure",               col = "man"),
  data.frame(target = "p_list", nm = "removal",                  col = "rem_harv"),
  data.frame(target = "p_list", nm = "atmospheric_deposition",   col = "ad"),
  data.frame(target = "p_list", nm = "weathering",                  col = "p_wea"),
  data.frame(target = "p_list", nm = "runoff_erosion",        col = "p_re"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(mappings))) {
  lst <- get(mappings$target[i])
  lst[[ mappings$nm[i] ]] <- p_budget[, c(key_cols, value = mappings$col[i]), with = FALSE]
  setnames(lst[[ mappings$nm[i] ]], mappings$col[i], "value")
  assign(mappings$target[i], lst)
  rm(lst)
}

# Add prefix "n" or "p" to combine them in one list
# add prefix to individual data.tables
lst_names <- c("n_list", "p_list")
data_all  <- do.call(c, lapply(lst_names, function(nm) {
  lst <- get(nm)
  if (startsWith(nm, "n_")) {
    setNames(lst, paste0("n_", names(lst)))
  } else {
    setNames(lst, paste0("p_", names(lst)))
  } 
}))

# add area and comm codes
for (dt in data_all) {
  dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
  dt[, comm_code := items_sua$comm_code[match(item_code, items_sua$item_code)]]
}

# SUA
E_sua <- lapply(data_all, format_extension)

# CBS
conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
cbs_extensions <- lapply(data_all, agg_sua_to_cbs)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)

# save versions for extension
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}




rm(list = ls())
gc()
