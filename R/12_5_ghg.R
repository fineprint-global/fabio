library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

# Prep -----------------
# read and tidy emissions
files <- list.files(path = "data/tidy", 
                    pattern = "emissions.*\\.rds$", 
                    full.names = TRUE)
data_list <- lapply(files, readRDS)
names(data_list) <- gsub("_emissions_tidy\\.rds$", "", basename(files))

# Aggregate countries not in FABIO to RoW and set key cols
data_list <- lapply(data_list, function(dt) {
  dt <- dt[, .(iso3c, item, year, element, area, unit, item_code, value)]
  dt[!iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]
  dt <- dt[, .(value = sum(value, na.rm = TRUE)),
                 by = .(iso3c, area, item, item_code, year, element, unit)]
  dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
  setkey(dt, "year", "item_code")
  return(dt)
})

# initialize lists for different emission types
list_ch4 <- list()
list_co2 <- list()
list_n2o <- list()

# set id cols
key_cols <- c("area_code", "item_code", "year")

# Livestock (CH4 and N2O)-----------------
lvst <- data_list[["lvst"]]

# filter relevant emission types (manure applied to crops or pasture is already attributed
# to crops)
lvst <- lvst[!grepl("Stocks|pasture|soils|applied|content|total", element), ]

# convert from kt to t
lvst[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
lvst <- dcast(lvst, area_code + item_code + year ~ element,
                   value.var = "value")

# add to lists for aggregating later
## create mapping
mappings <- rbind(
  data.frame(target = "list_ch4", nm = "lvst_enteric_fer",      col =  "Enteric fermentation (Emissions CH4)" ),
  data.frame(target = "list_ch4", nm = "lvst_manure_man",       col = "Manure management (Emissions CH4)"),
  data.frame(target = "list_n2o", nm = "lvst_dir_manure_man",   col = "Manure management (Direct emissions N2O)"),
  data.frame(target = "list_n2o", nm = "lvst_ind_manure_man",   col = "Manure management (Indirect emissions N2O)"),
  data.frame(target = "list_n2o", nm = "lvst_manure_man_total", col = "Manure management (Emissions N2O)"),
  stringsAsFactors = FALSE
)

## assign values to lists
for (i in seq_len(nrow(mappings))) {
  lst <- get(mappings$target[i])
  lst[[ mappings$nm[i] ]] <- lvst[, c(key_cols, value = mappings$col[i]), with = FALSE]
  setnames(lst[[ mappings$nm[i] ]], mappings$col[i], "value")
  assign(mappings$target[i], lst)
  rm(lst)
}

rm(lvst)

# Crops (N2O, CH4)--------------------------
# get n2o emissions from crops from NPK extension
crops_n <- readRDS("data/NPK/n2o_emis.rds")[, `:=` (item = NULL)]
crops_n[, area_code := regions$code[match(iso3c, regions$iso3c)]][, iso3c := NULL]

# convert from kg of elemental N to tonnes of n2o
cols <- c("n2o_n_syn", "n2o_n_man", "n2o_n_res", "n2o_n_drain", "n2o_n_vol",
          "n2o_n_leach", "n2o_n_total_direct", "n2o_n_total_indirect",
          "n2o_n_total")
crops_n[, (cols) := lapply(.SD, function(x) (x / 1000) * 44/28), .SDcols = cols]
setnames(crops_n, cols, gsub("_n_", "_", cols))

# get CH4 emissions from crops from fao data and exclude totals
crops_c <- data_list[["crop"]][element %like% "CH4" & item_code != 1712]

# convert from kt to t
crops_c[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
crops_c <- dcast(crops_c, area_code + item_code + year ~ element,
              value.var = "value")
crops <- merge(crops_n, crops_c, by = c("area_code", "item_code", "year"),
               all = TRUE)

# add to lists for aggregating later
## create mapping
mappings <- rbind(
  data.frame(target = "list_ch4", nm = "crops_rice",         col = "Rice cultivation (Emissions CH4)"),
  data.frame(target = "list_ch4", nm = "crops_res",          col = "Burning crop residues (Emissions CH4)"),
  data.frame(target = "list_n2o", nm = "crops_dir_syn",      col = "n2o_syn"),
  data.frame(target = "list_n2o", nm = "crops_dir_man",      col = "n2o_man"),
  data.frame(target = "list_n2o", nm = "crops_dir_res",      col = "n2o_res"),
  data.frame(target = "list_n2o", nm = "crops_dir_drain",    col = "n2o_drain"),
  data.frame(target = "list_n2o", nm = "crops_ind_vol",      col = "n2o_vol"),
  data.frame(target = "list_n2o", nm = "crops_ind_leach",    col = "n2o_leach"),
  data.frame(target = "list_n2o", nm = "crops_dir_total",    col = "n2o_total_direct"),
  data.frame(target = "list_n2o", nm = "crops_ind_total",    col = "n2o_total_indirect"),
  data.frame(target = "list_n2o", nm = "crops_total",        col = "n2o_total"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(mappings))) {
  lst <- get(mappings$target[i])
  lst[[ mappings$nm[i] ]] <- crops[, c(key_cols, mappings$col[i]), with = FALSE]
  setnames(lst[[ mappings$nm[i] ]], mappings$col[i], "value")
  assign(mappings$target[i], lst)
  rm(lst)
}


rm(crops_c, crops_n, crops)

# Net forest conversion (CO2, ha) --------------------
# get conversion attribution from Deduce 
# v.2.01. https://doi.org/10.5281/zenodo.18953516
# get data
fa_dl(file = "",
      link = "https://zenodo.org/records/18953516/files/1.%20DeDuCE_Deforestation_attributed%20to%20agricultural%20and%20forestry%20commodities%20(2001-2023).xlsx",
      path = "input/ghg/deforest.xlsx"
)
forest <- as.data.table(read_xlsx("input/ghg/deforest.xlsx"))
conc <- fread("inst/conc_deduce_fao.csv")

# add item and area codes
forest[, item_code := conc$item_code[match(Commodity, conc$Commodity)]]
forest <- forest[!is.na(item_code) & Year %in% years]
forest[, `:=` (area_code = regions$code[match(ISO, regions$iso3c)])]

# set value columns
cols <-   c("Deforestation attribution, unamortized (ha)",
            "Deforestation risk, amortized (ha)",
            "Deforestation emissions excl. peat drainage, unamortized (MtCO2)",
            "Deforestation emissions excl. peat drainage, amortized (MtCO2)",
            "Peatland drainage emissions (MtCO2)",
            "Deforestation emissions incl. peat drainage, amortized (MtCO2)")

# Deal with Sudan
forest[ISO == "SDN and SSD", ISO := "SDN"]

# Aggregate countries not in FABIO to RoW
forest[!ISO %in% regions$iso3c, `:=` (area_code = 999)]
forest <- forest[, lapply(.SD, sum, na.rm = TRUE),
                 by = .(area_code, Year, item_code),
                 .SDcols = cols]
setnames(forest, "Year", "year")

# Keep amortized, total emissions and land attribution (for luc pressures) and convert Mt to t
forest_em <- forest[, .(area_code, item_code, year, value = 
                          `Deforestation emissions excl. peat drainage, amortized (MtCO2)`*
                          1e6)]
luc <- forest[, .(area_code, item_code, year, value = 
                    `Deforestation risk, amortized (ha)`)]

# format and save luc directly
luc[, comm_code := items$comm_code[match(item_code, items$item_code)]]
E_luc <- format_extension(luc)  

conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
luc_cbs <- agg_sua_to_cbs(luc)
E_luc_cbs <- format_extension(luc_cbs, itms = items_cbs)

saveRDS(E_luc, "data/extensions/sua/luc_forest_to_agric.rds")
saveRDS(E_luc_cbs, "data/extensions/cbs/luc_forest_to_agric.rds")


# Add forest emissions to list
list_co2[["deforestation"]] <- forest_em

rm(forest, luc, luc_cbs, E_luc, E_luc_cbs, conc, items_cbs, forest_em)

# Drained organic soils (CO2) ----------
# get total drain emissions from FAO
drain_totals <- data_list[["drain"]][element == "Emissions (CO2)" & item_code != 6729]

# convert from kt to t
drain_totals[, `:=` (value = value * 1000, unit = "tonnes")]

# get drain shares from NPK extension
drain <- readRDS("data/NPK/drain_shares.rds") # in some cases, FAO reports small
# total emissions, but HWSD2 does not report any histosols -> in this case
# the drain emissions are assumed at 0 

# distribute cropland drain emissions acc. to drain shares
drain_crops <- drain_totals[item_code == 6727]
drain[, drain_co2_total := drain_crops$value[match(paste(iso3c, year),
                                                   paste(drain_crops$iso3c,
                                                         drain_crops$year))]]
drain[, value := emission_share * drain_co2_total][, `:=` (emission_share = NULL,
                                                           drain_co2_total = NULL)]


# attribute grassland drain emissions to grazing
drain_totals[item_code == 6728, item_code := 2001]
drain_grazing <- drain_totals[item_code == 2001, .(iso3c, year, item_code, value)]
drain <- rbind(drain, drain_grazing)

# replace iso3c with area_code
drain[, area_code := regions$code[match(iso3c, regions$iso3c)]][, iso3c := NULL]
setcolorder(drain, c("area_code", "year", "item_code", "value"))

list_co2[["crops_drain"]] <- drain

#tidy
rm(drain, drain_totals, drain_crops, drain_grazing)

# On-farm energy use (CO2, CH4, N2O) ----------------------
# TODO: improve estimation of forestry production share (use Exiobase/Gloria)

# get energy use from FAO
energy_total <- data_list[["energy"]][element != "Energy use in agriculture" & 
                                        item == "Total Energy"]

# convert from kt to t
energy_total[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
cols <- c("Emissions (CO2)" = "co2_energy",
          "Emissions (CH4)" = "ch4_energy",
          "Emissions (N2O)" = "n2o_energy")
energy_total[, colname := cols[element]]

energy_total <- dcast(energy_total, area_code + year ~ colname, value.var = "value")

# get production shares
prod <- readRDS("data/tidy/prod_trad_full.rds")

# filter and aggregate RoW
prod <- prod[year %in% years & element == "Production", 
             .(area_code, item_code, year, value)]
prod[!area_code %in% regions$code, area_code := 999]
prod <- prod[, .(production = sum(value, na.rm = TRUE)),
             by = .(area_code, year, item_code)]

# add grazing "production" from supply table and reduce by 75% to avoid overemphasizing its energy use
grass_prod <- readRDS("data/sup_final.rds")[year %in% years]
grass_prod <- grass_prod[item_code == 2001, 
                         .(area_code,  year, item_code, production = 0.25 * production)]

prod <- rbind(prod, grass_prod)

# filter for primary items and sum up their total production
prod_prim <- prod[item_code %in% items[processed == FALSE, item_code]]
prod_prim[, total_prod_prim := sum(production, na.rm = TRUE), by = .(area_code, year)]


# add forest total production (FAO energy domain includes forestry)
fore_prod <- readRDS("data/tidy/fore_prod_tidy.rds")[ year %in% years]
fore_prod[!area_code %in% regions$code, `:=` (area_code = 999, area = "RoW")]
fore_prod <- fore_prod[, .(production = sum(production, na.rm = TRUE)),
                       by = .(area_code, year)]

# Assume a conversion of 0.8t/m3 (rather crude)
# TODO: refine
fore_prod[, wood_production := production * 0.8][, production := NULL]

# Add total wood production to production of primary ag products and calculate
# total production incl. wood
prod_prim <- merge(prod_prim, fore_prod, by = c("area_code", "year"), all.x = TRUE)
prod_prim[, total_production := wood_production + total_prod_prim]

# get item shares of total production (incl. wood)
prod_prim[, share := production/total_production]

# distribute energy use to crops by production of primary items
energy <- energy_total[prod_prim, on = .(area_code, year)]

cols <- c("ch4_energy", "co2_energy", "n2o_energy")
energy[, (cols) := .SD * share, .SDcols = cols]

# add to lists
list_ch4[["farm_energy"]] <- energy[, .(area_code, year, item_code, value = ch4_energy)]
list_co2[["farm_energy"]] <- energy[, .(area_code, year, item_code,  value = co2_energy)]
list_n2o[["farm_energy"]] <- energy[, .(area_code, year, item_code, value = n2o_energy)]

rm(energy_total, energy, grass_prod, prod_prim, fore_prod)


# Waste from pre and postfarm production --------------
waste_totals <- data_list[["ppap"]]

# filter for emissions from waste relevant to industry (emissions from final
# demand waste are dealt with below)
waste_totals <- waste_totals[item %in% c("Incineration", "Industrial Wastewater", 
                                         "Solid Food Waste" ) &
                               !element %like% c("CO2eq")]

# convert from kt to t
waste_totals[, `:=` (value = value * 1000, unit = "tonnes")]

# widen totals
waste_totals[, colname := gsub(" ", "_", tolower(gsub("Emissions |[()]", "", 
                                                      paste(item, element))))]
waste_totals <- dcast(waste_totals, area_code + year ~ colname, value.var = "value")


## waste_incineration ------------------
# determine production shares for all agricultural products (processed and primary, 
# fishery and forestry not included, grass reduced by 75% to avoid overestimation)
prod[, share := production/sum(production, na.rm = TRUE), by = .(area_code, year)]

# distribute by production shares
waste_inc <- copy(prod)
waste_inc[, value := waste_totals$incineration_co2[match(
  paste(area_code, year), paste(waste_totals$area_code, waste_totals$year)
)]]
waste_inc[, value := value * share][
  , `:=` (share = NULL, production = NULL)]

prod[, share := NULL]

## Solid food waste -------------
# get loss-shares by item from SUAs
sua <- readRDS("data/tidy/sua_tidy.rds") # TODO: replace with balanced and completed sua version once available
waste_sol <- sua[, .(year, area_code, item_code, losses)]
waste_sol[!area_code %in% regions$code, area_code := 999]
waste_sol[, losses := sum(losses, na.rm = TRUE), by = .(year, area_code, item_code)]
waste_sol[, share := losses/sum(losses, na.rm = TRUE), by = .(year, area_code)]


# distribute totals according to shares
waste_sol[, total_ch4 := waste_totals$solid_food_waste_ch4[
  match(paste(area_code, year), paste(waste_totals$area_code, waste_totals$year))]]
waste_sol[, value := total_ch4 * share][, `:=` (total_ch4 = NULL, share = NULL, 
                                                losses = NULL)]

# reduce estimates by percentages of retail and household waste by comm_group
# -> these are included later in the fd extension
# to do so: get waste shares by production stage from https://www.fao.org/fileadmin/user_upload/suistainability/pdf/Global_Food_Losses_and_Food_Waste.pdf

### Tidy waste shares by production stage ---------
# Cleaning shares
shares <- fread("input/ghg/waste_shares.csv")
conc <- fread("inst/conc_waste_grps.csv")

# create full table
shares_full <- CJ(area_code = regions$code, item_code = items$item_code)
shares_full[, comm_group := items$comm_group[match(item_code, items$item_code)]]
shares_full[, waste_group := conc$waste_group[match(comm_group, conc$sua_group)]]
shares_full[, region := regions$waste_region[match(area_code, regions$code)]] 
shares_full <- shares_full[region != ""]
setcolorder(shares_full, c("area_code",  "region", "item_code",  "comm_group"))


# Add data
shares_full <- merge(shares_full, shares, by.x = c("region", "waste_group"),
                     by.y = c("region", "group"),
                     all.x = TRUE)


cols <- c( "harvest_production", "storage_transport", "processing", "distribution",
           "final_consumption")

# gap fill
# Convert share cols to numeric
shares_full[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]

# apply waste_group values to all commodities within the same comm_group
shares_full[, (cols) := lapply(.SD, function(x) {
  ifelse(is.na(x) | is.nan(x), mean(x, na.rm = TRUE), x)
}), by = .(region, waste_group), .SDcols = cols] 

# fill RoW with global mean
shares_full[, (cols) := {
  gmeans <- sapply(.SD, mean, na.rm = TRUE)
  lapply(seq_along(cols), function(i) {
    x <- .SD[[i]]
    ifelse(region == "Rest of World" & (is.na(x) | is.nan(x)), gmeans[i], x)
  })
}, by = .(comm_group), .SDcols = cols]

# shares_full[, (cols) := lapply(.SD, function(x) {
#   ifelse(is.na(x) | is.nan(x), mean(x, na.rm = TRUE), x)
# }), .SDcols = cols]   

# fill unavailable comm_groups with regional mean
shares_full[, (cols) := lapply(.SD, function(x) {
  ifelse(is.na(x) | is.nan(x), mean(x, na.rm = TRUE), x)
}), by = .(region), .SDcols = cols] 

# convert from % to fractions
shares_full[, (cols) := lapply(.SD, function(x) x/100), .SDcols = cols][, `:=`(waste_group = NULL, comm_group = NULL, harvest_production = NULL)]

shares <- copy(shares_full)
rm(shares_full)
# determine how much of total waste happens at retail and fd stage
## use a production cascade to reduce waste at every step to find total waste in %
shares[, prod_t0 := 100] # prod_t0 -> normalized production before harvest
shares[, prod_t1 := ifelse(item_code %in% items[processed == FALSE, item_code],
                           prod_t0 - (prod_t0 * storage_transport),
                           100)] # transport after harvest only ascribed to primary products
shares[, prod_t2 := ifelse(item_code %in% items[processed == TRUE, item_code],
                           prod_t1 - (prod_t1 * processing),
                           prod_t1)] # processing loss only ascribed to processed products
shares[, prod_t3 := prod_t2 - (prod_t2 * distribution)]
shares[, prod_t4 := prod_t3 - (prod_t3 * final_consumption)] # prod t4 -> percentage of 
# production that is not wasted

## find shares of fd of final waste
shares[, total_waste := prod_t0-prod_t4]  # determine total waste (in % of production)
shares[, waste_share_distribution := (prod_t2 * distribution)/ total_waste] # determine share of waste that gets lost at the retail level
shares[, waste_share_final_consumption := (prod_t3 * final_consumption)/total_waste] # determine share of waste that gets lost at the fd level

# add up retail and final demand waste and allocate them to final consumption 
#(model assumption)
shares[, fd_share := waste_share_distribution + waste_share_final_consumption]
shares <- shares[, .(area_code, item_code, fd_share)]

# reduce industrial food waste by retail and domestic share
waste_sol <- merge(waste_sol, shares, by = c("area_code", "item_code"), all.x = TRUE)
waste_sol[, fd_waste := value * fd_share]
waste_sol[, value := value - fd_waste]


list_ch4[["waste_solid"]] <- waste_sol[, .(area_code, item_code, year, value)]
list_co2[["waste_incineration"]] <- waste_inc

rm(shares)

## Industrial wastewater  -----------------
# distribute by production of processed products, but exclude most cereal products
# (no n2o emissions from waste occur here)
prod_proc <- prod[item_code %in% items[processed == TRUE, item_code] & 
                    !item_code %in% items[group == "cereals (excluding beer)" & 
                                            !item_code %in% c(64, 34, 23), item_code]]
prod_proc[, share := production/sum(production, na.rm = TRUE), by = .(area_code, year)]

# add totals
waste_ind <- merge(prod_proc, waste_totals[, .(area_code, year, industrial_wastewater_ch4, 
                                               industrial_wastewater_n2o)],
                   by = c("area_code", "year"), all.x = TRUE)

# define cols
cols <- c("industrial_wastewater_ch4", "industrial_wastewater_n2o")

# multiply to get values by item
waste_ind[, (cols) := lapply(.SD, function(x) x * share), .SDcols = cols]

# add to lists
list_ch4[["waste_ind_water"]] <- waste_ind[, .(area_code, year, item_code, 
                                               value = industrial_wastewater_ch4)]
list_n2o[["waste_ind_water"]] <- waste_ind[, .(area_code, year, item_code, 
                                               value = industrial_wastewater_n2o)]


rm(prod, prod_proc, waste_ind, waste_totals, waste_inc)

# Final demand -----------------
# Initialize lists
fd_list_ch4 <- list()
fd_list_co2 <- list()
fd_list_f_gases <- list()
fd_list_n2o <- list()

# get totals
fd_totals <- data_list[["ppap"]]
fd_totals <- fd_totals[ item %in% c("Food Retail", "Food Household Consumption", 
                                    "Domestic Wastewater") &
                          element %in%  c("Emissions (CO2)", "Emissions (CH4)", 
                                          "Emissions (N2O)", "Emissions (CO2eq) from F-gases (AR5)") ] 

# convert from kt to t
fd_totals <- fd_totals[ , `:=` (value = value * 1000, unit = "t")]

# create column with all info for widening
fd_totals[, element := tolower(gsub(".*\\((.*)\\).*", "\\1", element))]
fd_totals[, colname := paste0(tolower(gsub(" ", "_", item)), "_", 
                              gsub("ar5", "co2eq", element))]

fd_totals <- dcast(fd_totals, area_code + year ~ colname, value.var = "value")


# Distribute to products by consumption shares
sua <- readRDS("data/tidy/sua_tidy.rds") # TODO: replace with balanced sua version once available
sua[tourist < 0, tourist := 0] # preliminary
use_shares <- sua[, .(area_code, item_code, year, fc = na_sum(food, tourist))] # TODO: is this the only final use category?
use_shares[!area_code %in% regions$code, `:=` (area_code = 999)]
use_shares <- use_shares[, .(fc = sum(fc, na.rm = TRUE)), by = .(area_code, item_code, year)]
use_shares[, use_share := fc/sum(fc, na.rm = TRUE), by = .(area_code, year)]


# get full template
fd <- CJ(area_code = regions$code, year = years, item_code = items$item_code)

# add use shares to template
fd[, share := use_shares$use_share[match(paste(area_code, year, item_code),
                                         paste(use_shares$area_code,
                                               use_shares$year,
                                               use_shares$item_code))]]
fd[is.na(share), share := 0]

# add totals
fd <- merge(fd, fd_totals, by = c("area_code", "year"), all.x = TRUE)
cols <- c("domestic_wastewater_ch4", "domestic_wastewater_n2o"
          ,"food_household_consumption_ch4", "food_household_consumption_co2",
          "food_household_consumption_co2eq", "food_household_consumption_n2o",  
          "food_retail_ch4", "food_retail_co2", "food_retail_co2eq","food_retail_n2o") 
fd[, (cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols]

# mulitply shares with totals to arrive at final demand emissions
fd[, (cols) := lapply(.SD, function(x) x * share), .SDcols = cols][, share := NULL]

# add final demand emissions from solid food waste
fd[, solid_waste_ch4 := waste_sol$fd_waste[match(paste(area_code, item_code, year),
                                                 paste(waste_sol$area_code,
                                                       waste_sol$item_code,
                                                       waste_sol$year))]]
# add to lists for aggregations
mappings <- rbind(
  data.frame(target = "fd_list_ch4", nm = "domestic_wastewater",          col = "domestic_wastewater_ch4"),
  data.frame(target = "fd_list_ch4", nm = "food_household_consumption",   col = "food_household_consumption_ch4"),
  data.frame(target = "fd_list_ch4", nm = "food_retail",                  col = "food_retail_ch4"),
  data.frame(target = "fd_list_ch4", nm = "solid_waste",                  col = "solid_waste_ch4"),
  data.frame(target = "fd_list_co2", nm = "food_household_consumption",   col = "food_household_consumption_co2"),
  data.frame(target = "fd_list_co2", nm = "food_retail",                  col = "food_retail_co2"),
  data.frame(target = "fd_list_f_gases", nm = "food_household_consumption", col = "food_household_consumption_co2eq"),
  data.frame(target = "fd_list_f_gases", nm = "food_retail",                col = "food_retail_co2eq"),
  data.frame(target = "fd_list_n2o", nm = "domestic_wastewater",          col = "domestic_wastewater_n2o"),
  data.frame(target = "fd_list_n2o", nm = "food_household_consumption",   col = "food_household_consumption_n2o"),
  data.frame(target = "fd_list_n2o", nm = "food_retail",                  col = "food_retail_n2o"),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(mappings))) {
  lst <- get(mappings$target[i])
  lst[[ mappings$nm[i] ]] <- fd[, c(key_cols, value = mappings$col[i]), with = FALSE]
  setnames(lst[[ mappings$nm[i] ]], mappings$col[i], "value")
  assign(mappings$target[i], lst)
  rm(lst)
}

# tidy
rm(data_list, mappings, conc, waste_sol, fd, fd_totals, sua, use_shares)

# Total emissions ---------------
# set Nas to 0 everywhere 
for (i in c("list_ch4", "list_n2o", "list_co2",
            "fd_list_ch4", "fd_list_n2o", "fd_list_co2", "fd_list_f_gases")) {
  prefix <- gsub("list_", "", i)
  lst <- lapply(get(i), function(dt) {
    dt[, value := fifelse(is.na(value), 0, value)]
    dt
  })
  names(lst) <- paste0(prefix, "_", names(lst))
  assign(i, lst)
  rm(lst)
}


## Aggregate CO2, CH4, N2O -----------------

# Aggregate all data.tables within a list that do not have "total" in their
# name, save as an additional data.table in the list
targets <- c("list_ch4", "list_n2o", "list_co2",
             "fd_list_ch4", "fd_list_n2o", "fd_list_co2", "fd_list_f_gases")

for (tgt in targets) {
  nm  <- gsub("list_", "", tgt)
  lst <- get(tgt)
  lst[[nm]] <- rbindlist(lst[!grepl("total", names(lst), ignore.case = TRUE)], 
                         use.names = TRUE)[, 
                                           .(value = sum(value, na.rm = TRUE)), 
                                           by = .(area_code, year, item_code)]
  assign(tgt, lst)
}


## CO2eq --------------
# Assign 
gwp <- c(ch4 = 27, n2o = 273, co2 = 1, f_gases = 1)

for (tgt in targets) {
  lst <- get(tgt)
  lst <- setNames(lapply(names(lst), function(nm) {
    gwp_key <- names(gwp)[sapply(names(gwp), function(i) grepl(i, nm))]
    mult    <- as.numeric(gwp[gwp_key])
    copy(lst[[nm]])[, value := value * mult]
  }), names(lst))
  assign(paste0("gwp_", tgt), lst)
}

# Add up total co2eq ------------
gases <- names(gwp)

# merge and aggregate the data.tables from lists that end in the name of the 
# gas (these are the totals)
total <- merge_gwp_lists("gwp_list")
fd_total <- merge_gwp_lists("gwp_fd_list")

# Format as lists for extension aggregation
gwp_list_total <- list()
gwp_fd_list_total <- list()

gwp_list_total[["total"]] <- total
gwp_fd_list_total[["total"]] <- fd_total

rm(total, fd_total)

# Format as extensions -----------------
# add prefix to individual data.tables
lst_names <- ls(pattern = "^list_|^gwp_list")
data_all  <- do.call(c, lapply(lst_names, function(nm) {
  lst <- get(nm)
  if (startsWith(nm, "gwp_")) {
    setNames(lst, paste0("gwp_", names(lst)))
  } else {
    setNames(lst, paste0("ghg_", names(lst)))
  } 
}))

# add comm codes
data_all <- lapply(data_all, function(dt) {
  dt[, comm_code := items$comm_code[match(item_code, items$item_code)]]
  dt
})

# same for final demand version
fd_lst_names <- ls(pattern = "^fd_list_|^gwp_fd_list")
fd_data_all  <- do.call(c, lapply(fd_lst_names, function(nm) {
  lst <- get(nm)
  if (startsWith(nm, "gwp_")) {
    setNames(lst, paste0("gwp_", gsub("^fd_", "", names(lst))))
  } else {
    setNames(lst, paste0("ghg_", gsub("^fd_", "", names(lst))))
  }
}))

fd_data_all <- lapply(fd_data_all, function(dt) {
  dt[, comm_code := items$comm_code[match(item_code, items$item_code)]]
  dt
})

E_sua <- lapply(data_all, format_extension)
fd_E_sua <- lapply(fd_data_all, format_extension)

# convert to cbs level
conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")

cbs_extensions <- lapply(data_all, agg_sua_to_cbs)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)

#fd
fd_cbs_extensions <- lapply(fd_data_all, agg_sua_to_cbs)
fd_E_cbs <- lapply(fd_cbs_extensions, format_extension, itms = items_cbs)

# save
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}

for (nm in names(fd_E_sua)) {
  saveRDS(fd_E_sua[[nm]], paste0("data/extensions/fd_sua/", nm, ".rds"))
  saveRDS(fd_E_cbs[[nm]], paste0("data/extensions/fd_cbs/", nm, ".rds"))
}

rm(list = ls())
gc()
