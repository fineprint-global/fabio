library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

# Prep -----------------
# read and tidy emissions
files <- list.files(path = "data/tidy", 
                    pattern = "emissions.*\\.rds$", 
                    full.names = TRUE)
data_list <- lapply(files, readRDS)
names(data_list) <- gsub("_emissions_tidy\\.rds$", "", basename(files))

# Aggregate countries not in FABIO to RoW
data_list <- lapply(data_list, function(dt) {
  dt[!iso3c %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW")]
  dt <- dt[, .(value = sum(value, na.rm = TRUE)),
                 by = .(iso3c, area, item, item_code, year, element, unit)]
  dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
  return(dt)
})

# initialize lists for different emission types
list_ch4 <- list()
list_co2 <- list()
list_n2o <- list()

# Livestock (CH4 and N2O)-----------------
lvst <- data_list[["lvst"]]

# filter relevant emission types (manure applied to crops or pasture is already attributed
# to crops)
lvst <- lvst[!grepl("Stocks|pasture|soils|applied|content|total", element), ]

# convert from kt to t
lvst[, `:=` (value = value * 1000, unit = "tonnes")]

# rename elements for widening
cols <- c(
  "Enteric fermentation (Emissions CH4)"      = "ch4_ent_fer",
  "Manure management (Emissions CH4)"         = "ch4_man_man",
  "Manure management (Emissions N2O)"         = "n2o_total",
  "Manure management (Direct emissions N2O)"  = "n2o_direct",
  "Manure management (Indirect emissions N2O)"= "n2o_indirect"
)
lvst[, colname := cols[element]]


# widen by emission type
lvst <- dcast(lvst, area_code + item_code + year   ~ colname,
                   value.var = "value")

# add to lists for formatting later
list_ch4[["lvst"]] <- lvst[, .(area_code, item_code, year, ch4_ent_fer, ch4_man_man)] 
list_n2o[["lvst"]] <- lvst[, .(area_code, item_code, year, n2o_direct, n2o_indirect, n2o_total)]


# Crops (N2O, CH4)--------------------------
# get n2o emissions from crops from NPK extension
crops_n <- readRDS("data/NPK/n2o_emis.rds")

# convert from kg to tonnes
cols <- c("n2o_n_total_direct", "n2o_n_total_indirect", "n2o_n_total")
crops_n[, (cols) := lapply(.SD, function(x) x / 1000), .SDcols = cols]

# get CH4 emissions from crops from fao data
crops_c <- data_list[["crop"]][element %like% "CH4" & item_code != 1712]

# convert from kt to t
crops_c[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
cols <- c("Burning crop residues (Emissions CH4)" = "ch4_burn",
          "Rice cultivation (Emissions CH4)" = "ch4_rice")  
crops_c[, colname := cols[element]]

crops_c <- dcast(crops_c, area_code + item_code + year ~ colname,
              value.var = "value")

# add to lists for formatting later
list_n2o[["crops"]] <- crops_n[, .(area_code, item_code, year, n2o_n_total_direct, 
                                   n2o_n_total_indirect, n2o_n_total)]
list_ch4[["crops"]] <- crops_c

# Drained organic soils (CO2) ----------
# get total drain emissions from FAO
drain_totals <- data_list[["drain"]][element == "Emissions (CO2)" & item_code != 6729]

# convert from kt to t
drain_totals[, `:=` (value = value * 1000, unit = "tonnes")]

# get drain shares from NPK extension
drain <- readRDS("data/NPK/drain_shares.rds")

# distribute cropland drain emissions acc. to drain shares
drain_crops <- drain_totals[item_code == 6727]
drain[, drain_co2_total := drain_crops$value[match(paste(iso3c, year),
                                                   paste(drain_crops$iso3c,
                                                         drain_crops$year))]]
drain[, drain_co2 := emission_share * drain_co2_total][, `:=` (emission_share = NULL,
                                                               drain_co2_total = NULL)]


# attribute grassland drain emissions to grazing
drain_totals[item_code == 6728, item_code := 2001]
drain_grazing <- drain_totals[item_code == 2001, .(iso3c, year, item_code, drain_co2 = value)]
drain <- rbind(drain, drain_grazing)

list_co2[["drain"]] <- drain

#tidy
rm(drain, drain_totals, drain_grazing, drain_crops)

# On-farm energy use (CO2, CH4, N2O) ----------------------
# get energy use from FAO
energy_total <- data_list[["energy"]][element != "Energy use in agriculture" & 
                                  item == "Total Energy"]

# convert from kt to t
energy_total[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
cols <- c("Emissions (CO2)" = "energy_co2",
          "Emissions (CH4)" = "energy_ch4",
          "Emissions (N2O)" = "energy_n2o")
energy_total[, colname := cols[element]]

energy_total <- dcast(energy_total, area_code + year ~ colname, value.var = "value")

# get production shares
prod <- readRDS("data/tidy/prod_trad_full.rds")

# filter and aggregate RoW
prod <- prod[year %in% years & element == "Production" &
               item_code %in% items[processed == FALSE, item_code], 
             .(area_code, item_code, year, value)]
prod[!area_code %in% regions$code, area_code := 999]
prod <- prod[, .(production = sum(value, na.rm = TRUE)),
             by = .(area_code, year, item_code)]

# add grazing "production" from supply table
grass_prod <- readRDS("data/sup_final.rds")[year %in% years]
grass_prod <- grass_prod[item_code == 2001, 
                         .(area_code,  year, item_code,  production)]

prod <- rbind(prod, grass_prod)

# get prod shares
prod[, share := production/sum(production, na.rm = TRUE), by = .(area_code, year)]

# distribute energy use to crops by production of primary items
energy <- energy_total[prod, on = .(area_code, year)]
setcolorder(energy, "item_code", after = 2)

cols <- c("energy_ch4", "energy_co2", "energy_n2o")
energy[, (cols) := .SD * share, .SDcols = cols]

# add to lists
list_ch4[["energy"]] <- energy[, .(area_code, year, item_code, energy_ch4)]
list_co2[["energy"]] <- energy[, .(area_code, year, item_code, energy_co2)]
list_n2o[["energy"]] <- energy[, .(area_code, year, item_code, energy_n2o)]

# Waste from pre and postfarm production --------------
waste_totals <- data_list[["ppap"]]

# filter for in-scope individual items (for now) -> domestic wastewater significant,
# but out of scope for extensions -> TODO: should this be a final demand E vector?
waste_totals <- waste_totals[item %in% c("Incineration" , "Industrial Wastewater", "Solid Food Waste") &
                 !element %like% c("CO2eq")]

# convert from kt to t
waste_totals[, `:=` (value = value * 1000, unit = "t")]

# widen totals
waste_totals[, colname := gsub(" ", "_", tolower(gsub("Emissions |[()]", "", 
                                               paste(item, element))))]
waste_totals <- dcast(waste_totals, area_code + year ~ colname, value.var = "value")


## Incineration
# distribute by production shares
waste <- copy(prod)
waste[, incineration_co2 := waste_totals$incineration_co2[match(
  paste(area_code, year), paste(waste_totals$area_code, waste_totals$year)
)]]
waste[, incineration_co2 := round(incineration_co2 * share, 2)][
  , `:=` (share = NULL, production = NULL)]
waste[is.na(incineration_co2), incineration_co2 := 0]



#TODO
# distribute incineration and industrial wastewater by production 
# (only certain items for wastewater see overview)



#TODO
# distribute solid food waste loss percentages for different stages



# Net forest conversion (CO2) --------------------
# get conversion attribution from Deduce 
# v.1.01 (https://doi.org/10.5281/zenodo.13624636)
# get data
forest <- as.data.table(read_xlsx("input/ghg/DeDuCE_Deforestation_attribution_v1.0.1 (2001-2022).xlsx"))
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

# Aggregate countries not in FABIO to RoW
forest[!ISO %in% regions$iso3c, `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
forest <- forest[, lapply(.SD, sum, na.rm = TRUE),
                 by = .(area_code, Year, item_code),
                 .SDcols = cols]
setnames(forest, "Year", "year")

# Use amortized, total emissions and convert Mt to t
forest <- forest[, .(area_code, item_code, year, co2_forest_conv_amort = 
                       `Deforestation emissions excl. peat drainage, amortized (MtCO2)`*
                       1e6)]

# Todo: extrapolate to 2023 -> copy 2022 and scale to FAO?
# careful: comparable amortization assumptions?

# Add to list
list_co2[["forest"]] <- forest



# Aggregate ------------------
## CH4 --------------------
# automate as much as possible


## N2O -------------------


## CO2 -------------------



## CO2eq (all)

emissions <- CJ(year = years, area_code = regions$code, 
                 item_code = items$item_code)
emissions_test <- merge(emissions, lvst, by = c("year", "area_code", "item_code"),
                   all.x = TRUE)







