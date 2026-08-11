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

# in Deduce, Pasture is allocated to Cattle meat and Leather with a 95/5% split in statistical allocation (sLUC) (see Supplementary Information p. 18, Table 1, https://www.nature.com/articles/s43016-026-01305-4#additional-information 
# this attribution is reversed here by setting the items to "Grazing" (item code 2001), which is summed up in the next step
forest[`Commodity group` == "Pasture", item_code := 2001]

# Deal with Sudan
forest[ISO == "SDN and SSD", ISO := "SDN"]

# set value columns
cols <-   c("Deforestation attribution, unamortized (ha)",
            "Deforestation risk, amortized (ha)",
            "Deforestation emissions excl. peat drainage, unamortized (MtCO2)",
            "Deforestation emissions excl. peat drainage, amortized (MtCO2)",
            "Peatland drainage emissions (MtCO2)",
            "Deforestation emissions incl. peat drainage, amortized (MtCO2)")

# Aggregate countries not in FABIO to RoW (and sum grazing)
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
# The following script attributes emissions from On-Farm Energy use in three steps:
# 1) separate out forestry emissions from FAO totals based on Gloria-sector emissions (only crop/animal/fishery emissions are kept in Fabio)
# 2) split up FAO rest-of-world area (FAO only has 156 areas) to remaining Fabio areas
# 3) allocate on-farm emissions to items based on Value-Added for primary sector (ISIC-A) products (Value-Added attribution)

# get total on-farm energy use from FAO
energy_emissions_FAO <- data_list[["energy"]][element != "Energy use in agriculture" & 
                                        item == "Total Energy"]

# convert from kt to t
energy_emissions_FAO[, `:=` (value = value * 1000, unit = "tonnes")]

# widen by emission type
cols <- c("Emissions (CO2)" = "co2_energy_area_total",
          "Emissions (CH4)" = "ch4_energy_area_total",
          "Emissions (N2O)" = "n2o_energy_area_total")
energy_emissions_FAO[, colname := cols[element]]

energy_emissions_FAO <- dcast(energy_emissions_FAO, area_code + year ~ colname, value.var = "value")

# get Gloria emissions data (prepared in 00_8_gloria_intensity.R) to compute share of forestry from agriculture emissions, which needs to be subtracted
gloria_satellite_e <- readRDS("data/tidy/gloria_satellite_e.rds")
gloria_satellite_e <- gloria_satellite_e[year %in% years]

# Remove 249/Yemen "DYE" area code, as only the "YEM" area code is used in the FAO data concordance
gloria_satellite_e <- gloria_satellite_e[gloria_region_code != "DYE"]

# use OECD or EDGAR for calculations
gloria_satellite_e$gloria_e <- gloria_satellite_e$`'co2_excl_short_cycle_org_c_total_EDGAR_consistent'`

# sum up ROW regions (XEU, XUS, etc.. Gloria regions map to 999/ROW in FABIO)
gloria_satellite_e <- rbind(
  gloria_satellite_e[fabio_area_code != 999],
  gloria_satellite_e[fabio_area_code == 999,
                     c(list(gloria_region_code = "ROW",
                            fabio_area         = "Rest of World",
                            gloria_region_name = "Rest of World"),
                       lapply(.SD, sum, na.rm = TRUE)),
                     by = .(year, gloria_sector_code, gloria_sector_name),
                     .SDcols = setdiff(names(gloria_satellite_e)[sapply(gloria_satellite_e, is.numeric)],
                                       c("fabio_area_code", "year", "gloria_sector_code"))
  ][, fabio_area_code := 999]
)

# compute shares of each of the 23 Gloria sectors of emissions within a year and Fabio regions
gloria_satellite_e[, total_e_region_year := sum(gloria_e, na.rm = TRUE), 
                   by = .(fabio_area_code, year)]
gloria_satellite_e[, emission_share := gloria_e / total_e_region_year]

# for countries where no emissions account is present (e.g. SRB): apply average ROW shares to fill out table
# compute average ROW shares
row_shares <- gloria_satellite_e[, .(emission_share = sum(gloria_e, na.rm = TRUE)), 
                                 by = .(year, gloria_sector_code)]
row_shares[, row_share := emission_share / sum(emission_share, na.rm = TRUE), by = year]

# assign average ROW share split to countries with no emissions data
gloria_satellite_e[is.na(emission_share),
                   emission_share := row_shares[.SD,
                                                on = c("year", "gloria_sector_code"),
                                                x.row_share]]

# check: the shares need to be one for all gloria countries and years
gloria_satellite_e[, sum(emission_share), by = year]
stopifnot(all(round(gloria_satellite_e[, .(total = sum(emission_share)), by = .(year, fabio_area_code)]$total, 12) == 1)) # small deviations due to floating-point are caught by round

# prepare to split out forestry emissions from the agriculture sector
forestry_shares <- gloria_satellite_e[, .(
    forestry_share = sum(emission_share[gloria_sector_code == 21]),
    other_share = sum(emission_share[gloria_sector_code != 21])
  ), by = .(fabio_area_code, year)]

# prepare to apply shares of forestry versus other sectors to FAO emissions totals
energy_emissions_FAO <- energy_emissions_FAO[
  forestry_shares,
  on = .(area_code = fabio_area_code, year),
  nomatch = 0]

# calculate the parts of the total assigned to forestry and agriculture (incl. fishery) respectively
energy_emissions_FAO <- rbindlist(list(
  energy_emissions_FAO[, .(area_code,
                           year,
                           sector = "Forestry", # for Forbio
                           ch4_energy_area_total = ch4_energy_area_total * forestry_share,
                           co2_energy_area_total = co2_energy_area_total * forestry_share,
                           n2o_energy_area_total = n2o_energy_area_total * forestry_share )],
  energy_emissions_FAO[, .(area_code,
                           year,
                           sector = "Agriculture/fish",
                           ch4_energy_area_total = ch4_energy_area_total * other_share,
                           co2_energy_area_total = co2_energy_area_total * other_share,
                           n2o_energy_area_total = n2o_energy_area_total * other_share)]
))
setorder(energy_emissions_FAO, area_code, year, sector)

# There are 181 Fabio area codes, but FAO has data for 156 only, so that the FAO ROW account has to be further subdivided off 

# split the FAO totals by estimated GLORIA-based emissions, with the same table as the one used for forestry shares
setdiff(unique(regions$code), unique(energy_emissions_FAO$area_code)) # display unmatched areas
fao_unassigned_areas <- c(setdiff(unique(regions$code), unique(energy_emissions_FAO$area_code)), 999L) # the FABIO Rest-of-world region is kept, as it is not expected in FAO
fao_totals_split_key <- gloria_satellite_e[
  fabio_area_code %in% fao_unassigned_areas,
  .(gloria_e = sum(gloria_e, na.rm = TRUE)),
  by = .(fabio_area_code, year)
]

# calculate shares
fao_totals_split_key[, share := gloria_e / sum(gloria_e, na.rm = TRUE), by = year]

# distribute FAO ROW totals to shares of areas, by-year
fao_totals_redistributed <- fao_totals_split_key[
  energy_emissions_FAO[area_code == 999L],
  on = "year",
  allow.cartesian = TRUE
]

# calculate supplementary table with imputed emissions shares for the rest-of-world countries
fao_totals_redistributed <- fao_totals_redistributed[, .(
  area_code             = fabio_area_code,
  year                  = year,
  sector                = sector,
  ch4_energy_area_total = ch4_energy_area_total * share,
  co2_energy_area_total = co2_energy_area_total * share,
  n2o_energy_area_total = n2o_energy_area_total * share
)]

# re-combine with main FAO data
# resulting table has 181 regions, sums to the same totals as original FAO account
energy_emissions_FAO <- rbind(
  energy_emissions_FAO[area_code != 999L],
  fao_totals_redistributed
)

# Keep only agriculture part (forestry part goes into Forbio)
energy_emissions_FAO <- energy_emissions_FAO[sector != "Forestry"]

# From the resulting adjusted FAO totals, this step performs a Value-Added allocation of emissions to items
# This encodes the assumption that capital- and labour VA intensive items will be associated with higher energy use
# Since only ISIC-A (primary production) value added accounts are used, this ensures only primary production receives on-farm energy use (might include some non-primary FABIO items such as Butter and Wool, as their raw variants are directly attributed to farms in ISIC-A)

# get value added matrices
E_bamboo_list <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/E_bamboo.rds")

# transpose and filter to get one table covering all years
E_bamboo <- rbindlist(
  lapply(names(E_bamboo_list), function(yr) {
    mat <- t(E_bamboo_list[[yr]])
    dt  <- as.data.table(mat, keep.rownames = "id")
    dt[, year := as.integer(yr)]
    dt
  })
)

# tidy resulting objects
setcolorder(E_bamboo, c("year", "id", setdiff(names(E_bamboo), c("year", "id"))))
rm(E_bamboo_list); gc()

# select value-added accounts (see "/mnt/nfs_fineprint/tmp/fabio/v2/ex_bamboo_labels.csv" for column names)
# 14 years x 181 regions x 123 items
E_bamboo <- E_bamboo |> select(year, id, starts_with("VA_"))

# split regions and items from matrix names
E_bamboo$region <- substr(E_bamboo$id, 1, 3)
E_bamboo$comm_code <- substr(E_bamboo$id, 5, 8)
stopifnot(uniqueN(E_bamboo$region) == 181)

# add information for filtering
items_cbs <- fread("inst/items_full_123.csv")
E_bamboo <- merge(E_bamboo, items_cbs, by = "comm_code")
E_bamboo <- merge(E_bamboo, regions, by.x = "region", by.y = "iso3c")
E_bamboo$area_code <- E_bamboo$code

# calculate Value Added figures for ISIC A sector, omitting tls (tax less subsidy), as only positive value added elements are considered for the split
E_bamboo[, `:=`(
  VA_base_isic_a_exiobase = VA_capital_isic_a_exiobase + VA_wages_isic_a_exiobase,
  VA_base_isic_a_gloria   = VA_capital_isic_a_gloria   + VA_wages_isic_a_gloria
)]

# negative VA values can arise in the source data. For the purposes of the share split, they can be excluded
E_bamboo[VA_base_isic_a_exiobase < 0 | VA_base_isic_a_gloria < 0,
         .N, by = year]
E_bamboo[VA_base_isic_a_exiobase < 0, VA_base_isic_a_exiobase := 0]
E_bamboo[VA_base_isic_a_gloria < 0, VA_base_isic_a_gloria := 0]

# generate emissions table for on-farm energy use, where each FAO total is split of to the items with the established weights
onfarm_energy_emissions <- merge(energy_emissions_FAO, E_bamboo, by = c("area_code", "year"), all = T)

# calculate Value Added share of the group (Livestock, Crop, Fish) for a region/year total
onfarm_energy_emissions[, `:=`(
  share_exiobase = VA_base_isic_a_exiobase / sum(VA_base_isic_a_exiobase, na.rm = TRUE),
  share_gloria   = VA_base_isic_a_gloria   / sum(VA_base_isic_a_gloria,   na.rm = TRUE)
), by = c("area_code", "year")]

# apply shares - here, Gloria accounts can also be used instead of EXIOBASE
onfarm_energy_emissions[, `:=`(
  ch4_energy = ch4_energy_area_total * share_exiobase,
  co2_energy = co2_energy_area_total * share_exiobase,
  n2o_energy = n2o_energy_area_total * share_exiobase
)]

# add to lists
list_ch4[["farm_energy"]] <- onfarm_energy_emissions[, .(area_code, year, item_code, value = ch4_energy)]
list_co2[["farm_energy"]] <- onfarm_energy_emissions[, .(area_code, year, item_code, value = co2_energy)]
list_n2o[["farm_energy"]] <- onfarm_energy_emissions[, .(area_code, year, item_code, value = n2o_energy)]

rm(energy_emissions_FAO, E_bamboo, items_cbs, onfarm_energy_emissions, gloria_satellite_e, fao_totals_redistributed, fao_totals_split_key, forestry_shares, row_shares)
gc()

# Pre- and post agricultural processing -----------------------------------
## Waste (from pre and postfarm production) --------------
ppap <- data_list[["ppap"]]

# filter for emissions from waste relevant to industry (emissions from final
# demand waste are dealt with below)
waste_totals <- ppap[item %in% c("Incineration", "Industrial Wastewater", 
                                         "Solid Food Waste" ) &
                               !element %like% c("CO2eq")]

# convert from kt to t
waste_totals[, `:=` (value = value * 1000, unit = "tonnes")]

# widen totals
waste_totals[, colname := gsub(" ", "_", tolower(gsub("Emissions |[()]", "", 
                                                      paste(item, element))))]
waste_totals <- dcast(waste_totals, area_code + year ~ colname, value.var = "value")


### waste_incineration ------------------
# determine production shares for all agricultural products (processed and primary, 
# fishery and forestry not included, grass reduced by 75% to avoid overestimation)
prod <- readRDS("data/tidy/prod_trad_full.rds")[element=="Production" & unit=="tonnes",
                                                .(year,item_code,area_code,area,value)]
prod[, share := value/sum(value, na.rm = TRUE), by = .(area_code, year)]

# distribute by production shares
waste_inc <- copy(prod)
waste_inc[, value := waste_totals$incineration_co2[match(
  paste(area_code, year), paste(waste_totals$area_code, waste_totals$year)
)]]
waste_inc[, value := value * share][
  , `:=` (share = NULL)]

prod[, share := NULL]

### Solid food waste -------------
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

#### Tidy waste shares by production stage ---------
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
list_co2[["waste_incineration"]] <- waste_inc[, .(area_code, item_code, year, value)]

rm(shares)

### Industrial wastewater  -----------------
# distribute by production of processed products, but exclude most cereal products
# (no n2o emissions from waste occur here)
prod_proc <- prod[item_code %in% items[processed == TRUE, item_code] & 
                    !item_code %in% items[group == "cereals (excluding beer)" & 
                                            !item_code %in% c(64, 34, 23), item_code]]
prod_proc[, share := value/sum(value, na.rm = TRUE), by = .(area_code, year)]

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

## Other PPAP categories --------------------------------------------
other_pre_post_totals <- ppap[item %in% c("Food Processing",
                                          "Food Transport",
                                          "Food Packaging",
                                          #"Cold Chain F-Gas", # available as co2eq, but excluded for now
                                          "Pesticides Manufacturing",
                                          "Fertilizers Manufacturing"
) & !element %like% c("CO2eq")]

# convert from kt to t
other_pre_post_totals[, `:=` (value = value * 1000, unit = "tonnes")]

# widen totals
other_pre_post_totals[, colname := gsub(" ", "_", tolower(gsub("Emissions |[()]", "", 
                                                      paste(item, element))))]

other_pre_post_totals <- dcast(other_pre_post_totals, area_code + year ~ colname, value.var = "value")


### Processing, transport, packaging, pesticide: output-based allocation ----------------------

price_weights <- readRDS("~/FABIO/v2/data/total_value/FABIOv2_producer_total_values_isic_a.rds")
price_weights$year <- as.numeric(price_weights$year)

# Compute price share per crop within each country-year
price_weights[, price_share := `total_value [USD]` / sum(`total_value [USD]`, na.rm = TRUE),
              by = .(area_code, year)]

# check if the shares sum correctly
stopifnot("Price shares are not complete for all years" = price_weights[, round(sum(price_share, na.rm = TRUE)) == nrow(regions), by = year]$V1)
stopifnot("Price shares are not complete for all countries" = price_weights[, round(sum(price_share, na.rm = TRUE)) == length(years), by = area_code]$V1)

# For pesticide manufacturing, the approach is the same, but it is only distributed to crops
price_weights[, value_sans_meat := 0]
price_weights[
  comm_group %in% c("Cereals", "Roots and tubers", "Sugar crops", "Vegetables, fruit, nuts, pulses, spices", "Oil crops", "Fodder crops"), 
  value_sans_meat := `total_value [USD]`
]

# calculate weights for pesticides
price_weights[, total_sans_meat := sum(value_sans_meat, na.rm = TRUE), 
              by = .(area_code, year)]
price_weights[, price_share_sans_meat := fifelse(
  total_sans_meat > 0,
  value_sans_meat / total_sans_meat,
  NA # Explicitly place NA for the value if there is no data for the year, to impute in the next step
)]
price_weights[, total_sans_meat := NULL]

# Calculate averages by both area_code and item_code for imputation of missing years (needed for 200 (Singapore))
region_averages <- price_weights[!is.na(price_share_sans_meat), 
                                 .(avg_price_share = mean(price_share_sans_meat)), 
                                 by = .(area_code, item_code)]

# Impute only for areas where shares are NA (no value data for any crop products)
price_weights[region_averages, 
              price_share_sans_meat := ifelse(is.na(price_share_sans_meat), 
                                              i.avg_price_share, 
                                              price_share_sans_meat),
              on = .(area_code, item_code)]

# check if the shares sum correctly
stopifnot("Price shares are not complete for all years" = price_weights[, round(sum(price_share_sans_meat, na.rm = TRUE)) == nrow(regions), by = year]$V1)
stopifnot("Price shares are not complete for all countries" = price_weights[, round(sum(price_share_sans_meat, na.rm = TRUE)), by = area_code]$V1 == length(years))

# Keep only needed columns
price_weights <- price_weights[, .(area_code, year, item_code, price_share, price_share_sans_meat)]

# merge 
other_pre_post_totals <- merge(other_pre_post_totals, price_weights,
                                by = c("area_code", "year"),
                                allow.cartesian = TRUE)

# Fertilizer production: attribute based on shares of fertilizer (12_4)
fertilizer_shares <- readRDS("data/NPK/SF_application_sua.rds")

fertilizer_shares[, fert_total := N_kg + P_kg]
fertilizer_shares[, fert_share := fert_total / sum(fert_total, na.rm = T), 
                  by = .(iso3c, year)]

fertilizer_shares <- merge(regions, fertilizer_shares, by = "iso3c")
fertilizer_shares[, area_code := code]

fertilizer_shares <- fertilizer_shares[, c("area_code", "year", "item_code", "fert_share")]

other_pre_post_totals <- merge(other_pre_post_totals, fertilizer_shares, by = c("area_code", "year", "item_code"))

# Food processing: by price_share
other_pre_post_totals[, food_processing_ch4 := food_processing_ch4 * price_share]
other_pre_post_totals[, food_processing_co2 := food_processing_co2 * price_share]
other_pre_post_totals[, food_processing_n2o := food_processing_n2o * price_share]

# Food packaging: by price_share
other_pre_post_totals[, food_packaging_ch4 := food_packaging_ch4 * price_share]
other_pre_post_totals[, food_packaging_co2 := food_packaging_co2 * price_share]
other_pre_post_totals[, food_packaging_n2o := food_packaging_n2o * price_share]

# Food transport: by price_share
other_pre_post_totals[, food_transport_ch4 := food_transport_ch4 * price_share]
other_pre_post_totals[, food_transport_co2 := food_transport_co2 * price_share]
other_pre_post_totals[, food_transport_n2o := food_transport_n2o * price_share]

# Pesticide manufacturing: by price_share (only for crops)
other_pre_post_totals[, pesticides_manufacturing_ch4 := pesticides_manufacturing_ch4 * price_share_sans_meat]
other_pre_post_totals[, pesticides_manufacturing_co2 := pesticides_manufacturing_co2 * price_share_sans_meat]
other_pre_post_totals[, pesticides_manufacturing_n2o := pesticides_manufacturing_n2o * price_share_sans_meat]

# Fertilizer manufacturing: by fert_share
other_pre_post_totals[, fertilizers_manufacturing_co2 := fertilizers_manufacturing_co2 * fert_share]
other_pre_post_totals[, fertilizers_manufacturing_n2o := fertilizers_manufacturing_n2o * fert_share]
# fertilizer N2O emissions are not reported at the production step, hence not present in the data

# assign to lists; 
list_ch4[["ppap_food_processing"]]           <- other_pre_post_totals[, .(area_code, year, item_code, value = food_processing_ch4)]
list_co2[["ppap_food_processing"]]           <- other_pre_post_totals[, .(area_code, year, item_code, value = food_processing_co2)]
list_n2o[["ppap_food_processing"]]           <- other_pre_post_totals[, .(area_code, year, item_code, value = food_processing_n2o)]

list_ch4[["ppap_food_packaging"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_packaging_ch4)]
list_co2[["ppap_food_packaging"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_packaging_co2)]
list_n2o[["ppap_food_packaging"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_packaging_n2o)]

list_ch4[["ppap_food_transport"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_transport_ch4)]
list_co2[["ppap_food_transport"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_transport_co2)]
list_n2o[["ppap_food_transport"]]            <- other_pre_post_totals[, .(area_code, year, item_code, value = food_transport_n2o)]

list_ch4[["ppap_pesticides_manufacturing"]]  <- other_pre_post_totals[, .(area_code, year, item_code, value = pesticides_manufacturing_ch4)]
list_co2[["ppap_pesticides_manufacturing"]]  <- other_pre_post_totals[, .(area_code, year, item_code, value = pesticides_manufacturing_co2)]
list_n2o[["ppap_pesticides_manufacturing"]]  <- other_pre_post_totals[, .(area_code, year, item_code, value = pesticides_manufacturing_n2o)]

list_co2[["ppap_fertilizers_manufacturing"]] <- other_pre_post_totals[, .(area_code, year, item_code, value = fertilizers_manufacturing_co2)]
list_n2o[["ppap_fertilizers_manufacturing"]] <- other_pre_post_totals[, .(area_code, year, item_code, value = fertilizers_manufacturing_n2o)]

# tidy environment
rm(fertilizer_and_pesticides, fertilizer_shares, other_pre_post_totals, ppap, price_weights, process_pack_transport)

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
