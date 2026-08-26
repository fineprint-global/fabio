library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

conc <- fread("inst/conc_biodiversity_cfs.csv")
items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

# Get data ---------
## LC impact
# Two CF versions are run side by side so they can be compared: LC = LC-IMPACT v2,
# LC1 = LC-IMPACT v1.3 (built by the appendix at the end of 12_7).
LC_with_regions <- readRDS("data/extensions/tidy/lc_impact_country_level_tidy.rds")
LC_climate <- readRDS("data/extensions/tidy/lc_impact_climate_tidy.rds")

LC1_with_regions <- readRDS("data/extensions/tidy/lc_impact_v13_country_level_tidy.rds")
LC1_climate <- readRDS("data/extensions/tidy/lc_impact_v13_climate_tidy.rds")

FD_with_regions <- readRDS("data/extensions/tidy/fd_country_level_tidy.rds")
FD_climate <- readRDS("data/extensions/tidy/fd_climate_tidy.rds")

## Pressures
nms <- conc[source %in% c("LC", "FD"), unique(fabio_pressure)] 
files <- paste0("data/extensions/sua/", nms, ".rds")
pressures <- setNames(lapply(files, readRDS), nms)
pressures <- unformat_extension(pressures, nms, long = FALSE)

## Final demand pressures
nms <- conc[source %in% c("LC", "FD") & fd_fabio_pressure != "", 
            unique(fd_fabio_pressure)] 
files <- paste0("data/extensions/fd_sua/", nms, ".rds")
fdem_pressures <- setNames(lapply(files, readRDS), nms)
fdem_pressures <- unformat_extension(fdem_pressures, nms, long = FALSE)

# add f_gases to co2 pressure (it's measured in co2eq)
fdem_pressures[["ghg_co2"]] <- cbind(fdem_pressures[["ghg_co2"]],
                                     fdem_pressures[["ghg_f_gases"]][, .(f_gases = value)])
fdem_pressures[["ghg_co2"]][, value := value + f_gases][, f_gases := NULL]
fdem_pressures[["ghg_f_gases"]] <- NULL

# Prep pressures and impacts for merging -----------------
## LC-Impact ----------------
## country-level
LC_with_regions <- rbindlist(LC_with_regions,idcol = TRUE, use.names = TRUE)
LC_with_regions[, `:=` (colname = paste0("cf_", type, "_", tolower(realm), "_",  .id, "_", tolower(approach)),
                        cf = as.numeric(cf))] 
LC_with_regions <- dcast(LC_with_regions, iso3c  ~ colname, value.var = "cf") #sorry

LC_climate[, colname := paste0("cf_", type, "_", tolower(realm), "_",  "climate", "_", tolower(approach))]
LC_climate <- dcast(LC_climate, iso3c ~ colname, value.var = "cf")[, iso3c := NULL]


## LC-Impact v1.3 ----------------
## country-level
LC1_with_regions <- rbindlist(LC1_with_regions, idcol = TRUE, use.names = TRUE)
LC1_with_regions[, `:=` (colname = paste0("cf_", type, "_", tolower(realm), "_",  .id, "_", tolower(approach)),
                         cf = as.numeric(cf))]
LC1_with_regions <- dcast(LC1_with_regions, iso3c  ~ colname, value.var = "cf")

LC1_climate[, colname := paste0("cf_", type, "_", tolower(realm), "_",  "climate", "_", tolower(approach))]
LC1_climate <- dcast(LC1_climate, iso3c ~ colname, value.var = "cf")[, iso3c := NULL]


## Functional Diversity -----------------
## country-level
FD_with_regions <- rbindlist(FD_with_regions, idcol = TRUE, use.names = TRUE)
FD_with_regions[, .id := substr(.id, 1, 3)]
FD_with_regions[, `:=` (colname = paste0("cf_", type, "_", .id, "_", tolower(realm),
                                         "_", "eutrophication"),
                        cf = as.numeric(cf))] 

# only keep first combination of iso3c and colname (some CFs are not unique, data
# problem with the freshwater diversity CFs -> for now, this just randomly keeps the first
# CF value to make the integration operable)
# TODO: wait for data update with fixed data
FD_with_regions <- unique(FD_with_regions, by = c("iso3c", "colname"))
FD_with_regions <- dcast(FD_with_regions, iso3c  ~ colname, value.var = "cf") #sorry

FD_climate <- rbindlist(FD_climate, idcol = TRUE, use.names = TRUE)
FD_climate[, .id := substr(.id, 12, 14)]
FD_climate[, colname := paste0(paste0("cf_", type, "_",  .id, "_", tolower(realm), "_", "climate"))]
FD_climate <- dcast(FD_climate, iso3c ~ colname, value.var = "cf")[, iso3c := NULL]


# Merge pressures and impacts --------------------------
# merge 
impact_configs <- list(
  LC       = list(pressures = pressures,      dt = LC_with_regions,  climate_dt = LC_climate),
  LC_fdem  = list(pressures = fdem_pressures, dt = LC_with_regions,  climate_dt = LC_climate),
  LC1      = list(pressures = pressures,      dt = LC1_with_regions, climate_dt = LC1_climate),
  LC1_fdem = list(pressures = fdem_pressures, dt = LC1_with_regions, climate_dt = LC1_climate),
  FD       = list(pressures = pressures,      dt = FD_with_regions,  climate_dt = FD_climate),
  FD_fdem  = list(pressures = fdem_pressures, dt = FD_with_regions,  climate_dt = FD_climate)
)

impacts_list <- lapply(impact_configs, function(cfg) {
  merging_pressures_impacts(cfg$pressures, cfg$dt, cfg$climate_dt)
})

# Multiply to obtain total biodiversity damage per item/country/year combination
key_cols <- c("iso3c", "comm_code", "year", "value")
invisible(lapply(impacts_list, function(impacts) {
  multiply_pressures_impacts(impacts, key_cols)
}))

rm(LC_climate, LC_with_regions, LC1_climate, LC1_with_regions,
   FD_climate, FD_with_regions, impact_configs,
   fdem_pressures, pressures)

# Prep impacts for extension formatting --------------------
# Combine all impacts in one wide table
key_cols <- c("iso3c", "comm_code", "year")
impacts_list <- lapply(impacts_list, function(impacts) {
  Reduce(function(x, y) merge(x, y, by = key_cols, all = TRUE, sort = FALSE), impacts)
})

# split into LC, LC1 and FD list
LC_list <- impacts_list[c("LC", "LC_fdem")]
LC1_list <- impacts_list[c("LC1", "LC1_fdem")]
FD_list <- impacts_list[c("FD", "FD_fdem")]

rm(impacts_list)

# Final formatting LC-impact -------------
# aggregate within the same impact category/realm combinations
impact_categories <- c("freshwater_climate", "marine_climate", "terrestrial_climate",
                       "freshwater_eutrophication","terrestrial_land_use", 
                       "freshwater_water_use", "terrestrial_acidification") 
realms <- c("freshwater", "marine", "terrestrial")

LC_list <- lapply(names(LC_list), function(nm) {
  impact <- LC_list[[nm]]
  
  aggregate_impact_categories(impact, impact_categories, realms)
  impact_cols <- setdiff(colnames(impact), key_cols)
  
  setNames(
    lapply(impact_cols, function(col) {
      dt <- impact[, c(key_cols, col), with = FALSE]
      setnames(dt, col, "value")
      dt
    }),
    paste0("LCIM_EQ_", impact_cols)
  )
}) |> setNames(names(LC_list))

# add area and item codes
invisible(lapply(LC_list, function(sub_list) {
  lapply(sub_list, function(dt) {
    dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
    dt[, item_code := items$item_code[match(comm_code, items$comm_code)]]
  })
}))



# Final formatting LC-impact v1.3 -------------
# aggregate within the same impact category/realm combinations
# v1.3 has no marine climate CF (it distinguishes only Terrestrial and Aquatic
# ecosystems, and the aquatic factor is freshwater-fish based), and its freshwater
# eutrophication covers phosphorus only, so waterborne nitrogen is characterised as
# marine eutrophication instead. Own vectors, so the v2 run above is untouched.
impact_categories_lc1 <- c("freshwater_climate", "terrestrial_climate",
                           "freshwater_eutrophication", "marine_eutrophication",
                           "terrestrial_land_use",
                           "freshwater_water_use", "terrestrial_acidification")
realms_lc1 <- c("freshwater", "marine", "terrestrial")

LC1_list <- lapply(names(LC1_list), function(nm) {
  impact <- LC1_list[[nm]]
  
  aggregate_impact_categories(impact, impact_categories_lc1, realms_lc1)
  impact_cols <- setdiff(colnames(impact), key_cols)
  
  setNames(
    lapply(impact_cols, function(col) {
      dt <- impact[, c(key_cols, col), with = FALSE]
      setnames(dt, col, "value")
      dt
    }),
    paste0("LCIM1_EQ_", impact_cols)
  )
}) |> setNames(names(LC1_list))

# add area and item codes
invisible(lapply(LC1_list, function(sub_list) {
  lapply(sub_list, function(dt) {
    dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
    dt[, item_code := items$item_code[match(comm_code, items$comm_code)]]
  })
}))



# Final formatting functional diversity ----------------
# aggregate within the same impact category/realm/fd_metric combinations
impact_categories <- c("div_freshwater_climate", "eve_freshwater_climate", "ric_freshwater_climate",
                       "div_marine_climate", "eve_marine_climate", "ric_marine_climate",
                       "div_terrestrial_climate","eve_terrestrial_climate","ric_terrestrial_climate",
                       "div_freshwater_eutrophication", "eve_freshwater_eutrophication", "ric_freshwater_eutrophication",
                       "div_marine_eutrophication", "eve_marine_eutrophication", "ric_marine_eutrophication"
) 
realms <-  c("div_freshwater", "eve_freshwater", "ric_freshwater",
             "div_marine", "eve_marine", "ric_marine",
             "div_terrestrial", "eve_terrestrial", "ric_terrestrial")

FD_list <- lapply(names(FD_list), function(nm) {
  impact <- FD_list[[nm]]
  
  aggregate_impact_categories(impact, impact_categories, realms)
  impact_cols <- setdiff(colnames(impact), key_cols)
  
  setNames(
    lapply(impact_cols, function(col) {
      dt <- impact[, c(key_cols, col), with = FALSE]
      setnames(dt, col, "value")
      dt
    }),
    paste0("FD_EQ_", impact_cols)
  )
}) |> setNames(names(FD_list))

# add area and item codes
invisible(lapply(FD_list, function(sub_list) {
  lapply(sub_list, function(dt) {
    dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
    dt[, item_code := items$item_code[match(comm_code, items$comm_code)]]
  })
}))


# Final formatting all ---------------------
# mix and match lists
data_all <- c(FD_list[["FD"]], LC_list[["LC"]], LC1_list[["LC1"]])
fdem_data_all <- c(FD_list[["FD_fdem"]], LC_list[["LC_fdem"]], LC1_list[["LC1_fdem"]])

E_sua <- lapply(data_all, format_extension)
E_fdem_sua <- c(fdem_data_all, format_extension)

# convert to cbs level
conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")

cbs_extensions <- lapply(data_all, agg_sua_to_cbs)
fdem_cbs_extensions <- lapply(fdem_data_all, agg_sua_to_cbs)

E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)
E_fdem_cbs <-  c(fdem_cbs_extensions, format_extension, itms = items_cbs)

# save
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}

for (nm in names(E_fdem_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/fd_sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/fd_cbs/", nm, ".rds"))
}

rm(list = ls())
gc()