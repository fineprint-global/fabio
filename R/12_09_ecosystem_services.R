library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]
conc <- fread("inst/conc_biodiversity_cfs.csv")

# get ecosystem CFs
files <- list.files("input/biodiversity/ecosystem_services/", full.names = T)
CF_list <- lapply(files, fread)
names(CF_list) <- tolower(gsub("_country.*", "", list.files("input/biodiversity/ecosystem_services/")))

# tidy
CF_list <- lapply(CF_list, function(dt){
  # rename and filter
  dt <- dt[, .(country, country.code, pressure = lc_description, pressure_code =
                 GLOBIO4_lc_code, cf = IF, cf_unit = Unit)]
  dt <- dt[pressure_code %in% c(10, 11, 12, 20)]
  
  # tidy regions
  dt <- merge(dt, regions[, .(area_code = code, ecosystem_services)],
              by.x = "country.code", by.y = "ecosystem_services")
  
  # use China, mainland values for Macao and Hong Kong
  china_rows <- rbind(copy(dt[area_code == 41])[, area_code := 96], 
                      copy(dt[area_code == 41])[, area_code := 128])
  dt <- rbind(dt, china_rows)
  
  # use ATG values for ANT
  ant_rows <- copy(dt[area_code == 8])[, area_code := 151]
  dt <- rbind(dt, ant_rows)
  
  # use sri lanka rows for maldives (where necessary)
  if(!(132 %in% dt$area_code)){
    sri_rows <- copy(dt[area_code == 38])[, area_code := 132]
    dt <- rbind(dt, sri_rows)
  }
  
  # use global average for RoW
  row_rows <- copy(dt[area_code == 38])[, `:=` (area_code = 999, cf = mean(cf, na.rm = TRUE)), 
                                        by = pressure]
  dt <- rbind(dt, row_rows)
  
  #add names
  dt[, area := regions$name[match(area_code, regions$code)]][,`:=` (country = NULL, country.code = NULL)]
  setcolorder(dt, c("area_code", "area", "pressure", "pressure_code", "cf", "cf_unit"))
  
  # Iceland missing from pollination -> leaving this, hard to make assumptions for Iceland
  
  #convert units where necessary
  if(any(grepl("m^2", dt$cf_unit, fixed = TRUE))){
    dt[, `:=` (cf = cf * 10000, cf_unit = gsub("m^2", "ha", cf_unit, fixed = TRUE))]
  }else{
    dt
  }
})


# average "cropland, minimal use" and "cropland, intensive use" for pollination
# TODO: Ask Alexandra Marques for aggregated CFs
pollination <- CF_list[["pollination"]]
pollination[, `:=` (cf = mean(cf, na.rm = TRUE), pressure = "Cropland", 
                    pressure_code = 10), by = .(area_code)]

CF_list[["pollination"]] <- unique(pollination)
rm(pollination)

CF_list <- lapply(CF_list, function(dt){
  dt[, type := conc$fabio_pressure[match(pressure_code, conc$flow_id)]]
})

# get pressure data
nms <- conc[source %in% c("ES"), unique(fabio_pressure)] 
files <- paste0("data/extensions/sua/", nms, ".rds")
pressures <- setNames(lapply(files, readRDS), nms)
pressures <- unformat_extension(pressures, nms, long = FALSE)

# merge pressures and impacts
CF <- rbindlist(CF_list, idcol = TRUE, use.names = TRUE)
CF[, `:=` (colname = paste0("cf_", type, "_terrestrial_",  .id))] 
CF <- dcast(CF, area_code  ~ colname, value.var = "cf")
CF[, iso3c := regions$iso3c[match(area_code, regions$code)]]

impacts <- merging_pressures_impacts(pressures, CF)

# Multiply to obtain total biodiversity damage per item/country/year combination
key_cols <- c("iso3c", "comm_code", "year", "value")
invisible(lapply(names(impacts), function(nm) {
  dt <- impacts[[nm]]
  cols <- setdiff(colnames(dt), key_cols)
  dt[, (cols) := lapply(.SD, function(x) x * value), .SDcols = cols]
  dt[, value := NULL]
  setnames(dt, cols, gsub("cf_", "", cols))
}))

# # Combine all impacts in one wide table
key_cols <- c("iso3c", "comm_code", "year")
impacts <- Reduce(function(x, y) {
  merge(x, y, by = key_cols, all = TRUE, sort = FALSE)
}, impacts)


# aggregate within the same ecosystem service categories
ES_categories <- c("terrestrial_pollination", "terrestrial_carbon", 
                   "terrestrial_soil_erosion") 

invisible(lapply(ES_categories, function(cat) {
  cols <- colnames(impacts)[colnames(impacts) %like% cat]
  if(length(cols) > 0) {
    impacts[, (cat) := {
      rs <- rowSums(.SD, na.rm = TRUE)
      rs
    }, .SDcols = cols]
    impacts[, (cols) := NULL]
  }
}))

# prep extension formatting
impact_cols <- setdiff(colnames(impacts), c(key_cols))

mappings <- data.frame(
  target = paste0("ES_", impact_cols),
  col    = impact_cols,
  stringsAsFactors = FALSE
)

list_impacts <- setNames(
  lapply(impact_cols, function(col) {
    dt <- impacts[, c(key_cols, col), with = FALSE]
    setnames(dt, col, "value")
    dt
  }),
  paste0("ES_", impact_cols)
)

# add area and  item_codes
list_impacts <- lapply(list_impacts, function(dt) {
  dt[, area_code := regions$code[match(iso3c, regions$iso3c)]]
  dt[, item_code := items$item_code[match(comm_code, items$comm_code)]]
  dt
})

E_sua <- lapply(list_impacts, format_extension)

# aggregate to cbs
conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")

cbs_extensions <- lapply(list_impacts, agg_sua_to_cbs)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)

# save
for (nm in names(E_sua)) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}
           
