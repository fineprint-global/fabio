library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
conc <- fread("inst/conc_biodiversity_cfs.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]

path_lc <- "input/biodiversity/LC_impact/"
path_fun <- "input/biodiversity/functional_diversity/"

# Get data (LC-Impact + FD) -----------------
## LC-Impact ----------------
files <- c(climate_change_terrestrial    = "3_CF_CLIMATE_CHANGE.zip",
           climate_change_freshwater     = "3_CF_CLIMATE_CHANGE.zip",
           climate_change_marine         = "3_CF_CLIMATE_CHANGE.zip",
           acidification                 = "4_CF_ACIDIFICATION.zip",
           water_use                     = "5_CF_WATER_USE.zip",
           land_use                      = "6_CF_LAND_USE.zip",
           eutrophication_freshwater     = "8_CF_EUTROPHICATION.zip",
           invasive_species              = "9_CF_INVASIVE_SPECIES.zip")

fa_dl(file = files, path = path_lc, link = "https://zenodo.org/records/19202729/files/")

# extract from zip files
fa_extract(path_in = path_lc, files = files, path_out = path_lc,
           name = names(files),
           extr = c("CF_CLIMATE_CHANGE/EQ_ClimateChange_Terrestrial_gloPDF_Country_Aggregated_2025-12-09.xlsx",    
                    "CF_CLIMATE_CHANGE/EQ_ClimateChange_Freshwater_gloPDF_Country_Aggregated_2025-12-09.xlsx", 
                    "CF_CLIMATE_CHANGE/EQ_ClimateChange_Marine_gloPDF_Country_Aggregated_2025-12-09.xlsx",
                    "CF_ACIDIFICATION/EQ_TerrestrialAcidification_gloPDF_Country_Aggregated_2025-10-14.xlsx",
                    "CF_WATER_USE/EQ_WaterUse_gloPDF_Country_Aggregated_2025-12-08.xlsx",
                    "CF_LAND_USE/EQ_LandUse_gloPDF_Country_Aggregated_2025-12-17.xlsx",
                    "8_EUTROPHICATION/CF_EUTROPHICATION/EQ_FreshwaterEutrophication_gloPDF_Country_Aggregated_2026-03-24.xlsx",
                    "CF_INVASIVE_SPECIES/EQ_InvasiveSpecies_Terrestrial_gloPDF_Country_Aggregated_2025-10-14.xlsx"),
           col_types = list(NULL, NULL, NULL, NULL, NULL, NULL, NULL, list(sheet = "Average")),
           read_method = rep("read_xlsx", 8),
           )

# Tidy directory
dirs <- list.dirs(path_lc, recursive = FALSE)
dirs <- dirs[basename(dirs) %in% c("CF_CLIMATE_CHANGE", "CF_ACIDIFICATION", "CF_WATER_USE", 
                                   "CF_LAND_USE", "8_EUTROPHICATION", "CF_INVASIVE_SPECIES")]
unlink(dirs, recursive = TRUE)


# Combine all LC-Impact data in one list
files <- list.files(path = "input/biodiversity/LC_impact", 
                    pattern = ".rds$", 
                    full.names = TRUE)

LC_list <- lapply(files, readRDS)

LC_names <- c(
  climate_change_freshwater  = "EQ_ClimateChange_Freshwater_gloPDF_Country_Aggregated_2025-12-09.rds",
  climate_change_terrestrial = "EQ_ClimateChange_Terrestrial_gloPDF_Country_Aggregated_2025-12-09.rds",  
  climate_change_marine      = "EQ_ClimateChange_Marine_gloPDF_Country_Aggregated_2025-12-09.rds",       
  eutrophication_freshwater  = "EQ_FreshwaterEutrophication_gloPDF_Country_Aggregated_2026-03-24.rds",
  invasive_species           = "EQ_InvasiveSpecies_Terrestrial_gloPDF_Country_Aggregated_2025-10-14.rds",
  land_use                   = "EQ_LandUse_gloPDF_Country_Aggregated_2025-12-17.rds",
  acidification              = "EQ_TerrestrialAcidification_gloPDF_Country_Aggregated_2025-10-14.rds",
  water_use                  = "EQ_WaterUse_gloPDF_Country_Aggregated_2025-12-08.rds"
)

names(LC_list) <- names(LC_names)[match(basename(files), LC_names)]

# take out invasive species (different format)
invasive_species <- LC_list[["invasive_species"]]
LC_list[["invasive_species"]] <- NULL


## Functional Diversity ---------------------
# define files
files <-   c(climate_change = "Climate_change/CF_climate_change.xlsx",    
             div_fresh = "Freshwater_Eutrophication/EQ_FreshwaterEutrophication_FDiv_Aggregated_2026-02-26.xlsx",
             eve_fresh = "Freshwater_Eutrophication/EQ_FreshwaterEutrophication_FEve_Aggregated_2026-02-26.xlsx",
             ric_fresh = "Freshwater_Eutrophication/EQ_FreshwaterEutrophication_FRic_Aggregated_2026-02-26.xlsx",
             div_mar =   "Marine_Eutrophication/EQ_MarineEutrophication_FDiv_Aggregated_2026-02-26.xlsx",
             eve_mar =   "Marine_Eutrophication/EQ_MarineEutrophication_FEve_Aggregated_2026-02-26.xlsx",
             ric_mar =   "Marine_Eutrophication/EQ_MarineEutrophication_FRic_Aggregated_2026-02-26.xlsx")
files <- setNames(
  sapply(seq_along(files), function(i) paste0(path_fun, files[[i]])),
  names(files)
)


# combine all into one list
FD_list <- lapply(files, function(f) as.data.table(read_excel(f)))
names(FD_list) <-  names(files)



# Rename, tidy regions --------------------
# tidy data.tables within the lists
cols_remove <- c("FLOW_casnumber", "LCIAMethod_spatialResolution", "CF_Uncertainty_Lower",
                    "CF_Uncertainty_Higher",  "Species", "Matching_Flow_Status",
                    "Matching_Compartment_Status", "Matching_Compartment",
                    "LCIAMethod_type", "LCIAMethod_name", "CF_derivation",
                    "Matching_CF")
old_names <- c("FLOW_uuid", "FLOW_name", "LCIAMethod_location",
               "LCIAMethod_location_name", "CF", "CF_unit", 
               "CF_indicatorScale",  "FLOW_class0", "FLOW_class1",
               "FLOW_class2", "LCIAMethod_realm", "LCIAMethod_mathematicalApproach",
               "Scenario", "FD_metric")
new_names <- c("flow_id", "flow", "iso3c",
         "area", "cf", "cf_unit", 
         "cf_indicator_scale",  "flow_class0", "flow_class1",
         "flow_class2", "realm", "approach",
         "scenario", "fd_metric")

LC_list <- lapply(LC_list, function(dt) tidy_cfs(dt))
FD_list <- lapply(FD_list, function(dt) tidy_cfs(dt))

# For LC: use global values for RoW, where relevant, exclude countries not in FABIO
LC_list[c(4:7)] <- lapply(LC_list[c(4:7)], function(dt) {
  dt_row <- unique(dt[iso3c == "GLO"])
  dt_row <- dt_row[, `:=` (iso3c = "ROW", area = "RoW")]
  dt <- dt[iso3c %in% regions$iso3c]
  dt <- rbind(dt, dt_row)
})

# For FD: no global averages available, leaving out countries not in the FD data
FD_list[-1] <- lapply(FD_list[-1], function(dt){
  dt <- dt[iso3c %in% regions$iso3c]
})

# separate FD climate into one table per functional diversity metric
FD_climate_list <- split(FD_list[["climate_change"]], 
                         by = c("fd_metric"))
names(FD_climate_list) <- tolower(gsub(" |\\.", "_", names(FD_climate_list)))


# Value choices, unit conversions (LC + FD) --------
key_cols <- c("flow_id", "flow", "iso3c", "area", "cf", "cf_unit", "approach", "realm")

## Climate change (LC + FD)---------
# extract climate data.tables from LC and FD lists
climate_list <- list(LC = copy(LC_list[1:3]), FD = FD_climate_list)
rm(FD_climate_list)
LC_list[grep("climate_change", names(LC_list))] <- NULL
FD_list[["climate_change"]] <- NULL


# filter relevant values
climate_list <- lapply(climate_list, function(sublist) {
  lapply(sublist, function(dt) {
    
    # filter for relevant emissions and choose middle-of-the-road scenario
    dt <- unique(dt[(flow_id %in% conc$flow_id) &
                      scenario %in% c("rcp45", "General") & cf > 0 &
                      approach == "Average"])
    
    # convert units to match pressures
    dt[, `:=` (cf = cf * 1000, cf_unit = "PDF*y/t_em")] 
    dt[, ..key_cols]
    dt[, type := conc$fabio_pressure[match(flow_id, conc$flow_id)]]
  })
})

FD_climate_list  <- climate_list[["FD"]]
LC_climate <- rbindlist(climate_list[["LC"]], use.names = TRUE)


rm(climate_list)

## Eutrophication (LC) --------------
eut <- LC_list[["eutrophication_freshwater"]]

# choose relevant emissions
eut <- eut[flow_id %in% c("601311d7-4d5c-4d49-b131-69b73793ad0f", 
                          "247582e8-f296-4db4-94d3-ef1f7bea9a2d",
                          "b88d3b6d-229e-477e-bce1-e16376f75c7b",
                          "bc2fb99a-c9b4-4473-9870-089bcef3054f") &
             approach == "Average" &
             scenario %in% c("Combined impact of N and P based on Weights of proportion",
                             "Considering N-limited regions")] #TODO: could not access uuid flow definitions -> correct grassland definition -> usually the same anyway

# convert unit to match pressures
eut[, cf := as.numeric(cf)]
eut[cf_unit == "global PDF*y/m2*y", `:=` (cf = cf * 10000, 
                                          cf_unit = "global PDF*y/ha*y")]

eut <- eut[, ..key_cols]

LC_list[["eutrophication"]] <- eut
LC_list[["eutrophication_freshwater"]] <- NULL


## Acidification (LC) -----------------
acid <- LC_list[["acidification"]]
acid <- acid[flow_id == "08a91e70-3ddc-11dd-a2a9-0050c2490048" &
               scenario == "Agricult", ..key_cols]
acid <- acid[, ..key_cols]
LC_list[["acidification"]] <- acid

## Land use (LC) ------------------
lu <- LC_list[["land_use"]]
lu <- lu[flow %in% c("arable", "grassland/pasture/meadow", "from forest, natural") & approach == "Average" &
           (is.na(scenario) |scenario == "original_weighting_land_use" | scenario == "ReCiPe2016 logic")] # scenario only relevant for Row, chose lowest (conservative approach)
lu[, `:=` (cf = cf * 10000, cf_unit = "Global PDF/ha")]
lu <- lu[, ..key_cols]
LC_list[["land_use"]] <- lu


## Water use (LC)----------------
water <- LC_list[["water_use"]]
water <- water[flow_id == "419682fe-60fb-4b43-be89-bf2824b51104" &
                 approach == "Average"]
water <- water[, ..key_cols]

LC_list[["water_use"]] <- water


## Invasive species (LC) -------------
# exclude regions not in FABIO, use global CFs 
invasive_species <- invasive_species[importer %in% regions$iso3c &
                                       exporter %in% regions$iso3c ,] # not gap filling here, assumptions would be too wild

# convert units to match pressures
invasive_species <- invasive_species[, .(flow_id = NA_real_, flow = "trade_volume",
                                         exporter, importer, cf = CFglobal * 1000,
                                         cf_unit = "PDF global*yr/t",
                                         approach = "Average")]

saveRDS(invasive_species, "data/extensions/tidy/invasive_species_tidy.rds")


## FD list --------------
FD_list <- lapply(FD_list, function(dt) {
  dt[cf_unit %like% "km2", `:=`(cf = cf / 100, cf_unit = gsub("km2", "ha", cf_unit))]
  dt <- dt[approach == "Average" & flow_id %in% conc$flow_id]
  dt[, type := conc$fabio_pressure[match(flow_id, conc$flow_id)]]
  dt
})

# save ----------
# add pressure types to all CF data.tables
invisible(lapply(names(LC_list), function(nm){
  LC_list[[nm]][, type := conc$fabio_pressure[match(flow_id, conc$flow_id)]]
  LC_list[[nm]][is.na(type), type := conc$fabio_pressure[match(flow, conc$flow)]]
}))


#save
saveRDS(LC_list, "data/extensions/tidy/lc_impact_country_level_tidy.rds")
saveRDS(LC_climate, "data/extensions/tidy/lc_impact_climate_tidy.rds")
saveRDS(FD_list, "data/extensions/tidy/fd_country_level_tidy.rds")
saveRDS(FD_climate_list, "data/extensions/tidy/fd_climate_tidy.rds")


rm(list = ls())
gc()





