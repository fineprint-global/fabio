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

# Download LC-Impact from Zenodo (latest version 2 as of March 24, 2026)
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
                             "Considering N-limited regions")]

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
acid[, `:=` (cf = cf *  1.21589, unit = "global PDF*y/kg N")]
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


# LC-IMPACT v1.3 (appendix) ---------------------------------------------------
# Reads the LC-IMPACT v1.3 characterization factors (Verones et al. 2020,
# https://doi.org/10.5281/zenodo.6200606) into the same two objects as the v2 set
# above. Both versions are run side by side in 12_8 (LC = v2, LC1 = v1.3), they do
# not replace each other. Everything below carries an "lc13_" prefix, items, conc
# and regions are re-used from the top of this script.
# Value choice: all effects, 100 years -> the time horizon matches the AR6 GWP100
# factors used in 12_5. The certain/all axis has no counterpart elsewhere in FABIO
# and v2 offers no such choice at all, so neither is exposed as an option.

## Settings ------------
path_lc13 <- "input/biodiversity/LC_impact_v13/"
lc13_zip  <- "LC-Impact-v-1-3.zip"

# only these six workbooks are needed, the rasters, shapefiles, GoogleEarth layers
# and the two 100 MB+ USEtox workbooks are irrelevant here
lc13_needed <- c(
  climate       = "LC-Impact/2-climate change/Climate change CFs.xlsx",
  acidification = "LC-Impact/7-terrestrial acidification/CF_terrestrial_acidification.xlsx",
  eut_fresh     = "LC-Impact/8-freshwater eutrophication/CF_FWEutrophication.xlsx",
  eut_marine    = "LC-Impact/9-marine eutrophication/CFs_marine_eutrophication.xlsx",
  land_use      = "LC-Impact/11-Land stress/CFs_land_Use_average.xlsx",
  water_use     = "LC-Impact/12-water consumption/CFs_water_consumption_ecosystems_20180831.xlsx")

# `type` has to be the FABIO pressure name that 12_8 builds its column names from.
# Resolving it through `conc` instead of writing the literal keeps this branch tied
# to the same concordance as the v2 CFs above: rename a pressure in
# inst/conc_biodiversity_cfs.csv and this stops here, rather than silently emitting
# columns that `merging_pressures_impacts()` no longer matches (which would drop the
# CF and leave the extension at zero without an error). Looked up by the v2 BAMBOO
# flow name, so each v1.3 CF is pinned to the v2 CF it is meant to mirror.
lc13_pressure <- function(flow_name) {
  hit <- unique(conc[source == "LC" & flow == flow_name, fabio_pressure])
  if(length(hit) != 1L) {
    stop("Expected exactly one LC pressure in inst/conc_biodiversity_cfs.csv for ",
         "flow '", flow_name, "', found ", length(hit),
         if(length(hit)) paste0(": ", paste(hit, collapse = ", ")) else "", ".")
  }
  hit
}

# v1.3 identifies countries by plain English name, countrycode() resolves most of
# them. These are the ones it misses that are FABIO regions.
lc13_iso_override <- data.table(
  name = c("Byelarus", "Zaire", "Congo DRC", "Western Samoa", "Bahamas, The",
           "Gambia, The", "Myanmar (Burma)", "Tanzania, United Republic of",
           "The Former Yugoslav Republic of Macedonia"),
  iso3c = c("BLR", "COD", "COD", "WSM", "BHS",
            "GMB", "MMR", "TZA",
            "MKD"))

## Download, extract ------------
dir.create(path_lc13, recursive = TRUE, showWarnings = FALSE)

# fa_extract() cannot be used here: it unzips unconditionally and, with the default
# rm = TRUE, deletes the extracted files afterwards. The 4.5 GB archive is only
# needed on the first run, so it is fetched and checked only when a workbook is
# missing and can be deleted afterwards.
lc13_targets <- paste0(path_lc13, lc13_needed)
if(all(file.exists(lc13_targets))) {
  cat("Skipping extraction, already found:", length(lc13_targets),
      "LC-IMPACT v1.3 workbooks\n")
} else {
  options(timeout = max(72000, getOption("timeout")))
  fa_dl(file = lc13_zip, path = path_lc13, link = "https://zenodo.org/records/6200606/files/")
  # a truncated download is indistinguishable from a complete one to file.exists()
  lc13_size <- file.size(paste0(path_lc13, lc13_zip)) / 1e9
  if(is.na(lc13_size) || lc13_size < 4.4) {
    stop("LC-IMPACT v1.3 archive is only ", round(lc13_size, 2),
         " GB, expected ~4.5 GB. Delete it and resume the download.")
  }
  lc13_todo <- lc13_needed[!file.exists(lc13_targets)]
  cat("Extracting", length(lc13_todo), "of", length(lc13_needed),
      "LC-IMPACT v1.3 workbooks\n")
  invisible(lapply(lc13_todo, function(f) {
    unzip(paste0(path_lc13, lc13_zip), files = f, exdir = path_lc13)
  }))
  if(!all(file.exists(lc13_targets))) {
    stop("Extraction failed for:\n\t",
         paste(lc13_needed[!file.exists(lc13_targets)], collapse = ",\n\t"))
  }
}

## Helpers ------------
# read a sheet raw: no header assumption, everything character. v1.3 headers sit on
# row 1, 2 or 3 depending on the workbook.
lc13_read_raw <- function(path, sheet) {
  x <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                          col_types = "text", .name_repair = "minimal")
  as.data.table(setNames(as.list(x), paste0("v", seq_along(x))))
}

lc13_sheet <- function(path, pattern) {
  sheets <- readxl::excel_sheets(path)
  hit <- grep(pattern, sheets, ignore.case = TRUE)
  if(length(hit) != 1L) {
    stop("Expected exactly one sheet matching '", pattern, "' in ", basename(path),
         ". Sheets are: ", paste(sheets, collapse = " | "), ".")
  }
  sheets[hit]
}

lc13_col <- function(hdr, pattern, what) {
  hit <- grep(pattern, hdr, ignore.case = TRUE)
  if(length(hit) != 1L) {
    stop("Expected exactly one column matching '", pattern, "' for ", what,
         ", found ", length(hit), ": ", paste(hdr[hit], collapse = " | "), ".")
  }
  hit
}

lc13_hdr_row <- function(raw, pattern, what) {
  hit <- grep(pattern, trimws(raw[[1L]]))
  if(!length(hit)) stop("No header row matching '", pattern, "' in ", what, ".")
  hit[1L]
}

lc13_names <- function(raw, hdr_row) {
  nms <- trimws(unlist(raw[hdr_row], use.names = FALSE))
  bad <- is.na(nms) | !nzchar(nms)
  nms[bad] <- paste0("v", which(bad))
  nms
}

# carry a merged-cell label to the right (climate and land use label only the first
# column of each block)
lc13_fill_right <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  for(i in seq_along(x)[-1L]) if(is.na(x[i])) x[i] <- x[i - 1L]
  x
}

# country sheet -> data.table with the label column as `name`. `9-marine
# eutrophication` carries a second country block to the right, its extra rows drop
# out with the empty labels of the first one.
lc13_table <- function(raw, hdr_row) {
  dt <- raw[-seq_len(hdr_row)]
  setnames(dt, make.unique(lc13_names(raw, hdr_row)))
  setnames(dt, 1L, "name")
  dt[, name := trimws(name)]
  dt[!is.na(name) & nzchar(name)]
}

# several sheets write a blank (" ") where a country has no CF, e.g. marine
# eutrophication for BFA/ECU/IDN/RUS and acidification for the Pacific islands.
lc13_num <- function(x) suppressWarnings(as.numeric(x))

# single value from a global fallback sheet
lc13_global <- function(path, sheet_pattern, row_pattern, col_pattern, what) {
  raw <- lc13_read_raw(path, lc13_sheet(path, sheet_pattern))
  hdr <- which(apply(raw, 1L, function(r) any(grepl(col_pattern, trimws(r), ignore.case = TRUE))))
  if(!length(hdr)) stop("No header row matching '", col_pattern, "' in ", what, ".")
  hdr <- hdr[1L]
  j <- lc13_col(lc13_names(raw, hdr), col_pattern, what)
  i <- grep(row_pattern, trimws(raw[[1L]]), ignore.case = TRUE)
  i <- i[i > hdr]
  if(length(i) != 1L) stop("Expected exactly one '", row_pattern, "' row in ", what, ".")
  as.numeric(raw[[j]][i])
}

# name -> iso3c, restrict to FABIO. RoW takes the file's own global CF, as for the v2
# CFs above; every other FABIO region without a CF in the source is left missing and
# becomes a zero at the na.rm = TRUE aggregation in 12_8, again as for v2.
lc13_to_iso <- function(dt, glo, label) {
  nm  <- unique(trimws(dt$name))
  iso <- suppressWarnings(countrycode::countrycode(nm, "country.name", "iso3c"))
  ovr <- match(nm, lc13_iso_override$name)
  iso[!is.na(ovr)] <- lc13_iso_override$iso3c[ovr[!is.na(ovr)]]
  
  dt <- copy(dt)
  dt[, iso3c := iso[match(trimws(name), nm)]]
  dt <- dt[iso3c %in% regions$iso3c & !is.na(cf)]
  
  if(anyDuplicated(dt, by = c("iso3c", "type"))) {
    stop("Two v1.3 country rows map onto the same FABIO region in ", label, ".")
  }
  
  rbind(dt[, .(iso3c, type, cf)], glo[, .(iso3c = "ROW", type, cf)])
}

## Climate change (LC v1.3) ------------
# Global CFs only. v1.3 gives `Terrestrial ecosystems` and `Aquatic ecosystems`, the
# aquatic factor is Hanafiah et al. (2011) and freshwater-fish based -> freshwater.
# v1.3 climate is a mix of marginal and average by design (Verones et al. 2020
# Table 2), average is the closer of the two labels 12_8 knows.
lc13_clim_file  <- paste0(path_lc13, lc13_needed["climate"])
lc13_clim_raw   <- lc13_read_raw(lc13_clim_file,
                                 lc13_sheet(lc13_clim_file, "characterization factors"))
# two header rows: ecosystem type (merged) over value choice
lc13_clim_realm <- lc13_fill_right(unlist(lc13_clim_raw[1L], use.names = FALSE))
lc13_clim_vc    <- trimws(unlist(lc13_clim_raw[2L], use.names = FALSE))

lc13_clim_pick <- function(realm_pattern) {
  hit <- which(grepl(realm_pattern, lc13_clim_realm, ignore.case = TRUE) &
                 grepl("^All effects, 100", lc13_clim_vc, ignore.case = TRUE))
  if(length(hit) != 1L) {
    stop("Climate: expected exactly one column for ", realm_pattern, ", found ",
         length(hit), ".")
  }
  hit
}

# v1.3 substance name -> FABIO pressure, via the v2 flow names in `conc`
# (biogenic methane -> `Methane`, not `Fossil methane`)
lc13_ghg <- c("Carbon dioxide" = lc13_pressure("carbon dioxide (biogenic)"),
              "Methane"        = lc13_pressure("methane (biogenic)"),
              "Nitrous oxide"  = lc13_pressure("nitrous oxide"))

lc13_clim_dt <- lc13_clim_raw[-(1:2)]
lc13_climate <- data.table(
  flow        = trimws(lc13_clim_dt[[1L]]),
  Terrestrial = as.numeric(lc13_clim_dt[[lc13_clim_pick("Terrestrial ecosystems")]]),
  Freshwater  = as.numeric(lc13_clim_dt[[lc13_clim_pick("Aquatic ecosystems")]]))
lc13_climate <- lc13_climate[flow %in% names(lc13_ghg)]
if(nrow(lc13_climate) != length(lc13_ghg)) {
  stop("Climate: expected ", length(lc13_ghg), " GHG rows, found ", nrow(lc13_climate), ".")
}
lc13_climate[, type := unname(lc13_ghg[flow])]

lc13_climate <- melt(lc13_climate, id.vars = c("flow", "type"), variable.name = "realm",
                     value.name = "cf", variable.factor = FALSE)

# PDF*y/kg -> PDF*y/t, to match the tonne-based ghg pressures, as above for v2
lc13_climate[, `:=` (cf = cf * 1000, cf_unit = "global PDF*y/t_em",
                     approach = "Average", iso3c = "GLO", flow = NULL)]

## Terrestrial acidification (LC v1.3) ------------
# v1.3 models terrestrial acidification as marginal only, there is no average variant
# (Verones et al. 2020 Table 2) -> approach = "Marginal", same as the v2 CFs above.
lc13_acid_file <- paste0(path_lc13, lc13_needed["acidification"])
lc13_acid_raw  <- lc13_read_raw(lc13_acid_file,
                                lc13_sheet(lc13_acid_file, "^CF per countries$"))
lc13_acid_hdr  <- lc13_hdr_row(lc13_acid_raw, "^COUNTRY$", "acidification")
lc13_acid      <- lc13_table(lc13_acid_raw, lc13_acid_hdr)

# NOx and SOx are on the same sheet, FABIO has no pressure for either
lc13_acid <- data.table(
  name = lc13_acid$name,
  type = lc13_pressure("Ammonia"),
  # NH3 -> N, same factor as above for v2
  cf   = lc13_num(lc13_acid[[lc13_col(names(lc13_acid), "^CF NH3$", "acid NH3")]]) * 1.21589)

lc13_acid_glo <- data.table(
  type = lc13_pressure("Ammonia"),
  cf   = lc13_global(lc13_acid_file, "continent and global", "^Global$", "^CF NH3$",
                     "acid global NH3") * 1.21589)

lc13_acid <- lc13_to_iso(lc13_acid, lc13_acid_glo, "acidification")
lc13_acid[, `:=` (realm = "Terrestrial", approach = "Marginal",
                  cf_unit = "global PDF*y/kg N")]

## Eutrophication (LC v1.3) ------------
# v1.3 freshwater eutrophication is P-only, so N is routed to *marine* eutrophication
# instead -> new category marine_eutrophication. Both are modelled as linear in v1.3
# (Verones et al. 2020 Table 2), which is the average-equivalent of the two labels
# 12_8 knows -> approach = "Average". Both nutrients take the to-water CF (P to
# water, N to river), which matches the pressures; v2 above is soil-only, so the two
# versions are not on the same compartment.

### freshwater (P)
lc13_fweut_file <- paste0(path_lc13, lc13_needed["eut_fresh"])
lc13_fweut_raw  <- lc13_read_raw(lc13_fweut_file,
                                 lc13_sheet(lc13_fweut_file, "^Country CFs$"))
lc13_fweut_hdr  <- lc13_hdr_row(lc13_fweut_raw, "^Country$", "freshwater eutrophication")
lc13_fweut      <- lc13_table(lc13_fweut_raw, lc13_fweut_hdr)

# P to soil and the erosion CF are on the same sheet: no P-to-soil pressure in FABIO,
# and the erosion CF is PDFyr/m2yr, a land-area basis that cannot multiply kg P
lc13_fweut <- data.table(
  name = lc13_fweut$name,
  type = lc13_pressure("Phosphorus"),
  cf   = lc13_num(lc13_fweut[[lc13_col(names(lc13_fweut), "^CF for P emissions to water",
                                       "P to water")]]))

lc13_fweut_glo <- data.table(
  type = lc13_pressure("Phosphorus"),
  cf   = lc13_global(lc13_fweut_file, "^Global$", "^Global$", "^CF for P emissions to water",
                     "fw eut global P water"))

lc13_fweut <- lc13_to_iso(lc13_fweut, lc13_fweut_glo, "eutrophication fresh")
lc13_fweut[, realm := "Freshwater"]

### marine (N)
lc13_meut_file <- paste0(path_lc13, lc13_needed["eut_marine"])
lc13_meut_raw  <- lc13_read_raw(lc13_meut_file,
                                lc13_sheet(lc13_meut_file, "^country CFs$"))
lc13_meut_hdr  <- lc13_hdr_row(lc13_meut_raw, "^Country$", "marine eutrophication")
lc13_meut      <- lc13_table(lc13_meut_raw, lc13_meut_hdr)

# n_leaching_runoff has already left the soil towards surface water -> the river CF,
# not the soil one. The direct marine emissions in the second country block of this
# sheet have no FABIO pressure.
lc13_meut <- data.table(
  name = lc13_meut$name,
  type = lc13_pressure("nitrogen, total (excluding N2)"),
  cf   = lc13_num(lc13_meut[[lc13_col(names(lc13_meut), "freshwater \\(river\\)",
                                      "N to river")]]))

# the `global CFs` sheet is a bare name/value pair list, no header row
lc13_meut_glo_raw <- lc13_read_raw(lc13_meut_file, lc13_sheet(lc13_meut_file, "^global CFs$"))
lc13_meut_glo_row <- grep("freshwater \\(river\\)", trimws(lc13_meut_glo_raw[[1L]]),
                          ignore.case = TRUE)
if(length(lc13_meut_glo_row) != 1L) {
  stop("Expected exactly one global river CF row in marine eutrophication.")
}
lc13_meut_glo <- data.table(
  type = lc13_pressure("nitrogen, total (excluding N2)"),
  cf   = as.numeric(lc13_meut_glo_raw[[2L]][lc13_meut_glo_row]))

lc13_meut <- lc13_to_iso(lc13_meut, lc13_meut_glo, "eutrophication marine")
lc13_meut[, realm := "Marine"]

lc13_eut <- rbindlist(list(lc13_fweut, lc13_meut), use.names = TRUE)
lc13_eut[, `:=` (approach = "Average", cf_unit = "global PDF*y/kg")]

## Land use (LC v1.3) ------------
# `CFs_land_Use_average.xlsx` only - the marginal workbook and the marginal sheets
# inside this one are ignored -> approach = "Average". Occupation and transformation
# are invariant on the certain/all axis, so only the time horizon is chosen here.
# land_crop takes `Annual crops` only; luc_forest_to_agric takes the `Annual crops`
# transformation CF, which v1.3 defines as "to", not "from forest".
lc13_lu_file <- paste0(path_lc13, lc13_needed["land_use"])

lc13_lu_map <- list(
  occupation = c("Annual crops" = lc13_pressure("arable"),
                 "Pasture"      = lc13_pressure("grassland/pasture/meadow")),
  transf     = c("Annual crops" = lc13_pressure("from forest, natural")))
lc13_lu_sheets <- c(occupation = "^occupation average country",
                    transf     = "^transf\\. avg country 100y")
# the country sheets carry no global row, the ecoregion sheets do ("World average")
lc13_lu_eco_sheets <- c(occupation = "^occupation average ecoregion",
                        transf     = "^transf\\. avg ecoregion 100y")

lc13_read_lu <- function(sheet_pattern, map) {
  sh  <- lc13_sheet(lc13_lu_file, sheet_pattern)
  raw <- lc13_read_raw(lc13_lu_file, sh)
  hdr <- lc13_hdr_row(raw, "^(Country|eco_code)$", sh)
  # 3 header rows: the land use type (merged, labelling only its first column) over
  # Median / lower 95% / upper 95% per type. Only the median is used, the rest of the
  # pipeline carries no uncertainty either.
  lu_type <- lc13_fill_right(unlist(raw[hdr - 1L], use.names = FALSE))
  dt <- raw[-seq_len(hdr)]
  setnames(dt, make.unique(paste(lu_type, lc13_names(raw, hdr), sep = "|")))
  setnames(dt, 1L, "name")
  dt[, name := trimws(name)]
  dt <- melt(dt[!is.na(name) & nzchar(name)], id.vars = "name", variable.name = "key",
             value.name = "cf", variable.factor = FALSE)
  dt[, c("lu_type", "stat") := tstrsplit(key, "|", fixed = TRUE)]
  if(!all(names(map) %in% dt$lu_type)) {
    stop("Land use sheet '", sh, "' is missing: ",
         paste(setdiff(names(map), dt$lu_type), collapse = ", "), ".")
  }
  dt <- dt[lu_type %in% names(map) & stat == "Median"]
  dt[, .(name, type = unname(map[lu_type]), cf = lc13_num(cf))]
}

lc13_lu <- rbindlist(lapply(names(lc13_lu_sheets), function(sheet) {
  ctry <- lc13_read_lu(lc13_lu_sheets[[sheet]], lc13_lu_map[[sheet]])
  glo  <- lc13_read_lu(lc13_lu_eco_sheets[[sheet]], lc13_lu_map[[sheet]])
  glo  <- glo[name == "World average", .(type, cf)]
  if(!nrow(glo)) {
    stop("Land use: no 'World average' row on the ecoregion sheet for ", sheet, ".")
  }
  lc13_to_iso(ctry, glo, paste0("land use ", sheet))
}), use.names = TRUE)

# PDF-eq/m2 -> per ha, as above for v2
lc13_lu[, `:=` (cf = cf * 10000, realm = "Terrestrial", approach = "Average",
                cf_unit = "global PDF*y/ha")]

## Water consumption, ecosystems (LC v1.3) ------------
# v1.3 models water consumption for ecosystem quality as marginal only, there is no
# average variant (Verones et al. 2020 Table 2) -> approach = "Marginal". No unit
# conversion: the CF is PDF*yr/m3 and water_blue_total is in m3.
# All effects = surface and groundwater (certain effects would be surface only).
lc13_water_file <- paste0(path_lc13, lc13_needed["water_use"])
lc13_water_raw  <- lc13_read_raw(lc13_water_file,
                                 lc13_sheet(lc13_water_file, "^CF per countries$"))
lc13_water_hdr  <- lc13_hdr_row(lc13_water_raw, "^Country$", "water consumption")
lc13_water      <- lc13_table(lc13_water_raw, lc13_water_hdr)

lc13_water <- data.table(
  name = lc13_water$name,
  type = lc13_pressure("Water"),
  cf   = lc13_num(lc13_water[[lc13_col(names(lc13_water), "^CF all effects", "water CF")]]))

lc13_water_glo <- data.table(
  type = lc13_pressure("Water"),
  cf   = lc13_global(lc13_water_file, "^CF Global$", "^Global", "^CF all effects",
                     "water global"))

lc13_water <- lc13_to_iso(lc13_water, lc13_water_glo, "water use")
lc13_water[, `:=` (realm = "Freshwater", approach = "Marginal",
                   cf_unit = "global PDF*y/m3")]

## Assemble, check, save ------------
lc13_keep <- c("iso3c", "type", "realm", "approach", "cf", "cf_unit")

lc13_country <- list(land_use       = lc13_lu,
                     water_use      = lc13_water,
                     acidification  = lc13_acid,
                     eutrophication = lc13_eut)
lc13_country <- lapply(lc13_country, function(dt) dt[, ..lc13_keep])

# dry run of what 12_8 does with these objects, so a broken CF set fails here rather
# than silently becoming a 0 at the na.rm = TRUE aggregation
lc13_check <- rbindlist(lc13_country, idcol = TRUE, use.names = TRUE)
lc13_check[, `:=` (colname  = paste0("cf_", type, "_", tolower(realm), "_", .id, "_",
                                     tolower(approach)),
                   category = paste0(tolower(realm), "_", .id))]

lc13_categories <- c("terrestrial_land_use", "freshwater_water_use",
                     "terrestrial_acidification", "freshwater_eutrophication",
                     "marine_eutrophication")
lc13_bad_cat <- setdiff(unique(lc13_check$category), lc13_categories)
if(length(lc13_bad_cat)) {
  stop("realm/.id combinations not in 12_8's impact_categories: ",
       paste(lc13_bad_cat, collapse = ", "), ".")
}

if(anyDuplicated(lc13_check, by = c("iso3c", "colname"))) {
  stop("More than one CF per iso3c and column - dcast in 12_8 would silently aggregate.")
}

lc13_wide <- dcast(lc13_check, iso3c ~ colname, value.var = "cf")
if(anyNA(lc13_wide[iso3c == "ROW"])) stop("No global CF for RoW after reshaping.")

saveRDS(lc13_country, "data/extensions/tidy/lc_impact_v13_country_level_tidy.rds")
saveRDS(lc13_climate, "data/extensions/tidy/lc_impact_v13_climate_tidy.rds")


rm(list = ls())
