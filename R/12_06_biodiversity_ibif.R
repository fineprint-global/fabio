library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]
conc <- fread("inst/conc_biodiversity_cfs.csv")


# IBIFs -------------------------
# Read and tidy
fa_dl(file = "",
      link = "https://zenodo.org/records/16234138/files/IBIF_v2.xlsx?download=1",
      path = "input/biodiversity/IBIF.xlsx" # includes a readme with units
)

ibif <- read_excel_sheets("input/biodiversity/IBIF.xlsx", sheets = "BIFs per pressure")
ibif <- ibif[["BIFs per pressure"]]
ibif <- ibif[, .(iso3c = ISO3, CO2_overall, NH3_overall, NOx_overall,
                 LU_Cr_overall, LU_Pa_overall)]

# average impact factors for RoW
cols <- setdiff(names(ibif), "iso3c")
ibif[!iso3c %in% regions$iso3c, iso3c := "ROW"]
ibif <- ibif[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols, by = .(iso3c)]

# use "China, mainland" factors for Hong Kong and Macao
ibif <- rbind(ibif,
              ibif[iso3c == "CHN"][, iso3c := "HKG"],
              ibif[iso3c == "CHN"][, iso3c := "MAC"])

# Create full template
ibif_full <- CJ(year = years, iso3c = regions$iso3c, comm_code = items$comm_code)

# add impact factors to full table
ibif_full <- merge(ibif_full, ibif, by = "iso3c", all.x = TRUE)

# Combine impacts and pressures 
nms <- conc[source == "IBIF", unique(fabio_pressure)]
files <- paste0("data/extensions/sua/", nms, ".rds")
data_pressures <- setNames(lapply(files, readRDS), nms)
data_pressures[["fd_gwp_total"]] <- readRDS("data/extensions/fd_sua/gwp_total.rds")
pressures <- unformat_extension(data_pressures, c(nms, "fd_gwp_total"), long = TRUE)

# widen to get all pressures
pressures <- dcast(pressures, year + iso3c + comm_code ~ ext, value.var = "value")

# convert to units used in IBIF (ha -> km2; nh3_n to nh3; co2eq from t to kg)
pressures[, `:=` (land_crop = land_crop/100, land_grass = land_grass/100,
                   nh3 = n_nh3_n * 1.21589, co2_eq = gwp_total * 1000, fd_co2_eq = fd_gwp_total * 1000)]
pressures[, `:=`(n_nh3_n = NULL, gwp_total = NULL, fd_gwp_total = NULL)]

# get temporary meadows and pastures from cropland and add to grassland to match IBIF pasture definition
pressures[comm_code == "c158", land_grass := land_grass + land_crop]
pressures[comm_code == "c158", land_crop := 0]

# Define the mapping: ibif column -> pressure column
multiplications <- list(
  c(ibif = "NH3_overall",  pressure = "nh3"),
  c(ibif = "LU_Cr_overall", pressure = "land_crop"),
  c(ibif = "LU_Pa_overall", pressure = "land_grass"),
  c(ibif = "CO2_overall", pressure = "co2_eq"),
  c(ibif = "CO2_overall", pressure = "fd_co2_eq")
)

# merge pressures and impacts
ibif <- pressures[ibif_full, on = .(year, iso3c, comm_code)]

# multiply pressures and impacts
ibif_cols <- character(0)
for (m in multiplications) {
  new_col <- paste0("ibif", "_", m["pressure"])
  ibif_cols <- c(ibif_cols, new_col)
  ibif[, (new_col) := get(m["ibif"]) * get(m["pressure"])]
}

# sum up impacts over all pressures, except final demand
ibif_cols <- setdiff(ibif_cols, "ibif_fd_co2_eq")
ibif[, ibif_total := do.call(na_sum, .SD), .SDcols = ibif_cols]
ibif_cols <- c(ibif_cols, "ibif_total", "ibif_fd_co2_eq")

# tidy
ibif <- ibif[, c("year", "iso3c", "comm_code", ibif_cols), 
                 with = FALSE]
ibif[, item_code := items$item_code[match(comm_code, items$comm_code)]]
ibif[, area_code := regions$code[match(iso3c, regions$iso3c)]]


# format as extensions ---------
sua_extensions <- setNames(
  lapply(ibif_cols, function(col) {
    ibif[, .(year, iso3c, item_code, area_code, comm_code, value = get(col))]
  }),
  ibif_cols
)

conc <- fread("inst/conc_cbs_sua.csv")
items_cbs <- fread("inst/items_full_123.csv")
cbs_extensions <- lapply(sua_extensions, agg_sua_to_cbs)

# Format all -------------------
E_sua <- lapply(sua_extensions, format_extension)
E_cbs <- lapply(cbs_extensions, format_extension, itms = items_cbs)


# save
nm_fd <-  names(E_sua)[grepl("fd", names(E_sua))]
nms <- setdiff(names(E_sua), nm_fd)

# non-fd extensions
for (nm in nms) {
  saveRDS(E_sua[[nm]], paste0("data/extensions/sua/", nm, ".rds"))
  saveRDS(E_cbs[[nm]], paste0("data/extensions/cbs/", nm, ".rds"))
}

#fd extensions
saveRDS(E_sua[[nm_fd]], paste0("data/extensions/fd_sua/ibif_co2_eq.rds"))
saveRDS(E_cbs[[nm_fd]], paste0("data/extensions/fd_cbs/ibif_co2_eq.rds"))

saveRDS(E_sua[[nm_fd]], paste0("data/extensions/fd_sua/ibif_total.rds"))
saveRDS(E_cbs[[nm_fd]], paste0("data/extensions/fd_cbs/ibif_total.rds"))

# tidy
rm(list = ls())
gc()
