library(data.table)
library(tidyverse)
library(readxl)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

items <- fread("inst/sua/items_sua.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]


# Read and tidy intactness-based biodiversity impact factors (IBIFs) ----------
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

# Combine impacts and pressures ------------------------
# TODO: Add GHGs
names <- c("land_crop", "land_grass", "n_nh3_n", "n_n2o_n_total")
files <- paste0("data/extensions/sua/", names, ".rds")
data_pressures <- setNames(lapply(files, readRDS), names)

# re-format to match ibif
pressures <- rbindlist(
  lapply(names, function(p) {
    rbindlist(
      lapply(names(data_pressures[[p]]), function(yr) {
        mat <- data_pressures[[p]][[yr]]
        data.table(
          year     = as.integer(yr),
          col_key  = colnames(mat),
          value    = as.numeric(mat)
        )
      })
    )[, pressure := p]
  })
)

# Split col_key (e.g. "ARM_c001") into iso3c and item_code
pressures[, `:=`(iso3c = substr(col_key, 1, 3), comm_code = substr(col_key, 5, 8))]
pressures[, col_key := NULL]

# widen to get all pressures
pressures <- dcast(pressures, year + iso3c + comm_code ~ pressure, value.var = "value")

# convert to units used in IBIF (ha -> km2; elemental N to molecule weight)
pressures[, `:=` (land_crop = land_crop/100, land_grass = land_grass/100,
                  n2o = n_n2o_n_total * 44/28, nh3 = n_nh3_n * 1.21589)]
pressures[, `:=`(n_n2o_n_total = NULL, n_nh3_n = NULL)]

# Define the mapping: ibif column -> pressure column
multiplications <- list(
  c(ibif = "NH3_overall",  pressure = "nh3"),
  c(ibif = "LU_Cr_overall", pressure = "land_crop"),
  c(ibif = "LU_Pa_overall", pressure = "land_grass")
)

# merge pressures and impacts
biodiv <- pressures[ibif_full, on = .(year, iso3c, comm_code)]

# multiply pressures and impacts
# TODO: are the results reasonable?
ibif_cols <- character(0)
for (m in multiplications) {
  new_col <- paste0("ibif", "_", m["pressure"])
  ibif_cols <- c(ibif_cols, new_col)
  biodiv[, (new_col) := get(m["ibif"]) * get(m["pressure"])]
}

# sum up impacts over all pressures 
# TODO: are we sure summing up like this is correct?
biodiv[, ibif_total := do.call(na_sum, .SD), .SDcols = ibif_cols]
ibif_cols <- c(ibif_cols, "ibif_total")

# tidy
biodiv <- biodiv[, c("year", "iso3c", "comm_code", ibif_cols), 
                 with = FALSE]
biodiv[, item_code := items$item_code[match(comm_code, items$comm_code)]]
biodiv[, area_code := regions$code[match(iso3c, regions$iso3c)]]

# format as extensions ---------
sua_extensions <- setNames(
  lapply(ibif_cols, function(col) {
    biodiv[, .(year, iso3c, item_code, area_code, comm_code, value = get(col))]
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








