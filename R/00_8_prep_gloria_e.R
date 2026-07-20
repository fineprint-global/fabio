# ==============================================================================
# Author: Paul Höfle, 20.7.2026
#
# This script allows for importing and concordancing GLORIA satellite account values (e.g. emissions in Tonnes) onto FABIO items and regions
# It is currently used for separating out the emissions weight of the Forestry sector from Agriculture (requiring only GLORIA data, but item mapping is possible),
# Code scaffolding for mapping onto FABIO items is kept (but not executed) for possible other applications.
# ==============================================================================
library(data.table)
library(readxl)
library(qs2)
source("R/00_value_added_helpers.R")

# Configuration -----------------------------------------------------------
# Paths
GLORIA_README_PATH      <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled/labels/GLORIA_ReadMe_060.xlsx"
GLORIA_V_DIR            <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_producer_prices/V"
GLORIA_X_DIR            <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled/IOTs_producer_prices/X"
GLORIA_E_DIR            <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled/satellites" 

# Concordance files
INPUT_DIR               <- path.expand("inst/value_added") # for VA data from 13_* and 14_* scripts
ITEM_CONC_PATH          <- file.path(INPUT_DIR, "concordance_items_gloria_fabio.csv")
AREA_CONC_PATH          <- file.path(INPUT_DIR, "concordance_areas_gloria_fabio.csv")
FABIO_TV_PATH_A <- "data/total_value/FABIOv2_producer_total_values_isic_a.rds"
FABIO_TV_PATH_C <- "data/total_value/FABIOv2_producer_total_values_isic_c.rds"

# User-selected sectors and satellite indicators, as documented in the Gloria Readme XLSX
GLORIA_E_FILTERS         <- c("'co2_excl_short_cycle_org_c_total_EDGAR_consistent'", # note the backticks in the variable naming, this is not cleaned for the sake of naming uniformity
                              "'co2_excl_short_cycle_org_c_total_OECD_consistent'")
GLORIA_SECTOR_FILTER    <- c(1:23)

# Load FABIOv2 total values for BOTH ISIC levels --------------------------
message("Loading FABIOv2 total values (ISIC-A) ...")
fv_pack_a   <- prepare_fv(FABIO_TV_PATH_A)
fv_a        <- fv_pack_a$fv
value_col_a <- fv_pack_a$value_col
output_col_a<- fv_pack_a$output_col

message("Loading FABIOv2 total values (ISIC-C) ...")
fv_pack_c   <- prepare_fv(FABIO_TV_PATH_C)
fv_c        <- fv_pack_c$fv
value_col_c <- fv_pack_c$value_col
output_col_c<- fv_pack_c$output_col

if (!exists("years", mode = "numeric")) { # get years from value added tables
  working_years = sort(unique(fv_a$year))
} else if(exists("HAMPEL_Z")) { # OR set additional buffer years if doing error checking
  fabio_years   <- sort(unique(fv_a$year))
  buffer_years  <- c(
    seq(min(fabio_years) - HAMPEL_K, min(fabio_years) - 1L),
    seq(max(fabio_years) + 1L,       max(fabio_years) + HAMPEL_K)
  )
  working_years <- sort(union(fabio_years, buffer_years))
  message(sprintf(
    "  FABIOv2 covers %d-%d (%d years).  Stages 4a/4b operate on %d-%d (%d years: FABIO ± %d-year Hampel buffer); buffer dropped before 4b.",
    min(fabio_years),   max(fabio_years),   length(fabio_years),
    min(working_years), max(working_years), length(working_years),
    HAMPEL_K
  ))
} else { # OR take years from main variable set
  working_years = years
}

# GLORIA dimensions + indexing --------------------------------------------
message("Loading GLORIA dimension labels ...")

regions_tbl <- as.data.table(read_excel(GLORIA_README_PATH, sheet = "Regions"))
sectors_tbl <- as.data.table(read_excel(GLORIA_README_PATH, sheet = "Sectors"))
setorder(sectors_tbl, Lfd_Nr)

n_va      <- nrow(read_excel(GLORIA_README_PATH, sheet = "Value added and final demand"))
n_regions <- nrow(regions_tbl)
n_sectors <- nrow(sectors_tbl)
n_cols    <- n_regions * n_sectors

message(sprintf(
  "  %d regions × %d sectors = %d columns;  %d VA accounts per region.",
  n_regions, n_sectors, n_cols, n_va
))

col_idx <- data.table(
  col                = seq_len(n_cols),
  gloria_region_code = rep(regions_tbl$Region_acronyms,    each  = n_sectors),
  gloria_region_name = rep(regions_tbl$Region_names,       each  = n_sectors),
  gloria_sector_code = rep(as.integer(sectors_tbl$Lfd_Nr), times = n_regions),
  gloria_sector_name = rep(sectors_tbl$Sector_names,       times = n_regions)
)


# Concordance tables ------------------------------------------------------
load_item_conc <- function(path) {
  sc <- fread(path)[
    !is.na(GLORIA_sector_code) & !is.na(FABIO_item_code),
    .(gloria_sector_code = as.integer(GLORIA_sector_code),
      fabio_item_code    = as.integer(FABIO_item_code),
      fabio_item         = FABIO_item)
  ]
  sc <- unique(sc[!is.na(gloria_sector_code) & !is.na(fabio_item_code)])
  sc
}

message("Loading concordance CSVs ...")
item_conc <- load_item_conc(ITEM_CONC_PATH)

area_conc <- fread(AREA_CONC_PATH)[
  !is.na(GLORIA_region_code) & GLORIA_region_code != "" &
    !is.na(FABIO_area_code),
  .(gloria_region_code = as.character(GLORIA_region_code),
    fabio_area_code    = as.integer(FABIO_area_code),
    fabio_area         = FABIO_area)
]
area_conc <- unique(
  area_conc[
    !is.na(gloria_region_code) & gloria_region_code != "" &
      !is.na(fabio_area_code)
  ]
)

# Import Gloria vectors ---------------------------------------------------
# this part imports the selected values (configuration section)
# it only works with GLORIA-data at this stage

# process the user-selected GLORIA satellite indicators into a simple helper table for use in the loop
satellite_labels <- as.data.table(read_excel(GLORIA_README_PATH, sheet = "Satellites"))
satellite_index <- satellite_labels[Sat_indicator %in% GLORIA_E_FILTERS]
# satellite_index <- satellite_labels[Sat_indicator %like% "co2_excl_short_cycle_org_c_1A4" & Sat_head_indicator == "Emissions (EDGAR)"] # override for data exploration: view combinations of values, for instance all CO2 accounts
stopifnot(nrow(satellite_index) == length(GLORIA_E_FILTERS)) # Error check: indicates the indicator name is not availible

process_year_gloria <- function(yr) {
  x_path <- sprintf("%s/X_%d.qs2", GLORIA_X_DIR, yr)
  e_path <- sprintf("%s/TQ_%d.qs2", GLORIA_E_DIR, yr)
  
  # !file.exists(v_path) || 
  if (!file.exists(x_path)|| !file.exists(e_path)) {
    message("  Year ", yr, ":X or E missing, skipping.")
    return(NULL)
  }
  
  message("  Year ", yr, " ...")
  X_vec <- as.numeric(qs_read(x_path))
  E_mat <- as.matrix(qs_read(e_path))
  
  stopifnot(
    length(X_vec) == n_cols,
    ncol(E_mat) == n_cols
  )
  
  # slightly negative values are problematic for inversion - they are treated as data errors and set to 0
  X_vec[X_vec < 0] <- 0
  E_mat[E_mat < 0] <- 0
  
  E_mat <- E_mat * 1000 # convert from kt to t
  X_vec <- X_vec * 1000 # convert from k USD to USD (for comparison to FABIO dollar accounts)
  
  # Processing: Start with the base columns
  dt <- data.table(col = seq_len(n_cols), gloria_x = X_vec)
  
  # Assign labels
  dt <- col_idx[dt, on = "col"]
  dt[, col := NULL]
  dt[, year := yr]
  
  # Add each stressor as a separate column
  for (i in seq_len(nrow(satellite_index))) {
    stressor_name <- satellite_index$Sat_indicator[i]
    row_index <- satellite_index$Lfd_Nr[i]  # Get the actual row number in E_mat
    dt[, (stressor_name) := E_mat[row_index, ]]
  }
  
  dt[] # return processed table of selected value vectors + labels
}

x_files    <- list.files(GLORIA_X_DIR, pattern = "^X_\\d+\\.qs2$")
disk_years <- sort(as.integer(gsub("X_|\\.qs2", "", x_files)))
stopifnot(length(disk_years) > 0)

years   <- intersect(disk_years, working_years)
missing <- setdiff(working_years, disk_years)

stopifnot(length(years) > 0)
message(sprintf(
  "GLORIA X directory holds %d years (%d-%d); loading %d of them (%d-%d) for this run.",
  length(disk_years), min(disk_years), max(disk_years),
  length(years),      min(years),      max(years)
))

message("Loading GLORIA tables")
gloria_satellite_e <- rbindlist(lapply(years, process_year_gloria))

# Filter to relevant GLORIA sectors and value columns ---------------------
gloria_satellite_e <- gloria_satellite_e[gloria_sector_code %in% GLORIA_SECTOR_FILTER]
# gloria_satellite_e <- gloria_satellite_e[, c("gloria_region_code", "gloria_region_name", "year", "gloria_sector_code", "gloria_sector_name", "gloria_x", "gloria_e")]
setcolorder(gloria_satellite_e, c("gloria_region_code", "gloria_region_name", "year",
                                  "gloria_sector_code", "gloria_sector_name", 
                                  "gloria_x", GLORIA_E_FILTERS))


# apply FABIO area concordance --------------------------------------------
# dims are 191 * 23 * 14 after the join, so there are some areas (999 ROW and 249 Yemen) that need to be summed in the subsequent script.
gloria_satellite_e <- area_conc[
  gloria_satellite_e,
  on = "gloria_region_code",
  allow.cartesian = TRUE,
  nomatch = NULL
]

# save result -------------------------------------------------------------
saveRDS(gloria_satellite_e, "data/tidy/gloria_satellite_e.rds")