# 00_8_filter_gloria.R - calculate gloria_emissions_shares and gloria_emissions

# since energy_emissions_FAO includes forestry and fishery under agriculture, prod_prim totals need to be scaled up. 
# For this, shares of agriculture + fishery + forestry co2 emissions can be estimated using GLORIA.
# Co2 emissions have to be used, fossil fuel consumption does not seem to be recorded, only production
# "'co2_excl_short_cycle_org_c_total_OECD_consistent'" is almost equivalent to EDGAR, but small differences exist. EDGAR methodology is used here. 

# Setup ------------------------------------------------------------------------
library(data.table)
library(qs2)
years = 2010:2023

## Define paths and structure of the GLORIA data ------------------
# this section creates a df that represents the structure of region-sector-labels, analogous to labels/GLORIA_ReadMe_060.xlsx > Sequential_regionSector_labels
# this is done to make merging in labels more transparent 

gloria_dir <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled"
gloria_satellite_path <- file.path(gloria_dir, "satellite_account_csv_files")
gloria_labels_path <- file.path(gloria_dir, "labels/GLORIA_ReadMe_060.xlsx") 

# Sector indexes
sectors_agriculture <- 1L:15L
sectors_livestock <- 16L:20L
sectors_forestry <- 21L
sectors_fishery <- c(22L, 23L)

# Region labels
region_labels <- as.data.table(read_excel(gloria_labels_path, sheet = "Regions"))
setnames(region_labels, c("region_id", "iso3", "region_name"))
region_labels[, region_id := as.integer(region_id)]

# Sequential Region-Sector labels  
region_sector_labels <- as.data.table(read_excel(gloria_labels_path, sheet = "Sequential region-sector labels"))
region_sector_labels[, Lfd_Nr := as.integer(Lfd_Nr)]
region_sector_labels <- region_sector_labels[, c("Lfd_Nr", "Sequential_regionSector_labels")]

# Sector labels  
sector_labels <- as.data.table(read_excel(gloria_labels_path, sheet = "Sectors"))
setnames(sector_labels, 1:2, c("sector_id", "sector_name"))
sector_labels[, sector_id := as.integer(sector_id)]

# Satellite labels (for verification)
satellite_labels <- as.data.table(read_excel(gloria_labels_path, sheet = "Satellites"))
setnames(satellite_labels, 1:4, c("sat_id", "sat_head", "sat_indicator", "sat_unit"))

# the original matrix has 120 rows of industries, then 120 rows of the analogous product. 
# The "product" rows are already excluded in the processed matrix, as they are empty. The merge will thus only keep industry labels, this is included for completeness.
interweave <- c("industry", "product") 
region_sector_structure_df <- CJ(region_id = region_labels$region_id, interweave, sector_id = sector_labels$sector_id) # CJ = cross-join, creates a df with each possible combination of items in order of inputs
# the combinations of items follow the same construction as region-sector labels 
region_sector_labels <- cbind(region_sector_labels, region_sector_structure_df)

# this creates a concordance table between sector and region IDs with the full region-sector index
region_sector_labels <- merge(region_sector_labels, sector_labels, by = "sector_id", all.x = TRUE, sort = F)
region_sector_labels <- merge(region_sector_labels, region_labels, by = "region_id", all.x = TRUE, sort = F)



# Read/process and filter GLORIA emissions satellite account data ----------------------

emissions_filter <- satellite_labels[sat_indicator %in% c(
  "'ch4_total_EDGAR_consistent'", # Note: double ticks are in documentation XLSX, kept for consistency. Needed for filtering.
  "'n2o_total_EDGAR_consistent'",
  "'co2_excl_short_cycle_org_c_total_EDGAR_consistent'",
  "'GHG_total_EDGAR_consistent'"
)]

emissions_rows <- emissions_filter$sat_id
# ~/FABIO/v2/data/gloria_emissions_shares.rds

# reads the processed TQ matrix (only industry rows/columns) for each year, filters out relevant rows, and is concatenated to a full table of values for the relevant variables and years   
gloria_satellite_list <- list()
for (year in years) {
  satellite_file <- file.path("/mnt/nfs_fineprint/tmp/gloria/v060-compiled/satellites", sprintf("TQ_%s.qs2", year))
  TQ_year <- qs_read(satellite_file)
  TQ_year <- TQ_year[emissions_rows, , drop = FALSE]
  TQ_year <- as.data.table(TQ_year)
  TQ_year[, `:=`(sat_id = emissions_rows, year = year)]
  setcolorder(TQ_year, c("sat_id", setdiff(names(dt), "sat_id"))) # moves forward the key col
  setcolorder(TQ_year, c("year", setdiff(names(dt), "year"))) # moves forward the key col
  gloria_satellite_list[[length(gloria_satellite_list) + 1]] <- TQ_year
}
gloria_emissions <- rbindlist(gloria_satellite_list)

# Melt to long format
gloria_emissions <- melt(gloria_emissions, id.vars = c("year", "sat_id"), variable.name = "col_name", value.name = "value")

# Parse extracted column index
gloria_emissions[, col_id := as.integer(gsub("V", "", col_name))]
gloria_emissions[, col_name := NULL]

# merge previously constructed labels
gloria_emissions <- merge(gloria_emissions, region_sector_labels, by.x = "col_id", by.y = "Lfd_Nr", all.x = T)

# removes interstitial columns from region_sector_labels
gloria_emissions[, interweave := NULL]
gloria_emissions[, Sequential_regionSector_labels := NULL]
gloria_emissions[, col_id := NULL]

# Assign sector groups
gloria_emissions[, sector_group := fcase(
  sector_id %in% sectors_agriculture, "agriculture",
  sector_id %in% sectors_livestock, "livestock",
  sector_id %in% sectors_forestry, "forestry",
  sector_id %in% sectors_fishery, "fishery",
  default = "other"
)]

# Aggregate by region-year-sector_group
gloria_emissions_shares <- gloria_emissions[, .(
  emissions = sum(value, na.rm = TRUE)
), by = .(year, region_id, iso3, sector_group, sat_id)]

# Pivot wider
gloria_emissions_shares <- dcast(
  gloria_emissions_shares,
  year + region_id + iso3 + sat_id ~ sector_group,
  value.var = "emissions",
  fill = 0
)


# Calculate gloria_emissions_shares.rds table -----------------------------

# Calculate shares (agriculture + livestock + forestry + fishery as denominator)
gloria_emissions_shares[, emissions_total := agriculture + livestock + forestry + fishery]

# Here, co2 emissions are used to calculate shares, to proxy fossil-fuel energy use
gloria_emissions_shares <- gloria_emissions_shares[sat_id == emissions_filter[sat_indicator == "'co2_excl_short_cycle_org_c_total_EDGAR_consistent'"]$sat_id]

# for shares, livestock and agriculture are summed up into agriculture, as per FAO definitions
gloria_emissions_shares <- gloria_emissions_shares[emissions_total > 0 , .(
  year,
  region = iso3,
  # energy_use_agriculture = agriculture,
  # energy_use_forestry = forestry,
  # energy_use_fishery = fishery,
  energy_share_agriculture = ( agriculture + livestock ) / emissions_total,
  energy_share_forestry = forestry / emissions_total,
  energy_share_fishery = fishery / emissions_total
)]

# Verify shares sum to 1
gloria_emissions_shares[, share_check := energy_share_agriculture + energy_share_forestry + energy_share_fishery]
stopifnot(all(abs(gloria_emissions_shares$share_check - 1) < 1e-10))
gloria_emissions_shares[, share_check := NULL]

## Save emissions shares table (gloria_emissions_shares.rds) ----
saveRDS(gloria_emissions_shares, "data/gloria_emissions_shares.rds")


# Calculate emissions for product groups (later used to calculate average intensities for imputation) --------

# filter the 3 relevant GHGs, total is not needed here
gloria_emissions <- gloria_emissions[sat_id %in% emissions_filter[sat_indicator != "'GHG_total_EDGAR_consistent'"]$sat_id]

# add labels (factor to keep order of items), and pivot to have the 3 GHGs as columns
gloria_emissions <- merge(gloria_emissions, satellite_labels, by = "sat_id", sort = F)
gloria_emissions$sector_name <- factor(gloria_emissions$sector_name, levels = unique(gloria_emissions$sector_name))
gloria_emissions <- dcast(gloria_emissions, year + iso3 + sector_name + sector_group ~ sat_indicator, value.var = "value")

# keep only gas names from columns like "'ch4_total_EDGAR_consistent'"
colnames(gloria_emissions) <- 
  gas_names <- colnames(gloria_emissions) |>
  stringr::str_extract("(?<=')[^_]+")

## Save emissions table (gloria_emissions.rds) ----
saveRDS(gloria_emissions, "data/gloria_emissions.rds")
