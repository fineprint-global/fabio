# ==============================================================================
# FABIOv2 Value-Added Extensions from FSDN Data
#
# Description:
#   Builds country × farm-type FSDN totals for the GLORIA / EXIOBASE-style
#   value-added decomposition (compensation of employees D.1, taxes on
#   production D.29, subsidies on production D.39, consumption of fixed
#   capital K.1, net mixed income B.3n).  Per-farm averages are multiplied
#   by the farms-represented weighting coefficient SYS02 to give country ×
#   farm-type × year totals, then disaggregated onto the FABIOv2
#   (area_code, item_code) grid using:
#     - the FSDN ↔ FABIO farm-type / commodity concordance to identify
#       which FABIO items each farm-type maps to;
#     - the BioSAMs ↔ FABIO area concordance to translate FSDN's 2-letter
#       country codes to FABIO area_code (FSDN and BioSAMs share EU MS
#       ISO2 keying, so this concordance is reused as-is);
#     - FABIOv2 producer total_value [USD] as the within-group weight,
#       same disaggregation pattern as 03a_value_added_FABIO_v2_BioSAMs.R.
#
#   FABIO items can be priced at TWO ISIC levels (ISIC-A primary
#   agriculture, ISIC-C food-and-beverage manufacturing).  We assign each
#   item to one level via the GLORIA sector concordances (the sole source
#   of truth used elsewhere in the project — see BioSAMs script F3 — for
#   "what processing grain does this FABIO item live at").  On overlap
#   (items mapped at BOTH ISIC levels in GLORIA — wheat, milk, etc.) the
#   ISIC-A row's total_value wins, per the design choice that FSDN data
#   is exclusively primary agriculture and the raw-product producer
#   price is the right weight.  Items mapped only at ISIC-C (oils,
#   sugars, processed feeds, …) take the ISIC-C row's total_value.
#
# Pipeline:
#   FRONT HALF (load-once)
#   1. Load FSDN data; replace the "-" missing-value token by 0; split the
#      "Member State" string "(XX) Country Name" into a 2-letter code and
#      a country name; drop EU aggregate rows ("EU-28", "EU27_2020")
#      because they would double-count the MS rows on disaggregation.
#   2. Multiply per-farm averages by SYS02 (farms represented) to get
#      country × farm-type × year totals.
#   3. Build the long VA table — one row per (year, region_code, farm_type,
#      va_account) — for the four accounts described in `va_spec`.
#   4. Load the FSDN ↔ FABIO concordance and reshape from wide (14 rows ×
#      ~125 FABIO codes) to long (farm_type, fabio_item_code).
#   5. Load the BioSAMs ↔ FABIO area concordance and translate FSDN
#      region_code → fabio_area_code (with optional FADN→ISO2 remap).
#   6. Load BOTH FABIOv2 total-value RDS files (ISIC-A and ISIC-C) and
#      derive the per-item ISIC assignment from GLORIA's sector
#      concordances; assemble `fv_master` with ISIC-A priority on overlap.
#
#   BACK HALF (single pass — fv_master already carries every needed year)
#   7. Explode each FSDN VA row across its mapped FABIO items via the
#      FSDN concordance; attach per-(area, item, year) total_value from
#      fv_master.
#   8. Within each (year, fabio_area_code, farm_type, va_account) group,
#      compute share = total_value / Σ total_value.  Equal-weight
#      fallback when the group total is 0 (BioSAMs default behaviour),
#      configurable via ZERO_TV_FALLBACK to "drop" if you want zero-
#      weight groups to lose their FSDN value silently.
#   9. Multiply va_value × share, aggregate by (year, fabio_area_code,
#      fabio_item_code, va_account), then right-join onto the full
#      fv_master grid so unmapped cells appear as zeros — same coverage
#      convention as the BioSAMs / GLORIA outputs.
#
#  10. Pivot wide on va_account; attach FABIO informational columns
#      (row_id, iso3c, comm_code, comm_group, unit, total_product_output,
#      price [USD/unit], price_source, price_source_constituents,
#      total_value [USD]) from fv_master — same context columns the
#      BioSAMs pipeline carries through.
#  11. Stage 8 — zero out VA on rows with no product output (TPO is NA
#      or 0).  Mirrors stage 8 of 03a_value_added_FABIO_v2_BioSAMs.R.
#      No `preserve_items` carve-out (see header note above).
#  12. Stage 8b — per-component IHS+MAD winsorization on each VA
#      component's EUR-per-physical-unit intensity (component / TPO),
#      pooled per fabio_item with per-item θ optimisation.  Mirrors
#      stage 8b of 03a_value_added_FABIO_v2_BioSAMs.R.
#  13. Compute `gross_value_added [EUR]` row sum on post-cap values
#      and write outputs.
#  14. Per-year × per-account allocation-gap diagnostic, contrasting the
#      FINAL output against the FSDN input with per-stage totals
#      (post-disagg, post-zero, post-winsor) so the operator can see
#      which stage each gap came from.  Mirrors the BioSAMs stage-10
#      reconciliation attribution.
#
# Inputs:
#   - input/FSDN_data.xlsx
#   - concordances/FSDN_FABIOv2_concordance_Coco.xlsx
#   - concordances/concordance_areas_biosam_fabio.csv
#   - concordances/concordance_items_gloria_fabio.csv  (combined GLORIA↔FABIO
#                                                       item concordance, split
#                                                       by its ISIC column; only
#                                                       the FABIO item-code
#                                                       column is used, to
#                                                       derive ISIC assignment)
#   - 01_total_value_FABIO_v2/output/FABIOv2_producer_total_values_isic_a.rds
#   - 01_total_value_FABIO_v2/output/FABIOv2_producer_total_values_isic_c.rds
#
# Outputs:
#   - output/FABIOv2_FSDN_value_added.rds  / .csv
#       Carries a trailing `fsdn_mapped` logical column: TRUE where the FABIO
#       item is mapped by the FSDN ↔ FABIO concordance, FALSE where the row is
#       only a stage-B6 right-join zero-fill (item never mapped).  The synthesis
#       script reads this to decide which rows may overwrite GLORIA.
#   - output/diagnostics/FABIOv2_FSDN_phys_intensity_winsor.csv
#   - output/diagnostics/FABIOv2_FSDN_allocation_gaps.csv
#
# Units:
#   - All five accounts (D.1, D.29, D.39, K.1, B.3n)  : EUR
#   - FABIO total_value (weight only — dimensionless after share)  : USD
#
# Account map (FSDN → SNA / ESA 2010):
#   compensation_of_employees    [EUR, D.1 ]  = SE370 Wages paid
#   taxes_on_production          [EUR, D.29]  = SE390 Taxes
#   subsidies_on_production      [EUR, D.39]  = SE605 Total subsidies excl. investments
#   consumption_of_fixed_capital [EUR, K.1 ]  = SE360 Depreciation
#   net_mixed_income             [EUR, B.3n]  = SE415 FNVA − SE370 Wages paid
#
# Notes on B.3n:
#   FSDN's SE415 (Farm Net Value Added) is at factor cost — it has the
#   net subsidies/taxes balance SE600 added in via SE410.  Per SNA 2008
#   §7.9, mixed income for unincorporated household enterprises is the
#   residual of the generation of income account at factor cost AFTER
#   compensation of employees but BEFORE any property income deduction
#   (rent, interest).  So B.3n = SE415 − SE370.  Note this is NOT
#   SE420 (Farm Net Income), which subtracts rent and interest paid as
#   well — SE420 is closer to entrepreneurial income B.4n / a partial
#   B.5n.  For corporate-form farms (small minority in FSDN) the same
#   residual is B.2n net operating surplus; we report it under
#   net_mixed_income because FSDN's legal-form information is not
#   loaded here.  Split downstream via the FSDN A_TY_90 indicator if
#   you need the B.2n / B.3n distinction.
# ==============================================================================

library(data.table)
library(readxl)
source("R/00_value_added_config.R")


# ── Configuration ────────────────────────────────────────────────────────────

# Project layout matches the national-SUT builder (script 14_3) and GLORIA (script 14_1)
# pipelines: shared concordances/ folder, shared FABIO total_values
# output folder, separate per-script input/ and output/ folders.

INPUT_DIR        <- VA_VALUE_ADDED_INPUT_DIR
CONCORDANCE_DIR  <- VA_CONCORDANCE_DIR
OUTPUT_DIR       <- VA_VALUE_ADDED_OUTPUT_DIR
DIAG_DIR         <- VA_VALUE_ADDED_DIAG_DIR

# Manually staged: export the FSDN standard results from the public-database
# dashboard with the "Year", "Member State" and "14 Types of Farming" dimensions
# (see load_fsdn() for the full note).  There is no stable URL to auto-fetch it.
FSDN_DATA_PATH   <- file.path(INPUT_DIR,       "FSDN_data.xlsx")
FSDN_CONC_PATH   <- file.path(CONCORDANCE_DIR, "FSDN_FABIOv2_concordance_Coco.xlsx")

# Area concordance shared with BioSAMs (both pipelines key on EU MS ISO2).
AREA_CONC_PATH   <- file.path(CONCORDANCE_DIR, "concordance_areas_biosam_fabio.csv")

# GLORIA sector concordance — used ONLY to derive each FABIO item's ISIC
# assignment.  Same role as in script 14_3 (national SUTs); we don't use the
# GLORIA side of the mapping, only the FABIO_item_code column.  Both ISIC
# levels now live in ONE combined file, distinguished by its `ISIC` column
# ("A"/"C"); load_gloria_fabio_items() selects the level.  (Replaces the former
# per-level files concordance_sectors_gloria_isic_a_fabio.csv and ..._c_....csv.)
GLORIA_SECTOR_CONC_PATH <- VA_CONC_GLORIA_ITEMS

FABIO_TV_PATH_A  <- VA_FABIO_TV_ISIC_A_RDS
FABIO_TV_PATH_C  <- VA_FABIO_TV_ISIC_C_RDS

# How a missing value is encoded in FSDN_data.xlsx.  Replaced by 0 at
# load time
FSDN_NA_TOKEN    <- "-"

# Behaviour when a (year, fabio_area_code, farm_type, va_account) group
# has total_value = 0 across its mapped FABIO items (no producer-price
# signal at all for the year — including the all-NA case, which sums to
# 0 under na.rm = TRUE):
#   "equal_weight" : split equally across mapped items (1 / group_n),
#                    preserving the FSDN total at the cost of placing
#                    value in cells with no FABIO total_value.  This
#                    matches the BioSAMs script's behaviour.
#   "drop"         : share = 0; the FSDN value is silently lost.  Cleaner
#                    grids if you'd rather see explicit gaps in the
#                    allocation diagnostic than receive equal-weighted
#                    placeholders.
ZERO_TV_FALLBACK <- "equal_weight"

# FADN-vs-ISO2 country-code differences.  FSDN's "(XX) Country Name"
# prefix uses Eurostat-style codes for two countries that are ISO2'd
# differently in the BioSAMs area concordance:
#   FADN  ISO2  Country
#   ----  ----  -------
#   EL    GR    Greece
#   UK    GB    United Kingdom
# Add to this list if more code mismatches surface.
FSDN_AREA_CODE_REMAP <- c(
  "EL" = "GR",
  "UK" = "GB"
)

# Output base name (suffixed by .csv / .rds).
OUT_BASENAME     <- va_va_output_basename("FSDN")   # FABIOv2_FSDN_value_added (shared builder)
DIAG_BASENAME    <- "FABIOv2_FSDN_allocation_gaps"

# Stage 8b winsorization: cap at z = WINSOR_MAD_K MADs in IHS-transformed
# space on each VA component's EUR-per-physical-unit intensity, pooled
# per fabio_item with per-item θ optimisation.  Mirrors stage 8b of
# 03a_value_added_FABIO_v2_BioSAMs.R — same IHS+MAD machinery, same θ
# grid, same WINSOR_MAD_K default (3.5).  Tune here if the longer FSDN
# panel warrants a stricter cap than BioSAMs' 2010+2015 pool needed.
WINSOR_MAD_K              <- 3.5
DIAG_WINSOR_BASENAME  <- "FABIOv2_FSDN_phys_intensity_winsor"

va_ensure_dir(OUTPUT_DIR)
va_ensure_dir(DIAG_DIR)


# ── FSDN columns of interest ─────────────────────────────────────────────────
#
# Each per-farm average column listed below is multiplied by SYS02
# (farms represented) at load time to give country × farm-type × year
# totals.  Adding an account is a 2-line change: append to FSDN_COLS,
# add an entry to va_spec, and emit it from build_va_extensions().

FSDN_COLS <- list(
  wages_paid     = "(SE370) Wages paid (€/farm)",
  taxes          = "(SE390) Taxes (€/farm)",
  subs_excl_inv  = "(SE605) Total subsidies - excluding on investments (€/farm)",
  depreciation   = "(SE360) Depreciation (€/farm)",
  fnva           = "(SE415) Farm Net Value Added (€/farm)"
)
FSDN_FARMS_COL    <- "(SYS02) Farms represented (nb)"
FSDN_REGION_COL   <- "Member State"
FSDN_FARMTYPE_COL <- "14 Types of Farming"
FSDN_YEAR_COL     <- "Year"

# Output columns and units, in output order.  build_va_extensions() must
# emit one row per account here for every (year, region_code, farm_type).
# `sna_code` carries the ESA 2010 / SNA 2008 transaction code for each
# account and is purely informational (surfaced in the diagnostic CSV
# and summary print).
va_spec <- data.table(
  account = c(
    "compensation_of_employees",
    "taxes_on_production",
    "subsidies_on_production",
    "consumption_of_fixed_capital",
    "net_mixed_income"
  ),
  unit     = c("EUR",  "EUR",  "EUR",  "EUR", "EUR"),
  sna_code = c("D.1",  "D.29", "D.39", "K.1", "B.3n")
)
va_spec[, va_col := sprintf("%s [%s]", account, unit)]


# ── Helper: derive FABIO item ISIC level from GLORIA sector concordance ──────
#

load_gloria_fabio_items <- function(conc, isic_level) {
  required <- c("FABIO_item_code", "GLORIA_sector_code", "ISIC")
  missing  <- setdiff(required, names(conc))
  if (length(missing) > 0L)
    stop("GLORIA sector concordance is missing column(s): ",
         paste(missing, collapse = ", "),
         ".  Found: ", paste(names(conc), collapse = ", "))
  
  items <- sort(unique(as.integer(
    conc[toupper(trimws(as.character(ISIC))) == isic_level &
           !is.na(FABIO_item_code) & !is.na(GLORIA_sector_code),
         FABIO_item_code]
  )))
  if (length(items) == 0L)
    stop("No GLORIA→FABIO rows found for ISIC level '", isic_level,
         "' in ", GLORIA_SECTOR_CONC_PATH, ".  Check the `ISIC` column values.")
  items
}


# ── FSDN loader ──────────────────────────────────────────────────────────────
#
# Loads FSDN_data.xlsx, replaces the "-" missing-value token by 0, splits
# the "(XX) Country Name" Member State column into separate code and
# name columns, drops EU aggregate rows, multiplies per-farm averages
# by SYS02 to give country × farm-type × year totals, and applies the
# FSDN_AREA_CODE_REMAP (FADN→ISO2 fixes).

# Shared manual-download instruction, reused by the missing-file stop in
# load_fsdn() and the stale-vintage check in check_fsdn_year_coverage().
.fsdn_download_hint <- function(path)
  paste0(
    "Download the FSDN standard results manually from the public-database dashboard:\n",
    "  https://agridata.ec.europa.eu/extensions/FSDNPublicDatabase/FSDNPublicDatabase.html\n",
    "Export them (table export / bulk download) INCLUDING the dimensions\n",
    "  \"Year\", \"Member State\", and \"14 Types of Farming\"\n",
    "then save the .xlsx to:\n  ", path, "\n",
    "(The dashboard's download link is a one-off temporary URL, so it cannot be\n",
    "fetched automatically.)")


# ── Couple the manual-download notice to FABIO year coverage ─────────────────
#
check_fsdn_year_coverage <- function(fsdn,
                                     fabio_years = if (exists("VA_KEEP_YEARS")) VA_KEEP_YEARS else NULL,
                                     path = FSDN_DATA_PATH) {
  if (is.null(fabio_years) || !length(fabio_years)) {
    message("  (VA_KEEP_YEARS not in scope — skipping FSDN year-coverage check.)")
    return(invisible(NULL))
  }
  fsdn_years <- sort(unique(as.integer(fsdn$year)))
  uncovered  <- sort(setdiff(as.integer(fabio_years), fsdn_years))
  if (!length(uncovered)) {
    message(sprintf("  FSDN covers all %d FABIO output year(s) (%s).",
                    length(fabio_years), paste(range(fabio_years), collapse = "\u2013")))
    return(invisible(NULL))
  }
  msg <- sprintf(paste0(
    "FSDN does not cover %d FABIO output year(s): %s\n",
    "  (FSDN vintage on disk spans %s; FABIO needs %s.)\n",
    "These year(s) will carry NO FSDN value added unless an updated FSDN export ",
    "is staged.\nIf a newer FSDN vintage now exists, refresh it:\n\n%s\n",
    "(If this is just the normal FSDN publication lag, no action is needed.)"),
    length(uncovered), paste(uncovered, collapse = ", "),
    paste(range(fsdn_years),  collapse = "\u2013"),
    paste(range(fabio_years), collapse = "\u2013"),
    .fsdn_download_hint(path))
  if (isTRUE(as.logical(Sys.getenv("FSDN_STRICT_YEARS", "FALSE")))) stop(msg)
  warning(msg, call. = FALSE)
  invisible(uncovered)
}


load_fsdn <- function(path) {
  # The FSDN standard results are not available from a stable URL or API — the
  # public database (https://agridata.ec.europa.eu/extensions/FSDNPublicDatabase/
  # FSDNPublicDatabase.html) only serves them through an interactive dashboard,
  # whose "export / bulk download" produces a one-off temporary file.  So updated
  # vintages must be downloaded MANUALLY and saved to `path`.  The export MUST
  # include the dimensions "Year", "Member State", and "14 Types of Farming"
  # (these become the key columns the loader splits and joins on below); without
  # them the pipeline cannot build the country x farm-type x year grid.
  if (!file.exists(path))
    stop("FSDN data not found at:\n  ", path, "\n\n", .fsdn_download_hint(path))
  message("Loading FSDN data from ", path, " ...")
  fsdn <- as.data.table(read_excel(path, sheet = 1L))
  
  needed_keys <- c(FSDN_YEAR_COL, FSDN_REGION_COL, FSDN_FARMTYPE_COL,
                   FSDN_FARMS_COL)
  needed_vals <- unlist(FSDN_COLS, use.names = FALSE)
  needed      <- c(needed_keys, needed_vals)
  
  missing_cols <- setdiff(needed, names(fsdn))
  if (length(missing_cols) > 0L)
    stop("FSDN data is missing required column(s):\n  ",
         paste(missing_cols, collapse = "\n  "))
  
  fsdn <- fsdn[, c(needed), with = FALSE]
  
  # FSDN encodes missing as "-" in cells that are otherwise numeric, which
  # forces the column to character.  Convert "-" → NA → 0, then coerce
  # numeric. suppressWarnings absorbs any other stray strings (an
  # unparseable cell becomes NA → 0, same fallback as the Python script).
  numeric_cols <- c(FSDN_FARMS_COL, needed_vals)
  for (col in numeric_cols) {
    raw <- trimws(as.character(fsdn[[col]]))
    raw[raw == FSDN_NA_TOKEN] <- NA_character_
    set(fsdn, j = col, value = suppressWarnings(as.numeric(raw)))
  }
  for (col in numeric_cols) {
    set(fsdn, which(is.na(fsdn[[col]])), col, 0)
  }
  
  # Split "(XX) Country Name" into code and name.  Aggregate rows
  # ("EU-28", "EU27_2020") have no "(XX)" prefix — match returns NA on
  # those, so we drop them in the next step.
  ms_raw <- as.character(fsdn[[FSDN_REGION_COL]])
  m  <- regmatches(ms_raw, regexec("^\\(([^)]+)\\)\\s*(.*)$", ms_raw))
  fsdn[, region_code := vapply(m, function(x) if (length(x) == 3L) x[2L] else NA_character_, "")]
  fsdn[, region_name := vapply(m, function(x) if (length(x) == 3L) trimws(x[3L]) else NA_character_, "")]
  
  # Drop aggregate-region rows.  They roughly equal sum(MS rows) over
  # the same (year, farm_type) and would double-count totals if
  # disaggregated alongside the MS rows.
  agg_rows <- fsdn[is.na(region_code)]
  if (nrow(agg_rows) > 0L) {
    agg_codes <- sort(unique(as.character(agg_rows[[FSDN_REGION_COL]])))
    message(sprintf("  Dropping %d aggregate-region row(s) covering: %s",
                    nrow(agg_rows), paste(agg_codes, collapse = ", ")))
    fsdn <- fsdn[!is.na(region_code)]
  }
  
  # Apply FADN→ISO2 remap so codes line up with the BioSAMs area concordance.
  if (length(FSDN_AREA_CODE_REMAP) > 0L) {
    src <- names(FSDN_AREA_CODE_REMAP)
    dst <- unname(FSDN_AREA_CODE_REMAP)
    for (i in seq_along(src)) {
      fsdn[region_code == src[i], region_code := dst[i]]
    }
  }
  
  setnames(fsdn,
           old = c(FSDN_YEAR_COL, FSDN_FARMTYPE_COL),
           new = c("year",        "farm_type"))
  fsdn[, year := as.integer(year)]
  fsdn[, farm_type := as.character(farm_type)]
  
  # Multiply per-farm averages by farms-represented to give totals.
  # Tag names match FSDN_COLS keys so build_va_extensions can refer to
  # them directly without column-name juggling.
  for (tag in names(FSDN_COLS)) {
    src <- FSDN_COLS[[tag]]
    fsdn[, (tag) := as.numeric(get(src)) * as.numeric(get(FSDN_FARMS_COL))]
  }
  
  # Drop the original-named SE columns + SYS02 + the original Member State
  # string + region_name (the country name; we key on code downstream and
  # restore the FABIO name from fv_master at output time).
  drop_cols <- c(FSDN_REGION_COL, FSDN_FARMS_COL, needed_vals, "region_name")
  fsdn[, (drop_cols) := NULL]
  
  setcolorder(fsdn, c("year", "region_code", "farm_type",
                      names(FSDN_COLS)))
  
  message(sprintf(
    "  %d row(s) kept across %d year(s), %d region(s), %d farm-type(s).",
    nrow(fsdn), uniqueN(fsdn$year),
    uniqueN(fsdn$region_code), uniqueN(fsdn$farm_type)
  ))
  fsdn
}


# ── Build long VA table ──────────────────────────────────────────────────────
#
# One rbindlist row per VA account, hand-coded.  Adding an account is a
# 3-line change here plus matching entries in FSDN_COLS / va_spec at
# the top.  Mixed income is computed inline as SE415 − SE370 (FNVA is
# at factor cost, so subtracting wages paid recovers B.3n at factor
# cost — see SNA notes in the header).

build_va_extensions <- function(fsdn) {
  out <- rbindlist(list(
    
    # The five accounts below satisfy the generation-of-income identity
    #   NVA_factor_cost = D.1 + B.3n
    # at the country × farm-type × year level, with K.1 (depreciation)
    # bridging gross to net and (D.29 − D.39) bridging factor cost to
    # basic prices:
    #   NVA_basic = D.1 + D.29 − D.39 + B.3n
    #   GVA_basic = NVA_basic + K.1
    
    # D.1 — Compensation of employees.  SE370 already includes social
    # security charges and insurance for paid workers, matching the
    # SNA D.1 wages-and-salaries-plus-employers'-contributions concept.
    # Excludes implicit wages of unpaid family labour, which is the
    # correct treatment (those returns are part of B.3n mixed income).
    fsdn[, .(year, region_code, farm_type,
             va_account = "compensation_of_employees",
             va_value   = wages_paid,
             unit       = "EUR")],
    
    # D.29 — Other taxes on production.  SE390 is "farm taxes and other
    # dues … and taxes and charges on land and buildings," explicitly
    # excluding VAT (D.21 — tax on products) and personal income tax of
    # the holder.  Clean fit for D.29.
    fsdn[, .(year, region_code, farm_type,
             va_account = "taxes_on_production",
             va_value   = taxes,
             unit       = "EUR")],
    
    # D.39 — Other subsidies on production.  Per the Eurostat EAA
    # convention, essentially all current CAP support is D.39 rather
    # than D.31 (subsidies on products): decoupled payments, rural
    # development, environmental and LFA payments, subsidies on
    # intermediate consumption and on external factors all roll up
    # under D.39.  SE605 is therefore the broadest economically
    # precise aggregate.  SE406 (subsidies on investments) is a
    # capital transfer D.92, not D.39, and is intentionally not
    # included.
    # D.39 — stored NEGATIVE per GLORIA/EXIOBASE sign-adjusted convention
    fsdn[, .(year, region_code, farm_type,
             va_account = "subsidies_on_production",
             va_value   = -subs_excl_inv,              # was: subs_excl_inv
             unit       = "EUR")],
    
    # K.1 — Consumption of fixed capital (depreciation).  SE360 covers
    # plantations of permanent crops, farm buildings and fixed
    # equipment, land improvements, machinery, equipment, and forest
    # plantations.  Note FADN switched from replacement value to
    # historical/book value in 2014; if you build a long time series
    # this introduces a small structural break around the 2013/2014
    # boundary.
    fsdn[, .(year, region_code, farm_type,
             va_account = "consumption_of_fixed_capital",
             va_value   = depreciation,
             unit       = "EUR")],
    
    # B.3n — Net mixed income.  Residual of the generation of income
    # account at factor cost AFTER compensation of employees but BEFORE
    # any property income deduction (rent, interest).  In FADN/FSDN
    # terms FNVA (SE415) is at factor cost, so B.3n = SE415 − SE370.
    # Note this is NOT SE420 (Farm Net Income), which goes one step
    # further and subtracts rent and interest paid — that is B.5n /
    # entrepreneurial income territory, downstream of the VA stage.
    fsdn[, .(year, region_code, farm_type,
             va_account = "net_mixed_income",
             va_value   = fnva - wages_paid,
             unit       = "EUR")]
  ))
  
  # Validate against va_spec — catches typos before they propagate.
  missing <- setdiff(va_spec$account, unique(out$va_account))
  if (length(missing) > 0L)
    stop("build_va_extensions() did not emit the following account(s) ",
         "declared in va_spec: ", paste(missing, collapse = ", "))
  extra <- setdiff(unique(out$va_account), va_spec$account)
  if (length(extra) > 0L)
    stop("build_va_extensions() emitted account(s) not declared in va_spec: ",
         paste(extra, collapse = ", "))
  
  out[]
}


# ── FSDN ↔ FABIO concordance loader ──────────────────────────────────────────
#
# Concordance file layout (FSDN_FABIOv2_concordance_Coco.xlsx):
#   - Row 1: blank | "FABIO commodities" | 2805 | 2511 | … | 2960 | "SUM"
#            ← FABIO item codes as headers when read with col_names=TRUE.
#   - Row 2: blank | "Rice (Milled Equivalent)" | "Wheat …" | … | blank
#            ← FABIO item NAMES (informational; we drop this row).
#   - Row 3: "Farmtypes EUROPEAN COMISSIONS" | … all NA | "SUM"-NA
#            ← section label, dropped.
#   - Rows 4–17:    farm-type name | NA | 1 / NA … | row-sum
#                   ← the 14 farm-type rows.
#
# We read with col_names = FALSE so column names are positional ("...1",
# "...2", …) and we don't have to fight readxl's name-mangling on
# numeric headers, then assemble the long-form (farm_type, fabio_item_code)
# table directly from the cell positions.

load_fsdn_concordance <- function(path) {
  message("Loading FSDN ↔ FABIO concordance from ", path, " ...")
  
  raw <- as.data.table(read_excel(path, sheet = 1L, col_names = FALSE,
                                  .name_repair = "minimal"))
  if (nrow(raw) < 4L)
    stop("Concordance file ", path, " has fewer than 4 rows; ",
         "expected at least 1 header + 1 names + 1 section label + 1 data row.")
  
  n_col <- ncol(raw)
  
  # Row 1 of `raw` is the file's first row — the FABIO item codes.  In
  # the file the first two columns are blank (or label-only) and the
  # last column is "SUM"; the actual FABIO codes sit at columns 3 to
  # n_col - 1.
  code_row <- as.character(unlist(raw[1L, ]))
  code_idx <- seq.int(3L, n_col - 1L)
  fabio_codes <- suppressWarnings(as.integer(code_row[code_idx]))
  
  bad_codes <- which(is.na(fabio_codes))
  if (length(bad_codes) > 0L)
    stop("Could not parse FABIO item codes at concordance columns ",
         paste(code_idx[bad_codes], collapse = ", "),
         " (header values: '",
         paste(code_row[code_idx[bad_codes]], collapse = "', '"), "').")
  
  # Rows 4..nrow(raw) are the 14 farm-type data rows.  The farm-type
  # name lives in column 1 and the 1/NA cells in columns 3..n_col-1.
  data_rows <- 4L:nrow(raw)
  farm_types <- as.character(unlist(raw[data_rows, 1L, with = FALSE]))
  
  # Pull the value matrix as numeric — non-1 entries (typically NA) are
  # left as NA and later filtered out, so the only thing we need is for
  # cells holding "1" to resolve to 1.
  vals <- raw[data_rows, code_idx, with = FALSE]
  for (j in seq_along(vals)) {
    set(vals, j = j, value = suppressWarnings(as.numeric(vals[[j]])))
  }
  
  # Build the long table by indexing positions directly — bypasses
  # melt's factor-of-column-names round-trip, which silently collapses
  # to a single level when readxl/.name_repair leave columns unnamed.
  flag_mat <- as.matrix(vals)
  storage.mode(flag_mat) <- "numeric"
  
  idx  <- which(!is.na(flag_mat) & flag_mat != 0, arr.ind = TRUE)
  long <- data.table(
    farm_type       = farm_types[idx[, "row"]],
    fabio_item_code = fabio_codes[idx[, "col"]]
  )
  long <- unique(long[!is.na(farm_type) & farm_type != "" &
                        !is.na(fabio_item_code)])
  
  message(sprintf(
    "  %d (farm_type, fabio_item) link(s) loaded across %d farm-type(s) and %d FABIO item(s).",
    nrow(long), uniqueN(long$farm_type), uniqueN(long$fabio_item_code)
  ))
  long
}


# ============================================================================
# FRONT HALF — runs once
# ============================================================================

message("\n══════════════════════════════════════════════════════════════════════")
message("  Front half — load FSDN, concordances, FABIO total_values")
message("══════════════════════════════════════════════════════════════════════")

# F1. FSDN data and long VA table.
fsdn      <- load_fsdn(FSDN_DATA_PATH)
check_fsdn_year_coverage(fsdn)          # warn (or stop, if FSDN_STRICT_YEARS) when the staged FSDN vintage misses FABIO output years
va_long   <- build_va_extensions(fsdn)


# F2. FSDN ↔ FABIO concordance.
fsdn_conc <- load_fsdn_concordance(FSDN_CONC_PATH)

# Cross-check farm-type names between FSDN data and the concordance —
# any mismatch here means the disaggregation will silently drop the
# unmatched farm-type's VA, so flag it loudly.
fsdn_fts <- sort(unique(va_long$farm_type))
conc_fts <- sort(unique(fsdn_conc$farm_type))
diff_a   <- setdiff(fsdn_fts, conc_fts)
diff_b   <- setdiff(conc_fts, fsdn_fts)
if (length(diff_a) > 0L)
  warning("Farm-type(s) in FSDN data but NOT in concordance — their VA ",
          "will be dropped at disaggregation: ",
          paste(diff_a, collapse = " | "))
if (length(diff_b) > 0L)
  warning("Farm-type(s) in concordance but NOT in FSDN data: ",
          paste(diff_b, collapse = " | "))


# F3. Area concordance and FSDN region_code → fabio_area_code attach.
message("Loading area concordance from ", AREA_CONC_PATH, " ...")
area_conc <- load_area_conc(AREA_CONC_PATH, code_col = "BioSAM_area_code", fabio_col = "FABIO_area_code")
message(sprintf("  %d (region_code → fabio_area_code) entries.", nrow(area_conc)))

va_regions     <- sort(unique(va_long$region_code))
unmapped_regs  <- setdiff(va_regions, area_conc$region_code)
if (length(unmapped_regs) > 0L)
  warning("FSDN region_code(s) not found in area concordance — their VA ",
          "will be dropped at disaggregation: ",
          paste(unmapped_regs, collapse = ", "),
          ".  Extend FSDN_AREA_CODE_REMAP or the area concordance to fix.")


# F4. FABIOv2 total values, both ISIC levels.
message("Loading FABIOv2 total values (ISIC-A) ...")
fv_a <- prepare_fv(FABIO_TV_PATH_A, required_cols = c("area_code", "area", "item_code", "item", "year"), rename_to_stable = TRUE)
message(sprintf("  %d row(s); %d year(s); %d FABIO area(s); %d FABIO item(s).",
                nrow(fv_a), uniqueN(fv_a$year),
                uniqueN(fv_a$fabio_area_code), uniqueN(fv_a$fabio_item_code)))

message("Loading FABIOv2 total values (ISIC-C) ...")
fv_c <- prepare_fv(FABIO_TV_PATH_C, required_cols = c("area_code", "area", "item_code", "item", "year"), rename_to_stable = TRUE)
message(sprintf("  %d row(s); %d year(s); %d FABIO area(s); %d FABIO item(s).",
                nrow(fv_c), uniqueN(fv_c$year),
                uniqueN(fv_c$fabio_area_code), uniqueN(fv_c$fabio_item_code)))


# F5. Per-FABIO-item ISIC assignment via GLORIA sector concordances.
#
# GLORIA's sector concordances are the source of truth used elsewhere
# in this project (see BioSAMs script F3) for "what processing grain
# does this FABIO item live at": ISIC-A (primary agriculture) or
# ISIC-C (food/beverage manufacturing) or both.  On overlap (items
# mapped at BOTH ISIC levels in GLORIA — wheat, milk, etc.) the user
# directive is to weight by the ISIC-A producer total_value, on the
# grounds that FSDN data is exclusively primary agriculture and the
# raw-product price is the right weight.

message("Deriving per-FABIO-item ISIC assignment from GLORIA concordance ...")
if (!file.exists(GLORIA_SECTOR_CONC_PATH))
  stop("GLORIA sector concordance not found at ", GLORIA_SECTOR_CONC_PATH,
       ".  Cannot derive ISIC level assignment without it.")
gloria_conc    <- fread(GLORIA_SECTOR_CONC_PATH)
gloria_items_a <- load_gloria_fabio_items(gloria_conc, "A")
gloria_items_c <- load_gloria_fabio_items(gloria_conc, "C")

both_items   <- sort(intersect(gloria_items_a, gloria_items_c))
only_a_items <- sort(setdiff(gloria_items_a,    gloria_items_c))
only_c_items <- sort(setdiff(gloria_items_c,    gloria_items_a))

message(sprintf(
  "  ISIC-A only: %d item(s)   ISIC-C only: %d item(s)   Both ISIC levels: %d item(s)",
  length(only_a_items), length(only_c_items), length(both_items)
))


# F6. Assemble fv_master with ISIC-A priority on overlap.
#
#   - Rows for items in `gloria_items_a` (covers A-only and overlap):
#       taken from fv_a, tagged source_isic = "A".
#   - Rows for items in `gloria_items_c \ gloria_items_a` (C-only):
#       taken from fv_c, tagged source_isic = "C".
#
# This is the user's directive expressed as a partition: every FABIO
# item that has an ISIC level in GLORIA gets weighted by the
# total_value from exactly one ISIC level, with A winning ties.

message("Assembling fv_master (ISIC-A priority on overlap) ...")

fv_master_a <- fv_a[fabio_item_code %in% gloria_items_a]
fv_master_a[, source_isic := "A"]

fv_master_c <- fv_c[fabio_item_code %in% only_c_items]
fv_master_c[, source_isic := "C"]

fv_master <- rbindlist(list(fv_master_a, fv_master_c), use.names = TRUE)
fv_master[, year := as.integer(year)]

# Preserve unit attributes for downstream use, picking the ISIC-A label
# where present (it is, as long as gloria_items_a is non-empty).
attr(fv_master, "value_col_label")  <- attr(fv_a, "value_col_label")
attr(fv_master, "output_col_label") <- attr(fv_a, "output_col_label")

n_master_items <- uniqueN(fv_master$fabio_item_code)
message(sprintf(
  "  fv_master: %d row(s); %d year(s); %d FABIO area(s); %d FABIO item(s) [%d from A, %d from C].",
  nrow(fv_master), uniqueN(fv_master$year),
  uniqueN(fv_master$fabio_area_code), n_master_items,
  uniqueN(fv_master_a$fabio_item_code),
  uniqueN(fv_master_c$fabio_item_code)
))


# F7. Coverage check: does fv_master cover every FABIO item the FSDN
# concordance maps to?  Items missing from BOTH GLORIA concordances
# can't be assigned to an ISIC level, so any FSDN VA targeted at them
# is silently lost downstream.  Worth surfacing.

fsdn_target_items   <- sort(unique(fsdn_conc$fabio_item_code))
unassigned_items    <- setdiff(fsdn_target_items, c(gloria_items_a, gloria_items_c))
no_total_value      <- setdiff(fsdn_target_items, unique(fv_master$fabio_item_code))

if (length(unassigned_items) > 0L)
  warning(sprintf(
    "FSDN concordance targets %d FABIO item(s) absent from BOTH GLORIA concordances ",
    length(unassigned_items)),
    "(no ISIC assignment, no fv_master row, VA will be dropped at disagg): ",
    paste(unassigned_items, collapse = ", "))

# A-only items missing from fv_a, or C-only items missing from fv_c,
# are subtler: the ISIC assignment is fine but the FABIO total-value
# pipeline didn't compute a row for them.  Surface separately.
isic_assigned_no_tv <- setdiff(no_total_value, unassigned_items)
if (length(isic_assigned_no_tv) > 0L)
  warning(sprintf(
    "FSDN concordance targets %d FABIO item(s) with an ISIC assignment ",
    length(isic_assigned_no_tv)),
    "but no fv_master row (FABIO total_value pipeline didn't emit them at the priority ISIC level): ",
    paste(isic_assigned_no_tv, collapse = ", "))


# ============================================================================
# BACK HALF — single pass (fv_master carries every needed year)
# ============================================================================

message("\n══════════════════════════════════════════════════════════════════════")
message("  Back half — disaggregate FSDN onto FABIO grid (total_value-weighted)")
message("══════════════════════════════════════════════════════════════════════")


# B1. Map FSDN region_code → fabio_area_code.
#
# FSDN VA rows that don't match in the area concordance are dropped
# here (already warned about above at F3); the inner-join semantics
# of `nomatch = NULL` give us exactly that.

message("Mapping FSDN region_code → fabio_area_code ...")
va_long_fa <- area_conc[va_long, on = "region_code",
                        nomatch = NULL,
                        allow.cartesian = TRUE]
n_dropped <- nrow(va_long) - nrow(va_long_fa)
if (n_dropped > 0L)
  message(sprintf("  %d FSDN VA row(s) dropped (region_code without fabio_area_code).",
                  n_dropped))


# B2. Year intersection between FSDN and fv_master.
fsdn_years   <- sort(unique(va_long_fa$year))
fv_years     <- sort(unique(fv_master$year))
common_years <- intersect(fsdn_years, fv_years)

if (length(common_years) == 0L)
  stop("No years are present in BOTH FSDN data and FABIO total_values. ",
       "FSDN: ", paste(range(fsdn_years), collapse = "–"),
       "; FABIO: ", paste(range(fv_years), collapse = "–"))

if (length(setdiff(fsdn_years, fv_years)) > 0L)
  message(sprintf(
    "  %d FSDN year(s) absent from FABIO total_values, dropped: %s",
    length(setdiff(fsdn_years, fv_years)),
    paste(setdiff(fsdn_years, fv_years), collapse = ", ")
  ))

va_long_fa <- va_long_fa[year %in% common_years]
message(sprintf("  Processing %d year(s): %s",
                length(common_years), paste(range(common_years), collapse = "–")))


# B3. Explode FSDN VA across mapped FABIO items, attach total_value.
#
# Cartesian explode via the FSDN concordance: each (year, fabio_area_code,
# farm_type, va_account) row becomes K rows, one per mapped fabio_item_code.

message("Exploding FSDN VA across mapped FABIO items and attaching total_value ...")
exploded <- merge(va_long_fa, fsdn_conc, by = "farm_type",
                  allow.cartesian = TRUE)

# Attach per-(area, item, year) total_value from fv_master.  Items
# absent from fv_master (warned about above at F7) get NA total_value
# and are handled by the share computation below — they fall into the
# group_total = 0 branch with their cohort.
exploded <- merge(
  exploded,
  fv_master[, .(fabio_area_code, fabio_item_code, year, total_value, source_isic)],
  by    = c("fabio_area_code", "fabio_item_code", "year"),
  all.x = TRUE
)


# B4. Within-group share = total_value / Σ total_value, with fallback.
#
# Group key: (year, fabio_area_code, farm_type, va_account) — a single
# FSDN VA row at this stage.  share sums to 1 within each group when
# any positive total_value exists; falls back to equal-weight (1/N) or
# drop (share = 0) when the group total is 0.  Same pattern as the
# BioSAMs disaggregation, but with the additional FSDN_AREA_CODE
# coordinate that BioSAMs gets implicitly via its area concordance.

message(sprintf("Computing within-group shares (zero-total fallback: %s) ...",
                ZERO_TV_FALLBACK))
if (!ZERO_TV_FALLBACK %in% c("equal_weight", "drop"))
  stop("ZERO_TV_FALLBACK must be 'equal_weight' or 'drop'; got '",
       ZERO_TV_FALLBACK, "'.")

# group_total: sum(., na.rm = TRUE) returns 0 for an all-NA group, so
# == 0 covers both no-signal cases (all NA, all 0, or any mix).
# group_n: non-NA-aware row count — we want the equal-weight fallback
# to split across ALL mapped items in the group, not just those with
# a non-NA total_value.
exploded[, c("group_total", "group_n") := .(
  sum(total_value, na.rm = TRUE),
  .N
), by = .(year, fabio_area_code, farm_type, va_account)]

use_equal_weight <- (ZERO_TV_FALLBACK == "equal_weight")
exploded[, share := fifelse(
  group_total > 0,
  total_value / group_total,
  if (use_equal_weight) 1 / group_n else 0
)]

# Items with NA total_value inside an otherwise-positive group implicitly
# get share = NA (fifelse propagates), which means their value_split is
# NA and the downstream sum(., na.rm = TRUE) drops them — the finite
# cells' shares still sum to 1, so the FSDN total is preserved across
# them.  Same implicit behaviour as the BioSAMs script.

exploded[, va_value_split := va_value * share]


# B5. Aggregate to (year, fabio_area_code, fabio_item_code, va_account).

disagg <- exploded[, .(va_value = sum(va_value_split, na.rm = TRUE)),
                   by = .(year, fabio_area_code, fabio_item_code, va_account)]


# B6. Right-join onto the full fv_master grid × va_account so unmapped
# (area, item, year, account) cells appear as zeros — same coverage
# convention as the BioSAMs / GLORIA outputs.

message("Right-joining onto full fv_master grid ...")

# ── NEW: restrict to FSDN-covered areas only ─────────────────────────────────
fsdn_fabio_areas <- unique(va_long_fa$fabio_area_code)   # <── add this
# ─────────────────────────────────────────────────────────────────────────────

full_grid <- CJ(year            = common_years,
                fabio_area_code = fsdn_fabio_areas,      # <── was: unique(fv_master$fabio_area_code)
                fabio_item_code = unique(fv_master$fabio_item_code),
                va_account      = va_spec$account,
                unique = TRUE)

# Restrict to (area, item, year) cells that actually exist in fv_master
# — there's no point carrying a zero VA at a (area, item, year) coordinate
# the FABIO total-value pipeline didn't emit a row for.
fv_grid <- unique(fv_master[, .(fabio_area_code, fabio_item_code, year, source_isic)])
full_grid <- full_grid[fv_grid, on = c("fabio_area_code", "fabio_item_code", "year"),
                       nomatch = NULL,
                       allow.cartesian = TRUE]

result_long <- merge(full_grid, disagg,
                     by = c("year", "fabio_area_code", "fabio_item_code", "va_account"),
                     all.x = TRUE)
result_long[is.na(va_value), va_value := 0]


# ============================================================================
# Write outputs
# ============================================================================

message("\nPivoting wide on va_account and attaching FABIO names ...")

# Pivot wide on va_account.
result_wide <- dcast(
  result_long,
  year + fabio_area_code + fabio_item_code + source_isic ~ va_account,
  value.var = "va_value",
  fill      = 0
)

# Rename va_account columns with units, in spec order.
present  <- intersect(va_spec$account, names(result_wide))
old_cols <- present
new_cols <- va_spec$va_col[match(present, va_spec$account)]
setnames(result_wide, old = old_cols, new = new_cols)

# Define va_cols_in_output now — needed by the stage-8b loop below.  The
# downstream gross_value_added rowSums and the final setcolorder both
# consume it as well.
va_cols_in_output <- va_spec$va_col[match(present, va_spec$account)]


# ============================================================================
# Stage 8 + 8b — zero out no-output rows; per-component IHS+MAD winsorization
# ============================================================================
#
# Stage 8 zeros out VA on rows whose FABIO physical output (TPO) is NA
# or 0 — the FSDN value was placed there by the disaggregation share
# machinery, but there's no actual product activity to attach it to, so
# it's artefactual and we clear it.  There is no `preserve_items`
# carve-out here (unlike the BioSAMs ISIC-C pipeline): fv_master assigns
# each FABIO item to exactly one ISIC level via the GLORIA-priority
# rule at F6, so total_product_output is always the right physical
# quantity for that row.
#
# Stage 8b winsorizes each VA component (D.1 / D.29 / D.39 / K.1 / B.3n)
# SEPARATELY on its own EUR-per-physical-unit intensity
# (component / total_product_output), pooled per fabio_item_code with
# per-item θ optimisation.  No row-sum aggregation, no proportional
# rescaling — the post-cap component is just
# (capped_intensity × total_product_output).  The row sum
# `gross_value_added [EUR]` is derived only AFTER capping, in the next
# block, from the independently-capped pieces.
#
# Per-component capping (rather than capping the row sum and rescaling
# uniformly) avoids three sign-flip / amplification pathologies: A
# uniform va_scale = total_post / total_pre can be negative when
# components partially cancel (which would flip all components' signs
# together — D.1 going negative is economically impossible) or have
# |va_scale| ≫ 1 (which would inflate components disproportionately).
# Per-component capping eliminates both by construction.  An individual
# component's intensity IS allowed to flip sign at the cap — at this
# granularity that is just the legitimate output of clipping an outlier
# on its own component's distribution (D.29 / D.39 are mixed-sign by
# accounting construction across EU agriculture).  Sign-flip events
# are tracked in the diagnostic via `sign_flipped_at_cap` for visibility
# but NOT reverted.
#
# Stage 8b eligibility for the per-item MAD pool, evaluated PER COMPONENT:
#   - total_product_output is finite and strictly positive,
#   - the component value is finite and non-zero.
#
# Structural-zero rows for a component (component value = 0 — including
# rows just zeroed at stage 8) are excluded from that component's MAD
# pool so they don't drag the median to zero.  The other components on
# the same row are still pooled if non-zero.

message("\n══════════════════════════════════════════════════════════════════════")
message("  Stage 8 + 8b — no-output zero + per-component IHS+MAD winsorization")
message("══════════════════════════════════════════════════════════════════════")

diag_winsor_path <- file.path(DIAG_DIR, paste0(DIAG_WINSOR_BASENAME, ".csv"))

# Snapshot 1/2 for the stage-attribution allocation-gap diagnostic
# (downstream — see "Allocation-gap diagnostic" section).  Captures the
# per-(year, va_account) total AFTER disaggregation (B3–B6) but BEFORE
# any zeroing or capping.  The gap between fsdn_tot_yr and this snapshot
# is the VA lost when FSDN cells couldn't be matched to a FABIO target.
post_disagg_tot_yr <- result_long[
  , .(post_disagg_total = sum(va_value, na.rm = TRUE)),
  by = .(year, va_account)
]
#
# Note on row_id: fv_master inherits row_id from fv_a and fv_c, which
# were numbered independently by the upstream total_values pipeline.
# Each FABIO item lives at exactly one ISIC level in fv_master (by
# construction at F6), so row_id is unique within fv_master, but its
# numeric range overlaps between source_isic = "A" and "C" rows.
# Treat (row_id, source_isic) as the globally unique pair if cross-
# referencing back to the source files.
fv_attach <- unique(fv_master[, .(
  year, fabio_area_code, fabio_item_code,
  row_id, iso3c,
  fabio_area, comm_code, fabio_item, comm_group, unit,
  total_product_output,
  `price [USD/unit]`, price_source, price_source_constituents,
  total_value
)])
result_wide <- merge(result_wide, fv_attach,
                     by = c("year", "fabio_area_code", "fabio_item_code"),
                     all.x = TRUE)

# phys_denom: TPO when finite & strictly positive, NA otherwise.  Used
# by BOTH stage 8 (as the "no output" flag) and stage 8b (as the
# eligibility check + denominator).
result_wide[, phys_denom := fifelse(
  is.finite(total_product_output) & total_product_output > 0,
  as.numeric(total_product_output),
  NA_real_
)]


# ── 8. Zero out VA on rows with no product output ────────────────────────

no_output <- is.na(result_wide$phys_denom)
n_zeroed  <- sum(no_output)
if (n_zeroed > 0L) {
  for (vc in va_cols_in_output) {
    set(result_wide, which(no_output), vc, 0)
  }
  message(sprintf(
    "  Stage 8: zeroed VA on %d / %d row(s) with no product output (%.1f%%).",
    n_zeroed, nrow(result_wide), 100 * n_zeroed / nrow(result_wide)
  ))
} else {
  message("  Stage 8: every row has positive product output — nothing zeroed.")
}

# Snapshot 2/2 for the stage-attribution allocation-gap diagnostic.
# Captures the per-(year, va_account) total AFTER stage 8 zeroing but
# BEFORE stage 8b winsorization.  Computed as a per-column aggregate
# (avoids melting the whole wide table).  va_spec carries the
# (va_account, va_col) mapping defined at the top of the script.
post_zero_tot_yr <- rbindlist(lapply(va_cols_in_output, function(vc) {
  acc <- va_spec[va_col == vc, account]
  result_wide[, .(va_account     = acc,
                  post_zero_total = sum(get(vc), na.rm = TRUE)),
              by = year]
}))


# ── 8b. Per-component IHS+MAD winsorization on (component / TPO) ─────────

# Per-component diagnostic accumulator.  Combined into one long-format
# CSV at the end with a `va_account` column distinguishing components.
diag_rows <- vector("list", length(va_cols_in_output))
names(diag_rows) <- va_cols_in_output

for (vc in va_cols_in_output) {
  message(sprintf("  [%s] ─────────────────────────────────────────────", vc))
  diag_rows[[vc]] <- cap_component_by_item(result_wide, vc, k = WINSOR_MAD_K)
}

# Combined long-format diagnostic across all components.
diag_combined <- rbindlist(diag_rows, use.names = TRUE, fill = TRUE)
if (nrow(diag_combined) > 0L) {
  write_va_diagnostic(diag_combined, type = "winsor",
                      sort_first = "va_account",
                      out_path   = diag_winsor_path,
                      announce   = FALSE,
                      col_order  = c(
                        "va_account",
                        "fabio_area_code", "fabio_area",
                        "fabio_item_code", "fabio_item",
                        "year",
                        "intensity_pre", "intensity_post",
                        "cap_lower", "cap_upper",
                        "winsorized", "direction", "sign_flipped_at_cap",
                        "abs_change", "mad_z", "abs_mad_z", "item_theta"))
} else {
  fwrite(diag_combined, diag_winsor_path)
}

message(sprintf(
  "Per-component winsor diagnostic → %s  (%d row(s) across %d component(s)).",
  diag_winsor_path, nrow(diag_combined), length(va_cols_in_output)
))

# Drop the helper column.  total_product_output and total_value are
# KEPT in the output (per the BioSAMs schema) — we just rename them
# below to restore their bracketed unit labels.
result_wide[, phys_denom := NULL]


# ============================================================================
# Total VA summary column + final output schema
# ============================================================================

# Total VA summary column.
# With D.39 stored negative (sign-adjusted convention), the simple row
# sum of all five accounts equals GVA at basic prices:
#   gross_value_added = D.1 + D.29 + D.39_neg + K.1 + B.3n  ≈  SE131 − SE275
# Provided for easy cross-checking against Eurostat EAA, BioSAMs, and
# national accounts aggregates without requiring the caller to know the
# sign convention of each component column.  Computed AFTER stage 8b so
# the sum reflects post-cap component values.
result_wide[, (FSDN_TOTAL_COL) :=
              rowSums(.SD, na.rm = TRUE),
            .SDcols = va_cols_in_output]

# Restore the bracketed unit labels for the value and TPO columns.
value_col_label  <- attr(fv_master, "value_col_label")
output_col_label <- attr(fv_master, "output_col_label")
setnames(result_wide,
         old = c("total_value",    "total_product_output"),
         new = c(value_col_label,  output_col_label))

# Concordance-mapped flag.  Stage B6 right-joins onto the FULL fv_master item
# grid, so result_wide carries a row for EVERY item in FSDN-covered areas —
# including items the FSDN ↔ FABIO concordance never maps a farm-type onto,
# which come through as genuine right-join zero-fills.  `fsdn_mapped` marks the
# items the concordance DOES map something onto (fsdn_target_items, derived at
# stage F7 from fsdn_conc), so downstream consumers can tell a real FSDN value
# (or a mapped item that merely disaggregated to zero) apart from a never-mapped
# zero-fill — without re-parsing the concordance.  The synthesis script
# (03_..._synthesis.R) uses exactly this column to decide which rows may
# overwrite GLORIA.
result_wide[, fsdn_mapped := fabio_item_code %in% fsdn_target_items]
message(sprintf(
  "  fsdn_mapped flag: %d of %d output row(s) are concordance-mapped (%d distinct item(s)).",
  result_wide[fsdn_mapped == TRUE, .N], nrow(result_wide),
  result_wide[fsdn_mapped == TRUE, uniqueN(fabio_item_code)]))

# Final column order — matches the national-SUT VA output (script 14_3)
# context columns, with `source_isic` appended at the end as an
# FSDN-specific column tagging which ISIC level supplied the
# disaggregation weight (no analogue in BioSAMs, which runs each
# ISIC level as a separate output file), followed by `fsdn_mapped`.
setcolorder(result_wide, c(
  "row_id", "iso3c",
  "fabio_area_code", "fabio_area",
  "comm_code", "fabio_item_code", "fabio_item", "comm_group",
  "unit", "year",
  output_col_label,
  "price [USD/unit]", "price_source", "price_source_constituents",
  value_col_label,
  va_cols_in_output,
  FSDN_TOTAL_COL,
  "source_isic",
  "fsdn_mapped"
))
setorder(result_wide, year, fabio_area_code, fabio_item_code)

out_csv_path <- file.path(OUTPUT_DIR, paste0(OUT_BASENAME, ".csv"))
out_rds_path <- file.path(OUTPUT_DIR, paste0(OUT_BASENAME, ".rds"))
fwrite(result_wide, out_csv_path)
saveRDS(result_wide, out_rds_path)

message(sprintf("Main output: %d rows written to\n  %s\n  %s",
                nrow(result_wide), out_csv_path, out_rds_path))


# ============================================================================
# Allocation-gap diagnostic (per year × va_account)
# ============================================================================
#
# Contrasts the FINAL output (post-disagg + post-zero + post-winsor)
# against the FSDN input total per (year, va_account), with the two
# intermediate stage totals included so the operator can see WHERE the
# gap arose:
#
#   fsdn_total       — FSDN input, per (year, va_account)
#   post_disagg_total — after stage B3–B6 disaggregation
#   post_zero_total   — after stage 8 no-output zeroing
#   post_winsor_total — after stage 8b per-component MAD cap (= final
#                       output total)
#
# Per-stage deltas (delta_disagg, delta_zero, delta_winsor) attribute
# the net gap to each pipeline stage; they sum to gap_abs by
# construction.  Sign convention: positive = lost at this stage,
# negative = added (only possible at the cap on a balanced pool).
#
# The two stage snapshots `post_disagg_tot_yr` and `post_zero_tot_yr`
# are taken upstream in the Stage 8 + 8b section — see "Snapshot 1/2"
# and "Snapshot 2/2" comments there.

message("\nBuilding per-year allocation-gap diagnostic ...")

fsdn_tot_yr <- va_long_fa[
  , .(fsdn_total = sum(va_value, na.rm = TRUE)),
  by = .(year, va_account)
]

# Post-winsor totals — read straight off the final result_wide so the
# diagnostic reflects the actual published output, including all
# downstream zeroing and capping effects.
post_winsor_tot_yr <- rbindlist(lapply(va_cols_in_output, function(vc) {
  acc <- va_spec[va_col == vc, account]
  result_wide[, .(va_account        = acc,
                  post_winsor_total = sum(get(vc), na.rm = TRUE)),
              by = year]
}))

gap_yr <- Reduce(
  function(a, b) merge(a, b, by = c("year", "va_account"), all = TRUE),
  list(fsdn_tot_yr, post_disagg_tot_yr, post_zero_tot_yr, post_winsor_tot_yr)
)
for (col in c("fsdn_total", "post_disagg_total",
              "post_zero_total", "post_winsor_total")) {
  gap_yr[is.na(get(col)), (col) := 0]
}

# Per-stage deltas — positive = lost at this stage relative to its
# upstream total.  Sum to gap_abs by construction.  At the winsor stage
# delta can in principle be negative (cap raises the total) on a
# balanced pool, though in practice positive-skew intensities make the
# net reductive.
gap_yr[, `:=`(
  delta_disagg = fsdn_total        - post_disagg_total,
  delta_zero   = post_disagg_total - post_zero_total,
  delta_winsor = post_zero_total   - post_winsor_total
)]

gap_yr[, gap_abs := fsdn_total - post_winsor_total]
gap_yr[, gap_pct := fifelse(
  fsdn_total != 0,
  100 * gap_abs / fsdn_total,
  NA_real_
)]

# Attach unit + sna_code + the va_col label so the CSV is self-describing.
gap_yr <- merge(gap_yr,
                va_spec[, .(va_account = account, unit, sna_code, va_col)],
                by = "va_account", all.x = TRUE)
setcolorder(gap_yr, c("year", "va_account", "sna_code", "unit", "va_col",
                      "fsdn_total",
                      "post_disagg_total", "post_zero_total", "post_winsor_total",
                      "delta_disagg", "delta_zero", "delta_winsor",
                      "gap_abs", "gap_pct"))
setorder(gap_yr, year, va_account)

diag_path <- file.path(DIAG_DIR, paste0(DIAG_BASENAME, ".csv"))
fwrite(gap_yr, diag_path)
message(sprintf("Gap diagnostic: %d rows written to\n  %s",
                nrow(gap_yr), diag_path))

# Summary print: per-account aggregate across years.  Mean / median /
# worst overall gap_pct as before, plus a per-stage decomposition (mean
# delta-as-pct-of-FSDN) so the operator can see at a glance whether the
# gap is concentrated in disaggregation, zeroing, or winsorization.
gap_summary <- gap_yr[, {
  fsdn_abs_total <- sum(abs(fsdn_total), na.rm = TRUE)
  pct_of_total <- function(x) if (fsdn_abs_total > 0)
    100 * sum(x, na.rm = TRUE) / fsdn_abs_total else NA_real_
  .(mean_gap_pct       = mean(gap_pct, na.rm = TRUE),
    median_gap_pct     = median(gap_pct, na.rm = TRUE),
    worst_year         = year[which.max(abs(gap_pct))],
    worst_gap_pct      = gap_pct[which.max(abs(gap_pct))],
    pct_lost_disagg    = pct_of_total(delta_disagg),
    pct_lost_zero      = pct_of_total(delta_zero),
    pct_lost_winsor    = pct_of_total(delta_winsor))
}, by = .(va_account, sna_code, unit)]
setorder(gap_summary, va_account)

message("\nFinal allocation gap (FSDN vs final output), per account:")
message("  (positive = FSDN > final output; stage shares sum to overall gap)")
for (k in seq_len(nrow(gap_summary))) {
  message(sprintf(
    "  %-30s %-5s [%-3s]  mean %+6.2f%%   median %+6.2f%%   worst %+6.2f%% (%d)",
    gap_summary$va_account[k],
    gap_summary$sna_code[k],
    gap_summary$unit[k],
    gap_summary$mean_gap_pct[k],
    gap_summary$median_gap_pct[k],
    gap_summary$worst_gap_pct[k],
    gap_summary$worst_year[k]
  ))
  message(sprintf(
    "    └─ stage breakdown: disagg %+6.2f%% │ step-8 zero %+6.2f%% │ step-8b cap %+6.2f%%",
    gap_summary$pct_lost_disagg[k],
    gap_summary$pct_lost_zero[k],
    gap_summary$pct_lost_winsor[k]
  ))
}

message("\nDone.")