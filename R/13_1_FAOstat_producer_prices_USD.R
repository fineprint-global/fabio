# ============================================================================
# FAOSTAT Producer Prices: Build Dataset with USD Estimates
# ============================================================================
#
# Description
# -----------
# Processes FAOSTAT producer-price bulk data to produce a clean dataset
# of producer prices in USD per tonne. The pipeline:
#
#   1.  Restricts to annual producer-price rows.
#   2.  Removes FAOSTAT "flag" columns (those ending in 'F').
#   3.  Drops LCU rows where an SLC row already exists for the same
#       area-item pair.
#   4.  Fills missing SLC values using a PPI-derived scaling factor.
#   5.  Fills missing USD values using SLC divided by the FAOSTAT
#       exchange-rate series.
#   6.  Builds new USD rows where only SLC data exist.
#   7.  Merges a handful of historical country series (e.g. Czechoslovakia
#       -> Czechia / Slovakia) into their successor states.
#   8.  Drops all non-USD rows (SLC and PPI were only needed as inputs
#       to the USD-estimation steps above).
#   9.  Hampel filter on each (Area, Item) USD time series (threshold = 3,
#       half-window = 3; the scale is the per-series MAD). Flagged points
#       are replaced with the rolling-window median; a diagnostics table
#       listing every cell the filter evaluated is written to
#       ./output/diagnostics/producer_price_hampel_entries.csv.
#   10. Winsorizes USD values per Item Code, pooling
#       all (country, year) cells for each item, using a MAD-based
#       modified Z-score. A diagnostics table listing every evaluated
#       cell (with its per-item cap band) is written to
#       ./output/diagnostics/producer_price_winsorized_entries.csv.
#   11. Appends synthetic global-median USD rows (one per item) computed
#       from the winsorized country data.
#
# Note on step ordering
# ---------------------
# Global-median rows are built last, from the merged, Hampel-filtered,
# and winsorized country data, so the reported medians are not pulled
# by outliers or by split country records (e.g. both Czechia and
# Czechoslovakia contributing in the same year).
#
# The Hampel step runs BEFORE winsorization. This order matters: Hampel
# catches one-year excursions in a single country's series by comparison
# to that country's own local history; winsorization then caps the
# remaining outliers across the pooled distribution of all retained
# (country, year) cells for each item. Because years are pooled along
# with countries, winsorization treats both cross-country and cross-year
# departures from the item median as outliers. This order is shared with
# 13_2_clean_bilateral_trade_prices.R.
#
# Note on scope
# -------------
# Output years are KEEP_YEARS, derived from R/00_value_added_config.R
# (VA_KEEP_YEARS, which mirrors `years` in R/00_system_variables.R). To
# give the Hampel filter a full +/- HAMPEL_HALF_WINDOW of context at *both*
# edges of the output window, the pipeline carries BUFFER_YEARS
# (VA_BUFFER_YEARS: min/max(KEEP_YEARS) +/- HAMPEL_HALF_WINDOW) alongside
# KEEP_YEARS through every step from the year-column restriction onward, up
# to and including the Hampel pass. Buffer rows are dropped immediately
# after Hampel, before winsorization and the global-median construction.
#
# The PPI-based SLC imputation is a further exception: it runs on the
# raw (unrestricted) year columns so that out-of-window overlap years
# can still inform each (Area, Item) scaling factor. The working window
# (BUFFER_YEARS U KEEP_YEARS) is applied immediately after that step.
#
# To change the year range, edit R/00_system_variables.R only; this script
# and the 14_ stages follow automatically.
#
# If you widen KEEP_YEARS you may also want to revisit the winsorization
# granularity, since the per-Item (pooled across years) cap assumes
# roughly stable real price levels within the retained range.
#
# Input files (expected in ./input/fao/ -- the Normalized FAOSTAT bulk
# layout that R/00_1_prep_fao.R downloads; both are reshaped to the wide
# "_E_All_Data" layout on read via read_fao_normalized_wide()):
#   - Prices_E_All_Data_(Normalized).csv
#   - Exchange_rate_E_All_Data_(Normalized).csv
#
# Output file:
#   - ./output/Prices_E_All_Data_with_USD.csv
#
# Author: corne
# Last updated: 2026-06-14  (ported from dplyr/tidyr to data.table; the
#   pipeline logic, the output CSV, and both diagnostic CSVs are unchanged
#   -- see the diff harness in tests/diff_13_1.R)
# ============================================================================
#
# data.table port notes
# ----------------------
# This script previously attached dplyr/tidyr/readr/tibble. Because package
# attachments survive main.R's `rm(list = ls())`, those four packages stayed
# on the search path for every downstream data.table script (13_2 -> 15),
# making the run order load-bearing (dplyr::filter/lag masking stats::, etc.).
# The wrangling layer is now pure data.table, so the only package this script
# attaches is data.table -- the same one 13_2..14_5 already use.
#
# The final output CSV is written with data.table::fwrite(), matching the rest
# of the value-added pipeline: the 14_* stages (14_1..14_4) write their CSVs
# with fwrite, and this script's OWN diagnostics already do (via the shared
# build_*_diagnostic() helpers). The sole downstream consumer, 13_3, reads it
# back with fread(), so fwrite/fread round-trips natively. Values are identical
# to the previous readr::write_csv() output; only the on-disk float/quote
# formatting may differ slightly (the diagnostics, written by fwrite all along,
# are unaffected).
#
# Reference semantics: every helper that "modifies" its data frame takes a
# value-semantics copy first (copy()/fresh subsets), reproducing dplyr's
# copy-on-modify behaviour so callers' inputs are never mutated by reference.


# ---- Packages --------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
})

# Centralised config: resolves every path from FABIO_ROOT and sources the
# value-added helpers. Assumes the working directory is the FABIO repo root
# (FABIO convention; see R/00_value_added_config.R).
source("R/00_value_added_config.R")


# ---- Paths (resolved from R/00_value_added_config.R) -----------------------

INPUT_PRICES_CSV   <- VA_PRICES_RAW_CSV
INPUT_XR_CSV       <- VA_EXCHANGE_RATE_CSV
OUTPUT_PRICES_CSV  <- VA_PRICES_USD_CSV
DIAG_HAMPEL_CSV    <- file.path(VA_PRICE_DIAG_DIR, "producer_price_hampel_entries.csv")
DIAG_WINSOR_CSV    <- file.path(VA_PRICE_DIAG_DIR, "producer_price_winsorized_entries.csv")


# ---- Constants -------------------------------------------------------------

PRICE_SLC <- 5531L   # FAOSTAT element code: Producer price (SLC/tonne)
PRICE_USD <- 5532L   # FAOSTAT element code: Producer price (USD/tonne)
PPI       <- 5539L   # FAOSTAT element code: Producer price index

KEYS     <- c("Area Code", "Item Code")
AREA_KEY <- c("Area Code")

# Output years. Every downstream step after the Hampel pass (including
# winsorization, global medians, and the CSV written to disk) operates
# on this window only.
#
# Derived from R/00_value_added_config.R (which tracks `years` in
# R/00_system_variables.R) so the price pipeline follows the FABIO year
# range automatically. To change the years, edit R/00_system_variables.R only.
KEEP_YEARS <- VA_KEEP_YEARS

# Buffer years carried alongside KEEP_YEARS from step 6b through the
# Hampel filter (step 12), then dropped. They exist solely to give the
# Hampel filter a full +/- HAMPEL_HALF_WINDOW of context at the *edges* of
# KEEP_YEARS, and are never written to the output. They are derived in
# R/00_value_added_config.R as min/max(KEEP_YEARS) +/- HAMPEL_HALF_WINDOW
# (symmetric), the same rule 14_1_value_added_FABIO_v2_MRIOTs.R uses for its
# own buffer_years. Past the last released FAOSTAT year the high-side buffer
# years simply have no data and contribute nothing. Set
# VA_BUFFER_YEARS to integer(0) in the config to disable buffering entirely.
BUFFER_YEARS <- VA_BUFFER_YEARS

# Union of output + buffer years. Used from step 6b through step 12
# (Hampel). Step 12b drops the buffer before any cross-sectional or
# cross-item operations.
WORKING_YEARS <- VA_WORKING_YEARS

# Hampel filter parameters. Identical to script 13_2
# (13_2_clean_bilateral_trade_prices.R) so the two price pipelines can be
# compared on equal footing. With half-window = 3 the filter requires at
# least 2*half_window + 1 = 7 observations in a series to evaluate it;
# shorter series are passed through unchanged.
#
# Assigned FROM the central constants in R/00_value_added_config.R (section 5)
# so the two price pipelines and the 14_ VA stages can never drift apart. Names
# match hampel_filter()'s own argument names (half_window / threshold), so the
# same identifier means the same thing in every script -- no HAMPEL_K alias.
HAMPEL_THRESHOLD   <- VA_HAMPEL_THRESHOLD     # robust-z spike cutoff   (-> hampel_filter `threshold`)
HAMPEL_HALF_WINDOW <- VA_HAMPEL_HALF_WINDOW   # rolling-median half-window (-> hampel_filter `half_window`)

# MAD winsorization cutoff (robust |z|) for the per-item pooled cap below.
# Named here (not hidden as a function default) so the cap is auditable at the
# top of the script, matching 13_2 / 14_*; see the WINSOR_MAD_K note in
# R/00_value_added_config.R section 5. Value is a deliberate per-script choice.
WINSOR_MAD_K       <- 2.5                     # cap at median +/- WINSOR_MAD_K * MAD

# (target_area, source_area) pairs. Source rows are folded into target
# rows under the assumption that their non-missing year spans do not
# overlap (e.g. Czechoslovakia ends before Czechia begins).
COUNTRY_MERGES <- data.table(
  target_area = c("Belgium", "Czechia", "Ethiopia",
                  "Luxembourg", "Slovakia", "Sudan"),
  source_area = c("Belgium-Luxembourg", "Czechoslovakia", "Ethiopia PDR",
                  "Belgium-Luxembourg", "Czechoslovakia", "Sudan (former)")
)


# ---- Helper functions ------------------------------------------------------

#' Drop columns whose names end with "F"
#'
#' FAOSTAT bulk downloads interleave value columns (e.g. `Y2000`) with
#' flag columns (e.g. `Y2000F`). This removes the flag columns. (After
#' the Normalized->wide reshape there are no flag columns, so this is a
#' harmless no-op kept for robustness against other layouts.)
#'
#' @param df A data frame that may contain flag columns.
#' @return A data.table without flag columns.
drop_flag_columns <- function(df) {
  keep <- names(df)[!grepl("F$", names(df))]
  as.data.table(df)[, ..keep]
}


#' Get year-value column names
#'
#' A year column is a name beginning with "Y" followed by digits only,
#' such as `"Y2000"`.
#'
#' @param df A data frame.
#' @return Character vector of year column names, in their existing order.
get_year_cols <- function(df) {
  nm <- names(df)
  nm[grepl("^Y\\d+$", nm)]
}


#' Read a Normalized FAOSTAT bulk CSV and reshape it to the wide
#' "_E_All_Data" layout that the rest of this script expects.
#'
#' FABIO's prep step (R/00_1_prep_fao.R) downloads the *Normalized*
#' (long) FAOSTAT archives -- one row per (area, item, element, months,
#' year) with a single `Value` column. This script, however, was written
#' against the *wide* "_E_All_Data" layout: one `Y####` value column per
#' year, with interleaved `Y####F` flag columns. Rather than rewrite the
#' ~900 lines of wide-format logic that follow, we reshape the long file
#' back to wide on read, so every downstream step is unchanged.
#'
#' Reshape contract:
#'   - `Year` (the spread key) and `Value` (the spread value) are
#'     consumed by the cast; every remaining column is treated as a row
#'     identifier and preserved -- including `Element Code`, `Unit`,
#'     `Months`, and (for exchange rates) `ISO Currency Code`, so the
#'     existing filters and the multi-currency collapse keep working.
#'   - The single `Flag` column is dropped, so NO `Y####F` columns are
#'     produced; `drop_flag_columns()` therefore stays a harmless no-op.
#'   - `Year Code` (a duplicate of `Year`), the `*(M49)` / `*(CPC)`
#'     alternate-code columns, and `Note` are dropped as noise the wide
#'     pipeline never referenced.
#'
#' @param path Path to a Normalized FAOSTAT bulk CSV.
#' @param annual_only If TRUE, keep only `Months Code == 7021`
#'   ("Annual value") before reshaping. The exchange-rate file carries
#'   monthly rows that must be excluded here; the price file is filtered
#'   on `Months == "Annual value"` by the caller, so it passes FALSE and
#'   the `Months` column is preserved for that downstream filter.
#' @return A data.table in the wide layout, with numeric `Y####` year columns.
read_fao_normalized_wide <- function(path, annual_only = FALSE) {
  dt <- fread(path, na.strings = c("", "NA"))
  
  if (isTRUE(annual_only) && "Months Code" %in% names(dt)) {
    dt <- dt[which(dt[["Months Code"]] == 7021L)]
  }
  
  drop_cols <- intersect(
    c("Area Code (M49)", "Item Code (CPC)", "Year Code", "Flag", "Note"),
    names(dt)
  )
  if (length(drop_cols)) dt[, (drop_cols) := NULL]
  
  id_cols <- setdiff(names(dt), c("Year", "Value"))
  lhs     <- paste0("`", id_cols, "`", collapse = " + ")
  wide    <- dcast(
    dt,
    stats::as.formula(paste(lhs, "~ Year")),
    value.var = "Value"
  )
  
  yr_cols <- grep("^[0-9]{4}$", names(wide), value = TRUE)
  setnames(wide, yr_cols, paste0("Y", yr_cols))
  
  wide[]
}


#' Coerce selected columns to numeric
#'
#' Non-convertible values are replaced with `NA`. Operates on a copy, so
#' the caller's data frame is not modified by reference.
#'
#' @param df A data frame.
#' @param cols Character vector of columns to convert.
#' @return A data.table with the selected columns coerced to numeric.
coerce_numeric <- function(df, cols) {
  dt <- copy(as.data.table(df))
  for (cc in cols) set(dt, j = cc, value = suppressWarnings(as.numeric(dt[[cc]])))
  dt[]
}


#' Drop LCU rows when an SLC row exists for the same area-item pair
#'
#' Where both local-currency (LCU) and standard-local-currency (SLC)
#' rows exist for a given `(Area Code, Item Code)` combination, the
#' LCU row is redundant and is removed.
#'
#' @param df A data frame with at least `Unit`, `Area Code`, `Item Code`.
#' @return A data.table with redundant LCU rows removed (original order).
drop_lcu_when_slc_exists <- function(df) {
  dt <- copy(as.data.table(df))
  dt[, .has_slc := any(Unit == "SLC", na.rm = TRUE), by = c("Area Code", "Item Code")]
  out <- dt[!(Unit %in% "LCU" & .has_slc)]
  out[, .has_slc := NULL]
  out[]
}


#' Fill missing SLC values using PPI-derived scaling
#'
#' For each `(Area Code, Item Code)` pair:
#'   1. Compute the yearwise ratio `SLC / PPI` where both values are known.
#'   2. Summarise those ratios by their median -> a single scaling factor.
#'   3. Fill missing SLC values as `PPI * median_ratio` where PPI is known
#'      and non-zero.
#'
#' Only existing SLC rows are updated; no new rows are created.
#'
#' @param df A data frame.
#' @param year_cols Character vector of year columns.
#' @return A data.table with missing SLC values filled where possible.
fill_slc_from_ppi <- function(df, year_cols) {
  dt <- as.data.table(df)
  
  slc_keys <- unique(dt[`Element Code` == PRICE_SLC, ..KEYS])
  ppi_keys <- unique(dt[`Element Code` == PPI,       ..KEYS])
  
  common_keys <- merge(slc_keys, ppi_keys, by = KEYS)   # inner join
  if (nrow(common_keys) == 0L) return(copy(dt)[])
  
  # SLC / PPI series on their common keys: first row per key (mirrors
  # distinct(across(KEYS), .keep_all = TRUE)), then wide -> long.
  slc_first <- unique(dt[`Element Code` == PRICE_SLC, c(KEYS, year_cols), with = FALSE], by = KEYS)
  slc_first <- slc_first[common_keys, on = KEYS, nomatch = NULL]
  slc_long  <- melt(slc_first, id.vars = KEYS, measure.vars = year_cols,
                    variable.name = "year", value.name = "slc", variable.factor = FALSE)
  
  ppi_first <- unique(dt[`Element Code` == PPI, c(KEYS, year_cols), with = FALSE], by = KEYS)
  ppi_first <- ppi_first[common_keys, on = KEYS, nomatch = NULL]
  ppi_long  <- melt(ppi_first, id.vars = KEYS, measure.vars = year_cols,
                    variable.name = "year", value.name = "ppi", variable.factor = FALSE)
  
  paired <- merge(slc_long, ppi_long, by = c(KEYS, "year"), all.x = TRUE, sort = FALSE)
  
  # Median SLC/PPI ratio per area-item, using only years where both
  # series have a real (non-zero, non-NA) value.
  median_ratios <- paired[!is.na(slc) & !is.na(ppi) & ppi != 0,
                          .(med_ratio = median(slc / ppi, na.rm = TRUE)),
                          by = KEYS]
  
  # Fill gaps in SLC with PPI * median_ratio where possible.
  filled <- merge(paired, median_ratios, by = KEYS, all.x = TRUE, sort = FALSE)
  filled[, slc := fifelse(
    is.na(slc) & !is.na(ppi) & ppi != 0 & !is.na(med_ratio),
    ppi * med_ratio,
    slc
  )]
  
  filled_wide <- dcast(
    filled,
    stats::as.formula(paste0("`", paste(KEYS, collapse = "` + `"), "` ~ year")),
    value.var = "slc"
  )
  
  # Write the filled values back into the SLC rows of df, preserving row
  # order and leaving all non-SLC rows untouched. Matches each SLC row to
  # the lookup by composite key; rows with no lookup match keep originals.
  out      <- copy(dt)
  slc_idx  <- which(out$`Element Code` == PRICE_SLC)
  key_slc  <- do.call(paste, c(out[slc_idx, ..KEYS],        sep = "\r"))
  key_lkp  <- do.call(paste, c(filled_wide[, ..KEYS],       sep = "\r"))
  idx      <- match(key_slc, key_lkp)
  
  for (yc in year_cols) {
    has_update <- !is.na(idx)
    if (!any(has_update)) next
    new_vals   <- filled_wide[[yc]][idx[has_update]]
    orig_vals  <- out[[yc]][slc_idx[has_update]]
    set(out, i = slc_idx[has_update], j = yc, value = fcoalesce(new_vals, orig_vals))
  }
  out[]
}


#' Load and prepare exchange-rate data
#'
#' The file is filtered to rows where `Element Code == "SLC"` (note: in
#' the FAOSTAT exchange-rate file this code is stored as a string, unlike
#' the integer element codes in the main price file). Year columns are
#' coerced to numeric and renamed with the suffix `"_xr"`.
#'
#' A single `Area Code` may appear on multiple rows (e.g. one per ISO
#' Currency Code for countries that switched currency). Those rows are
#' collapsed to a single row per area by taking the first non-NA value
#' per year, rather than the first row outright.
#'
#' @param path Path to the exchange-rate CSV.
#' @param year_cols Character vector of year columns to retain.
#' @return A data.table with one row per `Area Code` and year columns
#'   renamed to e.g. `"Y2000_xr"`.
prepare_exchange_rates <- function(path, year_cols) {
  # Reshape the Normalized exchange-rate file to wide. annual_only = TRUE
  # drops the monthly rows (Months Code 7001-7012) that the long file
  # carries; the wide "_E_All_Data" file the old code read did not have
  # them, so this keeps the SLC series annual as before. `Element Code`
  # is a STRING ("SLC"/"LCU") in this file -- unlike the integer codes in
  # the price file -- so the filter below is unchanged.
  xr <- read_fao_normalized_wide(path, annual_only = TRUE)
  
  xr <- xr[`Element Code` == "SLC", c("Area Code", year_cols), with = FALSE]
  xr <- coerce_numeric(xr, year_cols)
  
  # Collapse multi-currency rows: take the first non-NA value per year.
  xr <- xr[, lapply(.SD, function(x) {
    v <- x[!is.na(x)]
    if (length(v)) v[1] else NA_real_
  }), by = "Area Code", .SDcols = year_cols]
  
  setnames(xr, year_cols, paste0(year_cols, "_xr"))
  xr[]
}


#' Fill missing values in existing USD rows using SLC and exchange rates
#'
#' For rows with element code 5532:
#'   - join matching SLC values by `(Area Code, Item Code)`,
#'   - join exchange rates by `Area Code`,
#'   - fill missing USD values as `SLC / exchange_rate` where the
#'     exchange rate is available and non-zero.
#'
#' @param df A data frame.
#' @param xr Exchange-rate table (from `prepare_exchange_rates`).
#' @param year_cols Character vector of year columns.
#' @return A data.table with existing USD rows filled where possible
#'   (non-USD rows first, then USD rows -- as before).
fill_existing_usd_rows <- function(df, xr, year_cols) {
  dt <- as.data.table(df)
  
  slc_lookup <- unique(dt[`Element Code` == PRICE_SLC, c(KEYS, year_cols), with = FALSE], by = KEYS)
  setnames(slc_lookup, year_cols, paste0(year_cols, "_slc"))
  
  usd_rows <- dt[`Element Code` == PRICE_USD]
  if (nrow(usd_rows) == 0L) return(copy(dt)[])
  
  usd_filled <- merge(usd_rows,   slc_lookup, by = KEYS,        all.x = TRUE, sort = FALSE)
  usd_filled <- merge(usd_filled, xr,         by = "Area Code", all.x = TRUE, sort = FALSE)
  
  for (y in year_cols) {
    y_slc <- paste0(y, "_slc")
    y_xr  <- paste0(y, "_xr")
    if (!y_slc %in% names(usd_filled) || !y_xr %in% names(usd_filled)) next
    
    fill_mask <- is.na(usd_filled[[y]]) &
      !is.na(usd_filled[[y_slc]]) &
      !is.na(usd_filled[[y_xr]])  &
      (usd_filled[[y_xr]] != 0)
    
    set(usd_filled, i = which(fill_mask), j = y,
        value = usd_filled[[y_slc]][fill_mask] / usd_filled[[y_xr]][fill_mask])
  }
  
  helper_cols <- grep("_slc$|_xr$", names(usd_filled), value = TRUE)
  if (length(helper_cols)) usd_filled[, (helper_cols) := NULL]
  
  non_usd <- dt[`Element Code` != PRICE_USD]
  rbindlist(list(non_usd, usd_filled[, names(dt), with = FALSE]), use.names = TRUE)
}


#' Build new USD rows for area-item pairs that have SLC data but no USD row
#'
#' The new USD values are computed as `SLC / exchange_rate` where both
#' inputs are present and the exchange rate is non-zero.
#'
#' @param df A data frame.
#' @param xr Exchange-rate table (from `prepare_exchange_rates`).
#' @param year_cols Character vector of year columns.
#' @return A data.table of new USD rows matching the schema of `df`.
#'   If no rows are created, returns an empty 0-row slice of `df`.
build_missing_usd_rows <- function(df, xr, year_cols) {
  dt <- as.data.table(df)
  
  slc_rows <- dt[`Element Code` == PRICE_SLC]
  usd_keys <- unique(dt[`Element Code` == PRICE_USD, ..KEYS])
  
  new_rows <- slc_rows[!usd_keys, on = KEYS]   # anti-join
  if (nrow(new_rows) == 0L) return(dt[0L])
  
  new_rows <- merge(copy(new_rows), xr, by = "Area Code", all.x = TRUE, sort = FALSE)
  
  for (y in year_cols) {
    y_xr <- paste0(y, "_xr")
    if (!y_xr %in% names(new_rows)) {
      set(new_rows, j = y, value = NA_real_)
      next
    }
    valid <- !is.na(new_rows[[y]]) &
      !is.na(new_rows[[y_xr]]) &
      (new_rows[[y_xr]] != 0)
    set(new_rows, j = y, value = fifelse(valid, new_rows[[y]] / new_rows[[y_xr]], NA_real_))
  }
  
  # Rewrite the element metadata to reflect the new series
  set(new_rows, j = "Element Code", value = PRICE_USD)
  if ("Element" %in% names(new_rows)) set(new_rows, j = "Element", value = "Producer Price (USD/tonne)")
  if ("Unit"    %in% names(new_rows)) set(new_rows, j = "Unit",    value = "USD")
  
  xr_cols <- grep("_xr$", names(new_rows), value = TRUE)
  if (length(xr_cols)) new_rows[, (xr_cols) := NULL]
  
  new_rows[, names(dt), with = FALSE]
}


# hampel_filter(): now in va_helpers.R


#' Apply the Hampel filter to every USD row's time series
#'
#' Each row of `df` is assumed to represent a single (Area, Item) USD
#' series -- which is true immediately after the `Element Code == USD`
#' filter in `main()`. Year values are read out in chronological order,
#' passed through `hampel_filter()`, and written back. Diagnostic arrays
#' (per-position window median, spike flag, and the per-series MAD scale)
#' are returned alongside the modified data frame.
#'
#' NA handling and observation-based windowing
#' -------------------------------------------
#' Only the non-NA cells of each row are passed to `hampel_filter()`,
#' as a compact vector indexed by observation rather than by calendar
#' year. The +/- `window` neighbours are therefore the `window` nearest
#' OBSERVED years on each side, not the calendar neighbours that may
#' happen to be NA. This matches the semantics of script 13_2
#' (13_2_clean_bilateral_trade_prices.R), which achieves the same effect
#' implicitly by working in long format and dropping NA-priced rows
#' before the by-group call.
#'
#' Results are written back to their original calendar slots: NA cells
#' stay NA in the output, and `flag_mat` / `med_mat` carry
#' filter outputs only at observed positions, leaving NA / FALSE
#' everywhere else.
#'
#' The caller is responsible for any chronological buffer around
#' KEEP_YEARS: the filter treats whatever year columns it is handed as
#' the full series. Passing `WORKING_YEARS` gives observations at the
#' edge of `KEEP_YEARS` a complete +/- `window` of context.
#'
#' @param df Data frame of USD rows (one per (Area, Item)).
#' @param year_cols Character vector of year column names.
#' @param threshold,half_window Passed to `hampel_filter()`.
#' @return Named list with `df` (data.table, year columns updated),
#'   `flags`, `med_win` (matrices), and `series_mad` (numeric vector).
hampel_usd_rows <- function(df, year_cols,
                            threshold   = HAMPEL_THRESHOLD,
                            half_window = HAMPEL_HALF_WINDOW) {
  # Chronological ordering of year columns so the Hampel window spans
  # contiguous OBSERVED years regardless of how they are laid out in `df`.
  ordered_years <- year_cols[order(as.integer(sub("^Y", "", year_cols)))]
  n_years       <- length(ordered_years)
  n_rows        <- nrow(df)
  
  # Pull year values into a numeric matrix for fast row access.
  year_mat  <- as.matrix(as.data.table(df)[, ..ordered_years])
  storage.mode(year_mat) <- "double"
  year_post <- year_mat
  
  flag_mat <- matrix(FALSE,    nrow = n_rows, ncol = n_years,
                     dimnames = list(NULL, ordered_years))
  med_mat  <- matrix(NA_real_, nrow = n_rows, ncol = n_years,
                     dimnames = list(NULL, ordered_years))
  series_mad_vec <- rep(NA_real_, n_rows)
  
  for (i in seq_len(n_rows)) {
    # Compact the row to its non-NA cells before handing it to the filter,
    # so its +/- `window` slice picks up the nearest OBSERVED years.
    x_full  <- year_mat[i, ]
    obs_idx <- which(!is.na(x_full))
    
    # All-NA row: nothing to do; matrices already initialized.
    if (length(obs_idx) == 0L) next
    
    r <- hampel_filter(x_full[obs_idx], half_window = half_window, threshold = threshold)
    
    year_post[i, obs_idx] <- r$values
    flag_mat[i, obs_idx]  <- r$is_spike
    med_mat[i, obs_idx]   <- r$window_median
    series_mad_vec[i]     <- r$series_mad
  }
  
  df_out <- copy(as.data.table(df))
  for (y in ordered_years) set(df_out, j = y, value = year_post[, y])
  
  list(df         = df_out,
       flags      = flag_mat,
       med_win    = med_mat,
       series_mad = series_mad_vec)
}


#' Write a diagnostics table of every cell the Hampel filter evaluated
#'
#' One row per `(Area, Item, Year)` cell that had a non-NA value going
#' in -- NOT just the flagged ones -- so the CSV can be filtered either
#' way downstream. Rows from ineligible series (n < min_obs) carry
#' NA window stats and NA `hampel_z` and were passed through unchanged.
#'
#' This is a thin reshape-and-delegate wrapper: it converts the wide
#' Hampel result to long and hands it to the shared
#' build_hampel_diagnostic() (00_value_added_helpers.R), so 13_1 and
#' 13_2 emit byte-compatible Hampel diagnostics.
#'
#' @param before Data frame of USD rows BEFORE Hampel (for pre-values
#'   and id columns). Row order must match `after_info$df`.
#' @param after_info Return value of `hampel_usd_rows()`.
#' @param year_cols Character vector of year columns.
#' @param path Output CSV path. Parent directory is created if needed.
#' @return (invisibly) the diagnostics data.table from
#'   `build_hampel_diagnostic()`.
write_hampel_diagnostics <- function(before, after_info, year_cols,
                                     path = DIAG_HAMPEL_CSV) {
  # Work on a plain data.frame (check.names = FALSE preserves the spaced
  # FAOSTAT column names) so the base `[`/`[[` indexing below behaves as
  # written regardless of whether `before` arrives as a data.table.
  before <- as.data.frame(before, check.names = FALSE, stringsAsFactors = FALSE)
  stopifnot(nrow(before) == nrow(after_info$df))
  
  id_cols <- intersect(
    c("Area Code", "Area", "Item Code", "Item", "Element Code", "Element", "Unit"),
    names(before)
  )
  
  ordered_years <- year_cols[order(as.integer(sub("^Y", "", year_cols)))]
  
  # Reshape the wide before/after frames and the per-position diagnostic
  # matrices into the LONG, one-row-per-cell shape the shared
  # build_hampel_diagnostic() consumes. One row per EVALUATED cell (every
  # cell with a non-NA input value), built year-major then in original row
  # order so the writer's stable sort preserves that order within ties.
  per_year <- lapply(ordered_years, function(y) {
    before_y <- before[[y]]
    keep     <- !is.na(before_y)
    if (!any(keep)) return(NULL)
    
    dt_y <- as.data.table(before[keep, id_cols, drop = FALSE])
    dt_y[, `:=`(
      Year                  = y,
      price                 = before_y[keep],
      price_hampel_filtered = after_info$df[[y]][keep],
      window_median         = after_info$med_win[keep, y],
      series_mad            = after_info$series_mad[keep],
      hampel_flag           = after_info$flags[keep, y]
    )]
    dt_y
  })
  
  diag_long <- rbindlist(per_year)
  
  va_ensure_dir(dirname(path))
  
  build_hampel_diagnostic(
    diag_long,
    key_cols = c(id_cols, "Year"),
    out_path = path
  )
}


# winsorize_mad(): now mad_winsorize() in va_helpers.R. The per-item band is
# built via compute_winsor_stats() (also va_helpers.R) inside
# winsorize_usd_by_item() below, so the band can feed build_winsor_diagnostic().


#' Winsorize USD rows per Item Code using MAD, and write the diagnostic
#'
#' For each item, all (country, year) USD observations are pooled into a
#' single vector and capped at median +/- k * scaled_MAD. The
#' capped values are written back to their original cells.
#'
#' Global-median rows (`Area Code == 5000`) should not be present when
#' calling this function -- they would otherwise contaminate their own
#' MAD calculation. The main pipeline builds them after this step.
#'
#' The band is computed with the shared `compute_winsor_stats()`
#' (00_value_added_helpers.R)
#'
#' @param df USD rows (country level only).
#' @param year_cols Character vector of year columns.
#' @param k Modified Z-score cutoff (defaults to the script-level
#'   WINSOR_MAD_K) passed through to `compute_winsor_stats()`.
#' @param diag_path Output CSV path for the winsor diagnostic.
#' @return A data.table with the same schema and winsorized year values.
winsorize_usd_by_item <- function(df, year_cols, k = WINSOR_MAD_K,
                                  diag_path = DIAG_WINSOR_CSV) {
  id_cols <- intersect(
    c("Area Code", "Area", "Item Code", "Item", "Element Code", "Element", "Unit"),
    names(df)
  )
  
  # Stable row id so clipped values scatter back to the original wide
  # rows in their original order (mirrors the old .row_id round-trip).
  dt <- as.data.table(df)
  dt[, .row_id := .I]
  
  # Wide -> long: ONE row per (row, year) cell. ALL cells are kept here
  # (including NA) so the wide round-trip below is loss-free and the main
  # output is unchanged; the diagnostic is filtered to finite cells later.
  long <- melt(
    dt,
    id.vars         = c(".row_id", id_cols),
    measure.vars    = year_cols,
    variable.name   = "Year",
    value.name      = "price",
    variable.factor = FALSE
  )
  
  # Per-item band, pooled across all (area, year) cells for each Item Code.
  item_stats <- compute_winsor_stats(
    long, by_cols = "Item Code", value_col = "price",
    k = k, min_obs = WINSOR_MIN_OBS
  )
  
  long[item_stats, `:=`(
    lo          = i.lo,
    hi          = i.hi,
    log_space   = i.log_space,
    n_item      = i.n_obs,
    item_center = i.center,
    item_scale  = i.scale
  ), on = "Item Code"]
  
  # Snapshot pre-winsor price and per-entry MAD z-score (in the cap's
  # space) BEFORE clipping, mirroring 13_2.
  long[, price_pre_wins := price]
  long[, mad_z := fifelse(
    !is.finite(item_scale) | item_scale == 0, NA_real_,
    fifelse(log_space == TRUE,
            (log(price) - item_center) / item_scale,
            (price      - item_center) / item_scale)
  )]
  
  # Clip. Reproduces mad_winsorize()'s pmin(pmax(x, lo), hi): NA prices and
  # NA-band items are left untouched (the comparisons are NA -> not selected).
  long[!is.na(lo) & price < lo, price := lo]
  long[!is.na(hi) & price > hi, price := hi]
  
  # Per-entry winsor diagnostic in the shared 13_2 schema, restricted to
  # finite cells (the set the cap was actually evaluated against).
  va_ensure_dir(dirname(diag_path))
  build_winsor_diagnostic(
    long[is.finite(price_pre_wins)],
    key_cols = c(id_cols, "Year"),
    out_path = diag_path
  )
  
  # Scatter clipped prices back to the wide frame, aligned by .row_id,
  # preserving original row order and every non-year column.
  wide <- dcast(long, .row_id ~ Year, value.var = "price")
  idx  <- match(dt$.row_id, wide$.row_id)
  out  <- copy(as.data.table(df))
  for (y in year_cols) set(out, j = y, value = wide[[y]][idx])
  out[]
}


#' Build synthetic global-median USD rows (one per item)
#'
#' For each item, the median USD producer price across all areas is
#' taken for each year. Metadata are copied from the first available
#' USD row per item (smallest Area Code) and overwritten to mark the row
#' synthetic (`Area Code = 5000`, `Area = "Global median"`).
#'
#' @param df A data frame.
#' @param year_cols Character vector of year columns.
#' @return A data.table of synthetic rows in the same column order as `df`.
build_global_median_usd_rows <- function(df, year_cols) {
  dt  <- as.data.table(df)
  usd <- dt[`Element Code` == PRICE_USD]
  
  meta_cols <- setdiff(names(dt), year_cols)
  
  # Template: first row per Item Code after ordering by (Item, Area), so
  # the smallest Area Code wins -- matches arrange |> distinct(.keep_all).
  usd_sorted <- copy(usd)
  setorderv(usd_sorted, c("Item Code", "Area Code"), na.last = TRUE)
  template <- unique(usd_sorted, by = "Item Code")[, ..meta_cols]
  
  medians <- usd[, lapply(.SD, median, na.rm = TRUE), by = "Item Code", .SDcols = year_cols]
  
  global_rows <- merge(template, medians, by = "Item Code", sort = FALSE)   # inner, 1:1
  
  if ("Area Code" %in% names(global_rows)) set(global_rows, j = "Area Code", value = VA_GLOBAL_MEDIAN_AREA_CODE)
  if ("Area"      %in% names(global_rows)) set(global_rows, j = "Area",      value = "Global median")
  if ("Area Code (M49)" %in% names(global_rows)) {
    set(global_rows, j = "Area Code (M49)",
        value = if (is.character(global_rows[["Area Code (M49)"]])) "'5000" else VA_GLOBAL_MEDIAN_AREA_CODE)
  }
  
  set(global_rows, j = "Element Code", value = PRICE_USD)
  if ("Element" %in% names(global_rows)) set(global_rows, j = "Element", value = "Global median producer price (USD/tonne)")
  if ("Unit"    %in% names(global_rows)) set(global_rows, j = "Unit",    value = "USD")
  
  global_rows[, names(dt), with = FALSE]
}


#' Merge historical country rows into target country rows
#'
#' For each `(target_area, source_area)` pair:
#'   - rows from `source_area` are relabelled to `target_area`;
#'   - matching rows (same non-area, non-year columns) are combined with
#'     target values taking precedence and source values filling gaps;
#'   - `source_area` rows are removed from the output.
#'
#' A single source area may appear in multiple merge pairs (e.g.
#' Czechoslovakia -> Czechia and -> Slovakia). Each target independently
#' pulls from the ORIGINAL source rows, so every successor inherits the
#' pre-split series. Source areas are removed only after all merges.
#'
#' @param df A data frame.
#' @param year_cols Character vector of year columns.
#' @param country_merges A table with columns `target_area`/`source_area`.
#' @return A data.table with the listed historical countries merged in.
merge_non_overlapping_countries <- function(df, year_cols, country_merges) {
  df  <- as.data.table(df)
  out <- copy(df)
  nm_out <- names(out)
  
  area_cols  <- intersect(c("Area", "Area Code", "Area Code (M49)"), nm_out)
  merge_cols <- setdiff(nm_out, c(year_cols, area_cols))
  
  for (i in seq_len(nrow(country_merges))) {
    target_area <- country_merges$target_area[i]
    source_area <- country_merges$source_area[i]
    
    target_rows <- out[Area == target_area]
    # Read source rows from the ORIGINAL df so a single source can feed
    # every target it is paired with.
    source_rows <- df[Area == source_area]
    
    if (nrow(source_rows) == 0L) next
    
    # Case 1: no target rows yet -> relabel source rows to target.
    if (nrow(target_rows) == 0L) {
      source_rows <- copy(source_rows)
      set(source_rows, j = "Area", value = target_area)
      
      target_meta <- unique(df[Area == target_area, ..area_cols])
      if (nrow(target_meta) > 0L) {
        for (col in area_cols) set(source_rows, j = col, value = target_meta[[col]][1])
      }
      
      out <- rbindlist(list(out, source_rows[, ..nm_out]), use.names = TRUE)
      next
    }
    
    # Case 2: both exist -> full-join on the shared identifier columns,
    # stitch year values with coalesce(tgt, src), and force every area
    # identifier to the target's values.
    target_meta <- unique(target_rows[, ..area_cols])
    
    merged <- merge(
      target_rows, source_rows,
      by = merge_cols, all = TRUE,
      suffixes = c("_tgt", "_src"), sort = FALSE
    )
    
    # area_cols are not in the join, so they arrive suffixed; (re)create
    # the bare columns set to the target's values.
    for (col in area_cols) set(merged, j = col, value = target_meta[[col]][1])
    
    for (y in year_cols) {
      yt <- paste0(y, "_tgt")
      ys <- paste0(y, "_src")
      if (yt %in% names(merged) && ys %in% names(merged)) {
        set(merged, j = y, value = fcoalesce(merged[[yt]], merged[[ys]]))
      } else if (yt %in% names(merged)) {
        set(merged, j = y, value = merged[[yt]])
      } else if (ys %in% names(merged)) {
        set(merged, j = y, value = merged[[ys]])
      } else {
        set(merged, j = y, value = NA_real_)
      }
    }
    
    merged <- merged[, c(area_cols, merge_cols, year_cols), with = FALSE]
    merged <- merged[, ..nm_out]
    
    # Drop only the old target rows; keep source rows for later pairs.
    out <- out[Area != target_area]
    out <- rbindlist(list(out, merged), use.names = TRUE)
  }
  
  # Drop every source area referenced by any merge pair.
  source_areas_used <- unique(country_merges$source_area)
  out[!(Area %in% source_areas_used)]
}


# ---- Main pipeline ---------------------------------------------------------

#' Build the final producer-price dataset with USD estimates
#'
#' See the step list in the file header. Year-window handling: SLC/PPI
#' imputation uses ALL raw years; the working window (BUFFER U KEEP) is
#' applied at step 6b and carried through Hampel; KEEP_YEARS is applied at
#' step 12b before winsorization and global medians.
#'
#' @return A data.table with the final processed dataset.
main <- function() {
  # Read the Normalized prices file and reshape to wide. annual_only is
  # FALSE here: the Months column is preserved so the next step's
  # `Months == "Annual value"` filter behaves exactly as before.
  producer_prices_raw <- read_fao_normalized_wide(INPUT_PRICES_CSV)
  
  # 2. Keep annual values only
  df <- producer_prices_raw[Months == "Annual value"]
  
  # 3. Drop flag columns
  df <- drop_flag_columns(df)
  
  # 4. Detect year columns and coerce ALL of them to numeric. We keep
  #    years outside KEEP_YEARS available for the SLC/PPI ratio step.
  all_year_cols <- get_year_cols(df)
  df <- coerce_numeric(df, all_year_cols)
  
  # 5. Drop LCU rows that are shadowed by an SLC row
  df <- drop_lcu_when_slc_exists(df)
  
  # 6. Fill missing SLC values from PPI, using every year of overlap.
  df <- fill_slc_from_ppi(df, all_year_cols)
  
  # 6b. Restrict to WORKING_YEARS (BUFFER_YEARS U KEEP_YEARS).
  working_year_cols <- intersect(paste0("Y", WORKING_YEARS), all_year_cols)
  drop_yc <- setdiff(all_year_cols, working_year_cols)
  if (length(drop_yc)) df[, (drop_yc) := NULL]
  
  # 7. Load exchange rates (working window, keyed by Area Code, _xr suffix)
  xr <- prepare_exchange_rates(INPUT_XR_CSV, working_year_cols)
  
  # 8. Fill existing USD rows using SLC / exchange-rate
  df <- fill_existing_usd_rows(df, xr, working_year_cols)
  
  # 9. Create USD rows for (Area, Item) pairs with SLC but no USD row
  usd_new <- build_missing_usd_rows(df, xr, working_year_cols)
  out     <- rbindlist(list(df, usd_new), use.names = TRUE)
  
  # 10. Merge historical countries into their successors
  out <- merge_non_overlapping_countries(out, working_year_cols, COUNTRY_MERGES)
  
  # 11. Keep USD rows only -- SLC and PPI were only scaffolding
  out <- out[`Element Code` == PRICE_USD]
  
  # 12. Hampel filter on each (Area, Item) USD time series, on the full
  #     working window so edge observations see a full +/- window.
  out_pre_hampel <- copy(out)
  hampel_result  <- hampel_usd_rows(out, working_year_cols)
  out            <- hampel_result$df
  write_hampel_diagnostics(out_pre_hampel, hampel_result, working_year_cols)
  
  # 12b. Drop buffer years. Everything from here operates on KEEP_YEARS.
  year_cols <- intersect(paste0("Y", KEEP_YEARS), working_year_cols)
  drop_buf  <- setdiff(working_year_cols, year_cols)
  if (length(drop_buf)) out[, (drop_buf) := NULL]
  
  # 13. MAD-based winsorization per Item Code.
  out <- winsorize_usd_by_item(out, year_cols, k = WINSOR_MAD_K)
  
  # 14. Synthetic global-median USD rows from the winsorized country data.
  global_median_usd <- build_global_median_usd_rows(out, year_cols)
  out               <- rbindlist(list(out, global_median_usd), use.names = TRUE)
  
  # 15. Stable sort on (Area Code, Item Code, Element Code). na.last = TRUE
  #     reproduces dplyr::arrange()'s NA-last ordering (the keys are
  #     non-NA identifiers, so this only matters for exact-tie stability).
  setorderv(out, c("Area Code", "Item Code", "Element Code"), na.last = TRUE)
  out[]
}


# ---- Run & write output ----------------------------------------------------

producer_prices_with_usd <- main()

va_ensure_dir(dirname(OUTPUT_PRICES_CSV))

# Written with data.table::fwrite() to match the rest of the VA pipeline
# (the 14_* CSV outputs and this script's own diagnostics all use fwrite).
# na = "" reproduces the previous write's empty-string NA encoding.
fwrite(
  producer_prices_with_usd,
  OUTPUT_PRICES_CSV,
  na = ""
)