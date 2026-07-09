# ==============================================================================
# Trade-derived producer prices at SUA level (bilateral_trade_prices.rds)
# ==============================================================================
#
# Description:
#   Builds per-country unit prices (USD / unit) from bilateral trade data,
#   aggregated on the exporter side, at SUA item granularity.
#
#   PHASE 1 -- SUA-level BTD construction
#     Starts from btd_sua_tidy.rds (item-disaggregated, pre-TCF,
#     pre-CBS-aggregation) and only:
#       * adds ethanol from BACI (HS 2207*)
#       * collapses non-current regions into RoW; drops RoW-internal flows
#       * drops tonnes rows for live-animal items (keeps head/An/1000 An, usd)
#
#   PHASE 2 -- Price pipeline
#     1. Reshape BTD wide on `unit` at the bilateral-flow level and keep
#        only flows with positive USD and at least one positive quantity.
#     2. Drop bilateral flows with tiny quantities (unreliable unit prices).
#     3. Aggregate to (exporter x item x year): sum(USD) / sum(quantity).
#     4. Hampel filter on each (exporter x item) time series (k = 3 MAD),
#        applied across 2 passes to handle masking from co-located spikes.
#     5. Per item (all countries and years pooled): log-normality check,
#        then MAD winsorization at median +/- k*MAD with k = 2.5
#        (equivalent to a robust |z| <= 2.5 cap). A tighter cap than the
#        Hampel step because this one is applied cross-sectionally rather
#        than along a single country-item series. Pooling across years
#        trades some bias for variance: a single cap band covers the
#        whole time series, but the MAD is well-defined for almost every
#        item, which is rarely the case at the item-year level.
#     6. Fill empty (area x item x year) cells with the item-year median
#        (falling back to the item-overall median).
#     7. Apply manual price caps for specific items as a final safety net
#        for cases where statistical filters still leave implausible prices.
#
# Glossary of grouping levels used below:
#   item-year    = one combination of (item_code, year); used for the
#                  first-choice median fallback in step 6.
#   item         = one item_code pooled across all countries and years;
#                  used for the cross-sectional winsorization in step 5
#                  and for the second-choice median fallback in step 6.
#   exporter x item = one country-item time series over years; used as
#                  the unit for the Hampel filter in step 4.
#
# Inputs:
#   data/tidy/btd_sua_tidy.rds  (SUA-level BTD, pre-TCF, pre-CBS-aggregation)
#   data/tidy/baci_tidy.rds     (used for ethanol HS 2207*)
#   inst/regions_full.csv
#   conc_btd-cbs.csv       (BTD<->CBS concordance, both ISIC sides in
#                                one table; the `isic` column "A"/"C" marks
#                                the side. Split into ISIC-A / ISIC-C views
#                                on load. Used for SUA-grain manual price
#                                caps in step 7 and CBS-grain overrides in
#                                section 7.5)
#
# Outputs:
#   output/bilateral_trade_prices.rds
#     columns: area_code, area, item_code, item, year, price, price_source,
#              own_series_gate_rejected
#     `price` is USD per unit of trade (tonne / head / 1000 head),
#     matching how the item is measured in BTD. Item granularity is SUA.
#     `price_source` tags each cell's provenance:
#       "trade_direct"       -- aggregated from bilateral trade for that
#                               (area, item, year).
#       "own_series_median"  -- no direct trade for that (area, item, year),
#                               but the series has direct obs in other years;
#                               filled with that series' own median.
#       "item_year_median"   -- no own-series median usable; filled with the
#                               cross-country median for that (item, year).
#       "item_median"        -- also no item-year median available; filled with
#                               the overall median for that item.
#       "none"               -- no trade data anywhere for this item; price is NA.
#     If the final manual cap (step 7) clipped the value, "_capped" is
#     appended to whichever of the above applied (e.g. "trade_direct_capped").
#     `own_series_gate_rejected` is TRUE where a series had its own obs but the
#     optional gate routed the cell to the cross-sectional median instead.
#   output/diagnostics/trade_prices_hampel_entries.csv
#     One row per (from_code, item_code, year) fed into the Hampel filter.
#     Carries pre- and post-filter price (pre = raw input to pass 1, post =
#     output of the final pass), the rolling-window median, the per-series
#     MAD that is the spike-test scale (`series_mad`, also reported as
#     `mad_used`), `hampel_z` based on the series MAD, the `is_spike` flag (TRUE
#     if flagged in ANY pass), and `abs_change`. Sorted by |hampel_z|
#     descending so flagged points float to the top.
#   output/diagnostics/trade_prices_winsorized_entries.csv
#     One row per (from_code, item_code, year) analysed by the per-item
#     MAD cap. Carries price_pre / price_post, the cap band, whether the
#     cap was evaluated in log space, per-entry mad_z, and a winsorized
#     flag. Sorted by |mad_z| descending.
#   output/bilateral_trade_prices_cbs_override.rds
#     CBS-grain price override for residual CBS items whose default SUA-
#     grain decomposition (script 13_3 sections 6.5c / 6.5d) is too thin to
#     give a stable price. Computed at CBS grain via sum(USD) / sum(qty
#     / tcf) over each item's BTD constituents (per the matching ISIC
#     concordance), then run through the SAME Phase-2 pipeline as the
#     SUA-grain prices: two-pass Hampel filter per (area x cbs) series
#     -> per-CBS-item cross-sectional MAD winsorization (k = 2.5) ->
#     gap-fill with cbs-year / cbs-overall medians -> manual cap. See
#     section 7.5 for details and the matching CBS-grain Hampel/winsor
#     diagnostic CSVs (output/diagnostics/trade_prices_cbs_*).
#     columns: area_code, year, price, price_source, cbs_item_code, isic_side,
#              own_series_gate_rejected
#     `isic_side` ("a" or "c") tells script 13_3 which residual table the
#     row should overwrite. `price_source` uses the prefix
#     "trade_cbs_*" to distinguish from the SUA-grain provenance labels
#     (including "trade_cbs_own_series_median" for own-series fills).
#   output/diagnostics/trade_prices_cbs_hampel_entries.csv
#   output/diagnostics/trade_prices_cbs_winsorized_entries.csv
#     CBS-grain analogues of the SUA-grain diagnostic CSVs above; same
#     schema, with `cbs_item_code` / `cbs_item` in place of `item_code`
#     / `item`. One row per (area_code, cbs_item_code, year) seen by
#     each filter.
# ==============================================================================

library("data.table")

# Centralised config: resolves every path from FABIO_ROOT and sources the
# value-added helpers. Assumes the working directory is the FABIO repo root
# (FABIO convention; see R/00_value_added_config.R).
source("R/00_value_added_config.R")
# FABIO-core tidy utilities (dt_rename, dt_filter, replace_RoW, na_sum). Sourced
# directly here (not via va_config) so the fabio-core dependency stays confined
# to this one script — see the dependency note in va_helpers.R.
source(FABIO_TIDY_FUNCTIONS_PATH)

# Hampel filter parameters. Assigned FROM the central constants in
# R/00_value_added_config.R (section 5) so this script and
# 13_1_FAOstat_producer_prices_USD.R — and the 14_ VA stages — can never drift
# apart. With half-window = 3 the filter needs at least 2*hw + 1 = 7 finite
# observations per series; shorter series pass through unchanged. Both call
# sites (SUA grain in step 4, CBS grain in step 7.5) reference these.
HAMPEL_HALF_WINDOW <- VA_HAMPEL_HALF_WINDOW   # rolling-median half-window
HAMPEL_THRESHOLD   <- VA_HAMPEL_THRESHOLD     # robust-z spike cutoff

# Hampel pass count. Bilateral trade series frequently carry CO-LOCATED spikes
# (two adjacent outliers), where the first pass's rolling median is itself pulled
# by a surviving neighbour; a second pass catches what the first masked. This is
# a deliberate per-caller choice unique to 13_2 (13_1 / 14_1 use the single-pass
# filter); it is named here rather than left as a bare `2L` literal so it is
# auditable, mirroring WINSOR_MAD_K. See the pass-count note in
# R/00_value_added_config.R section 5.
HAMPEL_PASSES      <- 2L

# Extra commodities the fabio_bcp repo prices from this output but that are not
# in FABIO's 123-item list. Hard-coded here (no dependency on the bcp item
# list) and UNIONed into the step-6 grid so the RDS always carries a row for
# each, even in years with no direct flow (item-median gap-fill then applies).
BCP_EXTRA_ITEMS <- data.table(
  item_code = c(97L, 165L, 265L, 266L, 1274L, 654L),
  item      = c("Triticale", "Molasses", "Castor oil seeds",
                "Oil of castor beans",
                "Animal or vegetable fats and oils and their fractions, chemically modified",
                "Brewing or distilling dregs and waste")
)


# ------------------------------------------------------------------------------
# Inputs and configuration
# ------------------------------------------------------------------------------

regions <- fread(VA_FABIO_REGIONS_CSV)[current == TRUE]

btd  <- readRDS(VA_FABIO_BTD_SUA_TIDY_BUFFERED_RDS)
baci <- readRDS(VA_FABIO_BACI_TIDY_RDS)

# BTD<->CBS concordance. A single combined table now carries both ISIC
# sides; the `isic` column ("A" / "C") encodes the side that used to be
# split across two files (conc_btd-cbs_isic-a.csv / -c.csv). We load it
# once and split it back into the `isic_a` / `isic_c` views the rest of
# the script consumes. Used twice:
#   * step 7        -- expand CBS-grain manual price caps to SUA grain
#                      via the union of both sides.
#   * section 7.5   -- pick constituents for each CBS override item
#                      (`overrides_cfg$isic_side` selects which side).
isic_conc <- fread(
  VA_CONC_BTD_CBS_ISIC,
  na.strings = c("", "NA")
)
# cbs_item_code arrives as a double (e.g. 2511.0); coerce to integer to
# match the integer CBS codes used in overrides_cfg / price_caps_cbs and
# to avoid type-mismatch warnings on the downstream joins.
isic_conc[, cbs_item_code := as.integer(cbs_item_code)]

isic_a <- isic_conc[isic == "A"]
isic_c <- isic_conc[isic == "C"]

OUTPUT_DIR       <- VA_PRICE_OUTPUT_DIR
OUTPUT_RDS_PATH  <- VA_BILATERAL_TRADE_PRICES_RDS
DIAG_DIR         <- VA_PRICE_DIAG_DIR
DIAG_HAMPEL      <- file.path(DIAG_DIR, "trade_prices_hampel_entries.csv")
DIAG_WINSOR      <- file.path(DIAG_DIR, "trade_prices_winsorized_entries.csv")

va_ensure_dir(OUTPUT_DIR)
va_ensure_dir(DIAG_DIR)


# ==============================================================================
# PHASE 1 -- SUA-level BTD construction (was 02a)
# ==============================================================================

# ------------------------------------------------------------------------------
# Ethanol
# ------------------------------------------------------------------------------

cat("\nAdding ethanol trade data.\n")

eth <- baci[grep("^2207[0-9]*$", item_code), ]
eth[, `:=`(item = "Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher", item_code = 632)]

eth <- dt_rename(eth, drop = FALSE,
                 rename = c("exporter" = "from", "exporter_code" = "from_code",
                            "importer" = "to",   "importer_code" = "to_code"))


# ------------------------------------------------------------------------------
# Merge
# ------------------------------------------------------------------------------

btd <- rbindlist(list(btd, eth), use.names = TRUE)


# ------------------------------------------------------------------------------
# RoW handling
# ------------------------------------------------------------------------------

# Collapse non-current regions into RoW. `regions` is already filtered
# to current == TRUE above, so regions$code gives the current-region
# whitelist.
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
                   codes = regions$code)

# Drop RoW-internal flows
btd <- dt_filter(btd, from_code != to_code)


# ------------------------------------------------------------------------------
# Live animals: drop tonnes, keep head/An/1000 An and usd
# ------------------------------------------------------------------------------

# Live-animal items identified as those reported in count-based units.
# btd_sua_tidy carries FAO unit labels verbatim ("Head", "An", "1000 An").
count_units <- c("head", "Head", "An", "1000 An")
live_items  <- unique(btd[unit %in% count_units, item_code])

btd <- btd[!(item_code %in% live_items & unit == "tonnes")]


# ------------------------------------------------------------------------------
# Aggregate values
# ------------------------------------------------------------------------------

# RoW collapse can produce duplicate (from, to, item, year, unit) keys; sum them.
key_cols <- setdiff(names(btd), "value")
btd <- btd[, .(value = na_sum(value)), by = key_cols]

setorder(btd, year)


# ==============================================================================
# PHASE 2 -- Price pipeline (was 02b)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Reshape to wide-by-unit at the bilateral flow level
# ------------------------------------------------------------------------------

prices <- dcast(
  btd[unit %in% c("usd", "tonnes", "An", "1000 An")],
  from + from_code + to + to_code + item + item_code + year ~ unit,
  value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0
)

# Keep only bilateral flows with positive USD and at least one positive qty
prices <- prices[usd > 0 & (tonnes > 0 | An > 0 | `1000 An` > 0)]


# ------------------------------------------------------------------------------
# 2. Drop bilateral flows with tiny quantities
# ------------------------------------------------------------------------------
# A handful of very expensive live specimens or a few kg of a luxury good
# can give absurd unit prices that then distort the exporter-year average.

qty_min_tonnes <- 1    # tonnes
qty_min_an     <- 10   # head of livestock
qty_min_1000an <- 1    # thousand head (e.g. poultry)

prices[, qty_used := fcase(
  tonnes    > 0, "tonnes",
  An        > 0, "An",
  `1000 An` > 0, "1000 An",
  default = NA_character_
)]

n_before <- nrow(prices)
prices <- prices[
  (qty_used == "tonnes"  & tonnes    >= qty_min_tonnes) |
    (qty_used == "An"      & An        >= qty_min_an)     |
    (qty_used == "1000 An" & `1000 An` >= qty_min_1000an)
]
cat("Dropped ", n_before - nrow(prices),
    " bilateral price observations below minimum quantity thresholds.\n", sep = "")
prices[, qty_used := NULL]


# ------------------------------------------------------------------------------
# 3. Aggregate to exporter-item-year: sum(USD) / sum(quantity)
# ------------------------------------------------------------------------------
# Preference order for the denominator: tonnes > An > 1000 An.
# This yields a quantity-weighted export unit value per country-item-year.

prices_exporter <- prices[, .(
  usd       = sum(usd,       na.rm = TRUE),
  tonnes    = sum(tonnes,    na.rm = TRUE),
  An        = sum(An,        na.rm = TRUE),
  `1000 An` = sum(`1000 An`, na.rm = TRUE)
), by = .(from_code, from, item_code, item, year)]

prices_exporter[, price := fcase(
  tonnes    > 0, usd / tonnes,
  An        > 0, usd / An,
  `1000 An` > 0, usd / `1000 An`,
  default = NA_real_
)]

prices_exporter <- prices_exporter[
  is.finite(price) & price > 0,
  .(from_code, from, item_code, item, year, price)
]


# ------------------------------------------------------------------------------
# 4. Hampel filter on each (exporter x item) time series, applied in 2 passes
# ------------------------------------------------------------------------------
# Each (exporter x item) series is run through the shared `hampel_filter()`
# / `hampel_filter_iterate()` (from va_helpers.R). The rule: flag a value
# more than `threshold` robust-z from its rolling-window MEDIAN, where the
# scale is the SERIES-level MAD (one value per series, used at every
# position). Flagged points are replaced with the window median; series
# with fewer than `min_obs` observations pass through unchanged.
#
# Using the series MAD as the sole scale means an extreme value at the very
# first/last year is still measured against a sane, positive scale -- the
# old per-window MAD could collapse to 0 in a near-constant edge window and
# let such a value through silently. See va_helpers.R section 2.
#
# Two passes (n_passes = 2): a second pass catches co-located spikes that
# masked each other on the first pass and re-cleans imputations whose window
# median was pulled by a surviving spike. Flags are UNIONED across passes
# (is_spike = flagged in ANY pass); window stats reflect the final pass.

setorder(prices_exporter, from_code, item_code, year)
prices_exporter[, c("price_hampel_filtered", "hampel_flag",
                    "window_median", "series_mad") := {
                      r <- hampel_filter_iterate(price,
                                                 half_window = HAMPEL_HALF_WINDOW,
                                                 threshold   = HAMPEL_THRESHOLD,
                                                 n_passes = HAMPEL_PASSES)
                      list(r$values, r$is_spike, r$window_median,
                           r$series_mad)
                    }, by = .(from_code, item_code)]

n_series   <- prices_exporter[, uniqueN(paste(from_code, item_code))]
n_eligible <- prices_exporter[, .N, by = .(from_code, item_code)][N >= 7L, .N]
n_flagged  <- prices_exporter[hampel_flag == TRUE, .N]

cat("\nHampel filter diagnostics (two-pass)\n")
cat("  Time series (exporter x item):     ", n_series,   "\n", sep = "")
cat("  Eligible (>= 7 obs):               ", n_eligible, "\n", sep = "")
cat("  Observations flagged (any pass):   ", n_flagged,  "\n", sep = "")

if (n_flagged > 0) {
  top_hampel <- prices_exporter[hampel_flag == TRUE, .N,
                                by = .(item_code, item)][order(-N)]
  cat("  Top items by Hampel flags:\n")
  print(head(top_hampel, 10L))
}

# build_hampel_diagnostic() (va_helpers.R) writes one row per key-tuple the
# filter saw -- not just the flagged ones. `dt` must carry: price,
# price_hampel_filtered, window_median, series_mad, hampel_flag. The CSV reports
# series_mad as both the scale and `mad_used`,
# hampel_z = (price_pre - window_median)/series_mad, is_spike (union across
# passes), sorted by |hampel_z| then |abs_change| desc.

build_hampel_diagnostic(prices_exporter,
                        c("from_code", "from", "item_code", "item", "year"),
                        DIAG_HAMPEL)
cat("  Diagnostic written:                ", DIAG_HAMPEL, "\n", sep = "")

prices_exporter[, price := price_hampel_filtered]
prices_exporter[, c("price_hampel_filtered", "hampel_flag",
                    "window_median", "series_mad") := NULL]

# Drop the Hampel buffer years; winsorization, gap-fill medians and the output
# grid below run on the keep window only.
prices_exporter <- prices_exporter[year %in% VA_KEEP_YEARS]


# ------------------------------------------------------------------------------
# 5. Per item (pooled across countries and years): log-normality check,
#    then MAD winsorization
# ------------------------------------------------------------------------------
# For each item (pooling all country-year observations), compare
# `price` vs `log(price)` and work in whichever space is
# more symmetric. Winsorize at median +/- WINSOR_MAD_K * MAD in that
# space, then back-transform. Items with fewer than WINSOR_MIN_OBS
# observations are left untouched.
#
# Pooling across years trades some bias for variance: one cap band
# covers the whole time series, but the MAD is well-defined for almost
# every item, which is rarely the case at the item-year level. The
# log-space option absorbs the multiplicative part of any time trend
# (inflation, scaling) so the cap is not driven purely by trend; a
# strong residual linear trend would, however, push tail years closer
# to the cap than they would be under per item-year processing.
#
# With WINSOR_MAD_K = 2.5 this caps entries whose robust z-score exceeds
# 2.5 in absolute value -- a tighter boundary than the classic 3-MAD rule.

WINSOR_MAD_K   <- 2.5   # cap at median +/- WINSOR_MAD_K * MAD (robust |z|)

# Cross-sectional MAD winsorization stats per group. Returns one row per
# distinct combination of `by_cols` with the cap band [lo, hi] and the
# center/scale that built it. Per group, log-space is chosen iff
# log(price) is more symmetric than price; the cap is built in
# whichever space was chosen and back-transformed for `lo` / `hi`.
# Groups with fewer than `min_obs` positive prices, or whose MAD is
# degenerate (NA / 0), carry NA caps and pass through unchanged when
# applied downstream.
# compute_winsor_stats(): now in va_helpers.R

item_stats <- compute_winsor_stats(prices_exporter, c("item_code", "item"), k = WINSOR_MAD_K, min_obs = WINSOR_MIN_OBS)

prices_exporter[item_stats, `:=`(
  lo           = i.lo,
  hi           = i.hi,
  log_space    = i.log_space,
  n_item       = i.n_obs,
  item_center  = i.center,
  item_scale   = i.scale
), on = .(item_code, item)]

# Snapshot pre-winsor price and per-entry MAD z-score BEFORE the cap is
# applied. mad_z is evaluated in the same space as the cap (log or linear,
# per log_space) so the sign and magnitude are directly comparable to the
# implied |z| = WINSOR_MAD_K threshold. Rows with no finite scale (short
# items, or zero MAD) carry mad_z = NA and cannot be winsorized.
prices_exporter[, price_pre_wins := price]
prices_exporter[, mad_z := fifelse(
  !is.finite(item_scale) | item_scale == 0, NA_real_,
  fifelse(log_space == TRUE,
          (log(price) - item_center) / item_scale,
          (price      - item_center) / item_scale)
)]

n_winsorized <- prices_exporter[!is.na(lo) & (price < lo | price > hi), .N]
prices_exporter[!is.na(lo) & price < lo, price := lo]
prices_exporter[!is.na(hi) & price > hi, price := hi]

cat("\nMAD winsorization diagnostics (k = ", WINSOR_MAD_K, ")\n", sep = "")
cat("  Items processed:                    ", nrow(item_stats),                     "\n", sep = "")
cat("  Items winsorized in log space:      ", sum(item_stats$log_space, na.rm=TRUE), "\n", sep = "")
cat("  Items with insufficient obs:        ", sum(is.na(item_stats$log_space)),     "\n", sep = "")
cat("  Observations winsorized:            ", n_winsorized,                          "\n", sep = "")

# Build and write a per-entry winsor diagnostic CSV.
# One row per (key_cols-tuple) that the per-group cap was evaluated
# against -- NOT just the clipped ones. Rows in groups with
# < WINSOR_MIN_OBS observations, or whose group-level scale was
# degenerate, carry NA caps / NA mad_z and have winsorized = FALSE (the
# cap conditions naturally fail on NA).
#
# `dt` must carry: price_pre_wins (snapshot before cap), price (after
# cap), lo, hi, log_space, n_item, mad_z. `key_cols` is a character
# vector of identifier columns placed at the front of the CSV.
#
# Sort is |mad_z| then |abs_change| descending, matching 01a's convention.
# build_winsor_diagnostic(): now in va_helpers.R

build_winsor_diagnostic(prices_exporter,
                        c("from_code", "from", "item_code", "item", "year"),
                        DIAG_WINSOR)
cat("  Diagnostic written:                 ", DIAG_WINSOR, "\n", sep = "")

prices_exporter[, c("lo", "hi", "log_space", "n_item",
                    "item_center", "item_scale",
                    "price_pre_wins", "mad_z") := NULL]


# ------------------------------------------------------------------------------
# 6. Build the (area x item x year) grid and fill gaps with item-year median
# ------------------------------------------------------------------------------

# Items at SUA level. Derived from the SUA-level `btd` built in Phase 1, so
# the grid covers every SUA item that appears in trade at least once. If you
# want a complete grid over a master SUA item list (analogous to 02b's use
# of items_full_123.csv -- i.e. including items with zero trade), load that
# file here and use it instead, e.g.:
#   items <- fread("/home/bruckner2/fabio/inst/items_full_123.csv")[, .(item_code, item)]
# BCP_EXTRA_ITEMS (top of file) is UNIONed in so the fabio_bcp extra items
# always get an output row; traded labels win, forced-in codes keep their own.
items <- unique(btd[, .(item_code, item)])
items <- rbindlist(list(items,
                        BCP_EXTRA_ITEMS[!item_code %in% items$item_code]),
                   use.names = TRUE)

all_codes <- union(regions$code, unique(prices_exporter$from_code))
all_years <- sort(intersect(unique(btd$year), VA_KEEP_YEARS))

grid <- CJ(area_code = all_codes,
           item_code = items$item_code,
           year      = all_years)
grid[items,   item := i.item, on = "item_code"]
grid[regions, area := i.name, on = c(area_code = "code")]
grid[area_code == 999 & is.na(area), area := "RoW"]

bilateral_trade_prices <- merge(
  grid,
  prices_exporter[, .(area_code = from_code, item_code, year, price)],
  by = c("area_code", "item_code", "year"),
  all.x = TRUE
)

# Tag the fill source as we go: "trade_direct" if the cell already has a
# price from the bilateral aggregation, otherwise set when a median fallback
# is applied. Unlike the Hampel / winsor intermediates, this column is kept
# in the saved RDS -- it's small, categorical, and directly annotates the
# adjacent `price`, so downstream consumers can filter on provenance (e.g.
# exclude "item_median" rows for sensitive analyses) without re-joining to
# a separate diagnostic.
bilateral_trade_prices[, price_source := fifelse(!is.na(price),
                                                 "trade_direct", NA_character_)]
bilateral_trade_prices[, own_series_gate_rejected := FALSE]

# Fallback medians: first by (item, year), then by item alone. Kept in
# named columns so the fifelse fills below read cleanly.
median_by_item_year <- prices_exporter[, .(price_item_year_median = median(price, na.rm = TRUE)),
                                       by = .(item_code, year)]
median_by_item      <- prices_exporter[, .(price_item_median      = median(price, na.rm = TRUE)),
                                       by = .(item_code)]

bilateral_trade_prices[median_by_item_year,
                       price_item_year_median := i.price_item_year_median,
                       on = .(item_code, year)]
bilateral_trade_prices[median_by_item,
                       price_item_median := i.price_item_median,
                       on = "item_code"]

n_missing_initial <- bilateral_trade_prices[is.na(price), .N]

# Own-series median, prioritised above the cross-sectional medians: fill each
# missing cell with the median of its own (area, item) direct observations.
if (PRICE_PREFER_OWN_SERIES_MEDIAN) {
  own_main <- own_series_median_fill(
    prices_exporter[, .(area_code = from_code, item_code, price)],
    series_cols = c("area_code", "item_code"), item_col = "item_code",
    winsor_stats = item_stats)
  bilateral_trade_prices[own_main, `:=`(own_med = i.own_med,
                                        own_reject = i.gate_rejected),
                         on = c("area_code", "item_code")]
  bilateral_trade_prices[is.na(price) & !is.na(own_med) & !own_reject,
                         `:=`(price = own_med, price_source = "own_series_median")]
  bilateral_trade_prices[is.na(price) & !is.na(own_med) & own_reject,
                         own_series_gate_rejected := TRUE]
  bilateral_trade_prices[, c("own_med", "own_reject") := NULL]
}
n_after_own        <- bilateral_trade_prices[is.na(price), .N]
n_filled_own       <- n_missing_initial - n_after_own

bilateral_trade_prices[is.na(price) & !is.na(price_item_year_median),
                       `:=`(price = price_item_year_median,
                            price_source = "item_year_median")]
n_after_item_year  <- bilateral_trade_prices[is.na(price), .N]
n_filled_item_year <- n_after_own - n_after_item_year
bilateral_trade_prices[is.na(price) & !is.na(price_item_median),
                       `:=`(price = price_item_median,
                            price_source = "item_median")]
n_filled_item      <- n_after_item_year - bilateral_trade_prices[is.na(price), .N]
bilateral_trade_prices[is.na(price_source), price_source := "none"]

cat("\nGap-filling diagnostics\n")
cat("  Grid cells (area x item x year):        ", nrow(bilateral_trade_prices),                        "\n", sep = "")
cat("  Cells filled from trade data directly:  ", nrow(bilateral_trade_prices) - n_missing_initial,    "\n", sep = "")
cat("  Cells filled from own-series median:     ", n_filled_own,                                       "\n", sep = "")
cat("  Cells filled from item-year median:     ", n_filled_item_year,                                  "\n", sep = "")
cat("  Cells filled from item median:          ", n_filled_item,                                       "\n", sep = "")
cat("  Own-series gate-rejected (-> x-section):", bilateral_trade_prices[own_series_gate_rejected == TRUE, .N], "\n", sep = "")
cat("  Cells still missing (no trade at all):  ", bilateral_trade_prices[is.na(price), .N],            "\n", sep = "")

# Before/after fill-mix: `after` is the realised price_source; `before` is the
# mix the cross-sectional-only ladder would have produced (own_series_median
# cells reassigned to the item-year / item median rung they would have hit).
after_mix  <- bilateral_trade_prices[, .(n_after = .N), by = price_source]
before_src <- bilateral_trade_prices[, fifelse(
  price_source == "own_series_median",
  fcase(!is.na(price_item_year_median), "item_year_median",
        !is.na(price_item_median),      "item_median",
        default = "none"),
  price_source)]
before_mix <- data.table(price_source = before_src)[, .(n_before = .N), by = price_source]
fill_mix   <- merge(before_mix, after_mix, by = "price_source", all = TRUE)
fill_mix[is.na(n_before), n_before := 0L][is.na(n_after), n_after := 0L]
setorder(fill_mix, -n_after)
cat("\nFill-mix (price_source counts) before vs after own-series rung\n")
print(fill_mix)

bilateral_trade_prices[, c("price_item_year_median", "price_item_median") := NULL]


# ------------------------------------------------------------------------------
# 7. Manual price caps for specific items
# ------------------------------------------------------------------------------
# Manual price caps. Defined at CBS grain because that's the level the
# limits were chosen against, then expanded to the SUA grain that
# `bilateral_trade_prices` is keyed on via the UNION of the ISIC-A and
# ISIC-C BTD<->CBS concordances. Some cap items are primary commodities
# living in ISIC-A (Horses 1096, Palm kernels 2562, ...); others are
# processed products living in ISIC-C only (Ricebran Oil 2581,
# Sesameseed Oil 2579), so we need both. A CBS that maps to several
# BTD sub-categories in the same concordance (e.g. cbs 2029 Poultry
# Birds -> btd 1057 / 1068 / 1072 / 1079 / 1083 in ISIC-A) gets the
# same cap applied to every sub-category. Pairs that happen to appear
# in both concordances are deduped on (cbs_item_code, btd_item_code)
# so the cap fires once per BTD code.
price_caps_cbs <- fread('
cbs_item_code,cbs_item,price_limit
2562,"Palm kernels",1500
2581,"Ricebran Oil",5000
2579,"Sesameseed Oil",6000
1096,"Horses",500
1107,"Asses",300
1110,"Mules",300
1126,"Camels",300
2029,"Poultry birds",5000
1140,"Rabbits and hares",8000
1150,"Rodents, other",8000
2000,"Fodder crops",25
1157, "Camelids, other",200
')

isic_for_caps <- unique(rbindlist(list(
  isic_a[!is.na(cbs_item_code) & !is.na(btd_item_code),
         .(cbs_item_code, btd_item_code, btd_item)],
  isic_c[!is.na(cbs_item_code) & !is.na(btd_item_code),
         .(cbs_item_code, btd_item_code, btd_item)]
)), by = c("cbs_item_code", "btd_item_code"))

# Warn if any CBS in price_caps_cbs has no mapping in either concordance
# (would be silently dropped otherwise).
unmapped <- setdiff(price_caps_cbs$cbs_item_code, isic_for_caps$cbs_item_code)
if (length(unmapped) > 0) {
  warning(
    "price_caps_cbs entries with no ISIC-A/C mapping (cap NOT applied): ",
    paste(price_caps_cbs[cbs_item_code %in% unmapped,
                         paste0(cbs_item_code, " ", cbs_item)],
          collapse = "; ")
  )
}

price_caps <- merge(
  price_caps_cbs[, .(cbs_item_code, price_limit)],
  isic_for_caps[, .(cbs_item_code, item_code = btd_item_code, item = btd_item)],
  by = "cbs_item_code"
)[, .(item_code, item, price_limit)]

bilateral_trade_prices <- merge(bilateral_trade_prices,
                                price_caps[, .(item_code, price_limit)],
                                by = "item_code", all.x = TRUE)

n_capped <- bilateral_trade_prices[
  !is.na(price_limit) & !is.na(price) & price > price_limit, .N
]
# Breakdown of which fill rung the capped cells came from, taken before the
# mutate appends the "_capped" suffix. A non-zero own_series_median count means
# an own-series median sits above a manual cap -- worth knowing, since the
# winsor band that bounds the own median is item-level and looser than these
# hand-set CBS limits, so own-series fills can legitimately be capped here.
capped_by_source <- bilateral_trade_prices[
  !is.na(price_limit) & !is.na(price) & price > price_limit,
  .(n_capped = .N), by = price_source]
bilateral_trade_prices[
  !is.na(price_limit) & !is.na(price) & price > price_limit,
  `:=`(price        = price_limit,
       price_source = paste0(price_source, "_capped"))
]
bilateral_trade_prices[, price_limit := NULL]

cat("\nManual price caps\n")
cat("  Items with caps:     ", nrow(price_caps), "\n", sep = "")
cat("  Observations capped: ", n_capped,         "\n", sep = "")
if (nrow(capped_by_source) > 0) {
  setorder(capped_by_source, -n_capped)
  cat("  Capped cells by fill rung:\n")
  print(capped_by_source)
  own_capped <- capped_by_source[price_source == "own_series_median", n_capped]
  if (length(own_capped) > 0L && own_capped > 0L)
    cat("  Note: own_series_median cells were capped (own median above a manual cap).\n")
}


# ------------------------------------------------------------------------------
# 7.5 CBS-grain override for thin-decomposition residuals
# ------------------------------------------------------------------------------
# Some residual CBS items decompose into BTD constituents that are
# individually too thin (sparse trade, sparse FAO SUA production) for the
# default 6.5c / 6.5d path in script 13_3 (production-weighted mean of per-
# BTD prices) to produce a stable CBS-grain price. For these items we
# compute the price directly at CBS grain — sum(USD) / sum(qty / tcf)
# across all BTD constituents per (exporter, year), bypassing the SUA-
# grain decomposition entirely.
#
# Dimensional convention. Quantities (tonnes / An / 1000 An) are scaled
# to fresh-equivalent units by dividing by tcf (mirroring the mass
# rescaling in 01_1_tidy_fao.R's tcf_apply at the BTD->CBS aggregation
# step). USD is NOT divided by tcf: it is real trade USD, and the ratio
#   sum(USD) / sum(qty / tcf)
# is a fresh-equivalent-quantity-weighted mean of per-BTD fresh-
# equivalent prices, in clean USD per fresh-equivalent unit. This
# DIVERGES from the OLD btd_full_tidy logic (which divided both USD and
# qty by tcf) in favor of a dimensionally consistent price; for
# constituents with tcf < 1 the override price will come out lower than
# the OLD numbers would have. For items with all-tcf=1 constituents
# (e.g. Poultry birds) the two are equivalent.
#
# Pipeline parity with the SUA-grain prices above: the CBS-grain series
# go through the exact same Phase-2 steps 3–7 as `prices_exporter`,
# only with the series identifier swapped from `item_code` to
# `cbs_item_code`. Concretely:
#   step 3 (CBS) — bilateral aggregation to (exporter, year) at CBS grain
#                  via sum(USD) / sum(qty / tcf), per CBS item.
#   step 4 (CBS) — Hampel filter applied in 2 passes (k = 3, window = 3)
#                  on each (area x cbs_item) series. Diagnostic CSV mirrors
#                  the SUA-grain `trade_prices_hampel_entries.csv`.
#   step 5 (CBS) — per-CBS-item cross-sectional MAD winsor with
#                  WINSOR_MAD_K = 2.5 (the same constant as SUA grain),
#                  log-space chosen by the same rule, with the
#                  same WINSOR_MIN_OBS guard. Diagnostic CSV mirrors
#                  `trade_prices_winsorized_entries.csv`.
#   step 6 (CBS) — gap-fill missing (area, year) cells per CBS item:
#                  cbs-year median first, cbs-overall median otherwise.
#   step 7 (CBS) — manual cap from price_caps_cbs (same table as the
#                  SUA-grain caps; the CBS row is matched directly here
#                  rather than being expanded to BTD codes first).
# Each override entry specifies the ISIC side whose default aggregation
# (6.5c for "a", 6.5d for "c") is replaced — the side determines which
# BTD<->CBS concordance is used to identify the constituents. Script 03
# excludes the listed CBS items from the default per-BTD aggregation
# entirely on that side, so the override is the SOLE source of price /
# price_source for those items.
#
# Items currently overridden:
#   - 2000 Fodder Crops    (ISIC-A: 11 BTDs, mix of tcf=1 forage/silage and
#                           tcf=0.3 hay; ISIC-C side is "both"-mapped and
#                           uses 6.5b — not overridden here.)
#   - 2029 Poultry Birds   (ISIC-A: 5 BTDs, all tcf=1, head/An/1000 An.)
#   - 2748 Hides and Skins (ISIC-C: 36 BTDs, mix of tcf=1 raw, tcf=0.8
#                           wet-salted/fur/hair, tcf=0.4 dry-salted.
#                           Not in ISIC-A.)

overrides_cfg <- data.table(
  cbs_item_code = c(2000L, 2029L, 2748L),
  cbs_item      = c("Fodder crops", "Poultry birds", "Hides and skins"),
  isic_side     = c("a",            "a",             "c")
)

DIAG_HAMPEL_CBS <- file.path(DIAG_DIR, "trade_prices_cbs_hampel_entries.csv")
DIAG_WINSOR_CBS <- file.path(DIAG_DIR, "trade_prices_cbs_winsorized_entries.csv")


# --- 7.5 step 3 (CBS): per-CBS bilateral aggregation -------------------------
# Returns one (area_code, year, price) table per CBS item, tagged with
# cbs_item_code/cbs_item/isic_side. Filtering, gap-fill, and capping are
# done together on the stacked table below so that diagnostics cover all
# CBS overrides at once (mirroring how the SUA-grain pipeline emits a
# single Hampel CSV and a single winsor CSV across all SUA items).
build_cbs_override_prices <- function(cbs_code, side) {
  conc <- if (side == "a") isic_a else isic_c
  btd_map <- conc[cbs_item_code == cbs_code & !is.na(btd_item_code),
                  .(btd_item_code, tcf)]
  if (nrow(btd_map) == 0L) {
    warning(sprintf("No ISIC-%s constituents for CBS %d; skipping override.",
                    toupper(side), cbs_code))
    return(NULL)
  }
  
  # Filter Phase-1 btd to only these constituents and attach tcf.
  d <- btd[item_code %in% btd_map$btd_item_code]
  d[btd_map, tcf := i.tcf, on = c(item_code = "btd_item_code")]
  d[is.na(tcf), tcf := 1]
  
  # Mass-only TCF rescaling: divide quantity rows by tcf to express them
  # in fresh-equivalent units; leave USD rows untouched. The post-sum
  # ratio is then USD per fresh-equivalent unit. Per-BTD-item internal
  # prices are unchanged; what shifts is the relative weight of each
  # constituent in the CBS-grain mean (heavier weight for tcf<1
  # constituents because each BTD-tonne represents more fresh-tonnes).
  d[unit != "usd", value := value / tcf]
  d[, tcf := NULL]
  
  # Wide by unit — sums tonnes / An / 1000 An (fresh-eq) and USD (real)
  # across BTD constituents within each (exporter, importer, year).
  flows <- dcast(
    d[unit %in% c("usd", "tonnes", "An", "1000 An")],
    from + from_code + to + to_code + year ~ unit,
    value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0
  )
  # dcast only emits columns for unit values present in `d`. A CBS subset
  # typically uses just one quantity unit (e.g. fodder is all tonnes,
  # poultry is all head/An/1000 An), so ensure the four expected unit
  # columns exist before filtering and aggregating below.
  for (u in c("usd", "tonnes", "An", "1000 An")) {
    if (!u %in% names(flows)) flows[, (u) := 0]
  }
  flows <- flows[usd > 0 & (tonnes >= qty_min_tonnes |
                              An     >= qty_min_an     |
                              `1000 An` >= qty_min_1000an)]
  
  # Aggregate to (exporter, year) at CBS grain: sum USD / sum fresh-eq qty.
  # Local name `cbs_prices` (not `prices`) so this doesn't shadow the
  # SUA-grain `prices` data.table built in section 1.
  cbs_prices <- flows[, .(
    usd       = sum(usd,       na.rm = TRUE),
    tonnes    = sum(tonnes,    na.rm = TRUE),
    An        = sum(An,        na.rm = TRUE),
    `1000 An` = sum(`1000 An`, na.rm = TRUE)
  ), by = .(area_code = from_code, year)
  ][, price := fcase(
    tonnes    > 0, usd / tonnes,
    An        > 0, usd / An,
    `1000 An` > 0, usd / `1000 An`,
    default = NA_real_
  )][is.finite(price) & price > 0,
     .(area_code, year, price)]
  
  cbs_prices[, `:=`(cbs_item_code = cbs_code, isic_side = side)]
  cbs_prices[]
}

cbs_prices_exporter <- rbindlist(
  Map(build_cbs_override_prices,
      overrides_cfg$cbs_item_code, overrides_cfg$isic_side),
  use.names = TRUE, fill = TRUE
)
cbs_prices_exporter[overrides_cfg, cbs_item := i.cbs_item, on = "cbs_item_code"]


# --- 7.5 step 4 (CBS): two-pass Hampel per (area_code x cbs_item_code) -------
setorder(cbs_prices_exporter, cbs_item_code, area_code, year)
cbs_prices_exporter[, c("price_hampel_filtered", "hampel_flag",
                        "window_median", "series_mad") := {
                          r <- hampel_filter_iterate(price,
                                                     half_window = HAMPEL_HALF_WINDOW,
                                                     threshold   = HAMPEL_THRESHOLD,
                                                     n_passes = HAMPEL_PASSES)
                          list(r$values, r$is_spike, r$window_median,
                               r$series_mad)
                        }, by = .(cbs_item_code, area_code)]

n_cbs_series   <- cbs_prices_exporter[, uniqueN(paste(area_code, cbs_item_code))]
n_cbs_eligible <- cbs_prices_exporter[, .N,
                                      by = .(area_code, cbs_item_code)
][N >= 7L, .N]
n_cbs_flagged  <- cbs_prices_exporter[hampel_flag == TRUE, .N]

cat("\nCBS-grain Hampel filter diagnostics (two-pass)\n")
cat("  Time series (area x cbs):          ", n_cbs_series,   "\n", sep = "")
cat("  Eligible (>= 7 obs):               ", n_cbs_eligible, "\n", sep = "")
cat("  Observations flagged (any pass):   ", n_cbs_flagged,  "\n", sep = "")
if (n_cbs_flagged > 0) {
  top_cbs_hampel <- cbs_prices_exporter[hampel_flag == TRUE, .N,
                                        by = .(cbs_item_code, cbs_item)
  ][order(-N)]
  cat("  Top CBS items by Hampel flags:\n")
  print(top_cbs_hampel)
}

# Per-entry CBS-grain Hampel diagnostic, schema parallel to DIAG_HAMPEL.
build_hampel_diagnostic(cbs_prices_exporter,
                        c("area_code", "cbs_item_code", "cbs_item", "year"),
                        DIAG_HAMPEL_CBS)
cat("  Diagnostic written:                ", DIAG_HAMPEL_CBS, "\n", sep = "")

cbs_prices_exporter[, price := price_hampel_filtered]
cbs_prices_exporter[, c("price_hampel_filtered", "hampel_flag",
                        "window_median", "series_mad") := NULL]

# Drop the Hampel buffer years; the CBS winsorization, gap-fill medians and grid
# below run on the keep window only.
cbs_prices_exporter <- cbs_prices_exporter[year %in% VA_KEEP_YEARS]


# --- 7.5 step 5 (CBS): per-CBS-item cross-sectional MAD winsorization --------
# Same machinery as the SUA-grain step 5 — `compute_winsor_stats` is
# called with a CBS-level grouping so the MAD reflects how spread the
# CBS-grain bundle prices are across countries+years; the resulting cap
# does for the override what the SUA step does for the per-item series.
cbs_item_stats <- compute_winsor_stats(cbs_prices_exporter,
                                       c("cbs_item_code", "cbs_item"), k = WINSOR_MAD_K, min_obs = WINSOR_MIN_OBS)

cbs_prices_exporter[cbs_item_stats, `:=`(
  lo           = i.lo,
  hi           = i.hi,
  log_space    = i.log_space,
  n_item       = i.n_obs,
  item_center  = i.center,
  item_scale   = i.scale
), on = .(cbs_item_code, cbs_item)]

cbs_prices_exporter[, price_pre_wins := price]
cbs_prices_exporter[, mad_z := fifelse(
  !is.finite(item_scale) | item_scale == 0, NA_real_,
  fifelse(log_space == TRUE,
          (log(price) - item_center) / item_scale,
          (price      - item_center) / item_scale)
)]

n_cbs_winsorized <- cbs_prices_exporter[!is.na(lo) & (price < lo | price > hi), .N]
cbs_prices_exporter[!is.na(lo) & price < lo, price := lo]
cbs_prices_exporter[!is.na(hi) & price > hi, price := hi]

cat("\nCBS-grain MAD winsorization diagnostics (k = ", WINSOR_MAD_K, ")\n", sep = "")
cat("  CBS items processed:                ", nrow(cbs_item_stats),                          "\n", sep = "")
cat("  Items winsorized in log space:      ", sum(cbs_item_stats$log_space, na.rm = TRUE),   "\n", sep = "")
cat("  Items with insufficient obs:        ", sum(is.na(cbs_item_stats$log_space)),          "\n", sep = "")
cat("  Observations winsorized:            ", n_cbs_winsorized,                              "\n", sep = "")

# Per-entry CBS-grain winsor diagnostic, schema parallel to DIAG_WINSOR.
build_winsor_diagnostic(cbs_prices_exporter,
                        c("area_code", "cbs_item_code", "cbs_item", "year"),
                        DIAG_WINSOR_CBS)
cat("  Diagnostic written:                 ", DIAG_WINSOR_CBS, "\n", sep = "")

cbs_prices_exporter[, c("lo", "hi", "log_space", "n_item",
                        "item_center", "item_scale",
                        "price_pre_wins", "mad_z") := NULL]


# --- 7.5 step 6 (CBS): build (area x cbs_item x year) grid and gap-fill ------
# Area set matches the SUA-grain grid (line ~669) so the override
# covers exactly the rows the default would have covered in script 13_3 —
# no leakage of `trade_btd_simple_mean` onto override items because the
# default aggregation simply doesn't run for them.
cbs_all_codes <- union(regions$code, unique(prices_exporter$from_code))
cbs_all_years <- sort(intersect(unique(btd$year), VA_KEEP_YEARS))

cbs_grid <- CJ(area_code     = cbs_all_codes,
               cbs_item_code = overrides_cfg$cbs_item_code,
               year          = cbs_all_years)
cbs_grid[overrides_cfg, `:=`(cbs_item  = i.cbs_item,
                             isic_side = i.isic_side),
         on = "cbs_item_code"]

cbs_override_all <- merge(
  cbs_grid,
  cbs_prices_exporter[, .(area_code, cbs_item_code, year, price)],
  by    = c("area_code", "cbs_item_code", "year"),
  all.x = TRUE
)
cbs_override_all[, price_source := fifelse(!is.na(price),
                                           "trade_cbs_direct", NA_character_)]
cbs_override_all[, own_series_gate_rejected := FALSE]

# Per-CBS-item medians for the gap-fill: cbs-year first, then cbs-overall.
median_by_cbs_year <- cbs_prices_exporter[
  !is.na(price),
  .(p_cbs_year = median(price, na.rm = TRUE)),
  by = .(cbs_item_code, year)
]
median_by_cbs <- cbs_prices_exporter[
  !is.na(price),
  .(p_cbs = median(price, na.rm = TRUE)),
  by = .(cbs_item_code)
]

cbs_override_all[median_by_cbs_year, p_cbs_year := i.p_cbs_year,
                 on = .(cbs_item_code, year)]
cbs_override_all[median_by_cbs,      p_cbs      := i.p_cbs,
                 on = "cbs_item_code"]

n_cbs_missing_initial <- cbs_override_all[is.na(price), .N]

# Own-series median, prioritised above the cbs cross-sectional medians: fill
# each missing cell with the median of its own (area, cbs_item) direct obs.
if (PRICE_PREFER_OWN_SERIES_MEDIAN) {
  own_cbs <- own_series_median_fill(
    cbs_prices_exporter[, .(area_code, cbs_item_code, price)],
    series_cols = c("area_code", "cbs_item_code"), item_col = "cbs_item_code",
    winsor_stats = cbs_item_stats)
  cbs_override_all[own_cbs, `:=`(own_med = i.own_med,
                                 own_reject = i.gate_rejected),
                   on = c("area_code", "cbs_item_code")]
  cbs_override_all[is.na(price) & !is.na(own_med) & !own_reject,
                   `:=`(price = own_med,
                        price_source = "trade_cbs_own_series_median")]
  cbs_override_all[is.na(price) & !is.na(own_med) & own_reject,
                   own_series_gate_rejected := TRUE]
  cbs_override_all[, c("own_med", "own_reject") := NULL]
}
n_cbs_after_own  <- cbs_override_all[is.na(price), .N]
n_cbs_filled_own <- n_cbs_missing_initial - n_cbs_after_own

cbs_override_all[is.na(price) & !is.na(p_cbs_year),
                 `:=`(price = p_cbs_year,
                      price_source = "trade_cbs_year_median")]
n_cbs_filled_year <- n_cbs_after_own - cbs_override_all[is.na(price), .N]
cbs_override_all[is.na(price) & !is.na(p_cbs),
                 `:=`(price = p_cbs,
                      price_source = "trade_cbs_overall_median")]
n_cbs_filled_overall <- n_cbs_after_own - n_cbs_filled_year -
  cbs_override_all[is.na(price), .N]

cat("\nCBS-grain gap-filling diagnostics\n")
cat("  Grid cells (area x cbs x year):         ", nrow(cbs_override_all),                            "\n", sep = "")
cat("  Cells filled directly:                  ", nrow(cbs_override_all) - n_cbs_missing_initial,    "\n", sep = "")
cat("  Cells filled from own-series median:     ", n_cbs_filled_own,                                  "\n", sep = "")
cat("  Cells filled from cbs-year median:      ", n_cbs_filled_year,                                  "\n", sep = "")
cat("  Cells filled from cbs-overall median:   ", n_cbs_filled_overall,                               "\n", sep = "")
cat("  Own-series gate-rejected (-> x-section):", cbs_override_all[own_series_gate_rejected == TRUE, .N], "\n", sep = "")
cat("  Cells still missing (no trade at all):  ", cbs_override_all[is.na(price), .N],                 "\n", sep = "")

# Before/after fill-mix, mirroring the main grid: own-series cells reassigned to
# the cbs median rung they would otherwise have hit.
after_mix_cbs  <- cbs_override_all[, .(n_after = .N), by = price_source]
before_src_cbs <- cbs_override_all[, fifelse(
  price_source == "trade_cbs_own_series_median",
  fcase(!is.na(p_cbs_year), "trade_cbs_year_median",
        !is.na(p_cbs),      "trade_cbs_overall_median",
        default = NA_character_),
  price_source)]
before_mix_cbs <- data.table(price_source = before_src_cbs)[, .(n_before = .N), by = price_source]
fill_mix_cbs   <- merge(before_mix_cbs, after_mix_cbs, by = "price_source", all = TRUE)
fill_mix_cbs[is.na(n_before), n_before := 0L][is.na(n_after), n_after := 0L]
setorder(fill_mix_cbs, -n_after)
cat("\nCBS-grain fill-mix (price_source counts) before vs after own-series rung\n")
print(fill_mix_cbs)

cbs_override_all[, c("p_cbs_year", "p_cbs") := NULL]


# --- 7.5 step 7 (CBS): manual cap from price_caps_cbs ------------------------
cbs_override_all[price_caps_cbs, price_limit := i.price_limit,
                 on = "cbs_item_code"]
n_cbs_capped <- cbs_override_all[
  !is.na(price_limit) & !is.na(price) & price > price_limit, .N
]
cbs_capped_by_source <- cbs_override_all[
  !is.na(price_limit) & !is.na(price) & price > price_limit,
  .(n_capped = .N), by = price_source]
cbs_override_all[
  !is.na(price_limit) & !is.na(price) & price > price_limit,
  `:=`(price        = price_limit,
       price_source = paste0(price_source, "_capped"))
]
cbs_override_all[, price_limit := NULL]

cat("\nCBS-grain manual price caps\n")
cat("  Observations capped: ", n_cbs_capped, "\n", sep = "")
if (nrow(cbs_capped_by_source) > 0) {
  setorder(cbs_capped_by_source, -n_capped)
  cat("  Capped cells by fill rung:\n")
  print(cbs_capped_by_source)
  own_capped <- cbs_capped_by_source[price_source == "trade_cbs_own_series_median", n_capped]
  if (length(own_capped) > 0L && own_capped > 0L)
    cat("  Note: own-series cells were capped (own median above a manual cap).\n")
}


# Final shape: (area_code, year, price, price_source, cbs_item_code, isic_side)
# matching the previous schema. cbs_item is dropped (was not in the original
# saved RDS); script 13_3 only uses the columns above.
setcolorder(cbs_override_all,
            c("area_code", "year", "price", "price_source",
              "cbs_item_code", "isic_side"))
cbs_override_all[, cbs_item := NULL]

OVERRIDE_RDS_PATH <- VA_BILATERAL_TRADE_PRICES_CBS_OVERRIDE_RDS
saveRDS(cbs_override_all, OVERRIDE_RDS_PATH)

cat("\nCBS-grain overrides\n")
cat("  Items: ", paste0(overrides_cfg$cbs_item_code, " (",
                        overrides_cfg$isic_side, ")", collapse = ", "),
    "\n", sep = "")
cat("  Rows:  ", nrow(cbs_override_all), "\n", sep = "")
cat("  Saved: ", OVERRIDE_RDS_PATH, "\n", sep = "")


# ------------------------------------------------------------------------------
# 8. Finalize and save
# ------------------------------------------------------------------------------

setcolorder(bilateral_trade_prices, c("area_code", "area", "item_code", "item",
                                      "year", "price", "price_source"))

saveRDS(bilateral_trade_prices, OUTPUT_RDS_PATH)
cat("\nSaved: ", OUTPUT_RDS_PATH, " (", nrow(bilateral_trade_prices), " rows)\n", sep = "")