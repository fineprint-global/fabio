# ==============================================================================
# FABIOv2 Producer Total Values
# ==============================================================================
#
# Description:
#   Constructs panels of total values for every row in the FABIO v2
#   multi-regional input-output (MRIO) table, split into two outputs along the
#   ISIC-A / ISIC-C concordance dimension. Each row is identified by a unique
#   (area x commodity x year) triple. Each output contains total product
#   output, national production volumes, unit prices (USD), and the derived
#   total value (USD) for each observation.
#
#   All non-FABIO inputs come from FAO data products: producer prices come
#   from the FAO Producer Prices dataset; bilateral trade data (BTD) and
#   commodity balance sheet (CBS) item codes come from FAO; supply
#   utilization accounts (SUA) and the Production_Crops_Livestock fallback
#   come from FAO; and the trade-derived unit prices loaded from
#   bilateral_trade_prices.rds (script 13_2) are themselves derived from FAO
#   BTD. The FABIO commodity space is structured around FAO CBS codes.
#
#   ISIC-A vs ISIC-C — membership and value formula:
#     Membership of each output (which FABIO items appear in each table) is
#     decided by the GLORIA <-> FABIO concordances, NOT by the BTD <-> CBS
#     concordances. A FABIO commodity can have a GLORIA mapping without a
#     BTD bundle behind it (Fish/Seafood 2960, Grazing 2001).
#     Using BTD <-> CBS for the membership filter silently dropped those
#     items from both outputs. The BTD <-> CBS concordance drives only
#     the value formula and the SUA-sum switch for "both"-mapped items
#     (`both_item_codes` below) and the residual CBS-price aggregations
#     in 7c / 7d.
#     - ISIC-A table: items mapped to any GLORIA ISIC-A sector.
#       total_value = total_product_output * price (the FABIO physical output
#       is the correct ISIC-A quantity).
#     - ISIC-C table: items mapped to any GLORIA ISIC-C sector.
#       For items ALSO in the ISIC-A concordance ("both"-mapped commodities
#       such as Wheat & products, Maize, Soyabeans, ...), the FABIO total
#       product output is the ISIC-A primary quantity and is NOT meaningful
#       at ISIC-C — the same FABIO row represents a different bundle of FAO
#       SUA items at ISIC-C (e.g. wheat flour, bran, pasta, bread, ...
#       rather than wheat itself). For these rows total_value is recomputed
#       at FAO SUA/BTD grain via the ISIC-C concordance:
#         total_value = sum over FAO BTD sub-categories b
#                         of fao_production_b * fao_btd_trade_price_b
#       where the BTD sub-categories come from the FAO BTD <-> CBS ISIC-C
#       concordance, fao_production is from the FAO SUA tidy table (with
#       FAO Production_Crops_Livestock fallback) at native BTD/SUA grain,
#       and fao_btd_trade_price is from bilateral_trade_prices.rds (the
#       FAO-BTD-derived prices produced by script 13_2). The `price` and
#       `price_source` columns are set to NA for "both"-mapped rows in the
#       ISIC-C output — neither the ISIC-A primary FAO producer price (for
#       FAO-overlap items like Wheat, Milk) nor the BTD-bundle
#       weighted price (for residuals like Fodder Crops) is the correct
#       unit price for the row's `total_value` (due to non-1 tcf's). 
#       The `sua_aggregated_value [USD]` column carries
#       the value, and `total_value_source = "SUA_aggregated"` flags these
#       rows. Alongside it, `sua_aggregated_production [tonnes]` carries
#       the bundle's gross physical output, summed over the SAME set of
#       BTD sub-categories that contributed to the value (those with both
#       a non-NA production and a non-NA price). It serves two downstream
#       roles: a sanity check against the ISIC-A primary
#       `total_product_output` on the same row (the ratio measures the
#       processing-chain throughput multiplier), and a meaningful
#       USD-per-tonne denominator for any value-added pipeline that would
#       otherwise divide by the ISIC-A primary tonnage on these rows.
#     - For items only in ISIC-C (processed commodities like
#       Soyabean Oil, Palm Oil, Raw Sugar) the regular output * price
#       formula applies — the FABIO output already represents the
#       processed quantity.
#
# Pipeline overview (mirrors the section structure below):
#   1. Load all input files and concordance tables, including the FAO BTD
#      <-> CBS concordances at ISIC-A and ISIC-C resolution; these split
#      the final outputs and drive every aggregation that crosses BTD/CBS
#      grain.
#   2. Sanity checks on loaded data and derive `residual_item_codes`
#      (FABIO commodities not in the FAO producer prices concordance).
#   3. Build a full (area x item x year) concordance between FAO and FABIO
#      v2. ALL joins use numeric codes (area_code, item_code) rather than
#      names.
#   4. Join FAO producer prices (with global-median fallback: area_code
#      5000) and national production volumes onto the concordance.
#   5. Compute production-weighted average prices per FABIO v2 row for the
#      "overlap" items (those with a direct FAO producer prices match), and
#      audit the FAO->CBS item concordance for tcf != 1 (5b).
#   6. Initialize residual rows (FABIO commodities NOT in the FAO producer
#      prices concordance) — price/source NA at this stage; populated per
#      ISIC level in Section 7.
#   7. FAO SUA/BTD-grain aggregations driven by the ISIC concordances:
#       - 7b sum aggregation for ISIC-C "both"-mapped items;
#       - 7c production-weighted CBS price for ISIC-A residuals;
#       - 7d production-weighted CBS price for ISIC-C residuals;
#       - 7e diagnostic CSV of (cbs, btd) pairs whose tcf != 1 was
#         ignored.
#   8. Combine overlap and residual rows; per ISIC level fill residual
#      prices, apply guards, and compute total_value:
#        ISIC-A: total_value = output * price
#        ISIC-C: total_value = sua_aggregated_value for "both"-mapped items
#                              else (output * price)
#
# price_source values:
#   - "FAO_exact"                   : single country-specific FAO producer
#                                     price.
#   - "FAO_weighted"                : production-weighted mean of several
#                                     country-specific FAO producer prices.
#   - "FAO_simple_mean"             : simple mean of country-specific FAO
#                                     producer prices (used when FAO
#                                     production is zero but FABIO output
#                                     is positive).
#   - "FAO_own_series_median"       : no exact match for this year, but the
#                                     country reports the item in other years;
#                                     its own-series producer-price median is
#                                     used in preference to the global median.
#   - "FAO_global_median"           : no country-specific FAO producer price
#                                     available; the FAO global-median
#                                     producer price (area_code 5000) was
#                                     used.
#   - "trade_btd_single"            : residual cbs commodity priced from a
#                                     single FAO BTD sub-category (per the
#                                     ISIC-A or ISIC-C concordance) whose
#                                     script-2 price is genuinely
#                                     trade-derived (trade_direct, possibly
#                                     _capped). A matching exporter-side
#                                     aggregate exists in the BTD data for
#                                     that (area, btd_item, year).
#   - "trade_btd_single_filled"     : like trade_btd_single, but the lone
#                                     priced constituent is one of script
#                                     2's median gap-fills (item_year_median
#                                     / item_median, possibly _capped) — no
#                                     bilateral trade underlies this cell
#                                     for that area; the price is a cross-
#                                     country median.
#   - "trade_btd_weighted"          : residual cbs commodity priced as a
#                                     production-weighted mean of multiple
#                                     FAO BTD sub-categories' trade-derived
#                                     prices.
#   - "trade_btd_simple_mean"       : residual cbs commodity priced as a
#                                     simple mean of FAO BTD sub-category
#                                     trade-derived prices (used when no
#                                     BTD has a positive FAO production
#                                     weight but at least one has a price).
#   The trade_btd_* labels apply to the ISIC-A and ISIC-C output tables
#   independently (see Section 7c / 7d): the same residual cbs row may
#   carry different prices in the two tables since the FAO BTD bundle
#   differs between concordances. The structural label above (trade_btd_*)
#   describes the SHAPE of the aggregation; script 13_2's own provenance
#   labels for the constituent BTD prices (trade_direct, item_year_median,
#   item_median, *_capped) are summarised in the companion column
#   `price_source_constituents` (see below).
#
# price_source_constituents column:
#   A semicolon-separated tally string summarising what's behind the row's
#   monetary aggregate. For every distinct constituent label among the
#   underlying BTD-grain prices, the tally lists:
#       label:N(w=W)   weighted-mean case (some BTD has positive production)
#       label:N(n=N')  simple-mean fallback case (no positive production
#                      weights; share is by count instead)
#   sorted by descending share. N is the count of constituent BTDs
#   carrying that label; W is the share of total production weight; N' is
#   the share of count. The qualifier (`w=` vs `n=`) tells the reader
#   whether the underlying aggregate used production weights or fell back
#   to a simple mean.
#   Populated for:
#     - ISIC-A residual rows priced by 7c (describes the wmean_price
#       aggregation across the ISIC-A BTD bundle for the residual CBS).
#     - ISIC-C residual rows priced by 7d (analogous, ISIC-C BTD bundle).
#     - ISIC-C "both"-mapped rows (overlap and residual): describes the
#       7b SUA-sum aggregation that produced `sua_aggregated_value`. For
#       these rows, `price` and `price_source` are blanked to NA but the
#       constituent tally is kept — it's the only place this provenance
#       lives once the price column is cleared.
#   NA for:
#     - Overlap rows priced from FAO producer prices (5a). The FAO-side
#       constituents are not currently tallied.
#     - CBS-grain override rows (script 13_2 §7.5). These are single-source
#       by construction; `price_source` already carries the full label
#       (trade_cbs_direct, trade_cbs_year_median, etc.).
#     - Rows with no priced constituent (price_source is NA or "none").
#
# Inputs:
#   - FABIO v2 IO labels                  (io_labels.csv)
#   - FABIO v2 total output               (X.rds)
#   - FAO producer prices area conc.      (concordance_areas_fao_producer_prices_fabio.csv)
#   - FAO producer prices item conc.      (concordance_items_fao_producer_prices_fabio.csv)
#   - FAO BTD <-> CBS conc.               (conc_btd-cbs.csv)
#       (single file; `isic` column "A"/"C" marks the side, split on load)
#   - GLORIA <-> FABIO conc.              (concordance_items_gloria_fabio.csv)
#       (single file; `ISIC` column "A"/"C" marks the side, split on load;
#        define output membership at each ISIC level; see Section 1k)
#   - FAO producer prices                 (Prices_E_All_Data_with_USD.csv)
#   - FAO SUA production                  (sua_tidy.rds)
#   - FAO Production_Crops_Livestock      (Production_Crops_Livestock_E_All_Data.csv)
#       (fallback when SUA production is missing)
#   - FAO BTD-derived trade prices        (bilateral_trade_prices.rds)
#       (output of script 13_2; raw input is FAO BTD)
#   - FAO BTD-derived CBS-grain overrides (bilateral_trade_prices_cbs_override.rds)
#       (output of script 13_2 section 7.5; CBS-grain price overrides for
#        residual items whose default per-BTD aggregation in 7c/7d
#        is too thin — see those sections for which items and ISIC sides.
#        These items are EXCLUDED from the default per-BTD aggregation
#        on their respective sides and supplied directly from the override,
#        so their `price_source` is always one of the `trade_cbs_*` labels.)
#
# Outputs:
#   - FABIOv2_producer_total_values_isic_a.rds / .csv
#   - FABIOv2_producer_total_values_isic_c.rds / .csv
#         (ISIC-C carries an extra column, `sua_aggregated_production [tonnes]`,
#          alongside `sua_aggregated_value [USD]` for the "both"-mapped rows;
#          NA for ISIC-C-only rows whose total_product_output is already the
#          right physical denominator.)
#   - FABIOv2_residual_btd_to_cbs_tcf_ignored.csv  (Section 7e):
#         (cbs, btd) pairs in the residual BTD->CBS aggregations of 7c/7d
#         whose concordance tcf != 1 was folded in as if tcf == 1.
#   - FABIOv2_overlap_fao_to_cbs_concordance_audit.csv  (Section 5b):
#         FAO_item_codes from the FAO producer prices concordance (Section 5a)
#         whose route to a CBS bucket is suspect — either the BTD<->CBS row
#         has tcf != 1 (unit mismatch) or there is no BTD<->CBS row at all.
# ==============================================================================

library(data.table)
source("R/00_value_added_config.R")


# ==============================================================================
# Configuration
# ==============================================================================

PATHS <- list(
  # FABIO v2 core data
  io_labels    = VA_FABIO_V2_IO_LABELS_CSV,
  total_output = VA_FABIO_V2_X_RDS,
  
  # FAO producer prices <-> FABIO area/item concordances (code-based).
  fao_areas = file.path(VA_CONCORDANCE_DIR, "concordance_areas_fao_producer_prices_fabio.csv"),
  fao_items = file.path(VA_CONCORDANCE_DIR, "concordance_items_fao_producer_prices_fabio.csv"),
  # FAO BTD <-> FAO CBS concordance. Single combined table; the `isic`
  # column ("A" / "C") marks the side that used to be split across two
  # files. Split back into ISIC-A / ISIC-C views on load (Section 1j).
  # Used to split the output into two tables and to decide which
  # total-value formula applies per item (see header).
  fao_btd_cbs_isic = VA_CONC_BTD_CBS_ISIC,
  # GLORIA <-> FABIO concordance. Single combined table; the `ISIC` column
  # ("A" / "C") marks the side that used to be split across two files.
  # Split back into ISIC-A / ISIC-C views on load (Section 1k). Defines
  # output membership at each ISIC level (which FABIO items appear in which
  # output table). Decoupled from the BTD <-> CBS concordance; see
  # Section 1k for rationale.
  gloria_items = VA_CONC_GLORIA_ITEMS,
  
  # FAO producer prices (USD; output of "13_1_FAOstat_producer_prices_USD.py").
  fao_producer_prices = VA_PRICES_USD_CSV,
  # FAO BTD-derived trade prices (output of "13_2_clean_bilateral_trade_prices.R").
  fao_btd_trade_prices = VA_BILATERAL_TRADE_PRICES_RDS,
  # CBS-grain price overrides for residual items with thin SUA
  # decomposition (output of script 13_2 section 7.5; consumed in sections
  # 7c / 7d below to overwrite the default per-BTD-item aggregation
  # for the listed CBS items only).
  fao_btd_trade_prices_cbs_override = VA_BILATERAL_TRADE_PRICES_CBS_OVERRIDE_RDS,
  
  # FAO SUA production (primary source for production volumes).
  fao_sua_production = VA_FABIO_SUA_TIDY_RDS,
  # FAO Production_Crops_Livestock (fallback when SUA is missing a value).
  fao_crops_livestock_production = VA_PRODUCTION_CROPS_LIVESTOCK_CSV,
  
  # Outputs — one pair per ISIC level, plus two diagnostics
  out_rds_isic_a = VA_FABIO_TV_ISIC_A_RDS,
  out_csv_isic_a = VA_FABIO_TV_ISIC_A_CSV,
  out_rds_isic_c = VA_FABIO_TV_ISIC_C_RDS,
  out_csv_isic_c = VA_FABIO_TV_ISIC_C_CSV,
  # Diagnostics — two distinct audits, one per aggregation pathway:
  #   * residual_btd_to_cbs_tcf_ignored  (Section 7e): (cbs, btd) pairs in the
  #       residual BTD->CBS aggregations of 7c/7d whose concordance tcf != 1
  #       was folded in as if tcf == 1 (intentional shortcut — flagged here).
  #   * overlap_fao_to_cbs_concordance_audit  (Section 5b): FAO_item_codes from
  #       the FAO producer prices concordance whose route to a CBS bucket is
  #       suspect — either the BTD<->CBS row has tcf != 1 (unit mismatch with
  #       FABIO output) or the FAO_item_code has no BTD<->CBS row at all.
  out_csv_residual_btd_tcf_ignored      = file.path(VA_TOTAL_VALUE_DIAG_DIR, "FABIOv2_residual_btd_to_cbs_tcf_ignored.csv"),
  out_csv_overlap_fao_concordance_audit = file.path(VA_TOTAL_VALUE_DIAG_DIR, "FABIOv2_overlap_fao_to_cbs_concordance_audit.csv")
)

# Area code used by the FAO producer prices CSV to denote the global median.
GLOBAL_MEDIAN_AREA_CODE <- VA_GLOBAL_MEDIAN_AREA_CODE


# ==============================================================================
# Helper Functions
# ==============================================================================

#' Sum of element-wise products production * price, with NA handling.
#'
#' Used to aggregate SUA-level (FAO_production x FAO_price) products to a
#' single value per (FABIO_area_code, FABIO_item_code, year). This is the
#' total-value formula for FABIO commodities at the ISIC-C level when the
#' same commodity also appears in the ISIC-A concordance — the FABIO physical
#' total_product_output then reflects the ISIC-A primary side, and the ISIC-C
#' value is computed independently from the SUA-level data.
#'
#' @param production  Numeric vector of production volumes.
#' @param price       Numeric vector of unit prices.
#' @return            Sum of production * price over rows where BOTH are
#'                    non-NA, or NA_real_ if no such row exists.
sum_value <- function(production, price) {
  valid <- !is.na(production) & !is.na(price)
  if (any(valid)) sum(production[valid] * price[valid]) else NA_real_
}


#' Sum of production over BTD sub-categories where price is also non-NA.
#'
#' Mirrors the inclusion criterion of `sum_value`: production is summed
#' over the same set of (production, price) rows that contribute to the
#' value sum. This way the implied weighted-average price
#' (sua_aggregated_value / sua_aggregated_production) is well-defined
#' over a CONSISTENT set of BTD sub-categories. If we summed all non-NA
#' production regardless of price coverage, country-years with sparse
#' price data would inflate the denominator without inflating the
#' numerator, and downstream USD/tonne ratios would carry artefactual
#' cross-country variation driven by price-data coverage rather than by
#' the underlying intensity.
#'
#' @param production  Numeric vector of production volumes.
#' @param price       Numeric vector of unit prices.
#' @return            Sum of production over rows where BOTH production
#'                    and price are non-NA, or NA_real_ if no such row
#'                    exists.
sum_production_where_priced <- function(production, price) {
  valid <- !is.na(production) & !is.na(price)
  if (any(valid)) sum(production[valid]) else NA_real_
}


#' Tally string of constituent price_source labels behind an aggregate.
#'
#' Given a vector of constituent price_source labels and a corresponding
#' vector of weights (typically production), returns a single string
#' summarising the distribution of constituent labels behind the
#' aggregate, sorted by descending share. Format per element:
#'   "label:N(w=W)"   when at least one constituent has positive weight
#'                    (production-weighted aggregate; W = share of total
#'                    weight contributed by that label)
#'   "label:N(n=N')"  when no constituent has positive weight
#'                    (simple-mean fallback; N' = share by count)
#' Elements joined by "; ". The qualifier (`w=` vs `n=`) signals which
#' regime the underlying aggregate used.
#'
#' Constituents with NA price_source are excluded — those are rows where
#' the underlying BTD-grain price was NA / "none" and so contributed
#' nothing to the aggregate. Returns NA_character_ if no constituent has
#' a non-NA price_source.
#'
#' @param price_source  Character vector of script-2 price_source labels.
#'                      "none" labels should be converted to NA upstream
#'                      so they're filtered out here.
#' @param weight        Numeric vector of weights (production volumes).
#' @return              A single character scalar (the tally string), or
#'                      NA_character_ if no constituent contributed.
build_source_tally <- function(price_source, weight) {
  ok <- !is.na(price_source)
  if (!any(ok)) return(NA_character_)
  ps <- price_source[ok]
  w  <- weight[ok]
  w[is.na(w)] <- 0
  
  if (sum(w) > 0) {
    qual   <- "w"
    by_lab <- tapply(w,  ps, sum)
    counts <- tapply(ps, ps, length)
    share  <- by_lab / sum(by_lab)
  } else {
    qual   <- "n"
    counts <- tapply(ps, ps, length)
    share  <- counts / sum(counts)
  }
  ord <- order(-share, -counts, names(share))
  paste(sprintf("%s:%d(%s=%.2f)",
                names(share)[ord], counts[ord], qual, share[ord]),
        collapse = "; ")
}


#' Aggregate BTD-grain prices to CBS grain via a BTD<->CBS concordance map.
#'
#' For every (area_code, cbs_item_code, year) reachable through the supplied
#' BTD<->CBS map, returns the production-weighted mean BTD-grain price (with
#' a simple-mean fallback when no BTD has a positive production weight),
#' a `price_source` label distinguishing single-BTD passthrough (split by
#' the constituent's script-2 provenance into "trade_btd_single" for real
#' trade-derived prices vs "trade_btd_single_filled" for median gap-fills),
#' genuine weighted aggregation, and the simple-mean fallback case, AND a
#' `price_source_constituents` tally describing the script-2 provenance
#' labels of the constituent BTD prices. Used by both Sections 7c and 7d
#' for residual CBS-price aggregation.
#'
#' Outer-merges production and price first: a BTD sub-category that has a
#' price but no production data can still contribute to the simple-mean
#' fallback (zero weight excludes it from the weighted mean by design).
#'
#' @param btd_map   data.table with at least (cbs_item_code, btd_item_code).
#' @param prod_dt   BTD-grain production: (area_code, btd_item_code, year, production).
#' @param price_dt  BTD-grain prices    : (area_code, btd_item_code, year, price,
#'                                         price_source).
#' @return          data.table at (area_code, item_code = cbs_item_code, year)
#'                  grain with columns: price, price_source,
#'                  price_source_constituents.
aggregate_btd_to_cbs <- function(btd_map, prod_dt, price_dt) {
  joined <- merge(prod_dt, price_dt,
                  by  = c("area_code", "btd_item_code", "year"),
                  all = TRUE)
  long <- merge(
    btd_map[, .(cbs_item_code, btd_item_code)],
    joined,
    by              = "btd_item_code",
    allow.cartesian = TRUE
  )
  agg <- long[
    ,
    .(
      price      = wmean_price(price, production),
      n_priced   = sum(!is.na(price)),
      n_weighted = sum(!is.na(price) &
                         !is.na(production) & production > 0),
      # Constituents whose script-2 provenance is genuinely trade-derived
      # (trade_direct, incl. trade_direct_capped) — as opposed to script
      # 2's median gap-fills (item_year_median / item_median), which exist
      # for every grid cell and do NOT correspond to any bilateral trade
      # datapoint for this area. Used below to split the single-
      # constituent label by provenance.
      n_direct   = sum(!is.na(price) &
                         grepl("^trade_direct", price_source)),
      # Tally over the constituent BTDs' script-2 price_source labels.
      # `fao_btd_trade_prices` is loaded in 7a with "none" replaced by NA,
      # so build_source_tally's `!is.na(price_source)` filter naturally
      # excludes BTDs that contributed nothing.
      price_source_constituents =
        build_source_tally(price_source, production)
    ),
    by = .(area_code, item_code = cbs_item_code, year)
  ]
  # Label the price source: distinguish single-BTD passthrough from genuine
  # weighted aggregation, and flag where wmean_price fell through to the
  # simple-mean fallback because no BTD had a positive production weight.
  # The single-constituent case is further split by the constituent's
  # script-2 provenance: "trade_btd_single" only when actual trade data
  # backs the price; "trade_btd_single_filled" when the lone price is a
  # script-2 median gap-fill (no BTD datapoint exists for this area).
  # (Formerly both were labelled "trade_btd_exact", which wrongly
  # suggested a traceable BTD record in every case.)
  agg[, price_source := fcase(
    n_priced == 0L,                   NA_character_,
    n_priced == 1L & n_direct == 1L,  "trade_btd_single",
    n_priced == 1L,                   "trade_btd_single_filled",
    n_weighted >= 1L,                 "trade_btd_weighted",
    default                           = "trade_btd_simple_mean"
  )]
  agg[, c("n_priced", "n_weighted", "n_direct") := NULL]
  agg
}


# ==============================================================================
# 1. Load All Inputs
# ==============================================================================

# --- 1a. FABIO v2 IO labels --------------------------------------------------
# row_id still constructed from (iso3c, comm_code) to match rownames(X.rds).
# area_code / item_code are the join keys for all downstream code-based merges.

labels <- fread(PATHS$io_labels)
labels[, row_id := paste(iso3c, comm_code, sep = "_")]

# --- 1b. FABIO v2 total product output (wide: row_id x years) ----------------
output_wide <- as.data.table(readRDS(PATHS$total_output), keep.rownames = "row_id")
year_cols   <- setdiff(names(output_wide), "row_id")

# Melt to long once; reused for both overlap and residual items.
output_long <- melt(
  output_wide,
  id.vars       = "row_id",
  measure.vars  = year_cols,
  variable.name = "year",
  value.name    = "total_product_output",
  variable.factor = FALSE
)

# --- 1c. Item concordance (FAO <-> FABIO v2) -----------------------------
concordance_items <- fread(PATHS$fao_items, encoding = "UTF-8")
if ("unused_FAO_items" %in% names(concordance_items)) concordance_items[, unused_FAO_items := NULL]

# Rows where BOTH codes are populated = items we can price from FAO producer prices.
concordance_items_overlap <- concordance_items[!is.na(FABIO_item_code) & !is.na(FAO_item_code)]

# --- 1d. Area concordance (FAO <-> FABIO v2) -----------------------------
# NOTE: concordance_areas is NOT filtered on code overlap. Rows with missing
# FAO_area_code (e.g. Bahamas, Gabon, Eswatini...) still carry a FABIO_area_code
# and will receive the FAO global-median producer price via the fallback in 4a.
concordance_areas <- fread(PATHS$fao_areas, encoding = "UTF-8")
if ("comments; second area" %in% names(concordance_areas)) {
  concordance_areas[, `comments; second area` := NULL]
}

# --- 1e. FAO producer prices (long format, USD only) ----------------------
fao_producer_prices_raw <- fread(PATHS$fao_producer_prices)

# Normalise the join-key column names to lowercase_underscore.
setnames(fao_producer_prices_raw,
         old = c("Area Code", "Item Code"),
         new = c("area_code", "item_code"))

# Strip year-column prefix: "Y2010" -> "2010".
yr_names <- grep("^[A-Z][0-9]{4}$", names(fao_producer_prices_raw), value = TRUE)
setnames(fao_producer_prices_raw, old = yr_names, new = sub("^[A-Z]", "", yr_names))

price_yr_cols <- grep("^[0-9]{4}$", names(fao_producer_prices_raw), value = TRUE)
fao_producer_prices <- melt(
  fao_producer_prices_raw[Unit == "USD"],
  id.vars       = setdiff(names(fao_producer_prices_raw), price_yr_cols),
  measure.vars  = price_yr_cols,
  variable.name = "year",
  value.name    = "price",
  variable.factor = FALSE
)[, .(area_code, item_code, year, price)]

# The bulk file is wide; the melt above creates a row for EVERY
# (area, item, year) cell, including NA-price cells (e.g. Venezuela
# 2015-2023, after the country stopped reporting). Keeping those rows
# poisons the exact-match join in 4a: a data.table update join assigns
# price_is_exact = TRUE whenever the KEYS match, even when the joined
# price is NA. The global-median fallback that subsequently fills the NA
# is then mislabelled "FAO_exact"/"FAO_weighted" downstream, and -- worse
# -- for multi-item FABIO commodities the median-filled constituents are
# wrongly included in the exact-only weighted price (weighted_exact /
# fallback_exact in section 5a). Dropping NA rows here restores
# price_is_exact to its documented meaning: TRUE iff a real
# country-specific price exists for that (area, item, year).
fao_producer_prices <- fao_producer_prices[!is.na(price)]

# --- 1f. FAO national production (SUA) ----------------------------------------
# Join keys are already area_code / item_code — no name remapping needed.
fao_sua_production <- as.data.table(readRDS(PATHS$fao_sua_production))[
  , .(area_code, item_code, year = as.character(year), production)
]

# --- 1g. FAO BTD-derived trade prices (output of script 13_2) -------------------
fao_btd_trade_prices_raw <- as.data.table(readRDS(PATHS$fao_btd_trade_prices))

# --- 1h. CBS-grain price overrides (output of script 13_2 section 7.5) -------
# Per-CBS price overrides for residual items whose default 7c / 7d
# aggregation produces unstable prices (thin SUA decomposition). Applied
# at the end of those sections to overwrite the affected rows only.
fao_btd_trade_prices_cbs_override_raw <- as.data.table(
  readRDS(PATHS$fao_btd_trade_prices_cbs_override)
)

# --- 1i. FAO Production_Crops_Livestock (fallback for SUA gaps) --------------
# The Normalized FAOSTAT bulk file is already long (one row per area /
# item / element / year), so no wide-to-long melt is needed: filter to
# "Production" (Element Code 5510), rename the join keys, and take the
# tidy columns directly. `year` is kept as a character string to match
# the SUA production series joined alongside it.
fao_crops_livestock_production <- fread(PATHS$fao_crops_livestock_production)[
  `Element Code` == 5510L,
  .(area_code     = `Area Code`,
    item_code     = `Item Code`,
    year          = as.character(Year),
    production_fao = Value)
][!is.na(production_fao)]

# --- 1j. FAO BTD <-> FAO CBS concordances (ISIC-A, ISIC-C) -------------------
# Each row is a (btd_item_code, cbs_item_code) pair; cbs_item_code matches
# the FABIO item_code used throughout this script. We need:
#   - the unique set of CBS codes per ISIC level (drives which FABIO
#     commodities go into each output table and which are "both"-mapped);
#   - the (cbs_item_code, btd_item_code) map for the ISIC-C side,
#     restricted to "both"-mapped CBS codes — this defines the FAO SUA
#     sub-categories whose (fao_production_btd * fao_btd_trade_price_btd)
#     products are summed up to give the ISIC-C total value for those
#     commodities (see Section 7).
concordance_btd_cbs <- fread(PATHS$fao_btd_cbs_isic, encoding = "UTF-8",
                             na.strings = c("", "NA"))
# cbs_item_code arrives as a double (e.g. 2511.0); coerce to integer to
# match the FABIO item_code space used throughout this script and to
# avoid type-mismatch warnings on the downstream joins.
concordance_btd_cbs[, cbs_item_code := as.integer(cbs_item_code)]

# Split the combined table back into the ISIC-A / ISIC-C views the rest
# of the script consumes (the `isic` column replaces the old -a / -c file
# split).
concordance_btd_cbs_isic_a <- concordance_btd_cbs[isic == "A"]
concordance_btd_cbs_isic_c <- concordance_btd_cbs[isic == "C"]

isic_a_item_codes <- unique(concordance_btd_cbs_isic_a[!is.na(cbs_item_code), cbs_item_code])
isic_c_item_codes <- unique(concordance_btd_cbs_isic_c[!is.na(cbs_item_code), cbs_item_code])
both_item_codes   <- intersect(isic_a_item_codes, isic_c_item_codes)

# BTD<->CBS map for ISIC-C, restricted to "both"-mapped CBS codes. Keeps
# tcf and the human-readable item names so Section 7e can produce a
# diagnostic of the non-1 tcf values that were ignored in the aggregation.
# Defensive: drop rows missing either code. tcf is NOT used in the
# aggregation itself (we sum production * price at BTD grain; tcf would
# only be relevant for converting BTD physical quantities into
# CBS-equivalent physical quantities, a separate operation).
isic_c_btd_for_both <- concordance_btd_cbs_isic_c[
  cbs_item_code %in% both_item_codes & !is.na(btd_item_code),
  .(cbs_item_code, cbs_item, btd_item_code, btd_item, tcf)
]

message(
  "ISIC concordance coverage: ",
  length(isic_a_item_codes), " CBS items in ISIC-A, ",
  length(isic_c_item_codes), " in ISIC-C, ",
  length(both_item_codes),   " in both."
)
message(
  "  ISIC-C BTD sub-categories for 'both'-mapped CBS codes: ",
  nrow(isic_c_btd_for_both), " (",
  uniqueN(isic_c_btd_for_both$btd_item_code), " distinct btd codes across ",
  uniqueN(isic_c_btd_for_both$cbs_item_code), " cbs codes)."
)


# --- 1k. GLORIA <-> FABIO concordances: output membership --------------------
# Membership of the two output tables (which FABIO items appear in each) is
# decided here, from the GLORIA <-> FABIO concordances. That is what
# the value-added scripts (14_1–14_3) consume. NOT derived from the BTD <-> CBS concordance:
# a FABIO commodity can have a GLORIA mapping without a BTD bundle behind
# it (Fish/Seafood 2960, Grazing 2001). Using BTD <-> CBS for
# the membership filter silently drops those items from both outputs.
#
# The BTD <-> CBS concordance keeps a separate, well-defined job: driving
# the SUA-sum formula switch for "both"-mapped items (`both_item_codes`
# above) and the residual CBS-price aggregations in 7c / 7d. It is
# allowed to be incomplete relative to the FABIO item space — a missing
# BTD bundle just means the row uses output * price instead of SUA-sum.
gloria_items <- fread(PATHS$gloria_items, encoding = "UTF-8",
                      na.strings = c("", "NA"))

# Split the combined table into the ISIC-A / ISIC-C views (the `ISIC`
# column "A"/"C" replaces the old -a / -c file split). Rows with no ISIC
# value (e.g. Cotton lint 2661, Cottonseed 2559, Palm kernels 2562,
# Grazing 2001 — "no VA assigned") match neither side and so are members
# of neither output, as before.
gloria_isic_a <- gloria_items[ISIC == "A"]
gloria_isic_c <- gloria_items[ISIC == "C"]

isic_a_member_codes <- unique(gloria_isic_a[
  !is.na(FABIO_item_code), FABIO_item_code])
isic_c_member_codes <- unique(gloria_isic_c[
  !is.na(FABIO_item_code), FABIO_item_code])

message(
  "Output membership (from GLORIA <-> FABIO concordances): ",
  length(isic_a_member_codes), " items in ISIC-A, ",
  length(isic_c_member_codes), " in ISIC-C."
)

# Diagnostic: items that are GLORIA-mapped but absent from the BTD <-> CBS
# concordance on BOTH ISIC sides — i.e. truly outside the BTD universe.
# Rows on these items take the output * price formula in Section 8, since
# no BTD bundle can be assembled for them at any granularity.
# Expected set (FABIO v2 inputs): Seed cotton (328), Grazing (2001),
# Jute-Like Fibres (2663), Sisal (2665), Fish/Seafood (2960).
#
# Items absent from the same-side BTD concordance but present on the other
# side (e.g. vegetable oils & meats on ISIC-A: BTD routing exists only on
# ISIC-C; live animals & most raw crops on ISIC-C: BTD routing exists only
# on ISIC-A) are NOT reported here. That asymmetry is structural — it
# reflects the GLORIA primary/processed split being on the opposite ISIC
# level from where the BTD bundle is defined — not a coverage gap, and
# the output * price fallback in Section 8 is the intended behaviour.
btd_any_item_codes   <- union(isic_a_item_codes, isic_c_item_codes)
gloria_a_outside_btd <- setdiff(isic_a_member_codes, btd_any_item_codes)
gloria_c_outside_btd <- setdiff(isic_c_member_codes, btd_any_item_codes)
if (length(gloria_a_outside_btd) > 0) {
  message(
    "  ", length(gloria_a_outside_btd),
    " ISIC-A GLORIA-mapped item(s) absent from BTD <-> CBS on both ISIC ",
    "sides (will use output * price; no SUA-sum / BTD-bundle weighting ",
    "available at any grain): ",
    paste(sort(gloria_a_outside_btd), collapse = ", ")
  )
}
if (length(gloria_c_outside_btd) > 0) {
  message(
    "  ", length(gloria_c_outside_btd),
    " ISIC-C GLORIA-mapped item(s) absent from BTD <-> CBS on both ISIC ",
    "sides (will use output * price; no SUA-sum / BTD-bundle weighting ",
    "available at any grain): ",
    paste(sort(gloria_c_outside_btd), collapse = ", ")
  )
}


# ==============================================================================
# 2. Sanity Checks and Derived Constants
# ==============================================================================

# --- 2a. Unit sanity check ---------------------------------------------------
# All overlap products should be measured in tonnes (same unit as FAO producer prices).
non_tonne <- labels[item_code %in% concordance_items_overlap$FABIO_item_code & unit != "tonnes"]
if (nrow(non_tonne)) {
  warning("Some overlap products are NOT in tonnes — review:")
  print(non_tonne)
} else {
  message("OK: All FAO-FABIO v2 overlap products are in tonnes.")
}

# --- 2b. Residual item codes -------------------------------------------------
# FABIO commodities NOT in the FAO producer prices concordance. These are
# priced in Section 7 (FAO SUA/BTD-level aggregations) rather than from a
# direct FAO producer price match in Section 5. Used throughout Sections 6
# (scaffold), 7 (per-ISIC residual aggregations) and 8 (output guards).
residual_item_codes <- setdiff(unique(labels$item_code),
                               unique(concordance_items_overlap$FABIO_item_code))


# ==============================================================================
# 3. Build Concordance: (FAO_area_code x FAO_item_code x year) ->
#                      (FABIO_area_code x FABIO_item_code)
# ==============================================================================
# Cartesian product of area and item overlaps, then crossed with years.

concordance <- cross_join(concordance_areas, concordance_items_overlap)
concordance <- cross_join(concordance, data.table(year = year_cols))


# ==============================================================================
# 4. Join Prices and Production onto the Concordance
# ==============================================================================

# --- 4a. Producer prices: exact match, then global-median fallback -----------
# price_is_exact = TRUE  -> price came from a direct (country, item, year) match
# price_is_own_series = TRUE -> price filled from this country's OWN producer-price
#                           median in other years (a real country price level, just
#                           not for this exact year); ranks above the global median.
# price_is_exact = FALSE -> price was filled from the FAO producer prices
#                           global-median row (area_code 5000), or is NA.
concordance[, price_is_exact := FALSE]
concordance[, price_is_own_series := FALSE]

concordance[
  fao_producer_prices,
  `:=`(price = i.price, price_is_exact = TRUE),
  on = .(FAO_area_code = area_code, FAO_item_code = item_code, year)
]

# Own-series median, prioritised above the area-5000 global row: where a country
# reports the item in some years but not this one, carry its own median forward
# rather than collapsing to the cross-country global median.
if (PRICE_PREFER_OWN_SERIES_MEDIAN) {
  fao_country_prices <- fao_producer_prices[area_code != GLOBAL_MEDIAN_AREA_CODE]
  fao_winsor_stats   <- compute_winsor_stats(fao_country_prices, by_cols = "item_code")
  own_fao <- own_series_median_fill(
    fao_country_prices[, .(area_code, item_code, price)],
    series_cols = c("area_code", "item_code"), item_col = "item_code",
    winsor_stats = fao_winsor_stats)
  concordance[own_fao, `:=`(own_med = i.own_med, own_reject = i.gate_rejected),
              on = .(FAO_area_code = area_code, FAO_item_code = item_code)]
  concordance[is.na(price) & !is.na(own_med) & !own_reject,
              `:=`(price = own_med, price_is_own_series = TRUE)]
  concordance[, c("own_med", "own_reject") := NULL]
}

concordance[
  is.na(price),
  price := fao_producer_prices[area_code == GLOBAL_MEDIAN_AREA_CODE][
    .SD, on = .(item_code = FAO_item_code, year), x.price
  ]
]
# NB: price_is_exact / price_is_own_series stay FALSE for rows filled by the
# global-median fallback above.

# --- 4b. National production volumes -----------------------------------------
concordance[
  fao_sua_production, production := i.production,
  on = .(FAO_area_code = area_code, FAO_item_code = item_code, year)
]

# --- 4c. Fill missing production from FAO Production_Crops_Livestock --------
concordance[
  is.na(production),
  production := fao_crops_livestock_production[
    .SD, on = .(area_code = FAO_area_code, item_code = FAO_item_code, year),
    x.production_fao
  ]
]
n_filled <- concordance[!is.na(production), .N] # for diagnostic below
message("After FAO Production_Crops_Livestock fallback: ", n_filled, " rows with production data.")

# Areas with no valid production data at all
no_prod_areas <- concordance[
  , .(n_valid = sum(!is.na(production) & production > 0)),
  by = .(FAO_area_code, FAO_area)
][n_valid == 0]
if (nrow(no_prod_areas) > 0) {
  message(nrow(no_prod_areas), " FAO area(s) have no valid production data:")
  print(no_prod_areas)
}

# --- 4d. Diagnostic: items with no production match --------------------------
items_with_prod <- union(
  unique(fao_sua_production[!is.na(item_code), item_code]),
  unique(fao_crops_livestock_production[!is.na(item_code), item_code])
)
unmatched <- setdiff(
  unique(concordance[!is.na(FAO_item_code), FAO_item_code]),
  items_with_prod
)
if (length(unmatched) > 0) {
  unmatched_pretty <- concordance_items_overlap[
    FAO_item_code %in% unmatched,
    unique(paste0(FAO_item_code, " = ", FAO_item))
  ]
  message(
    "NOTICE: ", length(unmatched), " FAO item(s) lack production volumes ",
    "in both FAO SUA and FAO Production_Crops_Livestock data.\n",
    "  Unmatched:\n", paste(unmatched_pretty, collapse = "\n")
  )
}


# ==============================================================================
# 5. Aggregate to FABIO-Level Prices (Overlap Items)
# ==============================================================================
# For each (FABIO_area_code x FABIO_item_code x year), compute a single price
# from potentially many FAO producer prices matches. The aggregation is done twice:
#   - *_exact: using only rows where price_is_exact == TRUE, i.e. prices that
#     came from a direct country-level FAO producer prices match.
#   - *_any  : using all non-NA prices, including the global-median fallback
#     injected in 4a.
# This lets us label the final price_source correctly — so a FABIO area whose
# price came entirely from the global-median fallback is tagged as such,
# instead of being mislabelled as "FAO_exact"/"FAO_weighted".

# --- 5a. Production-weighted aggregation -------------------------------------

fao_producer_prices_agg <- concordance[
  ,
  .(
    production     = sum(production, na.rm = TRUE),
    # Exact-only aggregates (direct FAO producer prices match)
    weighted_exact = wmean_price(fifelse(price_is_exact, price, NA_real_),
                                 production),
    fallback_exact = safe_mean(fifelse(price_is_exact, price, NA_real_)),
    n_exact        = sum(price_is_exact & !is.na(price)),
    # Own-series tier: exact plus this country's own-median fills (4a). Ranks
    # above the global-median fallback but below a direct exact match.
    weighted_own   = wmean_price(
      fifelse(price_is_exact | price_is_own_series, price, NA_real_), production),
    fallback_own   = safe_mean(
      fifelse(price_is_exact | price_is_own_series, price, NA_real_)),
    n_own          = sum((price_is_exact | price_is_own_series) & !is.na(price)),
    # All-price aggregates (includes 4a global-median fallback rows)
    weighted_any   = wmean_price(price, production),
    fallback_any   = safe_mean(price),
    n_any          = sum(!is.na(price))
  ),
  by = .(area_code = FABIO_area_code, item_code = FABIO_item_code, year)
]

# Start the total_values table from labels that matched FAO producer prices, joined to output.
overlap_values <- output_long[
  labels[item_code %in% unique(concordance_items_overlap$FABIO_item_code)],
  on = "row_id", nomatch = 0L, allow.cartesian = TRUE
]

# Assign price and price_source in one pass. Decision tree:
#   production > 0 (or NA) and we have an exact weighted price   -> weighted_exact
#   production == 0 and output > 0 and exact simple mean exists  -> fallback_exact
#   (otherwise no exact price is available; fall through)
#   production > 0 (or NA) and any-price weighted price exists   -> weighted_any
#   production == 0 and output > 0 and any-price mean exists     -> fallback_any
#
# Cases using *_exact keep the existing labels ("FAO_exact" / "FAO_weighted" /
# "FAO_simple_mean"). Cases that fall through to *_any are labelled
# "FAO_global_median" — the price is derived purely from the 4a fallback.
overlap_values[
  fao_producer_prices_agg,
  `:=`(
    production = i.production,
    price = fcase(
      # Exact, production-weighted
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_exact),
      i.weighted_exact,
      # Exact, simple-mean (production == 0 but FABIO has output)
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_exact),
      i.fallback_exact,
      # Own-series, production-weighted
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_own),
      i.weighted_own,
      # Own-series, simple-mean (production == 0 but FABIO has output)
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_own),
      i.fallback_own,
      # Any-price, production-weighted  (= global-median fallback)
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_any),
      i.weighted_any,
      # Any-price, simple-mean  (= global-median fallback)
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_any),
      i.fallback_any,
      default = NA_real_
    ),
    price_source = fcase(
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_exact),
      fifelse(i.n_exact == 1L, "FAO_exact", "FAO_weighted"),
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_exact),
      fifelse(i.n_exact == 1L, "FAO_exact", "FAO_simple_mean"),
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_own),
      "FAO_own_series_median",
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_own),
      "FAO_own_series_median",
      (is.na(i.production) | i.production > 0) &
        !is.na(i.weighted_any),
      "FAO_global_median",
      !is.na(i.production) & i.production == 0 &
        !is.na(total_product_output) & total_product_output > 0 &
        !is.na(i.fallback_any),
      "FAO_global_median",
      default = NA_character_
    )
  ),
  on = .(area_code, item_code, year)
]

# Remove prices where there is neither output nor production — the simple-mean
# price could be misleading here.
overlap_values[
  (is.na(total_product_output) | total_product_output <= 0) &
    (is.na(production) | production <= 0),
  `:=`(price = NA_real_, price_source = NA_character_)
]

# Schema parity with residual_values for the rbindlist in section 8a. The
# FAO-side overlap aggregation in 5a does not currently produce a
# constituent tally — only the BTD-side aggregations in 7b/7c/7d do.
# Initialised NA here so the column is present at stack time. For ISIC-C
# "both"-mapped overlap rows (e.g. Wheat) it gets overwritten in section
# 8e from sua_isic_c_agg with the 7b tally describing the SUA-sum value.
overlap_values[, price_source_constituents := NA_character_]


# --- 5b. Diagnostic: FAO->CBS item-concordance audit -------------------------
# Section 5a aggregates FAO producer prices to FABIO/CBS grain assuming
# tcf = 1 on every (FAO, FABIO) pair (the item concordance has no tcf
# column). Look up each FAO_item_code in the BTD<->CBS concordances by code
# only -- the question is "what's the tcf from this item to its CBS bucket",
# and FABIO_item_code is irrelevant to that since FABIO and CBS coding
# systems can route the same item to different bucket codes without it
# being a unit issue. Flag two classes:
#   "tcf_not_1"    : FAO_item_code maps to a BTD<->CBS row with tcf != 1.
#                    The producer price is in BTD-tonnes but FABIO output is
#                    in CBS-equivalent tonnes -- direct unit mismatch.
#   "no_btd_match" : FAO_item_code does not appear in the BTD concordance
#                    at all. tcf cannot be checked from this side.
# ISIC-A and ISIC-C BTD<->CBS pairs are disjoint (verified) and no btd code
# has more than one cbs/tcf row across the union, so a plain rbind + unique
# is safe and the per-btd lookup is unambiguous.
#
# This audit is co-located with Section 5a (the FAO producer prices
# aggregation it audits) rather than with the residual-aggregation tcf
# diagnostic in Section 7e — they check different things, so they're
# kept apart.
btd_lookup <- unique(rbindlist(list(
  concordance_btd_cbs_isic_a[
    !is.na(btd_item_code) & !is.na(cbs_item_code),
    .(btd_item_code, btd_cbs_code = cbs_item_code,
      btd_cbs_item = cbs_item, tcf)
  ],
  concordance_btd_cbs_isic_c[
    !is.na(btd_item_code) & !is.na(cbs_item_code),
    .(btd_item_code, btd_cbs_code = cbs_item_code,
      btd_cbs_item = cbs_item, tcf)
  ]
)))

fao_cbs_audit <- merge(
  concordance_items_overlap[, .(FAO_item_code, FAO_item, FABIO_item_code, FABIO_item)],
  btd_lookup,
  by.x  = "FAO_item_code", by.y = "btd_item_code",
  all.x = TRUE
)

fao_cbs_audit[, status := fcase(
  is.na(tcf),         "no_btd_match",
  tcf != 1,           "tcf_not_1",
  default              = "ok"
)]

fao_cbs_flagged <- fao_cbs_audit[
  status != "ok"
][order(status, FABIO_item_code, FAO_item_code)]

fwrite(
  fao_cbs_flagged,
  PATHS$out_csv_overlap_fao_concordance_audit
)
message(
  "Overlap FAO->CBS concordance audit: ",
  fao_cbs_flagged[status == "tcf_not_1",    .N], " tcf != 1; ",
  fao_cbs_flagged[status == "no_btd_match", .N], " no BTD match. ",
  "Wrote ", PATHS$out_csv_overlap_fao_concordance_audit
)


# ==============================================================================
# 6. Initialize Residual Items (no FAO producer prices overlap)
# ==============================================================================
# Items not in the FAO producer prices concordance are priced via FAO
# SUA/BTD-level concordance-based aggregation in Section 7 — once per
# ISIC level, since the BTD bundle for the same CBS commodity differs
# between ISIC-A and ISIC-C (e.g. cbs 2000 ISIC-A = forage maize, hay,
# ...; cbs 2000 ISIC-C = lucerne meal & pellets). The two output tables
# can therefore legitimately carry different prices for the same residual
# cbs row.
#
# This section only builds the row scaffold. price/production/price_source
# stay NA here and are populated when each ISIC-level table is built in
# Section 8 by joining the per-level cbs_price_resid_isic_{a,c} tables
# computed in 7c / 7d.

residual_values <- output_long[
  labels[item_code %in% residual_item_codes],
  on = "row_id", nomatch = 0L, allow.cartesian = TRUE
]
residual_values[, year := as.character(year)]

# Schema parity with overlap_values (final stack uses use.names = TRUE).
residual_values[, `:=`(
  production                = NA_real_,
  price                     = NA_real_,
  price_source              = NA_character_,
  price_source_constituents = NA_character_
)]


# ==============================================================================
# 7. FAO SUA/BTD-Level Aggregations
# ==============================================================================
# This section drives every total-value computation that doesn't reduce to
# a direct (FABIO commodity, area, year) FAO producer prices match. There
# are three:
#
#   7b  ISIC-C dual-mapped items (cbs in `both_item_codes`):
#           total_value = sum_{b in btd_isic_c} prod_b * price_b
#         FABIO `total_product_output` reflects the ISIC-A primary side and
#         is NOT the right ISIC-C quantity, so the value is built directly
#         from the FAO BTD bundle. Example: cbs 2511 "Wheat and products"
#         ISIC-A side = btd 15 "Wheat"; ISIC-C side = btd {16 flour,
#         17 bran, 18 pasta, 19 germ, 20 bread, ...}.
#
#   7c  ISIC-A residuals (cbs in residual_item_codes ∩ isic_a_item_codes):
#           cbs_price = wmean(price_b, weight = prod_b)   over btd_isic_a
#         Replaces the previous direct (item_code) join with the raw FAO
#         BTD trade prices, which only worked when FABIO's item_code
#         coincided numerically with a single SUA/BTD code (true for live
#         animals, silently NA for CBS aggregates with multiple BTD
#         sub-categories such as cbs 2000 Fodder crops).
#
#   7d  ISIC-C residuals (cbs in residual_item_codes ∩ isic_c_item_codes):
#           cbs_price = wmean(price_b, weight = prod_b)   over btd_isic_c
#         Same construction as 7c, ISIC-C concordance.
#
# Inputs at FAO BTD/SUA grain (built once in 7a):
#   - fao_btd_production  : production from FAO SUA (sua_tidy.rds) with
#                           FAO Production_Crops_Livestock fallback.
#   - fao_btd_trade_prices: FAO-BTD-derived unit prices from script 13_2's
#                           bilateral_trade_prices.rds (already includes
#                           script 13_2's internal item_year_median /
#                           item_median / capped fallbacks).
#
# tcf is intentionally NOT applied in any of these aggregations — consistent
# with Section 5's FAO-producer-prices-side aggregation, which has no tcf
# either. The (cbs, btd) pairs whose tcf is non-1 are listed by 7e for
# audit.

# --- 7a. Build FAO BTD-grain inputs (production and prices) ----------------
# Combined FAO SUA/BTD-level production: SUA where available, FAO
# Production_Crops_Livestock fallback otherwise. Both inputs are already at
# (area_code, item_code, year) grain with item_code = SUA/BTD code; we
# rename to btd_item_code for clarity downstream.
fao_btd_production <- merge(
  fao_sua_production[, .(area_code, btd_item_code = item_code,
                         year, production_sua = production)],
  fao_crops_livestock_production[, .(area_code, btd_item_code = item_code,
                                     year, production_fao)],
  by  = c("area_code", "btd_item_code", "year"),
  all = TRUE
)
fao_btd_production[, production := fcoalesce(production_sua, production_fao)]
fao_btd_production[, c("production_sua", "production_fao") := NULL]

# FAO BTD-grain trade prices (raw output of script 13_2, which derives unit
# prices from FAO BTD; year cast to character to match the rest of the
# pipeline). Carries `price_source` through so the BTD-side aggregations
# below (7b/7c/7d) can build a constituent tally summarising script 13_2's
# provenance labels behind each CBS-grain aggregate. Script 13_2 uses the
# label "none" for grid cells with no priced data (`price` is NA on
# those rows); we collapse "none" to NA here so build_source_tally's
# `!is.na(price_source)` filter naturally excludes constituents that
# contributed nothing to the aggregate.
fao_btd_trade_prices <- fao_btd_trade_prices_raw[, .(
  area_code,
  btd_item_code = item_code,
  year          = as.character(year),
  price,
  price_source  = fifelse(is.na(price) | price_source == "none",
                          NA_character_, price_source)
)]


# --- 7b. ISIC-C dual-mapped items: SUA-sum aggregation ---------------------
# Cross the (cbs, btd) map for both-items with the BTD-grain production
# table — one row per (cbs, btd, area, year) that has production data.
# Then attach the BTD-grain price.
sua_isic_c_long <- merge(
  isic_c_btd_for_both[, .(cbs_item_code, btd_item_code)],
  fao_btd_production,
  by              = "btd_item_code",
  allow.cartesian = TRUE
)
sua_isic_c_long[
  fao_btd_trade_prices,
  `:=`(price = i.price, price_source = i.price_source),
  on = .(area_code, btd_item_code, year)
]

# Aggregate to (area_code, cbs_item_code, year). `item_code` rename matches
# the column name in total_values for the join in section 8. Both factors
# are summed over the SAME set of (production, price) rows so that the
# implied weighted-average price (value / production) is well-defined.
# sum_value / sum_production_where_priced both return NA (rather than 0)
# when no BTD sub-category has both a non-NA production and a non-NA price
# — keeps gaps visible in the output.
#
# `price_source_constituents` tallies the script-2 provenance labels of
# the BTD constituents that went into `sua_aggregated_value`, weighted by
# production. This is the only place the constituent provenance lives for
# "both"-mapped rows in the ISIC-C output: section 8e blanks `price` and
# `price_source` on those rows but keeps the tally.
#
# Unit note. Every "both"-mapped CBS item happens to be in tonnes on both
# sides of the ISIC-A / ISIC-C concordance (cereals, pulses, vegetables,
# fruits, milk, cocoa, fish), so summing across the BTD sub-categories
# within a single CBS bundle is unit-consistent. The bundle does mix
# qualitatively different products (e.g. flour + bran + pasta + bread for
# cbs 2511) and the resulting tonnage is the bundle's GROSS physical
# output, not a primary-equivalent — tcf is intentionally not applied,
# consistent with the value side. Downstream consumers using the bundle
# tonnage as a phys_intensity denominator at ISIC-C are getting "USD per
# tonne of bundle output", which is exactly the right unit for an
# ISIC-C-grain VA-per-output ratio.
sua_isic_c_agg <- sua_isic_c_long[
  ,
  .(sua_aggregated_value      = sum_value(production, price),
    sua_aggregated_production = sum_production_where_priced(production, price),
    price_source_constituents = build_source_tally(price_source, production)),
  by = .(area_code, item_code = cbs_item_code, year)
]

message(
  "ISIC-C SUA/BTD aggregation: ",
  nrow(sua_isic_c_agg), " (area_code, cbs, year) rows; ",
  sua_isic_c_agg[!is.na(sua_aggregated_value),      .N], " with non-NA value, ",
  sua_isic_c_agg[!is.na(sua_aggregated_production), .N], " with non-NA bundle production."
)


# --- 7c. ISIC-A residual CBS-price aggregation -----------------------------
# For FABIO commodities NOT in the FAO producer prices concordance
# ("residuals") AND in the ISIC-A concordance, derive a CBS-level price as
# a production-weighted mean of their ISIC-A FAO BTD sub-categories'
# trade-derived prices. Replaces the previous direct (area_code, item_code,
# year) join with the raw FAO BTD trade prices — that join only worked
# when FABIO's item_code happened to coincide numerically with a single
# SUA/BTD code (true for most ISIC-A residuals such as live animals, but
# silently NA for CBS aggregates with multiple BTD sub-categories like cbs
# 2000 Fodder crops). tcf is intentionally ignored; non-1 tcf values are
# flagged in the 7e diagnostic.
#
# Override-handled CBS items are excluded from the default per-BTD
# aggregation entirely on the ISIC-A side and supplied directly from
# script 13_2's CBS-grain override (already filtered + winsorized + gap-
# filled + capped at CBS grain). The two row sets are disjoint by
# construction (different cbs_item_code) so they're stacked with
# rbindlist rather than merged via update-join. This guarantees that
# override items can never carry a residual `trade_btd_*` provenance
# label (`trade_btd_simple_mean` in particular) — the override is the
# sole source of price / price_source for them.

# `aggregate_btd_to_cbs()` (defined in Helper Functions) is the workhorse
# for both 7c and 7d. It outer-merges production and price first so that a
# BTD sub-category with a price but no production data can still contribute
# to the simple-mean fallback (zero weight excludes it from the weighted
# mean by design), then collapses to CBS grain via the supplied BTD<->CBS
# map and labels each row's price source.

# Override CBS item codes per ISIC side (handled exclusively by the
# CBS-grain override). Used to (i) restrict the default aggregation to
# non-override items below, (ii) filter the tcf diagnostic in 7e.
override_codes_isic_a <- unique(
  fao_btd_trade_prices_cbs_override_raw[isic_side == "a", cbs_item_code]
)
override_codes_isic_c <- unique(
  fao_btd_trade_prices_cbs_override_raw[isic_side == "c", cbs_item_code]
)

isic_a_btd_for_resid <- concordance_btd_cbs_isic_a[
  cbs_item_code %in% intersect(residual_item_codes, isic_a_item_codes) &
    !is.na(btd_item_code) &
    !cbs_item_code %in% override_codes_isic_a,
  .(cbs_item_code, cbs_item, btd_item_code, btd_item, tcf)
]
cbs_price_resid_isic_a_default <- aggregate_btd_to_cbs(
  isic_a_btd_for_resid, fao_btd_production, fao_btd_trade_prices
)

message(
  "ISIC-A residual CBS-price aggregation (default, override items excluded): ",
  nrow(cbs_price_resid_isic_a_default), " (area_code, cbs, year) rows; ",
  cbs_price_resid_isic_a_default[!is.na(price), .N], " with non-NA price across ",
  uniqueN(isic_a_btd_for_resid$cbs_item_code), " residual cbs codes."
)

# CBS-grain override rows for the override items (script 13_2 §7.5). Disjoint
# from the default rows by construction, so a plain rbindlist is correct.
# `price_source_constituents` is NA for override rows: the override is
# single-source by construction (priced directly at CBS grain in script 13_2
# §7.5), so `price_source` already carries the full label
# (trade_cbs_direct, trade_cbs_year_median, etc.) and a constituent tally
# would be meaningless.
cbs_override_isic_a <- fao_btd_trade_prices_cbs_override_raw[
  isic_side == "a",
  .(area_code, item_code = cbs_item_code,
    year = as.character(year), price, price_source,
    price_source_constituents = NA_character_)
]
cbs_price_resid_isic_a <- rbindlist(
  list(cbs_price_resid_isic_a_default, cbs_override_isic_a),
  use.names = TRUE
)
message(
  "  CBS-grain override stacked for ",
  uniqueN(cbs_override_isic_a$item_code), " ISIC-A item(s): ",
  paste(sort(unique(cbs_override_isic_a$item_code)), collapse = ", ")
)


# --- 7d. ISIC-C residual CBS-price aggregation -----------------------------
# Same construction as 7c but using the ISIC-C BTD<->CBS concordance.
# Includes "both"-mapped residuals (cbs 2000, 2586) so their `price` column
# in the ISIC-C output table reflects the ISIC-C bundle (informational —
# their total_value is from the SUA-sum aggregation in 7b, not output *
# price). As in 7c, override CBS items on the ISIC-C side (e.g.
# 2748 Hides and Skins) are excluded from the default per-BTD
# aggregation and supplied directly from the CBS-grain override.
isic_c_btd_for_resid <- concordance_btd_cbs_isic_c[
  cbs_item_code %in% intersect(residual_item_codes, isic_c_item_codes) &
    !is.na(btd_item_code) &
    !cbs_item_code %in% override_codes_isic_c,
  .(cbs_item_code, cbs_item, btd_item_code, btd_item, tcf)
]
cbs_price_resid_isic_c_default <- aggregate_btd_to_cbs(
  isic_c_btd_for_resid, fao_btd_production, fao_btd_trade_prices
)

message(
  "ISIC-C residual CBS-price aggregation (default, override items excluded): ",
  nrow(cbs_price_resid_isic_c_default), " (area_code, cbs, year) rows; ",
  cbs_price_resid_isic_c_default[!is.na(price), .N], " with non-NA price across ",
  uniqueN(isic_c_btd_for_resid$cbs_item_code), " residual cbs codes."
)

# CBS-grain override rows for the override items (script 13_2 §7.5). Note
# that for CBS items that are "both"-mapped (e.g. cbs 2000 Fodder Crops)
# the ISIC-C `price` column is later blanked in section 8e and value
# comes from sua_isic_c_agg (7b) — so an override here only matters
# for ISIC-C-only residuals (e.g. cbs 2748 Hides and Skins). NA tally
# for the same reason as on the ISIC-A side: override rows are
# single-source.
cbs_override_isic_c <- fao_btd_trade_prices_cbs_override_raw[
  isic_side == "c",
  .(area_code, item_code = cbs_item_code,
    year = as.character(year), price, price_source,
    price_source_constituents = NA_character_)
]
cbs_price_resid_isic_c <- rbindlist(
  list(cbs_price_resid_isic_c_default, cbs_override_isic_c),
  use.names = TRUE
)
message(
  "  CBS-grain override stacked for ",
  uniqueN(cbs_override_isic_c$item_code), " ISIC-C item(s): ",
  paste(sort(unique(cbs_override_isic_c$item_code)), collapse = ", ")
)


# --- 7e. Diagnostic: BTD<->CBS pairs with tcf != 1 -------------------------
# Lists (cbs, btd) pairs whose tcf != 1 in the residual CBS-price aggregations
# (7c and 7d), where ignoring tcf is a deliberate modelling choice that
# affects the result: the BTD-grain prices are collapsed via wmean_price into
# a single CBS-grain price, which is then multiplied by FABIO
# `total_product_output` (a CBS-grain quantity); when tcf != 1 the BTD price
# (USD per BTD-tonne) and the CBS quantity are not strictly comparable, and
# folding them together as if they were is the choice this CSV audits.
#
# Override-handled CBS items (script 13_2 §7.5: 2000, 2029 on ISIC-A side;
# 2748 on ISIC-C side) are NOT listed here. Their tcf is correctly
# applied at CBS grain in the override (mass-only rescaling on the
# constituent BTDs before sum(USD)/sum(qty)), so flagging them as
# "ignored" would be inaccurate. The exclusion is enforced upstream,
# in 7c / 7d, by filtering `isic_a_btd_for_resid` and
# `isic_c_btd_for_resid` against `override_codes_isic_a` /
# `override_codes_isic_c` — those tables already exclude override items
# by the time they reach this section.
#
# The 7b "both"-mapped sum aggregation is also NOT included here, even
# though many of its (cbs, btd) pairs have tcf != 1 (cheese 0.2, yoghurt
# 0.8, etc.). That aggregation operates at BTD grain end-to-end —
#   total_value = sum_b production_b * price_b
# with both factors in their native BTD units — so tcf is structurally
# irrelevant to it, not "ignored". Listing those pairs alongside the residual
# ones would conflate "we're taking a known shortcut" with "this factor
# doesn't enter the math at all".
tcf_ignored <- rbindlist(list(
  isic_a_btd_for_resid[tcf != 1,
                       .(used_in = "isic_a_resid_weighted", cbs_item_code, cbs_item,
                         btd_item_code, btd_item, tcf)],
  isic_c_btd_for_resid[tcf != 1,
                       .(used_in = "isic_c_resid_weighted", cbs_item_code, cbs_item,
                         btd_item_code, btd_item, tcf)]
))
setorder(tcf_ignored, used_in, cbs_item_code, btd_item_code)

fwrite(tcf_ignored, PATHS$out_csv_residual_btd_tcf_ignored)
message(
  "Residual BTD->CBS TCF diagnostic: ", nrow(tcf_ignored),
  " (cbs, btd) pairs with tcf != 1 ignored across the 7c/7d ",
  "residual aggregations. Wrote ", PATHS$out_csv_residual_btd_tcf_ignored
)


# ==============================================================================
# 8. Combine, Build ISIC-A and ISIC-C Tables, and Export
# ==============================================================================

# --- 8a. Standardise columns and stack ----------------------------------------
# Carry both codes and human-readable names into the final output. The
# SUA-aggregated ISIC-C value is filled in via a join right after the rename
# below — it's a (area_code, cbs_item_code, year)-keyed quantity computed in
# section 7 and is NA for rows whose item is not "both"-mapped.
col_order <- c("row_id", "iso3c", "area_code", "area",
               "item_code", "comm_code", "item", "comm_group", "unit", "year",
               "total_product_output", "production", "price",
               "price_source", "price_source_constituents")

overlap_values  <- overlap_values[, ..col_order]
residual_values <- residual_values[, ..col_order]

total_values <- rbindlist(list(overlap_values, residual_values), use.names = TRUE)

# --- 8b. Apply human-readable column names ------------------------------------
setnames(total_values, c(
  "row_id", "iso3c", "area_code", "area",
  "item_code", "comm_code", "item", "comm_group", "unit", "year",
  "total_product_output [tonnes]/[animals]/[1000 animals]",
  "production [tonnes]",
  "price [USD/unit]",
  "price_source",
  "price_source_constituents"
))

# --- 8c. Price-source distribution (after overlap/residual stack) ------------
# At this point, residual rows still carry price = NA — they're filled per
# ISIC level in 8d / 8e from the cbs_price_resid_isic_{a,c} aggregations.
message("Price source distribution (overlap rows; residuals filled per ISIC level):")
print(total_values[!is.na(price_source), .N, by = price_source][order(-N)])

# --- 8d. Build the ISIC-A table -----------------------------------------------
# All FABIO commodities mapped to ISIC-A. The FABIO total product output is
# the correct ISIC-A physical quantity for every row, so total_value is
# simply output * price.
#
# Membership comes from the GLORIA <-> FABIO ISIC-A concordance (Section 1k),
# not from the BTD <-> CBS ISIC-A concordance — see 1j for rationale.
#
# Residual rows: fill price from cbs_price_resid_isic_a (Section 7c).
# Overlap rows: already priced in Section 5; the residual table is keyed
# only on residual cbs codes, so the join below leaves them untouched.
isic_a_values <- total_values[item_code %in% isic_a_member_codes]

# Integrity check: catch the silent-drop class of bug at the membership
# filter. Any GLORIA-mapped item absent from FABIO labels (so with no
# row to land in `total_values`) shows up here.
gloria_a_unmatched <- setdiff(isic_a_member_codes, unique(isic_a_values$item_code))
if (length(gloria_a_unmatched) > 0) {
  warning(
    "ISIC-A: ", length(gloria_a_unmatched),
    " GLORIA-mapped item code(s) absent from FABIO labels — no rows in ",
    "output: ", paste(sort(gloria_a_unmatched), collapse = ", ")
  )
}

isic_a_values[
  cbs_price_resid_isic_a,
  `:=`(`price [USD/unit]`         = i.price,
       price_source               = i.price_source,
       price_source_constituents  = i.price_source_constituents),
  on = .(area_code, item_code, year)
]

# Residual-side output guard: avoid carrying a phantom price on rows with
# no FABIO output (would otherwise show up in Q&A as "priced but zero
# value" anomalies). Restricted to residual rows; overlap rows have their
# own (more permissive) guard in Section 5.
isic_a_values[
  item_code %in% residual_item_codes &
    (is.na(`total_product_output [tonnes]/[animals]/[1000 animals]`) |
       `total_product_output [tonnes]/[animals]/[1000 animals]` <= 0),
  `:=`(`price [USD/unit]`         = NA_real_,
       price_source               = NA_character_,
       price_source_constituents  = NA_character_)
]

isic_a_values[, `total_value [USD]` := fcase(
  `total_product_output [tonnes]/[animals]/[1000 animals]` == 0 |
    `price [USD/unit]` == 0, 0,
  default = `total_product_output [tonnes]/[animals]/[1000 animals]` *
    `price [USD/unit]`
)]
isic_a_values[, total_value_source := "output_x_price"]

# --- 8e. Build the ISIC-C table -----------------------------------------------
# All FABIO commodities mapped to ISIC-C. For commodities ALSO in the ISIC-A
# concordance ("both"-mapped), the FABIO total product output reflects the
# ISIC-A primary side, so total_value is taken from the FAO SUA/BTD-grain
# aggregate computed in section 7b: sum over the ISIC-C BTD sub-categories
# of (fao_production_btd * fao_btd_trade_price_btd). For commodities only
# in the ISIC-C concordance (processed items such as Soyabean Oil, Palm
# Oil, Raw Sugar) the regular output * price formula applies — the FABIO
# output already represents the processed quantity.
#
# `price` and `price_source` are explicitly set to NA for "both"-mapped rows
# at the end of this section (see "Step 4 — Both-mapped: clear price columns"
# below). The two candidates for that column would be (i) the FAO producer
# price for the ISIC-A primary commodity (Wheat, raw milk, ...) — already on
# the row from Section 5 — or (ii) the ISIC-C BTD-bundle weighted price
# computed by 7d for the residual ones (Fodder Crops 2000, Oilcrops Other Oil
# 2586). Neither is the right unit price for the row's `total_value`, and
# exposing them in the same column would mean it carries one of two
# different meanings depending on the row, so we blank both. The value lives
# in `sua_aggregated_value [USD]`; `total_value_source = "SUA_aggregated"`
# flags these rows. `price_source_constituents` is NOT blanked on these
# rows — it is overwritten in Step 3 with the 7b SUA-sum tally so that it
# describes `sua_aggregated_value` rather than the now-blanked `price`.
#
# Membership comes from the GLORIA <-> FABIO ISIC-C concordance (Section 1k),
# not from the BTD <-> CBS ISIC-C concordance — see 1j for rationale.
isic_c_values <- total_values[item_code %in% isic_c_member_codes]

# Integrity check, mirror of the ISIC-A one in section 8d.
gloria_c_unmatched <- setdiff(isic_c_member_codes, unique(isic_c_values$item_code))
if (length(gloria_c_unmatched) > 0) {
  warning(
    "ISIC-C: ", length(gloria_c_unmatched),
    " GLORIA-mapped item code(s) absent from FABIO labels — no rows in ",
    "output: ", paste(sort(gloria_c_unmatched), collapse = ", ")
  )
}

# Section 8e fill order. There are two BTD-side aggregations that can
# write to (price, price_source, price_source_constituents) on ISIC-C
# rows, and we have to decide who wins on the rows where they overlap
# (the "both"-mapped CBS items that are ALSO residuals — cbs 2000 Fodder
# Crops, cbs 2586 Oilcrops Other Oil — where 7d builds a wmean_price and
# 7b builds the SUA-sum):
#   Step 1. 7d join (cbs_price_resid_isic_c) — fills price /
#           price_source / price_source_constituents on residual rows,
#           including both-mapped residuals.
#   Step 2. Residual output guard — blanks the three columns on ISIC-C-
#           only residuals with no FABIO output. Both-mapped residuals
#           are excluded from this guard (their `total_product_output`
#           reflects the ISIC-A primary and is not the right
#           denominator).
#   Step 3. 7b join (sua_isic_c_agg) — fills sua_aggregated_value /
#           sua_aggregated_production on both-mapped rows, AND
#           OVERWRITES price_source_constituents with the SUA-sum tally.
#           This is intentional: the row's monetary aggregate on
#           both-mapped rows is `sua_aggregated_value`, so the tally
#           must describe that, not the parallel 7d wmean_price.
#   Step 4. Both-mapped blanking — clears price / price_source on
#           both-mapped rows but KEEPS price_source_constituents (now
#           describing sua_aggregated_value).
# This order differs from a naive "7b then 7d" flow because 7b has to
# come last on the tally column for both-mapped residuals.

# Step 1 — Residual rows: fill price from cbs_price_resid_isic_c (Section 7d).
isic_c_values[
  cbs_price_resid_isic_c,
  `:=`(`price [USD/unit]`         = i.price,
       price_source               = i.price_source,
       price_source_constituents  = i.price_source_constituents),
  on = .(area_code, item_code, year)
]

# Step 2 — Output guard for ISIC-C-only residuals: blank the price when no
# FABIO output is reported, to avoid carrying a phantom price on rows that
# contribute zero to total_value. Excluded from this guard: "both"-mapped
# items, where `total_product_output` reflects the ISIC-A primary quantity
# and is not the right denominator for an ISIC-C unit price in any case
# (see the "Both-mapped: clear price columns" step below, which sets their
# price to NA unconditionally).
isic_c_values[
  item_code %in% residual_item_codes &
    !(item_code %in% both_item_codes) &
    (is.na(`total_product_output [tonnes]/[animals]/[1000 animals]`) |
       `total_product_output [tonnes]/[animals]/[1000 animals]` <= 0),
  `:=`(`price [USD/unit]`         = NA_real_,
       price_source               = NA_character_,
       price_source_constituents  = NA_character_)
]

# Step 3 — Attach the BTD-aggregated ISIC-C value AND bundle production
# (and overwrite the constituent tally on both-mapped rows; see header
# block above for why this has to come AFTER the 7d join). The
# `sua_isic_c_agg` table only has rows for both-mapped CBS codes (it is
# built from `isic_c_btd_for_both`), so the tally overwrite only fires
# on those rows; ISIC-C-only residuals keep the tally written by step 1.
isic_c_values[
  sua_isic_c_agg,
  `:=`(`sua_aggregated_value [USD]`         = i.sua_aggregated_value,
       `sua_aggregated_production [tonnes]` = i.sua_aggregated_production,
       price_source_constituents            = i.price_source_constituents),
  on = .(area_code, item_code, year)
]

# Sanity check: by construction sua_aggregated_value and
# sua_aggregated_production are both NA or both non-NA on the same row
# (same `valid` mask in their two helpers). A divergence means one of the
# helpers was edited without updating the other.
divergent_value_prod <- isic_c_values[
  item_code %in% both_item_codes &
    is.na(`sua_aggregated_value [USD]`) !=
    is.na(`sua_aggregated_production [tonnes]`),
  .N
]
if (divergent_value_prod > 0L) {
  warning(
    divergent_value_prod, " 'both'-mapped rows have value/production ",
    "NA-disagreement; check sum_value vs sum_production_where_priced."
  )
}

# Step 4 — Both-mapped: clear price columns. See section header for
# rationale. price_source_constituents is INTENTIONALLY left alone — it
# now describes `sua_aggregated_value [USD]` (which is the row's actual
# monetary aggregate after this point) rather than the blanked `price`,
# and is the only place that provenance lives once price_source is NA.
isic_c_values[
  item_code %in% both_item_codes & !is.na(`sua_aggregated_value [USD]`),
  `:=`(`price [USD/unit]` = NA_real_, price_source = NA_character_)
]

isic_c_values[, total_value_source := fifelse(
  item_code %in% both_item_codes,
  "SUA_aggregated",
  "output_x_price"
)]

# total_value formula. Note the asymmetry — by design:
#   * Both-mapped rows (total_value_source == "SUA_aggregated"): take
#     sua_aggregated_value directly. `total_product_output` plays NO role
#     here, including the 0-zeros-out rule of the second branch — it
#     reflects the ISIC-A primary quantity, not the ISIC-C bundle, so a
#     zero or NA there says nothing about the ISIC-C value.
#   * ISIC-C-only rows: standard output * price with the 0-vs-NA guard;
#     output IS the right denominator here so a zero output really does
#     mean a zero ISIC-C value.
isic_c_values[, `total_value [USD]` := fcase(
  # Both-mapped: use SUA-level aggregate (NA propagates if unavailable).
  total_value_source == "SUA_aggregated",
  `sua_aggregated_value [USD]`,
  # ISIC-C-only: use the regular output * price formula, with the same
  # 0-vs-NA guard as the ISIC-A branch.
  `total_product_output [tonnes]/[animals]/[1000 animals]` == 0 |
    `price [USD/unit]` == 0, 0,
  default = `total_product_output [tonnes]/[animals]/[1000 animals]` *
    `price [USD/unit]`
)]

# --- 8f. Write outputs --------------------------------------------------------
saveRDS(isic_a_values, file = PATHS$out_rds_isic_a)
fwrite(isic_a_values, PATHS$out_csv_isic_a)

saveRDS(isic_c_values, file = PATHS$out_rds_isic_c)
fwrite(isic_c_values, PATHS$out_csv_isic_c)

message("Done. Wrote:")
message("  ISIC-A: ", PATHS$out_rds_isic_a)
message("          ", PATHS$out_csv_isic_a)
message("  ISIC-C: ", PATHS$out_rds_isic_c)
message("          ", PATHS$out_csv_isic_c)
message("  Diagnostics:")
message("    Residual BTD->CBS TCF ignored:   ", PATHS$out_csv_residual_btd_tcf_ignored)
message("    Overlap FAO->CBS concordance:    ", PATHS$out_csv_overlap_fao_concordance_audit)