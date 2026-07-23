# ==============================================================================
# FABIOv2 Value-Added — Merged ISIC-A + ISIC-C Pipeline
#   *** COMPONENT-SPLIT: wages / capital / taxes-less-subsidies ***
#   *** GLORIA and EXIOBASE bases from one engine ***
#
# Description:
#   Computes per-strand value added for FABIOv2 from an upstream MRIO database,
#   splitting that database's value-added block into THREE strands up front and
#   running every stage from the collapse onward INDEPENDENTLY per strand:
#       wages   — Compensation of employees
#       capital — Net operating surplus + consumption of fixed capital
#                 (+ rents / royalties, where the database separates them)
#       tls     — Taxes less subsidies on production
#
#   Two upstream bases are supported through a small ADAPTER each; everything
#   downstream of the adapter is a single shared ENGINE.  An adapter turns one
#   database-year into a long table of pure per-strand share intensities
#   (VA_strand / producer-price output) keyed (region, sector, va_component,
#   year), plus a per-year factor that converts that database's native currency
#   to USD.  The engine then does, identically for both bases:
#       stage 4a  Hampel filter on the share intensities (per region×sector×strand)
#       stage 4b  per-(sector, strand) IHS/MAD winsor
#       stage 5   map to FABIO, X-weighted, per strand; fish carve-out per strand
#       stage 6   value_added_<strand> [USD] = intensity_winsor × total_value
#                 (direct transfer for fish); unmapped FABIO items → 0
#       stage 7   per-strand physical-intensity Hampel (7a) + per-(item, strand)
#                 IHS/MAD cap (7b); re-derive the strand value (7c); pivot wide
#                 and DERIVE value_added [USD] = sum of the three strands (7d)
#       stage 8   GLORIA/EXIOBASE ↔ FABIO reconciliation on the derived total
#
#   The front half (stages 4a/4b on the database-wide intensities) runs once per
#   base; the back half (stages 5–8) runs twice per base, once per ISIC level,
#   via process_isic_level().
#
#   The three strands partition the database's VA rows, so their sum is the
#   database's total VA: no value is created or lost by the split — only the
#   cleaning is applied per strand instead of to the summed series.  The derived
#   total therefore reflects per-strand cleaning rather than cleaning of the
#   already-summed series.
#
# Adapters (see GLORIA_ADAPTER / EXIOBASE_ADAPTER below):
#   GLORIA v060   — V matrix has VA accounts on the row axis in per-region
#                   blocks; the collapse sums each strand's VA rows down each
#                   sector column within its region block.  VA-row → strand is
#                   read from the "Value added and final demand" sheet labels by
#                   an exact lookup.  Native currency 1000 USD (→ USD is ×1000).
#   EXIOBASE 3.10 — factor-input matrix F (9 × n_cols) already has one column
#                   per (region × industry) cell, so the collapse is a plain
#                   colSums over each strand's F rows.  Row 1 of F is NOT VA: it
#                   is added to basic-prices X to lift it to PRODUCER prices (the
#                   denominator of every share intensity), then negatives are
#                   floored at zero.  Rows 2..9 partition into strands by a
#                   hard-coded index map, cross-checked against the on-disk
#                   labels.  Native currency M.EUR (→ USD via the FAOSTAT
#                   Germany EUR/USD rate; see "Currency" below).
#
# Currency:
#   Each adapter supplies a per-year usd_factor (year → usd_per_unit).  Share
#   intensities are dimensionless, so currency only matters where a VA *level*
#   crosses to the FABIO side: the Fish, Seafood direct transfer and the stage-8
#   reconciliation totals.  Both multiply the native-currency level by
#   usd_per_unit[year].
#     GLORIA   : usd_per_unit = 1000 for every year (1000 USD → USD).
#     EXIOBASE : usd_per_unit = 1e6 / eur_per_usd, where eur_per_usd is Germany's
#                EUR/USD rate read from the FAOSTAT Exchange_rate_E_All_Data.csv
#                using Element Code "SLC" and Area Code 79 — the SAME source and
#                direction the synthesis script (14_4) uses as its single currency
#                source of truth (USD = native / rate).  A year with no rate
#                yields NA, so the fish transfer for that year falls through to 0
#                and the reconciliation EXIOBASE-side total is NA→0 (warned at
#                startup).
#
# Which bases to run:
#   DATABASES_TO_RUN selects which adapters run (default: both).  GLORIA needs
#   the readxl and qs2 packages; they are required only inside the GLORIA
#   adapter, so an EXIOBASE-only run does not need them installed.
#
# Outputs (per base, per ISIC level; <TAG> = "GLORIA" or "EXIOBASE"):
#   - output/FABIOv2_<TAG>_value_added_ISIC-{A,C}.rds  / .csv
#       FABIO context columns, then value_added_{wages,capital,tls} [USD], then
#       the derived value_added [USD] total last.  This schema is identical
#       across bases and is what the synthesis (14_4) consumes.
#   - output/diagnostics/FABIOv2_<TAG>_share_intensity_hampel.csv   (4a, long over va_component)
#   - output/diagnostics/FABIOv2_<TAG>_share_intensity_winsor.csv   (4b, long over va_component)
#   - output/diagnostics/FABIOv2_<TAG>_phys_intensity_hampel_ISIC-{A,C}.csv
#   - output/diagnostics/FABIOv2_<TAG>_phys_intensity_winsor_ISIC-{A,C}.csv
#   - output/diagnostics/FABIOv2_<TAG>_va_reconciliation_ISIC-{A,C}.csv  (derived total)
#   Diagnostic FILE names are unchanged; diagnostic COLUMN names are now generic
#   (region_code / region_name / sector_code / sector_name, and base_* in the
#   reconciliation) rather than gloria_* / exiobase_*.  The main outputs above
#   carry no such prefixes and are unaffected.
#
# Units:
#   - native VA, X (per base)        : 1000 USD (GLORIA) | M.EUR (EXIOBASE)
#   - va_intensity*                  : dimensionless (strand share of monetary output)
#   - FABIO total_product_output     : tonnes | animals | 1000 animals
#   - FABIO total_value              : USD
#   - va_phys_intensity*             : USD / physical unit (item-specific)
#   - value_added_<strand> [USD]     : USD
#   - value_added [USD]              : USD  (= wages + capital + tls)
#   - sua_aggregated_production      : tonnes (ISIC-C only)
# ==============================================================================

library(data.table)
source("R/00_value_added_config.R")


# ============================================================================
# SHARED CONFIGURATION
# ============================================================================

INPUT_DIR  <- VA_CONCORDANCE_DIR
OUTPUT_DIR <- VA_VALUE_ADDED_OUTPUT_DIR
DIAG_DIR   <- VA_VALUE_ADDED_DIAG_DIR

FABIO_TV_PATH_A <- VA_FABIO_TV_ISIC_A_RDS
FABIO_TV_PATH_C <- VA_FABIO_TV_ISIC_C_RDS

# FAOSTAT exchange-rate file (used only by the EXIOBASE adapter for M.EUR→USD).
# Germany's EUR/USD rate is taken from Element Code "SLC", Area Code 79 — the
# same source and direction the synthesis script (14_4) uses.
FX_PATH           <- VA_EXCHANGE_RATE_CSV
FX_ELEMENT_CODE   <- VA_FX_ELEMENT_CODE
FX_GERMANY_AREA   <- VA_GERMANY_AREA_CODE

HAMPEL_THRESHOLD   <- VA_HAMPEL_THRESHOLD   # Hampel spike test threshold (stages 4a, 7a)
WINSOR_MAD_K       <- 3.5                   # cross-sectional MAD cap, robust |z| (stages 4b, 7b)
HAMPEL_HALF_WINDOW <- VA_HAMPEL_HALF_WINDOW # Hampel half-window → full = 2*half_window+1 = 7 years

# FALSE bypasses the stage 7a/7b physical-intensity Hampel + MAD winsor for all
# items except fish, which is always filtered.
VA_PHYS_INTENSITY_FILTER <- FALSE

# The three strands, in output column order.  Both adapters assign their VA rows
# to exactly these.
VA_COMPONENTS <- c("wages", "capital", "tls")

# Output column names (identical across bases).
va_value_col  <- function(comp) sprintf("value_added_%s [USD]", comp)
VA_TOTAL_COL  <- BASE_TOTAL_COL   # "value_added [USD]" (shared; from helpers)
VA_VALUE_COLS <- vapply(VA_COMPONENTS, va_value_col, character(1))

FISH_SEAFOOD_ITEM <- "Fish, Seafood"
SUA_PROD_COL      <- "sua_aggregated_production [tonnes]"

# Which adapters to run.  Trim to a single name to build one base.
DATABASES_TO_RUN <- c("GLORIA", "EXIOBASE")

va_ensure_dir(OUTPUT_DIR)
va_ensure_dir(DIAG_DIR)


# ============================================================================
# SHARED ENGINE — BACK HALF (stages 5–8), runs once per ISIC level
# ============================================================================
#
# Generic over the upstream base.  `intensities` is the cleaned (post-4b) long
# table with generic keys region_code / sector_code / sector_name / va_component
# and native-currency columns va, x (plus va_intensity, va_intensity_winsor).
# `usd_factor` is data.table(year, usd_per_unit); the two level conversions
# (fish transfer, stage-8 totals) multiply native levels by usd_per_unit[year].

process_isic_level <- function(ctx, isic_level, sector_conc, fv,
                               value_col, output_col, fish_sector_codes,
                               use_sua_prod_denom) {
  
  intensities <- ctx$intensities
  usd_factor  <- ctx$usd_factor
  col_idx     <- ctx$col_idx
  area_conc   <- ctx$area_conc
  tag         <- ctx$file_tag
  key         <- ctx$key
  label       <- ctx$label
  
  suffix           <- sprintf("ISIC-%s", isic_level)
  out_rds_path     <- file.path(OUTPUT_DIR, paste0(va_va_output_basename(tag, suffix), ".rds"))
  out_csv_path     <- file.path(OUTPUT_DIR, paste0(va_va_output_basename(tag, suffix), ".csv"))
  diag_phys_hampel <- file.path(DIAG_DIR,   paste0(va_va_diag_basename(tag, "phys_intensity_hampel", suffix), ".csv"))
  diag_phys_winsor <- file.path(DIAG_DIR,   paste0(va_va_diag_basename(tag, "phys_intensity_winsor", suffix), ".csv"))
  diag_recon_path  <- file.path(DIAG_DIR,   paste0(va_va_diag_basename(tag, "va_reconciliation", suffix), ".csv"))
  
  message(sprintf("\n══════════════════════════════════════════════════════════════════════"))
  message(sprintf("  [%s] %s back half: stages 5–8  (per strand: %s)",
                  label, suffix, paste(VA_COMPONENTS, collapse = ", ")))
  message(sprintf("══════════════════════════════════════════════════════════════════════"))
  
  
  # ── 5. Apply concordance, aggregate to FABIO level (per strand) ───────────
  message(sprintf("[%s] Mapping %s → FABIO via concordance and aggregating (per strand) ...",
                  suffix, label))
  
  dt_mapped <- sector_conc[
    intensities, on = "sector_code", allow.cartesian = TRUE, nomatch = NULL
  ]
  dt_mapped <- area_conc[
    dt_mapped, on = "region_code", allow.cartesian = TRUE, nomatch = NULL
  ]
  
  # 5a. Non-fish: X-weighted intensity aggregation, per strand.  x is the
  # producer-price sector output (identical across strands); grouping by
  # va_component keeps each strand's numerator separate while the denominator
  # Σx is the same per group.  The weighting matters for N:1 AREA mappings
  # (e.g. FABIO's "Rest of World"); sector-side mappings are 1:1 or 1:N and
  # simply propagate the parent intensity to each FABIO child.
  fabio_intensities <- dt_mapped[
    !(sector_code %in% fish_sector_codes),
    .(va        = sum(va,                       na.rm = TRUE),
      va_winsor = sum(va_intensity_winsor * x,  na.rm = TRUE),
      x         = sum(x,                        na.rm = TRUE)),
    by = .(fabio_area_code, fabio_item_code, va_component, year)
  ]
  fabio_intensities[, va_intensity        := fifelse(x > 0, va        / x, NA_real_)]
  fabio_intensities[, va_intensity_winsor := fifelse(x > 0, va_winsor / x, NA_real_)]
  fabio_intensities[, `value_added_direct [USD]` := NA_real_]
  
  # 5b. Fish carve-out, per strand: cleaned strand-VA (native currency) ×
  # TPO-share disagg, converted to USD via usd_factor.  FABIOv2 has no producer
  # price for Fish, Seafood → no total_value → the intensity × total_value
  # formula in step 6 is undefined there, so the direct transfer is the
  # carve-out.  Disaggregation across the FABIO areas mapped to each upstream
  # region is proportional to each area's fish total_product_output.
  fish_item_codes       <- sector_conc[fabio_item == FISH_SEAFOOD_ITEM,
                                       unique(fabio_item_code)]
  stopifnot(length(fish_item_codes) == 1L)
  fish_item_code_scalar <- fish_item_codes[1L]
  
  fish_va_by_region <- intensities[
    sector_code %in% fish_sector_codes &
      is.finite(va_intensity_winsor) & is.finite(x),
    .(va_clean_native = sum(va_intensity_winsor * x, na.rm = TRUE)),
    by = .(region_code, va_component, year)
  ]
  
  # TPO is strand-independent; build the share table once.
  fish_tpo <- fv[
    fabio_item_code == fish_item_code_scalar,
    .(fabio_area_code, year, tpo = get(output_col))
  ]
  fish_tpo[!is.finite(tpo) | tpo < 0, tpo := 0]
  fish_tpo <- area_conc[fish_tpo, on = "fabio_area_code",
                        allow.cartesian = TRUE, nomatch = NULL]
  fish_tpo[, region_total_tpo := sum(tpo), by = .(region_code, year)]
  fish_tpo[, share := fifelse(region_total_tpo > 0, tpo / region_total_tpo, NA_real_)]
  
  fabio_fish_va <- fish_va_by_region[
    fish_tpo, on = c("region_code", "year"), allow.cartesian = TRUE
  ]
  fabio_fish_va[, va_share_native := va_clean_native * share]
  
  fabio_fish_va <- fabio_fish_va[, .(
    va_native       = sum(va_share_native, na.rm = TRUE),
    fabio_item_code = fish_item_code_scalar
  ), by = .(fabio_area_code, va_component, year)]
  
  # native → USD via usd_factor (GLORIA: ×1000; EXIOBASE: ×1e6/eur_per_usd).
  fabio_fish_va[usd_factor, usd_per_unit := i.usd_per_unit, on = "year"]
  fabio_fish_va[, `value_added_direct [USD]` := fifelse(
    is.finite(usd_per_unit), va_native * usd_per_unit, NA_real_
  )]
  fabio_fish_va[, c("va_native", "usd_per_unit") := NULL]
  
  fabio_fish_va[, `:=`(va                  = NA_real_,
                       va_winsor           = NA_real_,
                       x                   = NA_real_,
                       va_intensity        = NA_real_,
                       va_intensity_winsor = NA_real_)]
  
  fabio_intensities <- rbindlist(
    list(fabio_intensities, fabio_fish_va), use.names = TRUE
  )
  fabio_intensities[, year := as.integer(year)]
  
  
  # ── 6. Join FABIO total values, compute per-strand value_added ────────────
  #
  # Build a strand-complete FABIO skeleton (every fv row × every strand) so that
  # unmapped items get a full set of zero strands and the derived total is 0 for
  # them.
  message(sprintf("[%s] Joining FABIOv2 total values and computing per-strand value_added ...", suffix))
  
  n_comp  <- length(VA_COMPONENTS)
  fv_long <- fv[rep(seq_len(.N), each = n_comp)]
  fv_long[, va_component := rep(VA_COMPONENTS, times = nrow(fv))]
  
  result <- merge(
    fabio_intensities, fv_long,
    by    = c("fabio_area_code", "fabio_item_code", "va_component", "year"),
    all.y = TRUE
  )
  
  unmatched_items <- setdiff(unique(fv$fabio_item_code),
                             unique(fabio_intensities$fabio_item_code))
  if (length(unmatched_items) > 0L) {
    message(sprintf(
      "  %d FABIO item(s) have no %s sector mapping; all strands forced to 0: %s",
      length(unmatched_items), label, paste(unmatched_items, collapse = ", ")
    ))
  }
  
  # Per-strand value_added [USD]:
  #   - Fish rows: direct transfer from stage 5b.
  #   - Everything else: va_intensity_winsor × total_value.
  #   - Unmatched items / NA intensity: 0 by convention.
  result[, va_value := fifelse(
    !is.na(`value_added_direct [USD]`),
    `value_added_direct [USD]`,
    fifelse(is.na(va_intensity_winsor), 0, va_intensity_winsor * get(value_col))
  )]
  
  result[, c("va", "va_winsor", "x", "value_added_direct [USD]") := NULL]
  
  
  # ── 7. Physical-intensity Hampel + MAD on per-strand value_added ──────────
  #
  # phys_denom selection is strand-independent: ISIC-A uses TPO; ISIC-C prefers
  # positive sua_aggregated_production, falling through to positive TPO.
  if (use_sua_prod_denom && (SUA_PROD_COL %in% names(result))) {
    sua_prod_vec <- result[[SUA_PROD_COL]]
    result[, phys_denom := fcoalesce(
      fifelse(is.finite(sua_prod_vec) & sua_prod_vec > 0, sua_prod_vec, NA_real_),
      fifelse(is.finite(get(output_col)) & get(output_col) > 0, get(output_col), NA_real_)
    )]
  } else {
    result[, phys_denom := fifelse(
      is.finite(get(output_col)) & get(output_col) > 0, get(output_col), NA_real_
    )]
  }
  
  result[, va_phys_intensity := fifelse(
    is.finite(phys_denom) & is.finite(va_value), va_value / phys_denom, NA_real_
  )]
  
  
  # Rows subject to the physical-intensity filter: all rows when the switch is
  # on, fish only when off.  Non-filtered rows carry NA through 7a/7b so the cap
  # skips them and 7c leaves their va_value unchanged.
  phys_rows <- result[, VA_PHYS_INTENSITY_FILTER |
                        fabio_item_code == fish_item_code_scalar]
  
  # 7a. Hampel filter per (fabio_item, fabio_area, va_component).
  message(sprintf("[%s]   Stage 7a: Hampel filter per (fabio_item, fabio_area, va_component) over years ...", suffix))
  
  hampel_phys_result <- hampel_by_series(
    result[phys_rows], "va_phys_intensity", "va_phys_intensity_hampel",
    c("fabio_item_code", "fabio_area_code", "va_component"),
    half_window = HAMPEL_HALF_WINDOW, threshold = HAMPEL_THRESHOLD)
  
  result[, va_phys_intensity_hampel := fifelse(phys_rows, va_phys_intensity, NA_real_)]
  result[hampel_phys_result,
         va_phys_intensity_hampel := i.va_phys_intensity_hampel,
         on = .(fabio_item_code, fabio_area_code, va_component, year)]
  
  hampel_phys_diag <- merge(
    hampel_phys_result,
    result[, .(fabio_area_code, fabio_item_code, va_component, year,
               fabio_area, fabio_item, va_phys_intensity)],
    by = c("fabio_area_code", "fabio_item_code", "va_component", "year")
  )
  annotate_hampel_diagnostic(hampel_phys_diag,
                             pre_col  = "va_phys_intensity",
                             post_col = "va_phys_intensity_hampel")
  write_va_diagnostic(hampel_phys_diag, type = "hampel",
                      sort_first = "va_component",
                      out_path   = diag_phys_hampel,
                      col_order  = c(
                        "va_component",
                        "fabio_area_code", "fabio_area",
                        "fabio_item_code", "fabio_item",
                        "year",
                        "va_phys_intensity", "va_phys_intensity_hampel",
                        "window_median", "series_mad", "mad_used",
                        "is_spike", "direction", "abs_change",
                        "hampel_z", "abs_hampel_z"))
  
  
  # 7b. Per-(fabio_item, va_component) MAD cap on Hampel-filtered series.
  #     IHS+MAD, no hard cap (USD/tonne has no universal ceiling), pooled per
  #     item AND strand so each strand is capped on its own distribution.
  message(sprintf("[%s]   Stage 7b: MAD cap per (fabio_item, va_component) on Hampel-filtered series ...", suffix))
  message(sprintf("[%s]     Optimising IHS θ per (fabio_item, va_component) (pooled across all areas and years) ...", suffix))
  
  cap_strand_by_group(
    result,
    hampel_col = "va_phys_intensity_hampel",
    winsor_col = "va_phys_intensity_winsor",
    group_cols = c("fabio_item_code", "va_component"),
    other_key  = "fabio_area_code",
    theta_col  = "item_theta",
    k          = WINSOR_MAD_K,
    diag_id_cols = c("fabio_area_code", "fabio_item_code", "va_component", "year",
                     "fabio_area", "fabio_item",
                     "va_phys_intensity", "va_phys_intensity_hampel", "item_theta"),
    diag_col_order = c(
      "va_component",
      "fabio_area_code", "fabio_area",
      "fabio_item_code", "fabio_item",
      "year",
      "va_phys_intensity", "va_phys_intensity_hampel", "va_phys_intensity_winsor",
      "cap_lower", "cap_upper",
      "winsorized", "direction", "abs_change",
      "mad_z", "abs_mad_z", "item_theta"),
    out_path    = diag_phys_winsor,
    theta_label = "(item \u00d7 strand) cells")
  
  
  # 7c. Re-derive per-strand value_added from the twice-filtered intensity
  #     using the SAME phys_denom.
  result[, va_value := fifelse(
    !is.na(va_phys_intensity_winsor),
    va_phys_intensity_winsor * phys_denom, va_value
  )]
  
  result[, c("item_theta", "va_phys_intensity_hampel", "phys_denom") := NULL]
  if (SUA_PROD_COL %in% names(result)) result[, (SUA_PROD_COL) := NULL]
  
  
  # ── 7d. Pivot strands wide + derive total ─────────────────────────────────
  #
  # One row per (fabio_area, fabio_item, year) with the three cleaned strands as
  # columns and value_added [USD] = their row-wise sum, sitting last.  The dcast
  # is done on the three plain integer keys ONLY (context columns carry
  # brackets/spaces in their names); context is merged back from fv afterwards.
  result_wide <- dcast(
    result, fabio_area_code + fabio_item_code + year ~ va_component,
    value.var = "va_value", fill = 0
  )
  
  present <- intersect(VA_COMPONENTS, names(result_wide))
  if (length(present) > 0L)
    setnames(result_wide, present, vapply(present, va_value_col, character(1)))
  for (comp in setdiff(VA_COMPONENTS, present))
    result_wide[, (va_value_col(comp)) := 0]
  
  for (vc in VA_VALUE_COLS)
    set(result_wide, which(is.na(result_wide[[vc]])), vc, 0)
  
  result_wide[, (VA_TOTAL_COL) := rowSums(.SD, na.rm = TRUE), .SDcols = VA_VALUE_COLS]
  
  # Bring back the FABIO context columns from fv (unique per area/item/year).
  fv_ctx <- copy(fv)
  if (SUA_PROD_COL %in% names(fv_ctx)) fv_ctx[, (SUA_PROD_COL) := NULL]
  result_wide <- merge(
    result_wide, fv_ctx,
    by = c("fabio_area_code", "fabio_item_code", "year"), all.x = TRUE
  )
  
  ctx_order <- c("row_id", "iso3c",
                 "fabio_area_code", "fabio_area",
                 "comm_code", "fabio_item_code", "fabio_item", "comm_group",
                 "unit", "year",
                 output_col,
                 "price [USD/unit]", "price_source", "price_source_constituents",
                 value_col)
  ctx_order <- intersect(ctx_order, names(result_wide))   # tolerant of schema drift
  
  setcolorder(result_wide, c(ctx_order, VA_VALUE_COLS, VA_TOTAL_COL))
  setorder(result_wide, fabio_area_code, fabio_item_code, year)
  
  
  # ── 7e. Write outputs ─────────────────────────────────────────────────────
  saveRDS(result_wide, out_rds_path)
  fwrite(result_wide,  out_csv_path)
  
  message(sprintf(
    "[%s] Main output: %d rows written to %s  (total VA non-zero on %d rows, %.1f%%).",
    suffix, nrow(result_wide), out_rds_path,
    result_wide[get(VA_TOTAL_COL) != 0, .N],
    100 * result_wide[get(VA_TOTAL_COL) != 0, .N] / nrow(result_wide)
  ))
  for (comp in VA_COMPONENTS) {
    vc <- va_value_col(comp)
    message(sprintf(
      "    %-9s non-zero on %d rows; Σ = %.3e USD.",
      comp, result_wide[get(vc) != 0, .N], result_wide[, sum(get(vc), na.rm = TRUE)]
    ))
  }
  
  
  # ── 8. Reconciliation diagnostic per concordance group × region × year ────
  #
  # On the DERIVED total (= sum of strands).  Because the three strands
  # partition the VA rows, the base-side total here is the full VA total; the
  # FABIO side uses value_added [USD] from the wide result.  The base side is
  # converted to USD via usd_factor so both sides share a currency.
  message(sprintf("[%s] Building %s ↔ FABIO VA reconciliation diagnostic (on derived total) ...",
                  suffix, label))
  
  # 8a. Concordance groups = connected components of the bipartite graph.
  conc_edges <- unique(sector_conc[, .(sector_code, fabio_item_code)])
  conc_edges[, group_id := .I]
  repeat {
    prev_ids <- conc_edges$group_id
    conc_edges[, group_id := min(group_id), by = sector_code]
    conc_edges[, group_id := min(group_id), by = fabio_item_code]
    if (identical(prev_ids, conc_edges$group_id)) break
  }
  conc_edges[, group_id := as.integer(factor(group_id))]
  
  sector_lookup     <- unique(col_idx[, .(sector_code, sector_name)])
  fabio_item_lookup <- unique(sector_conc[, .(fabio_item_code, fabio_item)])
  
  group_members <- copy(conc_edges)
  group_members[sector_lookup,     sector_name     := i.sector_name, on = "sector_code"]
  group_members[fabio_item_lookup, fabio_item_name := i.fabio_item,  on = "fabio_item_code"]
  
  group_summary <- group_members[, .(
    sector_codes     = paste(sort(unique(sector_code)),     collapse = "; "),
    sector_names     = paste(sort(unique(sector_name)),     collapse = "; "),
    fabio_item_codes = paste(sort(unique(fabio_item_code)), collapse = "; "),
    fabio_item_names = paste(sort(unique(fabio_item_name)), collapse = "; "),
    n_sectors        = uniqueN(sector_code),
    n_fabio_items    = uniqueN(fabio_item_code)
  ), by = group_id]
  setorder(group_summary, group_id)
  
  message(sprintf(
    "  %d concordance groups  (%d with >1 %s sector, %d with >1 FABIO item).",
    nrow(group_summary), group_summary[n_sectors > 1L, .N], label,
    group_summary[n_fabio_items > 1L, .N]
  ))
  
  sector_to_group <- unique(conc_edges[, .(sector_code, group_id)])
  item_to_group   <- unique(conc_edges[, .(fabio_item_code, group_id)])
  
  # 8b. Base side per (group, region, year).  intensities is long over strand
  # and x repeats across strands, so first collapse to one row per (region,
  # sector, year): va summed over strands (= total VA), x taken once.
  base_rs <- intensities[, .(
    va = sum(va, na.rm = TRUE),
    x  = x[1L]
  ), by = .(region_code, sector_code, year)]
  
  base_side <- merge(
    base_rs[, .(region_code, sector_code, year, va, x)],
    sector_to_group, by = "sector_code", allow.cartesian = TRUE
  )
  base_side <- base_side[, .(
    base_va_native = sum(va, na.rm = TRUE),
    base_x_native  = sum(x,  na.rm = TRUE)
  ), by = .(group_id, region_code, year)]
  base_side[usd_factor, usd_per_unit := i.usd_per_unit, on = "year"]
  base_side[, `base_va_total [USD]` := fifelse(
    is.finite(usd_per_unit), base_va_native * usd_per_unit, NA_real_
  )]
  
  # 8c. FABIO side per (group, region, year) on the derived total.
  fabio_side <- merge(
    result_wide[, .(fabio_area_code, fabio_item_code, year, va = get(VA_TOTAL_COL))],
    item_to_group, by = "fabio_item_code", allow.cartesian = TRUE
  )
  fabio_side <- fabio_side[, .(fabio_va_usd = sum(va, na.rm = TRUE)),
                           by = .(group_id, fabio_area_code, year)]
  
  fabio_side <- area_conc[, .(region_code, fabio_area_code)][
    fabio_side, on = "fabio_area_code", allow.cartesian = TRUE, nomatch = NULL
  ]
  
  fabio_side <- merge(
    fabio_side,
    base_side[, .(group_id, region_code, year, base_x_native, base_va_native)],
    by = c("group_id", "region_code", "year"), all.x = TRUE
  )
  fabio_side[is.na(base_x_native),  base_x_native  := 0]
  fabio_side[is.na(base_va_native), base_va_native := 0]
  
  fabio_side[, w := {
    sx <- sum(base_x_native)
    sv <- sum(base_va_native)
    if      (is.finite(sx) && sx > 0) base_x_native  / sx
    else if (is.finite(sv) && sv > 0) base_va_native / sv
    else                              rep(1 / .N, .N)
  }, by = .(group_id, fabio_area_code, year)]
  
  fabio_side[, fabio_va_split_usd := fabio_va_usd * w]
  
  fabio_side <- fabio_side[, .(
    `fabio_va_total [USD]` = sum(fabio_va_split_usd, na.rm = TRUE)
  ), by = .(group_id, region_code, year)]
  
  # 8d. Full outer join, mismatch metrics, write.
  recon <- merge(
    base_side[, .(group_id, region_code, year, `base_va_total [USD]`)],
    fabio_side, by = c("group_id", "region_code", "year"), all = TRUE
  )
  add_reconciliation_metrics(
    recon,
    a_col = "base_va_total [USD]", b_col = "fabio_va_total [USD]",
    diff_col = "diff [USD]", abs_col = "abs_diff [USD]",
    ratio_col = "ratio_fabio_over_base", pct_col = "pct_mismatch")
  
  recon <- merge(recon, group_summary, by = "group_id", all.x = TRUE)
  
  setcolorder(recon, c(
    "group_id",
    "sector_codes", "sector_names",
    "fabio_item_codes", "fabio_item_names",
    "n_sectors", "n_fabio_items",
    "region_code", "year",
    "base_va_total [USD]", "fabio_va_total [USD]",
    "diff [USD]", "abs_diff [USD]",
    "ratio_fabio_over_base", "pct_mismatch"
  ))
  setorderv(recon, "abs_diff [USD]", order = -1L, na.last = TRUE)
  
  fwrite(recon, diag_recon_path)
  
  n_groups  <- uniqueN(recon$group_id)
  tot_base  <- recon[, sum(`base_va_total [USD]`,  na.rm = TRUE)]
  tot_fabio <- recon[, sum(`fabio_va_total [USD]`, na.rm = TRUE)]
  global_pct <- if (max(abs(tot_base), abs(tot_fabio)) > 0) {
    100 * (tot_fabio - tot_base) / max(abs(tot_base), abs(tot_fabio))
  } else NA_real_
  
  message(sprintf(
    "  Reconciliation written to %s\n    %d rows across %d groups.\n    Global totals:  %s = %.3e USD,  FABIO = %.3e USD  (%.2f%% symmetric mismatch).",
    diag_recon_path, nrow(recon), n_groups, label, tot_base, tot_fabio, global_pct
  ))
  
  invisible(result_wide)
}


# ============================================================================
# SHARED ENGINE — FRONT-HALF CLEANING (stages 4a/4b) + per-base driver
# ============================================================================
#
# run_database() takes an adapter, builds the raw per-strand intensities through
# the adapter, cleans them with the shared stage-4a Hampel and stage-4b winsor,
# then runs the back half once per ISIC level.

run_database <- function(adapter, shared) {
  tag <- adapter$file_tag
  
  message(sprintf("\n████████████████████  BASE: %s  ████████████████████", adapter$label))
  
  diag_share_hampel <- file.path(DIAG_DIR, sprintf("FABIOv2_%s_share_intensity_hampel.csv", tag))
  diag_share_winsor <- file.path(DIAG_DIR, sprintf("FABIOv2_%s_share_intensity_winsor.csv", tag))
  
  # ── Adapter front half: raw per-strand intensities + usd_factor + dims ─────
  front <- adapter$build_front_half(shared$working_years, shared$fabio_years)
  intensities <- front$intensities   # raw, native currency, mapped sectors only
  
  # ── Stage 4a: Hampel filter on pure intensities (per strand) ──────────────
  message(sprintf("[%s] Stage 4a: Hampel filter per (region, sector, va_component) over years ...", adapter$label))
  
  hampel_share_result <- hampel_by_series(
    intensities, "va_intensity", "va_intensity_hampel",
    c("region_code", "sector_code", "va_component"),
    half_window = HAMPEL_HALF_WINDOW, threshold = HAMPEL_THRESHOLD)
  
  intensities[, va_intensity_hampel := va_intensity]
  intensities[hampel_share_result,
              va_intensity_hampel := i.va_intensity_hampel,
              on = .(region_code, sector_code, va_component, year)]
  
  hampel_share_diag <- merge(
    hampel_share_result,
    intensities[, .(region_code, region_name, sector_code, sector_name,
                    va_component, year, va_intensity)],
    by = c("region_code", "sector_code", "va_component", "year")
  )
  annotate_hampel_diagnostic(hampel_share_diag,
                             pre_col  = "va_intensity",
                             post_col = "va_intensity_hampel")
  write_va_diagnostic(hampel_share_diag, type = "hampel",
                      sort_first = "va_component",
                      out_path   = diag_share_hampel,
                      indent     = "  ",
                      col_order  = c(
                        "va_component",
                        "region_code", "region_name",
                        "sector_code", "sector_name",
                        "year",
                        "va_intensity", "va_intensity_hampel",
                        "window_median", "series_mad", "mad_used",
                        "is_spike", "direction", "abs_change",
                        "hampel_z", "abs_hampel_z"))
  
  # Drop the Hampel buffer years; everything downstream sees fabio_years only.
  n_pre <- nrow(intensities)
  intensities <- intensities[year %in% shared$fabio_years]
  message(sprintf(
    "  Dropped %d buffer-year strand-rows; %d rows (fabio_years only) carried into stage 4b.",
    n_pre - nrow(intensities), nrow(intensities)
  ))
  
  # ── Stage 4b: per-(sector, va_component) MAD cap on Hampel-filtered series ─
  message(sprintf("[%s] Stage 4b: MAD cap per (sector, va_component) on Hampel-filtered series ...", adapter$label))
  message("  Optimising IHS θ per (sector, va_component) (pooled across all regions and years) ...")
  
  cap_strand_by_group(
    intensities,
    hampel_col = "va_intensity_hampel",
    winsor_col = "va_intensity_winsor",
    group_cols = c("sector_code", "va_component"),
    other_key  = "region_code",
    theta_col  = "sector_theta",
    k          = WINSOR_MAD_K,
    diag_id_cols = c("sector_code", "va_component", "year", "region_code",
                     "region_name", "sector_name",
                     "va_intensity", "va_intensity_hampel", "sector_theta"),
    diag_col_order = c(
      "va_component",
      "region_code", "region_name",
      "sector_code", "sector_name",
      "year",
      "va_intensity", "va_intensity_hampel", "va_intensity_winsor",
      "cap_lower", "cap_upper",
      "winsorized", "direction", "abs_change",
      "mad_z", "abs_mad_z", "sector_theta"),
    out_path    = diag_share_winsor,
    theta_label = "(sector \u00d7 strand) cells",
    write_label = "analyzable entries")
  
  # ── Back half, once per ISIC level ────────────────────────────────────────
  ctx <- list(
    label       = adapter$label,
    file_tag    = adapter$file_tag,
    key         = adapter$key,
    intensities = intensities,
    usd_factor  = front$usd_factor,
    col_idx     = front$col_idx,
    area_conc   = front$area_conc
  )
  
  process_isic_level(ctx, "A", front$sector_conc_a, shared$fv_a,
                     shared$value_col_a, shared$output_col_a,
                     adapter$fish_sectors_a, use_sua_prod_denom = FALSE)
  process_isic_level(ctx, "C", front$sector_conc_c, shared$fv_c,
                     shared$value_col_c, shared$output_col_c,
                     adapter$fish_sectors_c, use_sua_prod_denom = TRUE)
  
  invisible(NULL)
}


# ============================================================================
# ADAPTER: GLORIA v060
# ============================================================================
#
# V matrix (qs2) has VA accounts on the row axis in per-region blocks; the
# collapse sums each strand's VA rows down each sector column within its region
# block.  VA-row → strand is read from the "Value added and final demand" sheet
# labels by an EXACT lookup (GLORIA v060 uses fixed SNA-coded labels).  Native
# currency 1000 USD → USD is ×1000.

GLORIA_ADAPTER <- local({
  
  README_PATH <- VA_GLORIA_README_XLSX
  V_DIR       <- VA_GLORIA_V_DIR
  X_DIR       <- VA_GLORIA_X_DIR
  
  SECTOR_CONC_PATH <- VA_CONC_GLORIA_ITEMS   # shared with config; was a duplicate literal
  AREA_CONC_PATH   <- file.path(INPUT_DIR, "concordance_areas_gloria_fabio.csv")
  
  FISH_SECTORS_A <- c(22L, 23L)  # Fishing; Crustaceans and molluscs
  FISH_SECTORS_C <- c(46L)       # Fish products
  
  # Exact GLORIA v060 "Value added and final demand" row label → strand.  Keys
  # are matched verbatim (after trimws); a label absent here is final demand and
  # excluded.  ("Subsidies on production D.39" is signed in V, so summing it with
  # taxes inside `tls` gives taxes − subsidies directly.)
  VA_LABEL_TO_COMPONENT <- c(
    "Compensation of employees D.1"    = "wages",
    "Net operating surplus B.2n"       = "capital",
    "Net mixed income B.3n"            = "capital",
    "Consumption of fixed capital K.1" = "capital",
    "Taxes on production D.29"         = "tls",
    "Subsidies on production D.39"     = "tls"
  )
  
  load_sector_conc <- function(conc, isic_level) {
    required <- c("GLORIA_sector_code", "FABIO_item_code", "FABIO_item", "ISIC")
    missing  <- setdiff(required, names(conc))
    if (length(missing) > 0L)
      stop("GLORIA sector concordance is missing expected column(s): ",
           paste(missing, collapse = ", "), ".  Found: ",
           paste(names(conc), collapse = ", "))
    sc <- conc[
      toupper(trimws(as.character(ISIC))) == isic_level &
        !is.na(GLORIA_sector_code) & !is.na(FABIO_item_code),
      .(sector_code    = as.integer(GLORIA_sector_code),
        fabio_item_code = as.integer(FABIO_item_code),
        fabio_item      = FABIO_item)
    ]
    sc <- unique(sc[!is.na(sector_code) & !is.na(fabio_item_code)])
    if (nrow(sc) == 0L)
      stop("No GLORIA concordance rows for ISIC level '", isic_level,
           "' in ", SECTOR_CONC_PATH, ".")
    sc
  }
  
  resolve_va_components <- function(readme_path, n_va) {
    va_sheet <- as.data.table(readxl::read_excel(readme_path,
                                                 sheet = "Value added and final demand"))
    known    <- names(VA_LABEL_TO_COMPONENT)
    col_hits <- vapply(va_sheet,
                       function(col) sum(trimws(as.character(col)) %in% known),
                       integer(1))
    if (max(col_hits) == 0L)
      stop("No column in the GLORIA 'Value added and final demand' sheet contains ",
           "any of the expected VA labels (", paste(known, collapse = "; "), ").")
    label_col <- names(va_sheet)[which.max(col_hits)]
    labels    <- trimws(as.character(va_sheet[[label_col]]))
    strand    <- unname(VA_LABEL_TO_COMPONENT[labels])   # NA = final demand
    
    message(sprintf(
      "  VA/FD sheet: %d row(s); label column '%s' (%d VA rows matched, %d excluded).",
      length(labels), label_col, sum(!is.na(strand)), sum(is.na(strand))
    ))
    for (i in seq_along(labels)) {
      message(sprintf("    [%2d] %-45s → %s", i, substr(labels[i], 1L, 45L),
                      ifelse(is.na(strand[i]), "(final demand — excluded)", strand[i])))
    }
    for (comp in VA_COMPONENTS) {
      if (!any(strand == comp, na.rm = TRUE))
        stop("GLORIA strand '", comp, "' matched no VA-row label — check ",
             "VA_LABEL_TO_COMPONENT against the sheet's labels.")
    }
    strand
  }
  
  build_front_half <- function(working_years, fabio_years) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("GLORIA base needs the 'readxl' package (front-half label load).")
    if (!requireNamespace("qs2", quietly = TRUE))
      stop("GLORIA base needs the 'qs2' package (V/X matrix load).")
    
    message("Loading GLORIA dimension labels ...")
    regions_tbl <- as.data.table(readxl::read_excel(README_PATH, sheet = "Regions"))
    sectors_tbl <- as.data.table(readxl::read_excel(README_PATH, sheet = "Sectors"))
    setorder(sectors_tbl, Lfd_Nr)
    
    n_va      <- nrow(readxl::read_excel(README_PATH, sheet = "Value added and final demand"))
    n_regions <- nrow(regions_tbl)
    n_sectors <- nrow(sectors_tbl)
    n_cols    <- n_regions * n_sectors
    
    message(sprintf("  %d regions × %d sectors = %d columns;  %d VA accounts per region.",
                    n_regions, n_sectors, n_cols, n_va))
    
    message("Resolving VA-row → strand assignment ...")
    va_row_component <- resolve_va_components(README_PATH, n_va)
    stopifnot(length(va_row_component) == n_va)
    va_rows_by_component <- lapply(VA_COMPONENTS, function(comp) which(va_row_component == comp))
    names(va_rows_by_component) <- VA_COMPONENTS
    message(sprintf("  Strand row counts:  %s.",
                    paste(sprintf("%s=%d", VA_COMPONENTS,
                                  vapply(va_rows_by_component, length, integer(1))),
                          collapse = "  ")))
    
    col_idx <- data.table(
      col         = seq_len(n_cols),
      region_code = rep(regions_tbl$Region_acronyms,    each  = n_sectors),
      region_name = rep(regions_tbl$Region_names,       each  = n_sectors),
      sector_code = rep(as.integer(sectors_tbl$Lfd_Nr), times = n_regions),
      sector_name = rep(sectors_tbl$Sector_names,       times = n_regions)
    )
    
    # Concordances.
    message("Loading GLORIA concordance CSVs ...")
    sector_conc_all <- fread(SECTOR_CONC_PATH)
    sector_conc_a   <- load_sector_conc(sector_conc_all, "A")
    sector_conc_c   <- load_sector_conc(sector_conc_all, "C")
    
    area_conc <- fread(AREA_CONC_PATH)[
      !is.na(GLORIA_region_code) & GLORIA_region_code != "" & !is.na(FABIO_area_code),
      .(region_code     = as.character(GLORIA_region_code),
        fabio_area_code = as.integer(FABIO_area_code),
        fabio_area      = FABIO_area)
    ]
    area_conc <- unique(area_conc[!is.na(region_code) & region_code != "" & !is.na(fabio_area_code)])
    
    message(sprintf("  ISIC-A: %d sector mappings; ISIC-C: %d sector mappings; %d area mappings.",
                    nrow(sector_conc_a), nrow(sector_conc_c), nrow(area_conc)))
    stopifnot(all(FISH_SECTORS_A %in% sector_conc_a$sector_code))
    stopifnot(all(FISH_SECTORS_C %in% sector_conc_c$sector_code))
    
    # Per-region block collapse of V → per-strand VA (1000 USD).
    collapse_va <- function(V_mat) {
      out <- matrix(0, nrow = n_cols, ncol = length(VA_COMPONENTS),
                    dimnames = list(NULL, VA_COMPONENTS))
      for (r in seq_len(n_regions)) {
        cols_r <- ((r - 1L) * n_sectors + 1L):(r * n_sectors)
        rows_r <- ((r - 1L) * n_va      + 1L):(r * n_va)
        block  <- V_mat[rows_r, cols_r, drop = FALSE]
        for (k in seq_along(VA_COMPONENTS)) {
          sel <- va_rows_by_component[[VA_COMPONENTS[k]]]
          if (length(sel) > 0L) out[cols_r, k] <- colSums(block[sel, , drop = FALSE])
        }
      }
      out
    }
    
    process_year <- function(yr) {
      v_path <- sprintf("%s/V_%d.qs2", V_DIR, yr)
      x_path <- sprintf("%s/X_%d.qs2", X_DIR, yr)
      if (!file.exists(v_path) || !file.exists(x_path)) {
        message("  Year ", yr, ": V or X missing, skipping."); return(NULL)
      }
      message("  Year ", yr, " ...")
      V_mat <- as.matrix(qs2::qs_read(v_path))
      X_vec <- as.numeric(qs2::qs_read(x_path))
      stopifnot(nrow(V_mat) >= n_regions * n_va,
                ncol(V_mat) == n_cols, length(X_vec) == n_cols)
      X_vec[X_vec < 0] <- 0
      VA_mat <- collapse_va(V_mat)
      rm(V_mat); gc(verbose = FALSE)
      
      n_comp <- length(VA_COMPONENTS)
      dt <- data.table(
        col          = rep(seq_len(n_cols), times = n_comp),
        va_component = rep(VA_COMPONENTS,   each  = n_cols),
        va           = as.vector(VA_mat),
        x            = rep(X_vec,           times = n_comp)
      )
      dt <- col_idx[dt, on = "col"]
      dt[, col := NULL]
      dt[, va_intensity := fifelse(x > 0, va / x, NA_real_)]
      dt[, year := yr]
      dt[]
    }
    
    v_files    <- list.files(V_DIR, pattern = "^V_\\d+\\.qs2$")
    disk_years <- sort(as.integer(gsub("V_|\\.qs2", "", v_files)))
    stopifnot(length(disk_years) > 0)
    years   <- intersect(disk_years, working_years)
    missing <- setdiff(working_years, disk_years)
    if (length(missing) > 0L)
      warning(sprintf("GLORIA: requested working years not on disk: %s. Hampel context reduced at the affected edges.",
                      paste(missing, collapse = ", ")))
    stopifnot(length(years) > 0)
    message(sprintf("GLORIA V directory holds %d years (%d-%d); loading %d (%d-%d).",
                    length(disk_years), min(disk_years), max(disk_years),
                    length(years), min(years), max(years)))
    
    message("Computing pure GLORIA per-strand intensities ...")
    intensities <- rbindlist(lapply(years, process_year))
    
    mapped_union <- sort(unique(c(sector_conc_a$sector_code, sector_conc_c$sector_code)))
    intensities  <- intensities[sector_code %in% mapped_union]
    message(sprintf("  Restricted GLORIA intensities to %d sectors; %d strand-rows.",
                    length(mapped_union), nrow(intensities)))
    
    # Currency: 1000 USD → USD is ×1000, every year.
    usd_factor <- data.table(year = sort(unique(intensities$year)), usd_per_unit = 1000)
    
    list(intensities = intensities, usd_factor = usd_factor, col_idx = col_idx,
         sector_conc_a = sector_conc_a, sector_conc_c = sector_conc_c,
         area_conc = area_conc)
  }
  
  list(label = "GLORIA", file_tag = "GLORIA", key = "gloria",
       fish_sectors_a = FISH_SECTORS_A, fish_sectors_c = FISH_SECTORS_C,
       build_front_half = build_front_half)
})


# ============================================================================
# ADAPTER: EXIOBASE 3.10 (ixi)
# ============================================================================
#
# Factor-input matrix F (9 × n_cols) already has one column per (region ×
# industry) cell.  Row 1 is NOT VA: it lifts basic-prices X to PRODUCER prices
# (the share-intensity denominator); rows 2..9 partition into strands by a
# hard-coded index map cross-checked against the on-disk labels.  Native
# currency M.EUR → USD via the FAOSTAT Germany EUR/USD rate (Element Code "SLC",
# Area Code 79), USD = M.EUR × 1e6 / rate.

EXIOBASE_ADAPTER <- local({
  
  BASE        <- VA_EXIOBASE_DIR
  COMMON_UNIT <- file.path(BASE, "IOT_2022_ixi/unit.rds")
  F_UNIT      <- file.path(BASE, "IOT_2022_ixi/factor_inputs/unit.rds")
  x_path_fn   <- function(yr) file.path(BASE, sprintf("IOT_%d_ixi", yr), "x.rds")
  f_path_fn   <- function(yr) file.path(BASE, sprintf("IOT_%d_ixi", yr), "factor_inputs/F.rds")
  
  SECTOR_CONC_PATH <- file.path(INPUT_DIR, "concordance_items_exiobase_ixi_fabio.csv")
  AREA_CONC_PATH   <- file.path(INPUT_DIR, "concordance_areas_exiobase_fabio.csv")
  
  FISH_SECTORS_A <- c(19L)  # Fishing, operating of fish hatcheries and farms (05)
  FISH_SECTORS_C <- c(45L)  # Manufacture of fish products
  
  # Canonical EXIOBASE 3 factor-input row order:
  #   1  Taxes less subsidies on products purchased: Total  → producer-price lift (NOT VA)
  #   2  Other net taxes on production                      → tls   (stored NET)
  #   3-5 Compensation of employees ...                     → wages
  #   6-9 Operating surplus ...                             → capital
  TAXES_ROW_IDX <- 1L
  VA_ROWS_BY_COMPONENT <- list(wages = 3:5, capital = 6:9, tls = 2L)
  LABEL_PATTERNS <- c(
    lift    = "subsidies on products",
    tls     = "taxes on production",
    wages   = "compensation of employees",
    capital = "operating surplus"
  )
  
  classify_f_label <- function(lbl) {
    l    <- tolower(lbl)
    hits <- names(LABEL_PATTERNS)[
      vapply(LABEL_PATTERNS, function(p) grepl(p, l, fixed = TRUE), logical(1))
    ]
    if      (length(hits) == 1L) hits
    else if (length(hits) == 0L) NA_character_
    else                         paste0("ambiguous{", paste(hits, collapse = ","), "}")
  }
  
  load_sector_conc <- function(conc, isic_level) {
    required <- c("EXIOBASE_sector_code", "FABIO_item_code", "FABIO_item", "ISIC")
    missing  <- setdiff(required, names(conc))
    if (length(missing) > 0L)
      stop("EXIOBASE sector concordance is missing expected column(s): ",
           paste(missing, collapse = ", "), ".  Found: ",
           paste(names(conc), collapse = ", "))
    sc <- conc[
      toupper(trimws(as.character(ISIC))) == isic_level &
        !is.na(EXIOBASE_sector_code) & !is.na(FABIO_item_code),
      .(sector_code     = as.integer(EXIOBASE_sector_code),
        fabio_item_code = as.integer(FABIO_item_code),
        fabio_item      = FABIO_item)
    ]
    sc <- unique(sc[!is.na(sector_code) & !is.na(fabio_item_code)])
    if (nrow(sc) == 0L)
      stop("No EXIOBASE concordance rows for ISIC level '", isic_level,
           "' in ", SECTOR_CONC_PATH, ".")
    sc
  }
  
  # EUR/USD per year from FAOSTAT Germany SLC (synthesis-script method).
  # Reads the Normalized (long) exchange-rate file: filter to the SLC element
  # and Germany (Area Code 79), keep annual rows (Months Code 7021), then take
  # Year/Value directly -- no Y<year> melt. Mirrors the shared
  # read_faostat_exchange_long() in 00_value_added_helpers.R.
  build_eur_per_usd <- function() {
    xr <- as.data.table(fread(FX_PATH, encoding = "UTF-8"))
    for (col in c("Element Code", "Area Code", "Year", "Value"))
      if (!(col %in% names(xr)))
        stop("FAOSTAT FX file has no '", col, "' column. Found: ",
             paste(names(xr), collapse = ", "))
    xr <- xr[trimws(as.character(`Element Code`)) == FX_ELEMENT_CODE &
               suppressWarnings(as.integer(`Area Code`)) == FX_GERMANY_AREA]
    if ("Months Code" %in% names(xr)) xr <- xr[`Months Code` == 7021L]
    if (nrow(xr) == 0L)
      stop("No annual FAOSTAT rows with Element Code == '", FX_ELEMENT_CODE,
           "' and Area Code == ", FX_GERMANY_AREA, " (Germany) in ", FX_PATH, ".")
    long <- xr[, .(year        = suppressWarnings(as.integer(Year)),
                   eur_per_usd = suppressWarnings(as.numeric(Value)))]
    long <- long[is.finite(year) & is.finite(eur_per_usd) & eur_per_usd > 0]
    rt <- long[, .(eur_per_usd = eur_per_usd[1L]), by = year]
    setorder(rt, year)
    rt
  }
  
  build_front_half <- function(working_years, fabio_years) {
    message("Loading EXIOBASE common io labels ...")
    common_unit <- as.data.table(readRDS(COMMON_UNIT))
    nm_lower      <- tolower(names(common_unit))
    region_col_in <- names(common_unit)[grep("^reg",        nm_lower)[1]]
    sector_col_in <- names(common_unit)[grep("^(sec|prod)", nm_lower)[1]]
    if (is.na(region_col_in) || is.na(sector_col_in))
      stop("Could not locate region and sector/product columns in ", COMMON_UNIT)
    
    col_idx <- data.table(
      col         = seq_len(nrow(common_unit)),
      region_code = as.character(common_unit[[region_col_in]]),
      sector_name = as.character(common_unit[[sector_col_in]])
    )
    n_cols    <- nrow(col_idx)
    n_regions <- uniqueN(col_idx$region_code)
    n_sectors <- uniqueN(col_idx$sector_name)
    if (n_cols != n_regions * n_sectors)
      stop(sprintf("EXIOBASE common labels are not a clean region × sector grid: %d rows, %d regions, %d sectors.",
                   n_cols, n_regions, n_sectors))
    
    # Sector code: 1..n_sectors by first appearance (canonical EXIOBASE order).
    sector_lookup <- col_idx[, .(sector_name = unique(sector_name))]
    sector_lookup[, sector_code := seq_len(.N)]
    col_idx[sector_lookup, sector_code := i.sector_code, on = "sector_name"]
    # region_name = region code (EXIOBASE regions are 2-letter codes).
    col_idx[, region_name := region_code]
    setcolorder(col_idx, c("col", "region_code", "region_name", "sector_code", "sector_name"))
    message(sprintf("  %d regions × %d sectors = %d columns.", n_regions, n_sectors, n_cols))
    
    # Factor-input rows + strand-map validation.
    message("Loading EXIOBASE factor-input row labels ...")
    f_unit <- readRDS(F_UNIT)
    if (!is.data.frame(f_unit))
      stop("Unexpected structure for ", F_UNIT, "; expected a data.frame.")
    f_unit_dt <- as.data.table(f_unit)
    char_cols  <- names(f_unit_dt)[vapply(f_unit_dt,
                                          function(v) is.character(v) || is.factor(v), logical(1))]
    unit_col   <- grep("^unit$", names(f_unit_dt), ignore.case = TRUE, value = TRUE)
    label_cols <- setdiff(char_cols, unit_col)
    if (length(label_cols) >= 1L) {
      f_row_labels     <- as.character(f_unit_dt[[label_cols[1L]]])
      have_disk_labels <- TRUE
    } else {
      f_row_labels     <- as.character(rownames(f_unit))
      have_disk_labels <- FALSE
      warning("No descriptive label column in ", F_UNIT, "; skipping label cross-check.")
    }
    n_factor_inputs <- nrow(f_unit_dt)
    
    message("Resolving F-row → strand assignment ...")
    va_rows_by_component <- lapply(VA_ROWS_BY_COMPONENT, as.integer)
    all_assigned <- sort(c(TAXES_ROW_IDX, unlist(va_rows_by_component, use.names = FALSE)))
    if (!identical(all_assigned, seq_len(n_factor_inputs)))
      stop(sprintf("TAXES_ROW_IDX (%d) + strand rows {%s} do not partition 1..%d exactly.",
                   TAXES_ROW_IDX,
                   paste(sort(unlist(va_rows_by_component, use.names = FALSE)), collapse = ", "),
                   n_factor_inputs))
    
    role_assigned                <- rep(NA_character_, n_factor_inputs)
    role_assigned[TAXES_ROW_IDX] <- "lift"
    for (comp in VA_COMPONENTS) role_assigned[va_rows_by_component[[comp]]] <- comp
    
    if (have_disk_labels) {
      role_observed <- vapply(f_row_labels, classify_f_label, character(1), USE.NAMES = FALSE)
      bad <- which(is.na(role_observed) | role_observed != role_assigned)
      if (length(bad) > 0L) {
        detail <- paste(sprintf("    [%d] label=\"%s\"  assigned=%s  label implies=%s",
                                bad, f_row_labels[bad], role_assigned[bad],
                                ifelse(is.na(role_observed[bad]), "<none>", role_observed[bad])),
                        collapse = "\n")
        stop(sprintf("EXIOBASE factor-row labels in %s do not match the index map.\n%s",
                     F_UNIT, detail))
      }
      message("  Factor-row labels cross-checked against the index map — all consistent.")
    }
    message(sprintf("  %d factor-input rows.  Strand row counts: %s.",
                    n_factor_inputs,
                    paste(sprintf("%s=%d", VA_COMPONENTS,
                                  vapply(va_rows_by_component, length, integer(1))),
                          collapse = "  ")))
    
    # Concordances.
    message("Loading EXIOBASE concordance CSVs ...")
    sector_conc_all <- fread(SECTOR_CONC_PATH, encoding = "UTF-8")
    sector_conc_a   <- load_sector_conc(sector_conc_all, "A")
    sector_conc_c   <- load_sector_conc(sector_conc_all, "C")
    
    area_conc <- fread(AREA_CONC_PATH, encoding = "UTF-8")[
      !is.na(EXIOBASE_region) & EXIOBASE_region != "" & !is.na(FABIO_area_code),
      .(region_code     = as.character(EXIOBASE_region),
        fabio_area_code = as.integer(FABIO_area_code),
        fabio_area      = FABIO_area)
    ]
    area_conc <- unique(area_conc[!is.na(region_code) & region_code != "" & !is.na(fabio_area_code)])
    
    unknown_sectors <- setdiff(
      unique(c(sector_conc_a$sector_code, sector_conc_c$sector_code)),
      unique(col_idx$sector_code))
    unknown_regions <- setdiff(unique(area_conc$region_code), unique(col_idx$region_code))
    if (length(unknown_sectors) > 0L)
      stop("EXIOBASE concordance references sector codes not present in ", COMMON_UNIT,
           ": ", paste(sort(unknown_sectors), collapse = ", "))
    if (length(unknown_regions) > 0L)
      stop("EXIOBASE concordance references region codes not present in ", COMMON_UNIT,
           ": ", paste(sort(unknown_regions), collapse = ", "))
    
    message(sprintf("  ISIC-A: %d sector mappings; ISIC-C: %d sector mappings; %d area mappings.",
                    nrow(sector_conc_a), nrow(sector_conc_c), nrow(area_conc)))
    stopifnot(all(FISH_SECTORS_A %in% sector_conc_a$sector_code))
    stopifnot(all(FISH_SECTORS_C %in% sector_conc_c$sector_code))
    
    collapse_va <- function(F_mat) {
      out <- matrix(0, nrow = n_cols, ncol = length(VA_COMPONENTS),
                    dimnames = list(NULL, VA_COMPONENTS))
      for (k in seq_along(VA_COMPONENTS)) {
        sel <- va_rows_by_component[[VA_COMPONENTS[k]]]
        if (length(sel) > 0L) out[, k] <- colSums(F_mat[sel, , drop = FALSE])
      }
      out
    }
    
    process_year <- function(yr) {
      x_path <- x_path_fn(yr); f_path <- f_path_fn(yr)
      if (!file.exists(x_path) || !file.exists(f_path)) {
        message("  Year ", yr, ": X or F missing, skipping."); return(NULL)
      }
      message("  Year ", yr, " ...")
      
      X_raw <- readRDS(x_path)
      if (is.data.frame(X_raw) && ncol(X_raw) >= 3L) {
        X_dt <- as.data.table(X_raw)
        nm_lower_x   <- tolower(names(X_dt))
        region_col_x <- names(X_dt)[grep("^reg",        nm_lower_x)[1]]
        sector_col_x <- names(X_dt)[grep("^(sec|prod)", nm_lower_x)[1]]
        value_col_x  <- names(X_dt)[grep("^(indout|value|x)$", nm_lower_x)[1]]
        if (is.na(value_col_x)) {
          num_cols <- names(X_dt)[vapply(X_dt, is.numeric, logical(1))]
          if (length(num_cols) == 0L) stop("No numeric value column in ", x_path)
          value_col_x <- num_cols[1]
        }
        if (!is.na(region_col_x) && !is.na(sector_col_x)) {
          if (!identical(as.character(X_dt[[region_col_x]]), col_idx$region_code) ||
              !identical(as.character(X_dt[[sector_col_x]]), col_idx$sector_name))
            stop(sprintf("%s row order does not match common io labels.", x_path))
        }
        X_vec <- as.numeric(X_dt[[value_col_x]])
      } else if (is.matrix(X_raw) || is.data.frame(X_raw)) {
        X_vec <- as.numeric(X_raw[, 1])
      } else {
        X_vec <- as.numeric(X_raw)
      }
      
      F_raw <- readRDS(f_path)
      if (is.data.frame(F_raw)) {
        na_before <- sum(vapply(F_raw, function(col) sum(is.na(col)), integer(1)))
        for (j in seq_along(F_raw))
          if (!is.numeric(F_raw[[j]])) F_raw[[j]] <- suppressWarnings(as.numeric(F_raw[[j]]))
        na_after <- sum(vapply(F_raw, function(col) sum(is.na(col)), integer(1)))
        if (na_after > na_before)
          warning(sprintf("%s: %d cells became NA during character->numeric coercion.",
                          f_path, na_after - na_before))
      }
      F_mat <- as.matrix(F_raw)
      if (!is.numeric(F_mat)) storage.mode(F_mat) <- "double"
      
      stopifnot(length(X_vec) == n_cols,
                nrow(F_mat) == n_factor_inputs, ncol(F_mat) == n_cols)
      
      # Lift basic-prices X to producer prices, floor at zero.
      X_pp <- X_vec + as.numeric(F_mat[TAXES_ROW_IDX, ])
      X_pp[X_pp < 0] <- 0
      
      VA_mat <- collapse_va(F_mat)
      rm(F_mat); gc(verbose = FALSE)
      
      n_comp <- length(VA_COMPONENTS)
      dt <- data.table(
        col          = rep(seq_len(n_cols), times = n_comp),
        va_component = rep(VA_COMPONENTS,   each  = n_cols),
        va           = as.vector(VA_mat),
        x            = rep(X_pp,            times = n_comp)
      )
      dt <- col_idx[dt, on = "col"]
      dt[, col := NULL]
      dt[, va_intensity := fifelse(x > 0, va / x, NA_real_)]
      dt[, year := yr]
      dt[]
    }
    
    year_dirs  <- list.dirs(BASE, recursive = FALSE)
    year_match <- regmatches(year_dirs, regexpr("IOT_(\\d{4})_ixi", year_dirs))
    disk_years <- sort(as.integer(sub("IOT_(\\d{4})_ixi", "\\1", year_match)))
    disk_years <- disk_years[!is.na(disk_years)]
    stopifnot(length(disk_years) > 0)
    years   <- intersect(disk_years, working_years)
    missing <- setdiff(working_years, disk_years)
    if (length(missing) > 0L)
      warning(sprintf("EXIOBASE: working years not on disk: %s. Years outside coverage fall through to value_added = 0.",
                      paste(missing, collapse = ", ")))
    stopifnot(length(years) > 0)
    message(sprintf("EXIOBASE base holds %d years (%d-%d); loading %d (%d-%d).",
                    length(disk_years), min(disk_years), max(disk_years),
                    length(years), min(years), max(years)))
    
    message("Computing pure EXIOBASE per-strand intensities ...")
    intensities <- rbindlist(lapply(years, process_year))
    
    mapped_union <- sort(unique(c(sector_conc_a$sector_code, sector_conc_c$sector_code)))
    intensities  <- intensities[sector_code %in% mapped_union]
    message(sprintf("  Restricted EXIOBASE intensities to %d sectors; %d strand-rows.",
                    length(mapped_union), nrow(intensities)))
    
    # Currency: M.EUR → USD via Germany EUR/USD (SLC); usd_per_unit = 1e6 / rate.
    message("Loading FAOSTAT Germany EUR/USD (Element Code SLC, Area Code 79) ...")
    eur <- build_eur_per_usd()
    fx_missing <- setdiff(working_years, eur$year)
    if (length(fx_missing) > 0L)
      warning(sprintf("FAOSTAT SLC has no Germany rate for working year(s): %s.  Fish VA for those years falls through to 0; reconciliation EXIOBASE-side total NA→0 there.",
                      paste(fx_missing, collapse = ", ")))
    message(sprintf("  EUR/USD available for %d / %d working years (%d-%d).",
                    length(intersect(working_years, eur$year)), length(working_years),
                    min(eur$year), max(eur$year)))
    usd_factor <- eur[, .(year, usd_per_unit = 1e6 / eur_per_usd)]
    
    list(intensities = intensities, usd_factor = usd_factor, col_idx = col_idx,
         sector_conc_a = sector_conc_a, sector_conc_c = sector_conc_c,
         area_conc = area_conc)
  }
  
  list(label = "EXIOBASE", file_tag = "EXIOBASE", key = "exiobase",
       fish_sectors_a = FISH_SECTORS_A, fish_sectors_c = FISH_SECTORS_C,
       build_front_half = build_front_half)
})


ADAPTERS <- list(GLORIA = GLORIA_ADAPTER, EXIOBASE = EXIOBASE_ADAPTER)


# ============================================================================
# RUN
# ============================================================================
#
# FABIO total values (shared across bases) load once; then one full pass per
# selected base: adapter front half → stage 4a/4b → back half (ISIC-A, ISIC-C).

message("Loading FABIOv2 total values (ISIC-A) ...")
fv_pack_a    <- prepare_fv(FABIO_TV_PATH_A, required_cols = character(0),
                           drop_extra = c("production [tonnes]", "total_value_source", "sua_aggregated_value [USD]"))
fv_a         <- fv_pack_a$fv
value_col_a  <- fv_pack_a$value_col
output_col_a <- fv_pack_a$output_col

message("Loading FABIOv2 total values (ISIC-C) ...")
fv_pack_c    <- prepare_fv(FABIO_TV_PATH_C, required_cols = character(0),
                           drop_extra = c("production [tonnes]", "total_value_source", "sua_aggregated_value [USD]"))
fv_c         <- fv_pack_c$fv
value_col_c  <- fv_pack_c$value_col
output_col_c <- fv_pack_c$output_col

if (!(SUA_PROD_COL %in% names(fv_c)))
  warning(sprintf(
    "ISIC-C total_values has no `%s` column; ISIC-C phys_intensity falls back to total_product_output for ALL rows. Re-run the total-values step with SUA-production aggregation to populate it.",
    SUA_PROD_COL))

fabio_years   <- sort(unique(fv_a$year))
buffer_years  <- c(
  seq(min(fabio_years) - HAMPEL_HALF_WINDOW, min(fabio_years) - 1L),
  seq(max(fabio_years) + 1L,       max(fabio_years) + HAMPEL_HALF_WINDOW)
)
working_years <- sort(union(fabio_years, buffer_years))

message(sprintf(
  "  FABIOv2 covers %d-%d (%d years).  Stages 4a/4b operate on %d-%d (FABIO ± %d-year Hampel buffer); buffer dropped before 4b.",
  min(fabio_years), max(fabio_years), length(fabio_years),
  min(working_years), max(working_years), HAMPEL_HALF_WINDOW))

shared <- list(
  fv_a = fv_a, fv_c = fv_c,
  value_col_a = value_col_a, output_col_a = output_col_a,
  value_col_c = value_col_c, output_col_c = output_col_c,
  fabio_years = fabio_years, working_years = working_years
)

for (db_name in DATABASES_TO_RUN) {
  if (is.null(ADAPTERS[[db_name]]))
    stop("Unknown database in DATABASES_TO_RUN: '", db_name,
         "'. Known: ", paste(names(ADAPTERS), collapse = ", "))
  run_database(ADAPTERS[[db_name]], shared)
  gc(verbose = FALSE)
}

message("\nDone.")