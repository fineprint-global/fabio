# ==============================================================================
# FABIOv2 Value-Added — COMBINED (GLORIA *and* EXIOBASE bases, FSDN overlay)
#
# DUAL-BASE VARIANT.  The synthesis now runs ONCE PER BASE (see BASES below):
#   - GLORIA   base: script 14_1_value_added_FABIO_v2_MRIOTs.R output (GLORIA base).
#                    File names, diagnostic column names and `va_source` labels
#                    are byte-identical to the previous single-base version, so
#                    downstream consumers are unaffected.
#   - EXIOBASE base: script 14_1_value_added_FABIO_v2_MRIOTs.R output (EXIOBASE base).
#                    Same schema (both bases come from 14_1 and share the strand columns),
#                    so the identical overlay machinery applies; outputs carry
#                    an EXIOBASE_ tag and diagnostics use exiobase_* columns.
# Everything base-independent — the FSDN overlay build, the FAOSTAT FX tables,
# and the single live Eurostat fetch — is computed once and shared across bases.
#
# Overlays the FSDN value added (script 14_2) on top of the base value added
# (script 14_1).  The base is kept; a (area, item, year) row is overwritten with
# the FSDN figures iff ALL of the following hold:
#   - the FSDN output flags it `fsdn_mapped == TRUE` (the item is mapped by the
#     FSDN<->FABIO concordance, not just a stage-B6 right-join zero-fill),
#   - its FSDN total is non-NA,
#   - the FSDN row's `source_isic` matches the ISIC level being built (so on the
#     ISIC-C base only wine — the sole C-tagged FSDN item — overwrites, and
#     primary-agriculture rows never clobber the base's ISIC-C manufacturing
#     values), and
#   - the (area, year) is not in FSDN_COVERAGE_EXCLUDE (country-years FSDN does
#     not actually cover, which otherwise disaggregate to a spurious zero).
# Every other row keeps the base.  A mapped, in-coverage, ISIC-matched item that
# disaggregated to zero is still overwritten — to zero; the base-vs-FSDN
# diagnostic surfaces those cases.
#
# Currency: both bases are USD, FSDN is EUR.  FSDN is converted with Germany's
# SLC series from the FAOSTAT Exchange_rate_E_All_Data.csv (same file and
# direction as 13_1_FAOstat_producer_prices_USD.R): value_USD = value_EUR / rate.
#
# Breakdown: FSDN's five accounts are remapped onto the base's three strands
#   wages   <- compensation_of_employees                         (D.1)
#   capital <- consumption_of_fixed_capital + net_mixed_income   (K.1 + B.3n)
#   tls     <- taxes_on_production + subsidies_on_production      (D.29 + D.39)
# (D.39 is stored negative by script 14_2, so tls is a plain sum.)  The total is
# value_added = wages + capital + tls, keeping the base's invariant.
#
# Output mirrors the base schema plus two provenance columns: va_source
# ("GLORIA"/"EXIOBASE"/"FSDN"/...) and fsdn_source_isic (NA on kept rows).  Runs
# for both ISIC levels, for both bases.
#
# Outputs (per base, per ISIC level; <TAG> = "GLORIA_" or "EXIOBASE_"):
#   <OUTPUT_DIR>/FABIOv2_COMBINED_<TAG>value_added_ISIC-{A,C}.rds / .csv
#   <OUTPUT_DIR>/diagnostics/FABIOv2_COMBINED_<TAG><base>_vs_fsdn_ISIC-{A,C}.csv
# plus the fishing / national-SUT diagnostics named in their sections.
# ==============================================================================

library(data.table)
source("R/00_value_added_config.R")


# ── Configuration ────────────────────────────────────────────────────────────

OUTPUT_DIR <- VA_VALUE_ADDED_OUTPUT_DIR
DIAG_DIR   <- VA_VALUE_ADDED_DIAG_DIR
va_ensure_dir(DIAG_DIR)

# Bases the synthesis runs over — one full pass (FSDN overlay + fishing
# overwrite + national SUT overlays) per entry, producing a parallel COMBINED
# output per upstream database.  Fields:
#   label            value written to va_source on kept rows; banner label
#   key              lower-case prefix for base-side diagnostic columns
#                    ("<key>_total_usd", ...) and comparison buckets
#                    ("fsdn_zero_<key>_nonzero", ...)
#   file_tag         inserted into every output file name, naming the base
#                    explicitly ("GLORIA_" / "EXIOBASE_")
#   va_tag           base label baked into the value-added file name; the path
#                    itself is built via va_va_output_basename() (shared helper)
#   producing_script named in not-found / schema error messages
# Both 01 scripts emit the same strand schema (BASE_STRAND_COLS /
# BASE_TOTAL_COL below), which is what makes the shared machinery possible.
BASES <- list(
  GLORIA = list(
    label            = "GLORIA",
    key              = "gloria",
    file_tag         = "GLORIA_",
    va_tag           = "GLORIA",
    producing_script = "14_1_value_added_FABIO_v2_MRIOTs.R"
  ),
  EXIOBASE = list(
    label            = "EXIOBASE",
    key              = "exiobase",
    file_tag         = "EXIOBASE_",
    va_tag           = "EXIOBASE",
    producing_script = "14_1_value_added_FABIO_v2_MRIOTs.R"
  )
)

# All four name builders below come from R/00_value_added_helpers.R so the
# synthesis reader spells the value-added / COMBINED scheme exactly as the
# writers (14_1 / 14_2 / 14_3) do.  `suffix` here is the bare ISIC level
# ("A"/"C"); the value-added builder wants the full "ISIC-<level>" token.
BASE_VA_PATH <- function(base, suffix)
  file.path(OUTPUT_DIR, paste0(va_va_output_basename(base$va_tag, paste0("ISIC-", suffix)), ".rds"))
FSDN_VA_PATH <- file.path(OUTPUT_DIR, paste0(va_va_output_basename("FSDN"), ".rds"))
COMBINED_VA_PATH <- function(base, suffix)
  file.path(OUTPUT_DIR, paste0(va_combined_output_basename(base$file_tag, suffix), ".rds"))
COMBINED_CSV_PATH <- function(base, suffix)
  file.path(OUTPUT_DIR, paste0(va_combined_output_basename(base$file_tag, suffix), ".csv"))
BASE_FSDN_DIAG_PATH <- function(base, suffix)
  file.path(DIAG_DIR, sprintf("FABIOv2_COMBINED_%s%s_vs_fsdn_ISIC-%s.csv",
                              base$file_tag, base$key, suffix))

# EUR->USD: Germany's SLC row from the same FAOSTAT file script 14_1 reads.
EXCHANGE_RATE_PATH <- VA_EXCHANGE_RATE_CSV
GERMANY_AREA_CODE  <- VA_GERMANY_AREA_CODE
EXCHANGE_ELEMENT   <- VA_FX_ELEMENT_CODE

ISIC_LEVELS <- c("A", "C")

# (area, year) cells FSDN does not actually cover — these are kept as the base
# and never overwritten by FSDN (which disaggregates them to a spurious zero and
# would otherwise clobber a perfectly good base value).  Keyed by ISO3 + year
# and resolved against the base's `iso3c` column.  Extend as coverage changes.
#   HRV : EU accession mid-2013       -> no FADN/FSDN before 2013
#   GBR : left the EU end-Jan 2020    -> no FADN/FSDN after 2020
#   MLT : 2022-2023 not yet in the current FSDN extract
FSDN_COVERAGE_EXCLUDE <- rbindlist(list(
  data.table(iso3c = "HRV", year = VA_KEEP_YEARS[VA_KEEP_YEARS < 2013L]),
  data.table(iso3c = "GBR", year = VA_KEEP_YEARS[VA_KEEP_YEARS > 2020L]),
  data.table(iso3c = "MLT", year = VA_KEEP_YEARS[VA_KEEP_YEARS > 2021L])
))

# Wine (FABIO item 2655) is the only ISIC-C item FSDN overwrites.  FSDN measures
# production on agricultural (vineyard) farms only, so an FSDN wine total of zero
# does NOT mean there was no winemaking — the base's ISIC-C figure captures the
# manufacturing side FSDN can't see.  We therefore never let an FSDN zero
# overwrite a non-zero base WINE value (the `fsdn_zero_<base>_nonzero` case);
# such cells keep the base.  (A genuine FSDN wine value still overwrites as usual.)
WINE_ITEM_CODE <- 2655L

# Below this absolute USD magnitude a value counts as "zero" in the diagnostic
# (FSDN zero-fills are exact 0; this only guards floating dust).
DIAG_ZERO_TOL_USD <- 1e-6

# Column names (must match the 01 / 02 output schemas; BOTH 01 scripts —
# GLORIA and EXIOBASE — emit these identically).
# BASE_STRAND_COLS is just the unnamed canonical strand columns, in strand order.
BASE_STRAND_COLS <- unname(STRAND_TO_COL)
# BASE_TOTAL_COL + STRAND_TO_COL: moved to R/00_value_added_helpers.R (section 11).

FSDN_ACCOUNT_COLS <- c("compensation_of_employees [EUR]",
                       "taxes_on_production [EUR]",
                       "subsidies_on_production [EUR]",
                       "consumption_of_fixed_capital [EUR]",
                       "net_mixed_income [EUR]")
# FSDN_TOTAL_COL: moved to R/00_value_added_helpers.R (shared with the 14_2 writer).

# FSDN account -> strand remap; each strand is the na.rm sum of its EUR
# account columns (converted to USD below).
STRAND_FROM_FSDN <- list(
  wages   = "compensation_of_employees [EUR]",
  capital = c("consumption_of_fixed_capital [EUR]", "net_mixed_income [EUR]"),
  tls     = c("taxes_on_production [EUR]", "subsidies_on_production [EUR]")
)
KEY_COLS <- c("fabio_area_code", "fabio_item_code", "year")


# ── Helper: build the converted, remapped FSDN overlay ───────────────────────
#
# Converts EVERY FSDN row's accounts EUR->USD, aggregates into the three
# strands, derives the total, and keeps two flags: `fsdn_mapped` (read straight
# from script 14_2) and `.fsdn_overwrite_eligible` (= fsdn_mapped & non-NA total).
# All rows are kept (not just overwrite ones) so the diagnostic can compare
# them to the base.  Base-independent: built once, shared by every base pass.

build_fsdn_overlay <- function(fsdn_path, eur_usd) {
  if (!file.exists(fsdn_path))
    stop("FSDN output not found at:\n  ", fsdn_path,
         "\nRun script 14_2_value_added_FABIO_v2_FSDN.R first.")
  
  fsdn <- as.data.table(readRDS(fsdn_path))
  
  missing_cols <- setdiff(c(KEY_COLS, FSDN_ACCOUNT_COLS, FSDN_TOTAL_COL,
                            "fsdn_mapped"), names(fsdn))
  if (length(missing_cols) > 0L)
    stop("FSDN output is missing expected column(s): ",
         paste(missing_cols, collapse = ", "), ".\nCheck it was produced by script 14_2.")
  
  fsdn[, `:=`(fabio_area_code = as.integer(fabio_area_code),
              fabio_item_code = as.integer(fabio_item_code),
              year            = as.integer(year),
              fsdn_mapped     = as.logical(fsdn_mapped))]
  
  # Overwrite only concordance-mapped items (zeros included; a mapped item that
  # disaggregated to zero is overwritten to zero — see the diagnostic).
  fsdn[, .fsdn_overwrite_eligible := fsdn_mapped & !is.na(get(FSDN_TOTAL_COL))]
  if (!any(fsdn[[".fsdn_overwrite_eligible"]]))
    stop("No fsdn_mapped rows with a non-NA `", FSDN_TOTAL_COL, "` — nothing to overlay.")
  
  # Attach the yearly Germany rate; fail loudly on any missing/non-positive year.
  fsdn <- eur_usd[fsdn, on = "year"]
  bad_years <- sort(unique(fsdn[!(is.finite(rate_eur_per_usd) & rate_eur_per_usd > 0), year]))
  if (length(bad_years) > 0L)
    stop("Germany EUR/USD rate missing/non-positive for FSDN year(s): ",
         paste(bad_years, collapse = ", "), ".")
  
  # Convert accounts EUR->USD and aggregate into strands (USD = EUR / rate).
  n_row       <- nrow(fsdn)
  usd_account <- function(col) fsdn[[col]] / fsdn$rate_eur_per_usd
  for (strand in names(STRAND_FROM_FSDN)) {
    src_cols <- STRAND_FROM_FSDN[[strand]]
    usd_mat  <- vapply(src_cols, usd_account, numeric(n_row))
    dim(usd_mat) <- c(n_row, length(src_cols))   # keep a matrix when n_row == 1
    fsdn[, (STRAND_TO_COL[[strand]]) := rowSums(usd_mat, na.rm = TRUE)]
  }
  fsdn[, (BASE_TOTAL_COL) := rowSums(.SD, na.rm = TRUE), .SDcols = unname(STRAND_TO_COL)]
  
  src_isic_col <- if ("source_isic" %in% names(fsdn)) "source_isic" else NULL
  keep <- c(KEY_COLS, unname(STRAND_TO_COL), BASE_TOTAL_COL, src_isic_col,
            "fsdn_mapped", ".fsdn_overwrite_eligible")
  ov   <- fsdn[, ..keep]
  if (!is.null(src_isic_col)) setnames(ov, "source_isic", "fsdn_source_isic")
  setkeyv(ov, KEY_COLS)
  
  message(sprintf("FSDN overlay: %d row(s) converted; %d overwrite-eligible; Sum = %.3e USD.",
                  nrow(ov), ov[.fsdn_overwrite_eligible == TRUE, .N],
                  ov[.fsdn_overwrite_eligible == TRUE, sum(get(BASE_TOTAL_COL), na.rm = TRUE)]))
  ov[]
}


# ── Helper: base-vs-FSDN comparison diagnostic ───────────────────────────────
#
# From the post-merge / pre-overwrite `combined` table: writes the base vs the
# converted FSDN (total + three strands, all USD) side by side, with the
# eligibility flags and a `comparison` category for filtering — e.g.
# "fsdn_zero_gloria_nonzero" / "fsdn_zero_exiobase_nonzero" (an FSDN zero
# overwrote a non-zero base value).  Base-side columns carry base$key
# ("gloria_total_usd" on the GLORIA pass — identical to the previous version —
# "exiobase_total_usd" on the EXIOBASE pass).  Restricted to FSDN-covered
# areas, the only place a comparison is meaningful.

write_base_fsdn_diagnostic <- function(combined, suffix, fsdn_areas, base) {
  bk       <- base$key
  ov_total <- paste0(".ov_", BASE_TOTAL_COL)
  d <- combined[fabio_area_code %in% fsdn_areas]
  
  ctx  <- intersect(c("iso3c", "fabio_area", "comm_code", "fabio_item",
                      "comm_group", "unit"), names(d))
  diag <- d[, c(KEY_COLS, ctx), with = FALSE]
  
  base_total_col <- paste0(bk, "_total_usd")
  diag[, (base_total_col)  := d[[BASE_TOTAL_COL]]]
  diag[, fsdn_total_usd    := d[[ov_total]]]
  for (s in names(STRAND_TO_COL)) {
    gcol <- STRAND_TO_COL[[s]]
    diag[, (paste0(bk, "_", s, "_usd"))   := d[[gcol]]]
    diag[, (paste0("fsdn_",  s, "_usd"))  := d[[paste0(".ov_", gcol)]]]
  }
  
  base_tot <- diag[[base_total_col]]
  diag[, diff_total_usd     := fsdn_total_usd - base_tot]
  diag[, abs_diff_total_usd := abs(diff_total_usd)]
  diag[, rel_diff_total     := fifelse(abs(base_tot) > DIAG_ZERO_TOL_USD,
                                       diff_total_usd / abs(base_tot), NA_real_)]
  
  has_fsdn_row <- !is.na(d[["fsdn_mapped"]])          # matched an FSDN row
  is_mapped    <- has_fsdn_row & d[["fsdn_mapped"]]
  was_overwritten <- if (".fsdn_overwrite_applied" %in% names(d)) d[[".fsdn_overwrite_applied"]]
  else (!is.na(d[[".fsdn_overwrite_eligible"]]) & d[[".fsdn_overwrite_eligible"]])
  diag[, `:=`(fsdn_has_row = has_fsdn_row, fsdn_mapped = is_mapped,
              overwritten = was_overwritten)]
  
  fsdn_tot     <- diag$fsdn_total_usd
  base_is_zero <- is.finite(base_tot) & abs(base_tot) <= DIAG_ZERO_TOL_USD
  fsdn_is_zero <- is.finite(fsdn_tot) & abs(fsdn_tot) <= DIAG_ZERO_TOL_USD
  comparison <- rep(NA_character_, nrow(diag))
  comparison[!has_fsdn_row]               <- "no_fsdn_row"
  comparison[has_fsdn_row & !is_mapped]   <- "unmapped_zerofill"
  mapped_with_row <- has_fsdn_row & is_mapped
  comparison[mapped_with_row &  fsdn_is_zero & !base_is_zero] <- paste0("fsdn_zero_", bk, "_nonzero")
  comparison[mapped_with_row &  fsdn_is_zero &  base_is_zero] <- paste0("fsdn_zero_", bk, "_zero")
  comparison[mapped_with_row & !fsdn_is_zero &  base_is_zero] <- paste0("fsdn_nonzero_", bk, "_zero")
  comparison[mapped_with_row & !fsdn_is_zero & !base_is_zero] <- "both_nonzero"
  diag[, comparison := comparison]
  
  strand_pairs <- as.vector(rbind(
    paste0(bk, "_",   names(STRAND_TO_COL), "_usd"),
    paste0("fsdn_",   names(STRAND_TO_COL), "_usd")))
  setcolorder(diag, c(
    KEY_COLS, ctx,
    base_total_col, "fsdn_total_usd",
    "diff_total_usd", "abs_diff_total_usd", "rel_diff_total",
    strand_pairs,
    "fsdn_mapped", "fsdn_has_row", "overwritten", "comparison"))
  setorderv(diag, KEY_COLS)
  fwrite(diag, BASE_FSDN_DIAG_PATH(base, suffix))
  
  summ <- diag[, .(n = .N), by = comparison][order(-n)]
  message(sprintf("  Diagnostic (%s, %s): %d rows -> %s", base$label, suffix,
                  nrow(diag), BASE_FSDN_DIAG_PATH(base, suffix)))
  message("    ", paste(sprintf("%s=%d", summ$comparison, summ$n), collapse = "  "))
  invisible(diag)
}


# ── Core: overlay FSDN onto one base ISIC level ──────────────────────────────

combine_isic_level <- function(suffix, overlay, base) {
  base_path <- BASE_VA_PATH(base, suffix)
  if (!file.exists(base_path))
    stop(base$label, " base not found at:\n  ", base_path,
         "\nRun script ", base$producing_script, " first.")
  
  message(sprintf("\n=== %s [%s]: overlaying FSDN onto %s base ===",
                  suffix, base$label, base$label))
  base_dt <- as.data.table(readRDS(base_path))
  
  missing_cols <- setdiff(c(KEY_COLS, BASE_STRAND_COLS, BASE_TOTAL_COL), names(base_dt))
  if (length(missing_cols) > 0L)
    stop(base$label, " base (", suffix, ") is missing column(s): ",
         paste(missing_cols, collapse = ", "),
         ".\nCheck it was produced by ", base$producing_script, ".")
  
  base_dt[, `:=`(fabio_area_code = as.integer(fabio_area_code),
                 fabio_item_code = as.integer(fabio_item_code),
                 year            = as.integer(year))]
  orig_cols <- names(base_dt)
  n_base    <- nrow(base_dt)
  
  # Left-join the overlay; FSDN strands arrive under temporary ".ov_" names.
  ov <- copy(overlay)
  setnames(ov, c(unname(STRAND_TO_COL), BASE_TOTAL_COL),
           paste0(".ov_", c(unname(STRAND_TO_COL), BASE_TOTAL_COL)))
  combined <- merge(base_dt, ov, by = KEY_COLS, all.x = TRUE, sort = FALSE)
  if (nrow(combined) != n_base)
    stop("Row count changed during overlay merge for ", suffix, " [", base$label,
         "] — the FSDN overlay is not unique per (area, item, year).")
  
  if (!("fsdn_source_isic" %in% names(combined)))
    stop("FSDN overlay has no `fsdn_source_isic` column — the ISIC-aware ",
         "overwrite gate needs it.  Re-run script 14_2 so its output carries ",
         "`source_isic`.")
  
  # Effective overwrite gate — ISIC-aware AND coverage-aware:
  #   (1) Only overwrite a base row at level `suffix` when the FSDN row was
  #       itself weighted at that ISIC level (fsdn_source_isic == suffix).  This
  #       stops primary-agriculture (source_isic == "A") values from clobbering
  #       the base's ISIC-C manufacturing rows; on the C base only wine — the
  #       sole C-tagged FSDN item — overwrites.
  #   (2) Never overwrite (area, year) cells FSDN does not cover
  #       (FSDN_COVERAGE_EXCLUDE): those keep the base rather than being zeroed.
  #   (3) Never let an FSDN zero overwrite a non-zero base WINE value
  #       (fsdn_zero_<base>_nonzero): FSDN sees only farm-level production, so a
  #       zero there is "not observed", not "no winemaking" — keep the base.
  excl_key <- if ("iso3c" %in% names(combined))
    combined[, paste(iso3c, year)]
  else combined[, paste(fabio_area_code, year)]
  excl_set <- FSDN_COVERAGE_EXCLUDE[, paste(iso3c, year)]
  
  .base_tot <- combined[[BASE_TOTAL_COL]]
  .fsdn_tot <- combined[[paste0(".ov_", BASE_TOTAL_COL)]]
  wine_zero_keep <-
    combined$fabio_item_code == WINE_ITEM_CODE &
    is.finite(.fsdn_tot) & abs(.fsdn_tot) <= DIAG_ZERO_TOL_USD &
    is.finite(.base_tot) & abs(.base_tot) >  DIAG_ZERO_TOL_USD
  
  combined[, .fsdn_overwrite_applied :=
             !is.na(`.fsdn_overwrite_eligible`) & `.fsdn_overwrite_eligible` &
             !is.na(fsdn_source_isic) & fsdn_source_isic == suffix &
             !(excl_key %in% excl_set) &
             !wine_zero_keep]
  
  # Diagnostic first, while base originals and .ov_ FSDN values coexist.
  write_base_fsdn_diagnostic(combined, suffix, unique(overlay$fabio_area_code), base)
  
  # Overwrite strands + total on the eligible rows only.
  hit <- combined[, which(.fsdn_overwrite_applied)]
  for (col in c(unname(STRAND_TO_COL), BASE_TOTAL_COL))
    set(combined, i = hit, j = col, value = combined[[paste0(".ov_", col)]][hit])
  
  # Provenance, then drop temporaries and restore the base column order.
  combined[, va_source := fifelse(.fsdn_overwrite_applied, "FSDN", base$label)]
  if (!("fsdn_source_isic" %in% names(combined)))
    combined[, fsdn_source_isic := NA_character_]
  combined[va_source != "FSDN", fsdn_source_isic := NA_character_]
  combined[, c(grep("^\\.ov_", names(combined), value = TRUE),
               ".fsdn_overwrite_eligible", ".fsdn_overwrite_applied", "fsdn_mapped") := NULL]
  setcolorder(combined, c(orig_cols, "va_source", "fsdn_source_isic"))
  setorderv(combined, KEY_COLS)
  
  saveRDS(combined, COMBINED_VA_PATH(base, suffix))
  fwrite(combined,  COMBINED_CSV_PATH(base, suffix))
  
  n_fsdn <- length(hit)
  message(sprintf("  %s [%s]: %d rows (%d FSDN, %.1f%%; %d %s). Sum = %.3e USD -> %s",
                  suffix, base$label, nrow(combined), n_fsdn,
                  100 * n_fsdn / nrow(combined),
                  nrow(combined) - n_fsdn, base$label,
                  combined[, sum(get(BASE_TOTAL_COL), na.rm = TRUE)],
                  COMBINED_VA_PATH(base, suffix)))
  invisible(combined)
}


# ============================================================================
# SECTION 2 — OECD SUT fishing (ISIC A03) overwrite on the ISIC-A combined base
# ----------------------------------------------------------------------------
#
# Overwrites the fishing value added (FABIO item 2960 "Fish, Seafood", the SOLE
# ISIC-A fishing item) with the activity-A03 value added published in the OECD
# Supply-Use table "Use, Value added and its components by activity" (table
# T1600, 2-digit ISIC Rev 4; dataflow OECD.SDD.NAD : DSD_NASU@DF_USEVA_T1600),
# staged to OECD_SUT_PATH below by R/00_9_prep_value_added.R.
#
# ONLY the ISIC-A base is touched.  Item 2960 also exists on the ISIC-C base as
# fish PROCESSING, which the A03 capture/aquaculture column does not measure,
# so (exactly like wine on the C base) the ISIC-C fishing row keeps the base.
#
# Strand mapping (the OECD SUT publishes the full GVA identity directly):
#   wages   <- D1                               compensation of employees
#   capital <- B2A3G  (or B2G + B3G)            gross operating surplus + mixed income
#   tls     <- D29X39                           other taxes less subsidies on production
#   total   <- B1G   ==  wages + capital + tls  (keeps the base's invariant)
# Any single missing strand is recovered from the identity B1G = D1 + B2A3G +
# D29X39 rather than guessed.  Cells still missing a component after that are
# DROPPED (kept as the base), and counted in the message log.
#
# Currency: the OECD SUT is national currency (UNIT_MEASURE "XDC", UNIT_MULT =
# millions).  Converted to USD with the SAME shared FAOSTAT SLC table the whole
# script uses (load_lcu_usd, passed in as `rates_all`).  Because FABIO area
# codes ARE FAO numeric area codes (cf. GERMANY_AREA_CODE = 79), the SLC table
# joins straight onto fabio_area_code; the OECD ISO3 REF_AREA is resolved to
# fabio_area_code via the (iso3c, fabio_area_code) pairs the combined base
# already carries — no extra concordance file is needed.
#
# Provenance: rows get va_source in {OECD_SUT, EUROSTAT_NAMA, <base label>} and
# fsdn_source_isic NA.  A single fishing diagnostic per base (base | OECD SUT |
# Eurostat NAMA | written value) is written alongside the FSDN one.
#
# Source precedence (FIXED — Eurostat NAMA always wins): for EU/EFTA fishing
# cells Eurostat NAMA wins (nama_10_a64, capital via the identity); OECD SUT
# then covers the rest of the OECD and any Eurostat gaps; the base last.  The
# wine-style zero guard applies to every source (an exact-zero total never
# clobbers a non-zero incumbent).  Eurostat is fetched ONCE (before the base
# loop) and shared by every base pass and the fishing diagnostics.
# ============================================================================

# ── Configuration (OECD SUT fishing overwrite) ───────────────────────────────

# OECD SUT "Use, Value added and its components by activity" table (T1600),
# staged as a year-bounded CSV by R/00_9_prep_value_added.R. The loader below is
# a pure reader; it filters activities/transactions in memory. The staged path is
# the shared producer/consumer contract, defined once in R/00_value_added_config.R.
OECD_SUT_PATH <- VA_OECD_SUT_CSV

FISHING_ITEM_CODE   <- 2960L      # FABIO "Fish, Seafood" (comm_code c123), ISIC-A
FISHING_ISIC_LEVEL  <- "A"        # overwrite the ISIC-A base only
OECD_SUT_VA_SOURCE_LABEL <- "OECD_SUT"

# OECD SUT activity / filter / transaction constants (OECD_SUT_FISHING_ACTIVITY,
# OECD_SUT_FILTERS, OECD_SUT_TX): moved to R/00_value_added_helpers.R (section 11).

# When the OECD SUT reports A03 as an exact zero but the base's fishing value is
# non-zero, treat the zero as "not separately compiled" and KEEP the base
# (mirrors the wine guard above).  Set FALSE to let a genuine OECD SUT zero
# overwrite.
OECD_SUT_ZERO_KEEP <- TRUE

FISHING_DIAG_PATH <- function(base)
  file.path(DIAG_DIR, sprintf("FABIOv2_COMBINED_%sfishing_diagnostic_ISIC-A.csv",
                              base$file_tag))


# Fishing wrapper — preserves the EXACT contract the fishing overwrite expects:
# the sole ISIC-A fishing item stamped on and the overlay keyed on KEY_COLS.
load_oecd_sut_fishing <- function(path, iso3_to_area, lcu_usd) {
  ov <- load_oecd_sut_activity(path, iso3_to_area, lcu_usd, OECD_SUT_FISHING_ACTIVITY)
  if (is.null(ov) || nrow(ov) == 0L) return(ov)
  ov[, fabio_item_code := FISHING_ITEM_CODE]
  ov[, iso3 := NULL]
  keep <- c(KEY_COLS, unname(STRAND_TO_COL), BASE_TOTAL_COL)
  ov   <- ov[, ..keep]
  setkeyv(ov, KEY_COLS)
  ov[]
}


# ── Helper: single fishing diagnostic (base | OECD SUT | Eurostat NAMA | written) ─
#
# One file per base, one row per fishing (area, year) cell, assembled from three
# snapshots:
#   `pre`      — fishing rows BEFORE any overwrite, carrying the base originals
#                (<base$key>_*), the OECD SUT overlay (oecd_sut_*), and the
#                stage-1 OECD gate, emitted as `oecd_overwrote_<base$key>`;
#   `combined` — the FINAL table, for the value actually written (final_*) and
#                its provenance `va_source` (<base label> / OECD_SUT /
#                EUROSTAT_NAMA);
#   `eu_nama`  — the Eurostat NAMA total (nama_total_usd), joined on (iso3c, year).
# Two verdicts sit side by side: `oecd_sut_vs_<base$key>` (would-be OECD-vs-base
# buckets, mirroring the FSDN diagnostic vocabulary) and `nama_vs_oecd_sut` (do
# the two independent sources agree on the total, within USED_OK_PCT).  The
# latter is precedence-free AND base-free: it pits Eurostat against the OECD
# candidate, neither of which depends on the base.

write_fishing_diagnostic <- function(pre, combined, eu_nama, base) {
  if (is.null(pre) || nrow(pre) == 0L) return(invisible(NULL))
  bk   <- base$key
  diag <- copy(pre)
  
  # Value actually written + provenance, joined by key (order-independent).
  fin <- combined[fabio_item_code == FISHING_ITEM_CODE,
                  c(KEY_COLS, BASE_TOTAL_COL, unname(STRAND_TO_COL), "va_source"),
                  with = FALSE]
  setnames(fin, c(BASE_TOTAL_COL, unname(STRAND_TO_COL)),
           c("final_total_usd", paste0("final_", names(STRAND_TO_COL), "_usd")))
  diag <- merge(diag, fin, by = KEY_COLS, all.x = TRUE, sort = FALSE)
  
  # Eurostat NAMA baseline (NA off-coverage / when no Eurostat).
  if (!is.null(eu_nama) && "iso3c" %in% names(diag))
    diag <- merge(diag, eu_nama[, .(iso3c, year, nama_total_usd = total_usd)],
                  by = c("iso3c", "year"), all.x = TRUE, sort = FALSE)
  else
    diag[, nama_total_usd := NA_real_]
  
  # (a) OECD SUT candidate vs the base — buckets + total diff.  NOTE: this
  # judges the OECD SUT *candidate* against the base, BEFORE any Eurostat
  # override; it is NOT a verdict on the value finally written (see `va_source`
  # for that).
  base_tot         <- diag[[paste0(bk, "_total_usd")]]
  oecd_sut_tot     <- diag$oecd_sut_total_usd
  has_oecd_sut_cell<- is.finite(oecd_sut_tot)
  base_is_zero     <- is.finite(base_tot)     & abs(base_tot)     <= DIAG_ZERO_TOL_USD
  oecd_sut_is_zero <- is.finite(oecd_sut_tot) & abs(oecd_sut_tot) <= DIAG_ZERO_TOL_USD
  diag[, oecd_sut_diff_total_usd := oecd_sut_tot - base_tot]
  diag[, oecd_sut_rel_diff_total := fifelse(abs(base_tot) > DIAG_ZERO_TOL_USD,
                                            (oecd_sut_tot - base_tot) / abs(base_tot),
                                            NA_real_)]
  vs_col <- paste0("oecd_sut_vs_", bk)
  diag[, (vs_col) := fcase(
    !has_oecd_sut_cell,                  "no_oecd_sut_cell",
    oecd_sut_is_zero & !base_is_zero,    paste0("oecd_sut_zero_", bk, "_nonzero"),
    oecd_sut_is_zero &  base_is_zero,    paste0("oecd_sut_zero_", bk, "_zero"),
    !oecd_sut_is_zero & base_is_zero,    paste0("oecd_sut_nonzero_", bk, "_zero"),
    default =                            "both_nonzero")]
  
  # (b) Eurostat NAMA baseline vs the OECD SUT candidate — a PRECEDENCE-FREE
  # check.  A written-vs-NAMA verdict is tautological: Eurostat always overrides,
  # so wherever a NAMA cell exists the written value IS the NAMA value.  Guard
  # the 0/0 case: when BOTH are ~0 they agree exactly, but the raw percent is
  # 0/0 = NaN and would otherwise fall through to "DIFFERS" (e.g. landlocked
  # Luxembourg, OECD 0 vs NAMA 0).
  oecd_sut_and_nama_both_zero <-
    is.finite(diag$oecd_sut_total_usd) & abs(diag$oecd_sut_total_usd) <= DIAG_ZERO_TOL_USD &
    is.finite(diag$nama_total_usd)     & abs(diag$nama_total_usd)     <= DIAG_ZERO_TOL_USD
  diag[, nama_vs_oecd_sut_pct := fifelse(
    abs(nama_total_usd) > DIAG_ZERO_TOL_USD,
    100 * abs(oecd_sut_total_usd - nama_total_usd) / abs(nama_total_usd),
    NA_real_)]
  diag[, nama_vs_oecd_sut := fcase(
    !is.finite(nama_total_usd),            "no Eurostat",
    !is.finite(oecd_sut_total_usd),        "no_oecd_sut_cell",
    oecd_sut_and_nama_both_zero,           "agrees",
    nama_vs_oecd_sut_pct < USED_OK_PCT,    "agrees",
    default =                              "DIFFERS")]
  
  setorderv(diag, KEY_COLS)
  fwrite(diag, FISHING_DIAG_PATH(base))
  
  oecd_sut_summ <- diag[, .N, by = c(vs_col)][order(-N)]
  nama_summ     <- diag[, .N, by = nama_vs_oecd_sut][order(-N)]
  message(sprintf("  Fishing diagnostic [%s]: %d row(s) -> %s",
                  base$label, nrow(diag), FISHING_DIAG_PATH(base)))
  message(sprintf("    OECD-SUT vs %s: ", base$label),
          paste(sprintf("%s=%d", oecd_sut_summ[[vs_col]], oecd_sut_summ$N), collapse = "  "))
  message("    NAMA vs OECD SUT:   ",
          paste(sprintf("%s=%d", nama_summ$nama_vs_oecd_sut, nama_summ$N), collapse = "  "))
  invisible(diag)
}


# ── Core: overwrite fishing on one base's ISIC-A combined table ──────────────

overwrite_fishing_oecd_sut <- function(lcu_usd, eu_nama, base) {
  suffix   <- FISHING_ISIC_LEVEL
  bk       <- base$key
  cmb_path <- COMBINED_VA_PATH(base, suffix)
  if (!file.exists(cmb_path))
    stop("Combined ISIC-", suffix, " base [", base$label, "] not found at:\n  ",
         cmb_path, "\nThe fishing overwrite must run AFTER the combine step.")
  
  message(sprintf("\n=== OECD SUT fishing (A03) overwrite on the ISIC-%s base [%s] ===",
                  suffix, base$label))
  combined <- as.data.table(readRDS(cmb_path))
  
  for (col in c("iso3c", KEY_COLS, BASE_TOTAL_COL, unname(STRAND_TO_COL)))
    if (!(col %in% names(combined)))
      stop("Combined base is missing column `", col, "` — cannot overwrite fishing.")
  
  # ISO3 -> fabio_area_code straight from the base (no extra concordance).
  xwalk <- unique(combined[!is.na(iso3c) & nzchar(iso3c),
                           .(iso3 = iso3c,
                             fabio_area_code = as.integer(fabio_area_code))])
  
  oecd_sut <- load_oecd_sut_fishing(OECD_SUT_PATH, xwalk, lcu_usd)
  if (is.null(oecd_sut) || nrow(oecd_sut) == 0L) {
    message("  OECD SUT yielded no usable fishing cells — base left unchanged.")
    return(invisible(combined))
  }
  
  # Attach OECD SUT (under ".oecd_sut_" names) to the combined base.
  setnames(oecd_sut, c(unname(STRAND_TO_COL), BASE_TOTAL_COL),
           paste0(".oecd_sut_", c(unname(STRAND_TO_COL), BASE_TOTAL_COL)))
  n0 <- nrow(combined)
  combined <- merge(combined, oecd_sut, by = KEY_COLS, all.x = TRUE, sort = FALSE)
  if (nrow(combined) != n0)
    stop("Row count changed during the OECD SUT fishing merge — the overlay is not ",
         "unique per (area, item, year).")
  
  oecd_sut_tot <- combined[[paste0(".oecd_sut_", BASE_TOTAL_COL)]]
  base_tot     <- combined[[BASE_TOTAL_COL]]
  is_fish      <- combined$fabio_item_code == FISHING_ITEM_CODE
  has_oecd_sut <- is_fish & is.finite(oecd_sut_tot)
  
  # Wine-style guard: don't let an OECD SUT zero clobber a non-zero base
  # fishing value.
  oecd_sut_zero_keep <- OECD_SUT_ZERO_KEEP & has_oecd_sut &
    abs(oecd_sut_tot) <= DIAG_ZERO_TOL_USD &
    is.finite(base_tot) & abs(base_tot) > DIAG_ZERO_TOL_USD
  
  combined[, .oecd_sut_overwrite := has_oecd_sut & !oecd_sut_zero_keep]
  
  # Snapshot fishing rows NOW, while base originals and ".oecd_sut_" values
  # coexist; the single fishing diagnostic is written at the end, after all
  # overwrites.
  ctx <- intersect(c("iso3c", "fabio_area", "comm_code", "fabio_item",
                     "comm_group", "unit"), names(combined))
  pre <- combined[fabio_item_code == FISHING_ITEM_CODE,
                  c(KEY_COLS, ctx, BASE_TOTAL_COL, unname(STRAND_TO_COL),
                    paste0(".oecd_sut_", c(BASE_TOTAL_COL, unname(STRAND_TO_COL))),
                    ".oecd_sut_overwrite"), with = FALSE]
  setnames(pre,
           c(BASE_TOTAL_COL, unname(STRAND_TO_COL),
             paste0(".oecd_sut_", c(BASE_TOTAL_COL, unname(STRAND_TO_COL))),
             ".oecd_sut_overwrite"),
           c(paste0(bk, "_total_usd"), paste0(bk, "_", names(STRAND_TO_COL), "_usd"),
             "oecd_sut_total_usd",     paste0("oecd_sut_", names(STRAND_TO_COL), "_usd"),
             # Stage-1 flag: did OECD SUT beat the base, BEFORE the Eurostat
             # override?  A TRUE here with va_source == EUROSTAT_NAMA just means
             # Eurostat later won the cell — it is not a contradiction.
             paste0("oecd_overwrote_", bk)))
  
  # Overwrite the eligible fishing rows only.
  hit <- combined[, which(.oecd_sut_overwrite)]
  for (col in c(unname(STRAND_TO_COL), BASE_TOTAL_COL))
    set(combined, i = hit, j = col, value = combined[[paste0(".oecd_sut_", col)]][hit])
  
  # Provenance.
  if (!("va_source" %in% names(combined))) combined[, va_source := base$label]
  combined[hit, va_source := OECD_SUT_VA_SOURCE_LABEL]
  if ("fsdn_source_isic" %in% names(combined))
    combined[hit, fsdn_source_isic := NA_character_]
  
  # ---- Eurostat vs OECD on fishing rows -------------------------------------
  # Eurostat NAMA OVERRIDES OECD wherever a complete
  # Eurostat cell exists; OECD then only survives for geographies/years Eurostat
  # doesn't cover, and the base covers what neither does.  The wine-style zero
  # guard still applies: an exact-zero Eurostat total never clobbers a non-zero
  # incumbent value (base, or an OECD value already written on this row).
  n_eu_fill <- 0L
  eu_fill <- build_eu_fill(eu_nama)
  if (!is.null(eu_fill)) {
    n1 <- nrow(combined)
    combined <- merge(
      combined,
      eu_fill[, .(iso3c, year,
                  .eu_w = wages_usd, .eu_c = capital_usd,
                  .eu_t = tls_usd,   .eu_tot = total_usd, .eu_src = eu_src)],
      by = c("iso3c", "year"), all.x = TRUE, sort = FALSE)
    if (nrow(combined) != n1)
      stop("Row count changed during the Eurostat merge — eu_fill is not ",
           "unique per (iso3c, year).")
    
    g_tot   <- combined[[BASE_TOTAL_COL]]      # current value (OECD where it won, else base)
    is_fish <- combined$fabio_item_code == FISHING_ITEM_CODE
    eu_ok   <- is_fish & is.finite(combined$.eu_tot)   # Eurostat always overrides
    eu_zero_keep <- OECD_SUT_ZERO_KEEP & eu_ok &
      abs(combined$.eu_tot) <= DIAG_ZERO_TOL_USD &
      is.finite(g_tot) & abs(g_tot) > DIAG_ZERO_TOL_USD
    eu_hit <- which(eu_ok & !eu_zero_keep)
    
    if (length(eu_hit) > 0L) {
      set(combined, eu_hit, STRAND_TO_COL[["wages"]],   combined$.eu_w[eu_hit])
      set(combined, eu_hit, STRAND_TO_COL[["capital"]], combined$.eu_c[eu_hit])
      set(combined, eu_hit, STRAND_TO_COL[["tls"]],     combined$.eu_t[eu_hit])
      set(combined, eu_hit, BASE_TOTAL_COL,             combined$.eu_tot[eu_hit])
      set(combined, eu_hit, "va_source",                combined$.eu_src[eu_hit])
      if ("fsdn_source_isic" %in% names(combined))
        set(combined, eu_hit, "fsdn_source_isic", NA_character_)
    }
    combined[, c(".eu_w", ".eu_c", ".eu_t", ".eu_tot", ".eu_src") := NULL]
    n_eu_fill <- length(eu_hit)
    message(sprintf("  Eurostat NAMA wrote (fill + override) %d fishing cell(s).",
                    n_eu_fill))
  }
  
  # Drop temporaries (the ".oecd_sut_" value columns AND ".oecd_sut_overwrite",
  # which the regex already covers), keep the existing column order, re-sort,
  # re-save.
  combined[, grep("^\\.oecd_sut_", names(combined), value = TRUE) := NULL]
  setorderv(combined, KEY_COLS)
  
  # Single fishing diagnostic: base | OECD SUT | Eurostat NAMA | written value.
  write_fishing_diagnostic(pre, combined, eu_nama, base)
  
  saveRDS(combined, COMBINED_VA_PATH(base, suffix))
  fwrite(combined,  COMBINED_CSV_PATH(base, suffix))
  
  # Final provenance tally on the fishing item (net of any Eurostat override).
  fish_src <- combined[fabio_item_code == FISHING_ITEM_CODE, .N, by = va_source][order(-N)]
  message(sprintf("  ISIC-%s [%s] fishing (item %d) final source mix: %s. Sum = %.3e USD -> %s",
                  suffix, base$label, FISHING_ITEM_CODE,
                  paste(sprintf("%s=%d", fish_src$va_source, fish_src$N), collapse = "  "),
                  combined[, sum(get(BASE_TOTAL_COL), na.rm = TRUE)],
                  COMBINED_VA_PATH(base, suffix)))
  invisible(combined)
}


# ── Eurostat NAMA fishing source (config + live fetch) ───────────────────────
#
# The Eurostat A03 NAMA source feeds two things: the fill/override in the fishing
# overwrite (Eurostat always overrides OECD where a complete cell exists) and the
# NAMA column of the single fishing diagnostic.  Fetched LIVE, ONCE (before the
# base loop), and shared by every base pass.  Skips gracefully (warning, no
# error) when the `eurostat` package is missing or the fetch fails (e.g.
# offline), so sourcing this script never hard-fails here.
#
#   NAMA nama_10_a64     nace_r2 A03  unit CP_MEUR  (no B2A3G -> capital via identity)
# EUR millions; converted to USD with the same Germany EUR/USD rate the FSDN
# overlay uses (the `eur_usd` object from RUN), so both sources — OECD and NAMA —
# sit in one currency.  Code-version traps the 07 script flagged are kept: D1
# (not D11), D29X39 (not D21X31).

USED_OK_PCT <- 15    # OECD candidate vs Eurostat NAMA baseline: agreement threshold

# Fishing wrapper — preserves the exact call used by the fishing overwrite.
load_eurostat_fishing <- function(eur_usd) load_eurostat_nama_activity(eur_usd, EU_A03)


# ── Helper: Eurostat NAMA fill per (iso3c, year) ─────────────────────────────
#
# Keeps only cells with ALL FOUR strands finite (so the invariant
# total = wages + capital + tls holds on a fill), de-dups per (iso3c, year), and
# tags the provenance.  Returns NULL if nothing usable.

build_eu_fill <- function(eu_nama) {
  if (is.null(eu_nama)) return(NULL)
  pref <- eu_nama[is.finite(wages_usd) & is.finite(capital_usd) &
                    is.finite(tls_usd) & is.finite(total_usd)]
  if (nrow(pref) == 0L) return(NULL)
  pref <- copy(pref)[, eu_src := "EUROSTAT_NAMA"]
  unique(pref, by = c("iso3c", "year"))[]
}


# ============================================================================
# SECTION 3 — National SUT value-added overlays (Canada CAN + Brazil BRA, A & C)
# ----------------------------------------------------------------------------
# For EACH source in NATIONAL_SUT_SOURCES it re-reads
# each ISIC level of the CURRENT base's COMBINED table from disk, overwrites
# the rows that source's script covers, and re-saves.
#
# One generic engine, several national sources.  Each entry in
# NATIONAL_SUT_SOURCES describes one country's SUT-derived VA output (script 14_3:
# Canada and Brazil); to add another national source, append a spec —
# no new functions needed.
#
# Each source is a single country whose area is DISJOINT from FSDN's coverage
# and from every other source here (CAN vs BRA), so there is no FSDN<->source
# or source<->source conflict and the order of NATIONAL_SUT_SOURCES is
# irrelevant: wherever the source script produced a value it overwrites the
# base.  On the ISIC-A base it also overwrites the OECD-SUT / Eurostat fishing
# value for its own country — the national SUT (StatCan Detail SUT, IBGE TRU)
# is the source those figures are themselves derived from, so it wins at home.
# (To keep the fishing step's choice for a country instead, drop
# FISHING_ITEM_CODE from that source's overlay before the merge, or reorder the
# steps inside the base loop.)
#
# Conversions (mirror the FSDN overlay in build_fsdn_overlay):
#   units  : source LCU -> USD via the SAME shared FAOSTAT SLC table
#            (rates_all, already in scope from RUN);
#            USD = value_src * unit_factor / rate, where unit_factor scales the
#            source's published unit to whole LCU (Canada: 1000 CAD -> 1000;
#            Brazil: 10^6 BRL -> 1e6).  Each country's rate joins straight onto
#            fabio_area_code (= FAO area code), so no area code is hardcoded.
#   strands: wages <- LABOUR, capital <- CAPITAL, tls <- TLS; total = sum of
#            the three, keeping the base's invariant value_added = w + c + t.
#
# Zero guard (NATIONAL_SUT_ZERO_KEEP, default TRUE): an exact-zero source total
# never clobbers a non-zero base value (mirrors the wine and OECD-SUT-fishing
# guards).  Both StatCan and IBGE zero a FABIO cell when the country has no
# product output, which is a genuine economic zero — but a concordance gap can
# also zero a cell the base fills, so the conservative default keeps the base
# and surfaces the case in the diagnostic.  Set FALSE to let a genuine
# no-output zero overwrite.
#
# Output (per base, per source, per ISIC level; <TAG> as in the header):
#   re-saves <OUTPUT_DIR>/FABIOv2_COMBINED_<TAG>value_added_ISIC-{A,C}.rds / .csv
#     with va_source gaining "CANADA_SUT" / "BRAZIL_SUT" on overwritten rows
#   <DIAG_DIR>/FABIOv2_COMBINED_<TAG><base>_vs_canada_ISIC-{A,C}.csv
#   <DIAG_DIR>/FABIOv2_COMBINED_<TAG><base>_vs_brazil_ISIC-{A,C}.csv
# ============================================================================

# ── Configuration (national SUT overlays) ────────────────────────────────────

# Keep incumbent when source == 0 but incumbent != 0 (see header).  One switch
# for all sources, matching the former CANADA_ZERO_KEEP / BRAZIL_ZERO_KEEP.
NATIONAL_SUT_ZERO_KEEP <- TRUE

# One spec per national source.  Fields:
#   name         banner / message label
#   key          prefix for diagnostic columns ("<key>_total_usd", ...) and
#                `comparison` buckets ("<key>_zero_<base>_nonzero", ...)
#   va_source    provenance label written to the combined table's va_source
#   script       producing script, named in skip-warnings / schema errors
#   va_tag       base/source label baked into the value-added file name; the
#                path is built via va_va_output_basename() (shared helper)
#   account_cols source VA column -> strand (wages / capital / tls)
#   total_col    source total column (existence-checked only; the engine
#                re-derives the total as the strand sum, keeping the invariant)
#   unit_factor  multiplier taking the source's published unit to whole LCU,
#                applied BEFORE the FX divide
#   fx_label     currency pair named in the missing-rate error message
NATIONAL_SUT_SOURCES <- list(
  list(
    name         = "Canada",
    key          = "canada",
    va_source    = "CANADA_SUT",
    script       = "14_3_value_added_FABIO_v2_national_SUTs.R",
    va_tag       = "CanadaSUT",   # 14_3 writes FABIOv2_CanadaSUT_value_added_ISIC-<x>
    account_cols = c(wages   = "LABOUR [1000 CAD]",     # StatCan unit: 1000 CAD
                     capital = "CAPITAL [1000 CAD]",
                     tls     = "TLS [1000 CAD]"),
    total_col    = "value_added [1000 CAD]",
    unit_factor  = 1000,                                # 1000 CAD -> CAD
    fx_label     = "CAD/USD"
  ),
  list(
    name         = "Brazil",
    key          = "brazil",
    va_source    = "BRAZIL_SUT",
    script       = "14_3_value_added_FABIO_v2_national_SUTs.R",
    va_tag       = "BrazilSUT",   # 14_3 writes FABIOv2_BrazilSUT_value_added_ISIC-<x>
    account_cols = c(wages   = "LABOUR [10^6 BRL]",     # IBGE unit: 10^6 BRL
                     capital = "CAPITAL [10^6 BRL]",
                     tls     = "TLS [10^6 BRL]"),
    total_col    = "value_added [10^6 BRL]",
    unit_factor  = 1e6,                                 # 10^6 BRL -> BRL
    fx_label     = "BRL/USD"
  )
)

# Per-base, per-source diagnostic path
# (FABIOv2_COMBINED_GLORIA_gloria_vs_canada_ISIC-A.csv, ...).
NAT_DIAG_PATH <- function(base, spec, suffix)
  file.path(DIAG_DIR, sprintf("FABIOv2_COMBINED_%s%s_vs_%s_ISIC-%s.csv",
                              base$file_tag, base$key, spec$key, suffix))


# ── Helper: build one source's converted, remapped overlay for one ISIC level ─
#
# Reads the producing script's per-level output, converts every row's accounts
# source-unit -> USD via the shared FAOSTAT rate (joined on fabio_area_code, so
# each country picks up its own LCU/USD series), aggregates into the three
# strands, derives the total, and flags `.nat_overwrite_eligible` (= finite
# total).  Base-independent.  Returns NULL (with a warning) if the producing
# script has not been run for this level.

build_national_sut_overlay <- function(spec, suffix, rates_all) {
  path <- file.path(OUTPUT_DIR, paste0(va_va_output_basename(spec$va_tag, paste0("ISIC-", suffix)), ".rds"))
  if (!file.exists(path)) {
    warning(spec$name, " SUT output not found at:\n  ", path,
            "\nRun ", spec$script, " first — ", spec$name, " overlay (",
            suffix, ") skipped.")
    return(NULL)
  }
  nat <- as.data.table(readRDS(path))
  
  miss <- setdiff(c(KEY_COLS, unname(spec$account_cols), spec$total_col),
                  names(nat))
  if (length(miss) > 0L)
    stop(spec$name, " output (", suffix, ") is missing column(s): ",
         paste(miss, collapse = ", "), ".\nCheck it was produced by ",
         spec$script, ".")
  
  nat[, `:=`(fabio_area_code = as.integer(fabio_area_code),
             fabio_item_code = as.integer(fabio_item_code),
             year            = as.integer(year))]
  
  # LCU/USD (SLC) from the shared table; fail loudly on any missing/non-positive.
  nat <- merge(nat, rates_all, by = c("fabio_area_code", "year"), all.x = TRUE)
  bad <- nat[!(is.finite(rate_lcu_per_usd) & rate_lcu_per_usd > 0),
             sort(unique(paste(fabio_area_code, year)))]
  if (length(bad) > 0L)
    stop("No FAOSTAT ", EXCHANGE_ELEMENT, " (", spec$fx_label, ") rate for ",
         spec$name, " cell(s): ", paste(bad, collapse = ", "), ".")
  
  # source unit -> USD, then remap accounts onto the three strands.
  for (strand in names(spec$account_cols)) {
    src <- spec$account_cols[[strand]]
    nat[, (STRAND_TO_COL[[strand]]) :=
          get(src) * spec$unit_factor / rate_lcu_per_usd]
  }
  nat[, (BASE_TOTAL_COL) := rowSums(.SD, na.rm = TRUE),
      .SDcols = unname(STRAND_TO_COL)]
  nat[, .nat_overwrite_eligible := is.finite(get(BASE_TOTAL_COL))]
  
  keep <- c(KEY_COLS, unname(STRAND_TO_COL), BASE_TOTAL_COL,
            ".nat_overwrite_eligible")
  ov   <- nat[, ..keep]
  setkeyv(ov, KEY_COLS)
  message(sprintf(
    "%s overlay (%s): %d row(s); %d overwrite-eligible; Sum = %.3e USD.",
    spec$name, suffix, nrow(ov), ov[.nat_overwrite_eligible == TRUE, .N],
    ov[.nat_overwrite_eligible == TRUE, sum(get(BASE_TOTAL_COL), na.rm = TRUE)]))
  ov[]
}


# ── Helper: base-vs-source comparison diagnostic ─────────────────────────────
#
# Mirrors write_base_fsdn_diagnostic: the base vs the converted source (total +
# the three strands, all USD) side by side, restricted to the source's area,
# with a `comparison` bucket (e.g. "canada_zero_gloria_nonzero" /
# "canada_zero_exiobase_nonzero") for filtering.  Base-side columns carry
# base$key, source-side columns spec$key; on the GLORIA pass this reproduces
# the former per-country diagnostics exactly.  Called from
# overwrite_national_sut while the base originals and .nat_ values coexist.

write_base_national_diagnostic <- function(combined, suffix, spec, base) {
  k        <- spec$key
  bk       <- base$key
  nat_area <- unique(combined[!is.na(.nat_overwrite_eligible), fabio_area_code])
  d <- combined[fabio_area_code %in% nat_area]
  
  ctx  <- intersect(c("iso3c", "fabio_area", "comm_code", "fabio_item",
                      "comm_group", "unit"), names(d))
  diag <- d[, c(KEY_COLS, ctx), with = FALSE]
  
  base_total_col <- paste0(bk, "_total_usd")
  nat_total_col  <- paste0(k,  "_total_usd")
  diag[, (base_total_col) := d[[BASE_TOTAL_COL]]]
  diag[, (nat_total_col)  := d[[paste0(".nat_", BASE_TOTAL_COL)]]]
  for (s in names(STRAND_TO_COL)) {
    gcol <- STRAND_TO_COL[[s]]
    diag[, (paste0(bk, "_", s, "_usd")) := d[[gcol]]]
    diag[, (paste0(k,  "_", s, "_usd")) := d[[paste0(".nat_", gcol)]]]
  }
  
  base_tot  <- diag[[base_total_col]]
  nat_total <- diag[[nat_total_col]]
  diag[, diff_total_usd     := nat_total - base_tot]
  diag[, abs_diff_total_usd := abs(diff_total_usd)]
  diag[, rel_diff_total     := fifelse(abs(base_tot) > DIAG_ZERO_TOL_USD,
                                       diff_total_usd / abs(base_tot),
                                       NA_real_)]
  
  has_nat   <- !is.na(d[[".nat_overwrite_eligible"]])
  base_zero <- is.finite(base_tot)  & abs(base_tot)  <= DIAG_ZERO_TOL_USD
  nat_zero  <- is.finite(nat_total) & abs(nat_total) <= DIAG_ZERO_TOL_USD
  diag[, (paste0(k, "_has_row")) := has_nat]
  diag[, overwritten := d[[".nat_overwrite_applied"]]]
  diag[, comparison := fcase(
    !has_nat,                    paste0("no_", k, "_row"),
    nat_zero & !base_zero,       paste0(k, "_zero_", bk, "_nonzero"),
    nat_zero &  base_zero,       paste0(k, "_zero_", bk, "_zero"),
    !nat_zero & base_zero,       paste0(k, "_nonzero_", bk, "_zero"),
    default =                    "both_nonzero")]
  
  strand_pairs <- as.vector(rbind(
    paste0(bk, "_", names(STRAND_TO_COL), "_usd"),
    paste0(k,  "_", names(STRAND_TO_COL), "_usd")))
  setcolorder(diag, c(
    KEY_COLS, ctx,
    base_total_col, nat_total_col,
    "diff_total_usd", "abs_diff_total_usd", "rel_diff_total",
    strand_pairs,
    paste0(k, "_has_row"), "overwritten", "comparison"))
  setorderv(diag, KEY_COLS)
  diag_path <- NAT_DIAG_PATH(base, spec, suffix)
  fwrite(diag, diag_path)
  
  summ <- diag[, .(n = .N), by = comparison][order(-n)]
  message(sprintf("  %s diagnostic (%s, %s): %d rows -> %s", spec$name,
                  base$label, suffix, nrow(diag), diag_path))
  message("    ", paste(sprintf("%s=%d", summ$comparison, summ$n), collapse = "  "))
  invisible(diag)
}


# ── Core: overlay one source onto one COMBINED base ISIC level (on disk) ─────

overwrite_national_sut <- function(spec, suffix, rates_all, base) {
  combined_path <- COMBINED_VA_PATH(base, suffix)
  if (!file.exists(combined_path))
    stop("COMBINED base [", base$label, "] not found at:\n  ", combined_path,
         "\nThe national SUT overlays run after the combine step — run that first.")
  
  ov <- build_national_sut_overlay(spec, suffix, rates_all)
  if (is.null(ov)) return(invisible(NULL))
  
  message(sprintf("\n=== %s [%s]: overlaying %s SUT onto COMBINED base ===",
                  suffix, base$label, spec$name))
  combined <- as.data.table(readRDS(combined_path))
  combined[, `:=`(fabio_area_code = as.integer(fabio_area_code),
                  fabio_item_code = as.integer(fabio_item_code),
                  year            = as.integer(year))]
  orig_cols <- names(combined)
  n_base    <- nrow(combined)
  
  # Left-join the overlay; source strands arrive under temporary ".nat_" names.
  ov2 <- copy(ov)
  setnames(ov2, c(unname(STRAND_TO_COL), BASE_TOTAL_COL),
           paste0(".nat_", c(unname(STRAND_TO_COL), BASE_TOTAL_COL)))
  combined <- merge(combined, ov2, by = KEY_COLS, all.x = TRUE, sort = FALSE)
  if (nrow(combined) != n_base)
    stop("Row count changed during ", spec$name, " overlay merge for ", suffix,
         " [", base$label, "] — the ", spec$name,
         " overlay is not unique per (area, item, year).")
  
  # Effective overwrite gate: eligible AND not a zero-keep cell.
  .bs <- combined[[BASE_TOTAL_COL]]
  .nt <- combined[[paste0(".nat_", BASE_TOTAL_COL)]]
  zero_keep <- NATIONAL_SUT_ZERO_KEEP &
    is.finite(.nt) & abs(.nt) <= DIAG_ZERO_TOL_USD &
    is.finite(.bs) & abs(.bs) >  DIAG_ZERO_TOL_USD
  combined[, .nat_overwrite_applied :=
             !is.na(.nat_overwrite_eligible) & .nat_overwrite_eligible & !zero_keep]
  
  # Diagnostic first, while base originals and .nat_ values coexist.
  write_base_national_diagnostic(combined, suffix, spec, base)
  
  # Overwrite strands + total on the eligible rows only.
  hit <- combined[, which(.nat_overwrite_applied)]
  for (col in c(unname(STRAND_TO_COL), BASE_TOTAL_COL))
    set(combined, i = hit, j = col, value = combined[[paste0(".nat_", col)]][hit])
  
  # Provenance: source-written rows get va_source = spec label, fsdn isic NA.
  combined[.nat_overwrite_applied == TRUE,
           `:=`(va_source = spec$va_source,
                fsdn_source_isic = NA_character_)]
  
  # Drop temporaries and restore the original column order.  The "^\\.nat_"
  # pattern already matches the two flag columns (.nat_overwrite_eligible /
  # _applied), so listing them again would be a duplicate assignment — let the
  # grep catch everything.
  combined[, grep("^\\.nat_", names(combined), value = TRUE) := NULL]
  setcolorder(combined, orig_cols)
  setorderv(combined, KEY_COLS)
  
  saveRDS(combined, combined_path)
  fwrite(combined,  COMBINED_CSV_PATH(base, suffix))
  n_hit <- length(hit)
  message(sprintf(
    "  %s [%s]: %s overwrote %d row(s) (%.2f%% of base). Sum = %.3e USD -> %s",
    suffix, base$label, spec$name, n_hit, 100 * n_hit / nrow(combined),
    combined[, sum(get(BASE_TOTAL_COL), na.rm = TRUE)], combined_path))
  invisible(combined)
}

# ============================================================================
# RUN
# ============================================================================
#
# Base-independent inputs first (one read / one fetch each), then one full
# synthesis pass per base: FSDN overlay -> OECD-SUT + Eurostat fishing
# overwrite -> national SUT overlays.  Order within a pass is the original
# script's order.

rates_all <- faostat_rate_table(EXCHANGE_RATE_PATH, element = EXCHANGE_ELEMENT)          # all countries, one read
eur_usd   <- rates_all[fabio_area_code == GERMANY_AREA_CODE,
                       .(year, rate_eur_per_usd = rate_lcu_per_usd)]
if (nrow(eur_usd[is.finite(rate_eur_per_usd) & rate_eur_per_usd > 0]) == 0L)
  stop("Germany's ", EXCHANGE_ELEMENT, " rate (area code ", GERMANY_AREA_CODE,
       ") is all-NA / non-positive — cannot convert FSDN EUR->USD.")
setkey(eur_usd, year)

overlay <- build_fsdn_overlay(FSDN_VA_PATH, eur_usd)   # base-independent
eu_nama <- load_eurostat_fishing(eur_usd)              # fetched once, shared

for (base in BASES) {
  message(sprintf("\n████████████████████  BASE: %s  ████████████████████", base$label))
  
  # 1. FSDN overlay onto both ISIC levels of this base (writes the combined
  #    ISIC-A / ISIC-C base to disk for the fishing step below to read).
  invisible(lapply(ISIC_LEVELS, combine_isic_level, overlay = overlay, base = base))
  
  # 2. OECD SUT fishing overwrite + Eurostat NAMA fill/override (ISIC-A only).
  # Writes the fishing-overwritten COMBINED ISIC-A output + diagnostic to disk;
  # its return is no longer captured — the forestry reference that used to read
  # the iso3<->area crosswalk from it now sources inst/regions_full.csv directly.
  overwrite_fishing_oecd_sut(rates_all, eu_nama, base)
  message(sprintf("\nOECD SUT fishing overwrite + Eurostat NAMA fallback + diagnostic complete [%s].",
                  base$label))
  
  # 3. National SUT overlays (Canada, Brazil), both ISIC levels.
  for (.spec in NATIONAL_SUT_SOURCES) {
    invisible(lapply(ISIC_LEVELS, function(sfx)
      overwrite_national_sut(.spec, sfx, rates_all, base)))
    message(sprintf("\n%s SUT overlay complete [%s] (ISIC-A and ISIC-C).",
                    .spec$name, base$label))
  }
}

message("\nDone.")