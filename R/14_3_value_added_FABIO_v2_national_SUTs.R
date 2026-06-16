# ==============================================================================
# FABIOv2 Value-Added from national Supply & Use Tables — Canada + Brazil
#                                                          (ISIC-A + ISIC-C)
#
#   Both conversions are the value-added row of the INDUSTRY TECHNOLOGY
#   ASSUMPTION — Model B in Ch. 11 of the Eurostat Manual of Supply, Use and
#   Input-Output Tables (2008 ed., KS-RA-07-013): each industry's/activity's VA
#   is split across the products it makes in proportion to each product's share
#   of that unit's gross output.  No make-matrix inversion, always non-negative.
#
#   Conservation — read carefully, the identity is PARTIAL by design:
#     The full Model-B identity Σ_p VA_p = Σ_i VA_i holds only when the sum over
#     p runs over EVERY product an industry makes (its make-shares then sum to 1
#     per industry).  This pipeline keeps only the agri-food CONCORDANCE products
#     in the numerator while the denominator stays each unit's TOTAL gross
#     output, so per-industry make-shares sum to s_i = (in-scope output)/(total
#     output) ≤ 1.  The conserved quantity is therefore
#         Σ_{p∈concordance} VA_p  =  Σ_i VA_i · s_i
#     i.e. only the in-scope fraction of each unit's VA is allocated; the
#     out-of-scope remainder Σ_i VA_i·(1−s_i) is CORRECTLY excluded (it belongs
#     to non-food products and must not land on FABIO commodities).  This is the
#     intended behaviour, NOT full conservation.  Both builders make it auditable:
#     they report (a) the full all-products invariant Σ_p VA_p = Σ_i VA_i as a
#     ~0% sanity check, and (b) the agri-food coverage fraction Σ_conc/Σ_act, and
#     write a per-unit leakage diagnostic (s_i, VA allocated vs. retained) to
#     diagnostics/FABIOv2_<tag>_make_share_leakage.csv so a mis-built concordance
#     that drops intended output out of scope shows up as an unexpectedly low s_i.
#
# The three retained VA accounts (built from each source's primary-input rows):
#   CAN  LABOUR  = Wages and salaries + Employers' social contributions
#        CAPITAL = Gross operating surplus + Gross mixed income
#        TLS     = Taxes on production − Subsidies on production
#   BRA  LABOUR  = Remunerações
#        CAPITAL = Excedente operacional bruto e rendimento misto bruto
#        TLS     = Outros impostos sobre a produção + Outros subsídios à produção
#               (IBGE stores subsidies NEGATIVE, so both TLS rows are added as-is)
#
# Output VA is carried in each SOURCE's units, as three columns
# CAPITAL/LABOUR/TLS plus their row-wise sum `value_added`:
#   CAN  [1000 CAD]            BRA  [10^6 BRL]
#
# Outputs (per country × ISIC level), all under OUTPUT_DIR:
#   FABIOv2_<tag>_value_added_ISIC-{A,C}.rds / .csv          (<tag>=CanadaSUT|BrazilSUT)
#   diagnostics/FABIOv2_<tag>_phys_intensity_winsor_ISIC-{A,C}.csv
#   diagnostics/FABIOv2_<tag>_va_reconciliation_ISIC-{A,C}.csv
#   diagnostics/FABIOv2_<tag>_make_share_leakage.csv  (per-unit s_i; agnostic to ISIC level)
#
# To run only one country, edit COUNTRIES_TO_RUN at the bottom.
# ==============================================================================

library(data.table)
library(dplyr)
source("R/00_value_added_config.R")


# ── Shared paths & universal constants ───────────────────────────────────────

FABIO_TV_PATH_A <- VA_FABIO_TV_ISIC_A_RDS
FABIO_TV_PATH_C <- VA_FABIO_TV_ISIC_C_RDS

OUTPUT_DIR <- VA_VALUE_ADDED_OUTPUT_DIR
DIAG_DIR   <- VA_VALUE_ADDED_DIAG_DIR

VA_ACCOUNTS <- c("CAPITAL", "LABOUR", "TLS")   # account schema (both countries)
WINSOR_MAD_K    <- 3.5                             # stage-8b cap, z MADs in IHS space

va_ensure_dir(OUTPUT_DIR)
va_ensure_dir(DIAG_DIR)

RULE  <- strrep("\u2500", 45)   # ─ component sub-rule (stage 8b)
RULE2 <- strrep("\u2550", 70)   # ═ section box rule


# ============================================================================
# SHARED HELPERS  (used by both countries' back halves and/or builders)
# ============================================================================


# ── Disaggregation shares (single-country: no area dimension) ────────────────
#
# For each (sut_item_code, year) the source product maps (via the concordance)
# to a set of FABIO item_codes; each cell's share is its total_value [USD] over
# the group's total_value sum, with equal-weight fallback when that sum is 0/NA.
compute_disaggregation_shares <- function(fv, item_conc, value_col) {
  base <- fv[, .(fabio_area_code, fabio_item_code, year,
                 total_value = get(value_col))]
  
  shares <- item_conc[, .(sut_item_code, fabio_item_code)][
    base, on = "fabio_item_code", allow.cartesian = TRUE, nomatch = NULL
  ]
  
  shares[, c("group_total", "group_n") := .(
    sum(total_value, na.rm = TRUE), .N
  ), by = .(sut_item_code, year)]
  
  shares[, share := fifelse(group_total == 0, 1 / group_n,
                            total_value / group_total)]
  
  shares[, c("group_total", "group_n", "total_value") := NULL]
  shares[, year := as.integer(year)]
  setkey(shares, sut_item_code, fabio_area_code, fabio_item_code, year)
  shares
}

# Split each product VA value across mapped FABIO cells, aggregate.
disaggregate_va <- function(va_prod, shares) {
  va_disagg <- shares[
    va_prod, on = c("sut_item_code", "year"),
    nomatch = 0L, allow.cartesian = TRUE
  ]
  va_disagg[, va_value_split := va_value * share]
  va_disagg[, .(va_value = sum(va_value_split, na.rm = TRUE)),
            by = .(fabio_area_code, fabio_item_code, va_account, year)]
}


# ── Connected components of the bipartite (sut_item, fabio_item) graph ───────
# Union-find with path compression + union by rank — deterministic and
# order-independent.  Returns one integer group id per input EDGE.
connected_components <- function(src, fabio) {
  bkey  <- paste0("b\u0001", as.character(src))
  fkey  <- paste0("f\u0001", as.character(fabio))
  nodes <- unique(c(bkey, fkey))
  idx   <- setNames(seq_along(nodes), nodes)
  parent <- seq_along(nodes)
  rank_  <- integer(length(nodes))
  find <- function(i) { r <- i; while (parent[r] != r) r <- parent[r]
  while (parent[i] != r) { nxt <- parent[i]; parent[i] <<- r; i <- nxt }; r }
  union <- function(a, b) {
    ra <- find(a); rb <- find(b); if (ra == rb) return(invisible())
    if (rank_[ra] < rank_[rb]) { tmp <- ra; ra <- rb; rb <- tmp }
    parent[rb] <<- ra
    if (rank_[ra] == rank_[rb]) rank_[ra] <<- rank_[ra] + 1L
  }
  bi <- as.integer(idx[bkey]); fi <- as.integer(idx[fkey])
  for (e in seq_along(bi)) union(bi[e], fi[e])
  roots <- vapply(bi, find, integer(1))
  as.integer(factor(roots))
}


# ── Source ↔ FABIO VA reconciliation diagnostic (single-country) ─────────────
#
run_va_reconciliation_diagnostic <- function(
    result, va_prod, item_conc, va_total_col, diag_path,
    fabio_total_post_disagg, fabio_total_post_zero,
    src_abbr, unit_suffix, source_name
) {
  message(sprintf("  Building %s \u2194 FABIO VA reconciliation diagnostic ...",
                  source_name))
  
  conc_edges <- unique(item_conc[, .(sut_item_code, fabio_item_code)])
  conc_edges[, group_id := connected_components(sut_item_code, fabio_item_code)]
  
  fabio_item_lookup <- unique(result[, .(fabio_item_code, fabio_item)])
  group_members <- copy(conc_edges)
  group_members[fabio_item_lookup, fabio_item_name := i.fabio_item,
                on = "fabio_item_code"]
  
  group_summary <- group_members[, .(
    src_item_codes   = paste(sort(unique(sut_item_code)),           collapse = "; "),
    fabio_item_codes = paste(sort(unique(fabio_item_code)),         collapse = "; "),
    fabio_item_names = paste(sort(unique(na.omit(fabio_item_name))), collapse = "; "),
    n_src_items      = uniqueN(sut_item_code),
    n_fabio_items    = uniqueN(fabio_item_code)
  ), by = group_id]
  setorder(group_summary, group_id)
  
  SRC <- toupper(src_abbr)
  message(sprintf(
    "    %d concordance groups  (%d with >1 %s item, %d with >1 FABIO item).",
    nrow(group_summary), group_summary[n_src_items > 1L, .N], SRC,
    group_summary[n_fabio_items > 1L, .N]
  ))
  
  # Each item belongs to exactly one connected component; enforce a strict 1:1
  # map (min group) so the merges below cannot fan a value across groups.
  src_item_to_group   <- unique(conc_edges[, .(sut_item_code,   group_id)])[
    , .(group_id = min(group_id)), by = sut_item_code]
  fabio_item_to_group <- unique(conc_edges[, .(fabio_item_code, group_id)])[
    , .(group_id = min(group_id)), by = fabio_item_code]
  stopifnot(uniqueN(src_item_to_group$sut_item_code)     == nrow(src_item_to_group),
            uniqueN(fabio_item_to_group$fabio_item_code) == nrow(fabio_item_to_group))
  
  src_side <- merge(va_prod, src_item_to_group,
                    by = "sut_item_code", allow.cartesian = TRUE)
  src_side <- src_side[, .(src_va_total = sum(va_value, na.rm = TRUE)),
                       by = .(group_id, year)]
  
  fabio_side <- merge(
    result[, .(fabio_item_code, year, fabio_va = get(va_total_col))],
    fabio_item_to_group, by = "fabio_item_code", allow.cartesian = TRUE
  )
  fabio_side <- fabio_side[, .(fabio_va_total = sum(fabio_va, na.rm = TRUE)),
                           by = .(group_id, year)]
  
  recon <- merge(src_side, fabio_side, by = c("group_id", "year"), all = TRUE)
  add_reconciliation_metrics(
    recon, a_col = "src_va_total", b_col = "fabio_va_total",
    diff_col = "diff", abs_col = "abs_diff",
    ratio_col = "ratio_fabio_over_src", pct_col = "pct_mismatch")
  
  recon <- merge(recon, group_summary, by = "group_id", all.x = TRUE)
  
  # Flavour the column names with the country's abbreviation + unit, so the CSV
  # is identical to the corresponding former single-country script.
  rn <- c(
    src_item_codes       = sprintf("%s_item_codes",        src_abbr),
    n_src_items          = sprintf("n_%s_items",           src_abbr),
    src_va_total         = sprintf("%s_va_total%s",        src_abbr, unit_suffix),
    fabio_va_total       = sprintf("fabio_va_total%s",     unit_suffix),
    diff                 = sprintf("diff%s",               unit_suffix),
    abs_diff             = sprintf("abs_diff%s",           unit_suffix),
    ratio_fabio_over_src = sprintf("ratio_fabio_over_%s",  src_abbr)
  )
  setnames(recon, names(rn), unname(rn))
  setcolorder(recon, c(
    "group_id", rn[["src_item_codes"]], "fabio_item_codes", "fabio_item_names",
    rn[["n_src_items"]], "n_fabio_items", "year",
    rn[["src_va_total"]], rn[["fabio_va_total"]],
    rn[["diff"]], rn[["abs_diff"]],
    rn[["ratio_fabio_over_src"]], "pct_mismatch"
  ))
  setorderv(recon, rn[["abs_diff"]], order = -1L, na.last = TRUE)
  fwrite(recon, diag_path)
  
  # Headline totals computed DIRECTLY (immune to any grouping artefact).
  n_groups     <- uniqueN(recon$group_id)
  mapped_fabio <- unique(item_conc$fabio_item_code)
  tot_src   <- va_prod[, sum(va_value, na.rm = TRUE)]
  tot_fabio <- result[fabio_item_code %in% mapped_fabio,
                      sum(get(va_total_col), na.rm = TRUE)]
  global_pct <- if (max(abs(tot_src), abs(tot_fabio)) > 0)
    100 * (tot_fabio - tot_src) / max(abs(tot_src), abs(tot_fabio)) else NA_real_
  
  delta_disagg <- fabio_total_post_disagg - tot_src
  delta_zero   <- fabio_total_post_zero   - fabio_total_post_disagg
  delta_winsor <- tot_fabio               - fabio_total_post_zero
  pct_of_src <- function(x) if (is.finite(tot_src) && tot_src != 0)
    100 * x / abs(tot_src) else NA_real_
  
  unit_label <- trimws(gsub("[][]", "", unit_suffix))   # "1000 CAD" / "10^6 BRL"
  message(sprintf(paste(
    "    Reconciliation written to %s",
    "      %d rows across %d groups.",
    "      Global VA totals (%s), mapped subset only:",
    "        %s total                               : %10.3e",
    "        FABIO after disagg     (post step-7)  : %10.3e   \u0394 %+10.3e  (%+7.2f%% \u2014 cells dropped at step-6 join)",
    "        FABIO after no-output zero (post 8)   : %10.3e   \u0394 %+10.3e  (%+7.2f%% \u2014 step-8 zeroing)",
    "        FABIO after winsorization (post 8b)   : %10.3e   \u0394 %+10.3e  (%+7.2f%% \u2014 step-8b per-component MAD cap)",
    "      Net mismatch (final \u2212 %s): %+.3e  (%+.2f%% symmetric).",
    sep = "\n"),
    diag_path, nrow(recon), n_groups, unit_label, SRC, tot_src,
    fabio_total_post_disagg, delta_disagg, pct_of_src(delta_disagg),
    fabio_total_post_zero,   delta_zero,   pct_of_src(delta_zero),
    tot_fabio,               delta_winsor, pct_of_src(delta_winsor),
    SRC, tot_fabio - tot_src, global_pct))
  
  invisible(NULL)
}


# ============================================================================
# SHARED BACK HALF — runs once per (country × ISIC level)
# ============================================================================

process_isic_level <- function(
    isic_level, item_conc, fv, value_col, output_col, va_prod_all,
    source_tag, source_name, unit_suffix, src_abbr,
    preserve_items = integer(0)
) {
  suffix        <- sprintf("ISIC-%s", isic_level)
  va_total_col  <- paste0("value_added", unit_suffix)
  va_cols_final <- paste0(VA_ACCOUNTS, unit_suffix)
  SRC           <- toupper(src_abbr)
  
  out_rds_path     <- file.path(OUTPUT_DIR, paste0(va_va_output_basename(source_tag, suffix), ".rds"))
  out_csv_path     <- file.path(OUTPUT_DIR, paste0(va_va_output_basename(source_tag, suffix), ".csv"))
  diag_winsor_path <- file.path(DIAG_DIR,   paste0(va_va_diag_basename(source_tag, "phys_intensity_winsor", suffix), ".csv"))
  diag_recon_path  <- file.path(DIAG_DIR,   paste0(va_va_diag_basename(source_tag, "va_reconciliation", suffix), ".csv"))
  
  message(sprintf("\n%s", RULE2))
  message(sprintf("  %s  %s back half", source_name, suffix))
  message(RULE2)
  
  # Restrict the (shared) product VA table to this level's concordance products.
  va_prod <- va_prod_all[sut_item_code %in% unique(item_conc$sut_item_code)]
  
  # ── 5. Disaggregation shares ─────────────────────────────────────────────
  message(sprintf("[%s] Computing disaggregation shares ...", suffix))
  shares <- compute_disaggregation_shares(fv, item_conc, value_col)
  
  # ── 6. Disaggregate product VA into FABIO cells ──────────────────────────
  message(sprintf("[%s] Disaggregating VA ...", suffix))
  va_disagg <- disaggregate_va(va_prod, shares)
  message(sprintf("  %d disaggregated rows.", nrow(va_disagg)))
  
  keys_in  <- unique(va_prod[, .(sut_item_code, year)])
  keys_ok  <- unique(shares[,  .(sut_item_code, year)])
  dropped  <- fsetdiff(keys_in, keys_ok)
  if (nrow(dropped) > 0L)
    message(sprintf("  %d (%s item, year) cells had no FABIO target and were dropped.",
                    nrow(dropped), SRC))
  
  # ── 7. Pivot wide and right-join onto FABIO total_values ─────────────────
  message(sprintf("[%s] Pivoting and joining onto FABIO total values ...", suffix))
  va_wide <- dcast(va_disagg,
                   fabio_area_code + fabio_item_code + year ~ va_account,
                   value.var = "va_value", fill = 0)
  va_cols_old <- intersect(VA_ACCOUNTS, names(va_wide))
  setnames(va_wide, old = va_cols_old, new = paste0(va_cols_old, unit_suffix))
  for (acc in setdiff(VA_ACCOUNTS, va_cols_old))
    va_wide[, (paste0(acc, unit_suffix)) := 0]
  
  result <- va_wide[fv, on = .(fabio_area_code, fabio_item_code, year)]
  for (vc in va_cols_final)
    set(result, which(is.na(result[[vc]])), vc, 0)
  
  fabio_total_post_disagg <- result[, sum(unlist(.SD), na.rm = TRUE), .SDcols = va_cols_final]
  
  result[, phys_denom := fifelse(
    is.finite(get(output_col)) & get(output_col) > 0,
    as.numeric(get(output_col)), NA_real_)]
  result[, preserve_idx := fabio_item_code %in% preserve_items]
  
  # ── 8. Zero out VA on rows with no product output (preserve carve-out) ────
  no_output   <- is.na(result$phys_denom)
  preserve    <- result$preserve_idx
  to_zero     <- no_output & !preserve
  n_preserved <- sum(no_output & preserve)
  if (any(to_zero)) {
    for (vc in va_cols_final) set(result, which(to_zero), vc, 0)
    message(sprintf("  Zeroed VA on %d / %d rows with no product output.",
                    sum(to_zero), nrow(result)))
  }
  if (n_preserved > 0L)
    message(sprintf("  Preserved VA on %d no-output row(s) for double-mapped FABIO item(s).",
                    n_preserved))
  
  fabio_total_post_zero <- result[, sum(unlist(.SD), na.rm = TRUE), .SDcols = va_cols_final]
  
  # ── 8b. Per-component IHS+MAD winsorization on (component / TPO) ──────────
  message(sprintf("[%s] Stage 8b: per-component MAD cap on (component / TPO) ...", suffix))
  diag_rows <- vector("list", length(va_cols_final)); names(diag_rows) <- va_cols_final
  
  for (vc in va_cols_final) {
    message(sprintf("  [%s] %s", vc, RULE))
    diag_rows[[vc]] <- cap_component_by_item(
      result, vc, k = WINSOR_MAD_K, exempt_col = "preserve_idx")
  }
  
  diag_combined <- rbindlist(diag_rows, use.names = TRUE, fill = TRUE)
  if (nrow(diag_combined) > 0L) {
    write_va_diagnostic(diag_combined, type = "winsor",
                        sort_first = "va_account",
                        out_path   = diag_winsor_path,
                        announce   = FALSE,
                        col_order  = c(
                          "va_account", "fabio_area_code", "fabio_area",
                          "fabio_item_code", "fabio_item", "year",
                          "intensity_pre", "intensity_post", "cap_lower", "cap_upper",
                          "winsorized", "direction", "sign_flipped_at_cap",
                          "abs_change", "mad_z", "abs_mad_z", "item_theta"))
  } else {
    fwrite(diag_combined, diag_winsor_path)
  }
  message(sprintf("  Per-component winsor diagnostic \u2192 %s  (%d rows).",
                  diag_winsor_path, nrow(diag_combined)))
  
  result[, c("phys_denom", "preserve_idx") := NULL]
  
  # ── 9. Total VA column + final column order ──────────────────────────────
  result[, (va_total_col) := rowSums(.SD, na.rm = TRUE), .SDcols = va_cols_final]
  setcolorder(result, c(
    "row_id", "iso3c", "fabio_area_code", "fabio_area",
    "comm_code", "fabio_item_code", "fabio_item", "comm_group",
    "unit", "year", output_col,
    "price [USD/unit]", "price_source", "price_source_constituents",
    value_col, va_cols_final, va_total_col))
  setorder(result, fabio_area_code, fabio_item_code, year)
  
  saveRDS(result, out_rds_path)
  fwrite(result,  out_csv_path)
  va_filled <- result[get(va_total_col) != 0, .N]
  message(sprintf("[%s] Main output: %d rows \u2192 %s  (VA non-zero on %d rows, %.1f%%).",
                  suffix, nrow(result), out_rds_path, va_filled,
                  100 * va_filled / nrow(result)))
  
  # ── 10. Reconciliation diagnostic ────────────────────────────────────────
  run_va_reconciliation_diagnostic(
    result, va_prod, item_conc, va_total_col, diag_recon_path,
    fabio_total_post_disagg = fabio_total_post_disagg,
    fabio_total_post_zero   = fabio_total_post_zero,
    src_abbr = src_abbr, unit_suffix = unit_suffix, source_name = source_name)
  
  invisible(result)
}


# ============================================================================
# CANADA FRONT HALF — StatCan Detail SUT (cansim 36-10-0478-01), pure reader
# ============================================================================
#
# Reads three thin slices of the StatCan Detail SUT, all STAGED as plain CSVs by
# R/00_9_prep_value_added.R (this script does NO cansim/network access):
#   (a) Use  / Basic price / VA primary-input rows, by industry;
#   (b) Supply (Make) rows for the concordance products  (numerators);
#   (c) Supply (Make) rows for the industries that make them (denominators).
# Year/concordance/VA-label scope is fixed at stage time by 00_9 (which derives
# it from the same config this script reads), so the slices already match what
# this reader expects — there is no cache-staleness check or auto re-pull here.
# Industry VA → product VA via the Make-share (Model B) allocation.

# ── Canada config ────────────────────────────────────────────────────────────
CAN_CONC_PATH        <- VA_CONC_CANADA_SUT
# Canada SUT slices are staged as plain CSVs by R/00_9_prep_value_added.R; this
# script is a pure READER of them (no cansim, no network). The staged paths are
# the shared producer/consumer contract, so they are defined ONCE in
# R/00_value_added_config.R — do NOT hard-code them here (changing one side only
# silently breaks the handoff). Bound to short local names for readability.
CAN_CACHE_DIR        <- VA_CAN_SUT_DIR
CAN_USE_CSV          <- VA_CAN_USE_CSV
CAN_NUM_CSV          <- VA_CAN_NUM_CSV
CAN_DEN_CSV          <- VA_CAN_DEN_CSV
CAN_AVAIL_CSV        <- VA_CAN_AVAIL_CSV

# StatCan Use/Supply selectors (verify against your cansim vintage if loads warn).
CAN_USE_VALUE     <- "Use"
CAN_SUPPLY_VALUE  <- "Supply"
CAN_BASIC_PRICE   <- "Basic price"        # (some vintages: "Basic prices")
CAN_GEO_VALUE     <- "Canada"

# VA primary-input row labels and their grouping/sign into the three accounts.
CAN_LABOUR_LABELS  <- c("Wages and salaries",
                        "Employers' social contributions",
                        "Employers\u2019 social contributions")  # ASCII + typographic apostrophe
CAN_CAPITAL_LABELS <- c("Gross operating surplus", "Gross mixed income")
CAN_TLS_POS_LABELS <- c("Taxes on production")
CAN_TLS_NEG_LABELS <- c("Subsidies on production")
# Supply-side aggregate product rows to exclude when summing industry output.
# "Total products" is cansim's per-industry column total in the Make matrix (equal
# to the sum of the leaf products); leaving it in DOUBLES industry gross output and
# halves every make-share.  Both compute_industry_output() and the Model-B invariant
# filter on this list, so adding it here fixes the denominator consistently.
CAN_SUPPLY_TOTAL_LABELS <- c("Total", "Total products")
# Supply-table "Industry" members that are NOT production industries: the valuation
# bridge (margins, taxes), the import column, and column totals.  They "supply"
# products in the accounting sense but carry no value-added, so they're excluded
# from the make-industry set (they would otherwise show up as VA-less non-matches).
# Matched via nrm_label(), so a typographic apostrophe in the data is tolerated.
CAN_SUPPLY_NONINDUSTRY_LABELS <- c(
  "Total industries",
  "Total supply at purchasers' prices",
  "Total supply at basic prices",
  "Trade margins",
  "Transportation, gas and storage margins",
  "Taxes on products",
  "International imports")

# ── Canada helpers ───────────────────────────────────────────────────────────

# ── Canada processing functions (operate on collected slices; no network) ────
process_use_va <- function(use_raw, use_years) {
  cols <- detect_sut_columns(use_raw)
  if (is.na(cols$ind_label))
    stop("Use-table slice has no 'Industry' column \u2014 cannot build industry-level VA.")
  use <- use_raw[, .(
    year     = as.integer(REF_DATE),
    industry = trimws(as.character(get(cols$ind_label))),
    product  = trimws(as.character(get(cols$prod_label))),
    value    = as.numeric(VALUE))]
  use <- use[year %in% use_years]
  use[, product := trimws(sub("\\s*\\[[^][]+\\]\\s*$", "", product))]   # drop " [PRM...]" suffix
  use[, product := nrm_label(product)]
  lab <- function(v) nrm_label(v)
  use[, `:=`(va_account = NA_character_, sign = 1)]
  use[product %in% lab(CAN_LABOUR_LABELS),  va_account := "LABOUR"]
  use[product %in% lab(CAN_CAPITAL_LABELS), va_account := "CAPITAL"]
  use[product %in% lab(CAN_TLS_POS_LABELS), `:=`(va_account = "TLS", sign =  1)]
  use[product %in% lab(CAN_TLS_NEG_LABELS), `:=`(va_account = "TLS", sign = -1)]
  
  found  <- unique(use[!is.na(va_account), product])
  expect <- unique(lab(c(CAN_LABOUR_LABELS, CAN_CAPITAL_LABELS,
                         CAN_TLS_POS_LABELS, CAN_TLS_NEG_LABELS)))
  missing_lab <- setdiff(expect, found)
  if (length(missing_lab) > 0L)
    warning("These VA primary-input labels were NOT found in the cansim Use slice ",
            "(check spelling in the config block): ", paste(missing_lab, collapse = " | "))
  
  use <- use[!is.na(va_account)]
  use[, .(va_value = sum(sign * value, na.rm = TRUE)),
      by = .(industry, va_account, year)]
}

compute_industry_output <- function(den_raw, use_years) {
  dcols <- detect_sut_columns(den_raw)
  den <- den_raw[, .(
    year     = as.integer(REF_DATE),
    industry = trimws(as.character(get(dcols$ind_label))),
    prod_lab = trimws(sub("\\s*\\[[^][]+\\]\\s*$", "",
                          as.character(get(dcols$prod_label)))),
    value    = as.numeric(VALUE))]
  den <- den[year %in% use_years &
               !(prod_lab %in% CAN_SUPPLY_TOTAL_LABELS) & is.finite(value)]
  # NB: a single product legitimately dominating an industry's output (e.g. Fishing
  # is ~99% 'Fishery products') is EXPECTED and fine here, so there is no max-share
  # alarm.  A lingering per-column AGGREGATE (the only real concern) is caught
  # precisely by the arithmetic tripwire in build_product_va_canada (2*value ==
  # column sum), which supersedes the old fuzzy >60% heuristic.
  den[, .(ind_output = sum(value, na.rm = TRUE)), by = .(industry, year)]
}

#' Make-share leakage diagnostic (shared by both countries).
#'
#' The concordance-restricted allocation conserves only Σ_i VA_i·s_i, where
#' s_i = (in-scope gross output of unit i) / (total gross output of unit i) is
#' the fraction of unit i's output that maps to agri-food concordance products
#' (see the header's "Conservation" note).  This makes s_i and the resulting
#' allocated-vs-retained VA split visible per unit×year, so a mis-built
#' concordance — which would silently drop a unit's intended output out of scope
#' and depress total VA with no error — surfaces as an unexpectedly low s_i.
#'
#' @param va_ind   long unit VA: <unit_col>, va_account, year, va_value.
#' @param sup_prep in-scope (concordance) make numerators: <unit_col>, year, sup_val.
#' @param ind_out  total unit gross output: <unit_col>, year, ind_output.
#' @param unit_col name of the unit key ("industry" for CAN, "activity_code" BRA).
#' @return data.table, one row per (unit, year), sorted by ascending in_scope_share.
make_share_leakage <- function(va_ind, sup_prep, ind_out, unit_col) {
  in_scope <- sup_prep[, .(in_scope_output = sum(sup_val, na.rm = TRUE)),
                       by = c(unit_col, "year")]
  va_unit  <- va_ind[, .(va_total = sum(va_value, na.rm = TRUE)),
                     by = c(unit_col, "year")]
  # Base on ind_out so units with a denominator but no in-scope output (s_i = 0)
  # still appear; in-scope/VA totals coalesce to 0 where the join misses.
  leak <- merge(ind_out, in_scope, by = c(unit_col, "year"), all.x = TRUE)
  leak <- merge(leak,    va_unit,  by = c(unit_col, "year"), all.x = TRUE)
  leak[, `:=`(in_scope_output = fcoalesce(in_scope_output, 0),
              va_total        = fcoalesce(va_total,        0))]
  leak[, in_scope_share := fifelse(ind_output > 0, in_scope_output / ind_output, NA_real_)]
  leak[, `:=`(va_allocated = va_total * fcoalesce(in_scope_share, 0),
              va_retained  = va_total * (1 - fcoalesce(in_scope_share, 0)))]
  setorderv(leak, "in_scope_share", na.last = TRUE)
  setcolorder(leak, c(unit_col, "year", "ind_output", "in_scope_output",
                      "in_scope_share", "va_total", "va_allocated", "va_retained"))
  leak[]
}

#' Write the leakage diagnostic + a one-line economy-wide coverage summary.
#' Pure I/O wrapper so both builders emit an identical artefact.
write_make_share_leakage <- function(leak, source_tag, unit_noun) {
  path <- file.path(DIAG_DIR, sprintf("FABIOv2_%s_make_share_leakage.csv", source_tag))
  fwrite(leak, path)
  tot_va  <- leak[, sum(va_total,     na.rm = TRUE)]
  tot_alc <- leak[, sum(va_allocated, na.rm = TRUE)]
  message(sprintf(
    "  Make-share leakage: %d %s\u00d7year unit(s); allocated %.6e of %.6e %s VA (%.1f%% in-scope). Diagnostic \u2192 %s",
    nrow(leak), unit_noun, tot_alc, tot_va, unit_noun,
    if (tot_va != 0) 100 * tot_alc / tot_va else NA_real_, path))
  invisible(path)
}

#' Make-share allocation (Model B): VA_product = Σ_industries VA_industry ×
#' Supply[p,i] / IndustryOutput[i].  Every (product, account, year) cell kept.
allocate_va_canada <- function(va_ind, sup_prep, ind_out) {
  shares <- merge(sup_prep, ind_out, by = c("industry", "year"))
  shares[, make_share := fifelse(ind_output > 0, sup_val / ind_output, 0)]
  alloc <- merge(va_ind, shares, by = c("industry", "year"), allow.cartesian = TRUE)
  alloc[, va_share := va_value * make_share]
  alloc[, .(va_value = sum(va_share, na.rm = TRUE)),
        by = .(sut_item_code, va_account, year)]
}

# ── Canada builder: concordances + cfg in, (va_prod_all, use_years) out ──────
build_product_va_canada <- function(cfg, item_conc_a, item_conc_c, conc_all) {
  # Canada SUT slices are staged as plain CSVs by R/00_9_prep_value_added.R.
  # This builder is a pure READER of them: no cansim, no network.
  for (.p in c(CAN_USE_CSV, CAN_NUM_CSV, CAN_DEN_CSV, CAN_AVAIL_CSV))
    if (!file.exists(.p))
      stop("Canada SUT slice not staged: ", .p,
           "\n  Run R/00_9_prep_value_added.R first to stage the Canada SUTs.")
  
  requested_years <- cfg$years
  avail_years <- sort(unique(as.integer(fread(CAN_AVAIL_CSV)$avail_year)))
  use_years   <- intersect(requested_years, avail_years)
  if (length(use_years) == 0L)
    stop("None of YEARS [", paste(range(requested_years), collapse = "\u2013"),
         "] are in the staged Canada slices (available: ",
         paste(range(avail_years), collapse = "\u2013"), ").")
  if (!all(requested_years %in% avail_years))
    message(sprintf("  Note: requested %s; staged coverage %s \u2014 proceeding with %s.",
                    paste(range(requested_years), collapse = "\u2013"),
                    paste(range(avail_years),     collapse = "\u2013"),
                    paste(range(use_years),       collapse = "\u2013")))
  
  # (a) Use-table VA by industry (staged slice).
  message("Reading staged Use-table VA slice (Basic price) ...")
  use_raw <- fread(CAN_USE_CSV)
  va_ind  <- process_use_va(use_raw, use_years)
  message(sprintf("  %d industry x account x year VA rows; accounts: %s.",
                  nrow(va_ind), paste(sort(unique(va_ind$va_account)), collapse = ", ")))
  
  # (b) Make-matrix numerators for the concordance products.
  message("Reading staged Supply (Make) numerator slice ...")
  num_raw <- fread(CAN_NUM_CSV)
  sup_prep <- prep_supply_numerators(num_raw, conc_all$sut_item_code,
                                     conc_all$sut_item, use_years)
  # Drop supply-table valuation/margin/import/total pseudo-columns: they "supply"
  # products but are not production industries and carry no VA (see config).  This
  # keeps them out of the industry set, the denominator pull, and the leakage table;
  # the allocation result is unchanged (they have no Use-side VA to allocate).
  .nonind <- nrm_label(CAN_SUPPLY_NONINDUSTRY_LABELS)
  n_nonind <- sup_prep[nrm_label(industry) %in% .nonind, uniqueN(industry)]
  if (n_nonind > 0L) {
    message(sprintf("  Excluding %d supply-table non-industry column(s) (margins/taxes/imports/totals).",
                    n_nonind))
    sup_prep <- sup_prep[!(nrm_label(industry) %in% .nonind)]
  }
  rm(.nonind)
  # The normalized label is the join key (matches va_ind & ind_out); the RAW member
  # is the denominator FILTER key, since cansim normalization rewrites Industry just
  # like Product and the staged denominator slice was filtered PRE-normalization.
  needed_industries     <- sort(unique(sup_prep$industry))
  needed_industries_raw <- sort(unique(sup_prep$industry_raw))
  message(sprintf("  %d industries make the concordance products (%d raw member string(s)).",
                  length(needed_industries), length(needed_industries_raw)))
  
  # (c) Make-matrix denominators (industry gross output).
  message("Reading staged Supply (Make) denominator slice ...")
  den_raw <- fread(CAN_DEN_CSV)
  # A superset cache (e.g. an earlier 42-industry pull) is treated as "covering" a
  # later industry subset, so the loaded slice may still physically contain the
  # valuation/margin/import/total pseudo-columns.  Drop them here so ind_out, the
  # invariant, the leakage table, and the Use/Supply guard all see real industries
  # only — regardless of whether the slice came fresh or from a superset cache.
  .dind  <- detect_sut_columns(den_raw)$ind_label
  .dropi <- nrm_label(trimws(as.character(den_raw[[.dind]]))) %in% nrm_label(CAN_SUPPLY_NONINDUSTRY_LABELS)
  if (any(.dropi)) den_raw <- den_raw[!.dropi]
  rm(.dind, .dropi)
  
  # Denominator de-doubling is handled by CAN_SUPPLY_TOTAL_LABELS (now incl.
  # "Total products"), excluded inside compute_industry_output() and the invariant's
  # sup_full so both stay consistent.  This block is a TRIPWIRE only (changes no data):
  # after removing the known total labels, it checks arithmetically per industry x year
  # for any row STILL equal to the sum of the others (2*value == column sum).  Such a
  # row is a new/unlisted aggregate that would double industry output and halve every
  # make-share — invisible to the Model-B invariant (it cancels in num & den).  If one
  # survives, its label must be added to CAN_SUPPLY_TOTAL_LABELS.
  {
    .ic   <- detect_sut_columns(den_raw)
    .lab  <- nrm_label(trimws(sub("\\s*\\[[^][]+\\]\\s*$", "",
                                  as.character(den_raw[[.ic$prod_label]]))))
    .keep <- !(.lab %in% nrm_label(CAN_SUPPLY_TOTAL_LABELS))
    .chk  <- data.table(
      ind = trimws(as.character(den_raw[[.ic$ind_label]]))[.keep],
      yr  = suppressWarnings(as.integer(den_raw$REF_DATE))[.keep],
      lab = .lab[.keep],
      v   = suppressWarnings(as.numeric(den_raw$VALUE))[.keep])[is.finite(v)]
    .chk[, cs := sum(v), by = .(ind, yr)]
    .surv <- unique(.chk[cs > 0 & abs(2 * v - cs) <= 1e-6 * cs, lab])
    if (length(.surv) > 0L)
      warning(sprintf(
        paste0("Make denominator still contains an uncaught per-industry aggregate after ",
               "excluding CAN_SUPPLY_TOTAL_LABELS: %s.  Each equals its column sum and will ",
               "DOUBLE industry output (halving every make-share; the Model-B invariant ",
               "cannot see this).  Add the label(s) to CAN_SUPPLY_TOTAL_LABELS."),
        paste(.surv, collapse = " | ")), immediate. = TRUE)
    rm(.ic, .lab, .keep, .chk, .surv)
  }
  ind_out <- compute_industry_output(den_raw, use_years)
  
  miss_ind <- setdiff(needed_industries, unique(ind_out$industry))
  if (length(miss_ind) > 0L) {
    frac_missing <- length(miss_ind) / length(needed_industries)
    msg <- sprintf(
      "%d of %d industry(ies) supplying concordance products have no gross-output denominator: %s",
      length(miss_ind), length(needed_industries), paste(miss_ind, collapse = " | "))
    # A handful (e.g. International imports) genuinely lack a domestic make-output
    # denominator.  Losing a LARGE share instead means the denominator filter failed
    # to match \u2014 the Industry-normalization mismatch \u2014 which silently leaks the bulk
    # of the economy's VA out of the allocation, so fail loudly rather than drop it.
    if (frac_missing > 0.20)
      stop(msg, "\n  >>> ", round(100 * frac_missing), "% of make-industries lost their ",
           "denominator. This is the cansim Industry-normalization mismatch, not a data ",
           "gap. Ensure the staged numerator slice carries 'industry_raw' (re-run ",
           "R/00_9_prep_value_added.R to re-stage the Canada slices).")
    warning(msg, " \u2014 DROPPED.", immediate. = TRUE)
  }
  
  message("Allocating industry VA to products via Supply (Make) shares ...")
  va_prod_all <- allocate_va_canada(va_ind, sup_prep, ind_out)
  message(sprintf("  %d product x account x year VA rows (%d CAN products covered).",
                  nrow(va_prod_all), uniqueN(va_prod_all$sut_item_code)))
  
  # Model-B invariant (allocate to ALL products) + agri-food coverage.
  #
  # NOTE re Brazil: Brazil reads the WHOLE Make matrix, so its check compares the
  # economy-wide industry total against the product total.  Canada's denominator
  # pull deliberately fetches ONLY the concordance-making industries (the rest make
  # no in-scope product), so `den_raw`/`ind_out` cover a SUBSET of the economy.  The
  # make-shares-sum-to-1 invariant therefore conserves VA over exactly that covered
  # subset, not the whole economy.  Comparing it against the economy-wide total would
  # spuriously read ~ -64% (the uncovered industries), so the invariant is taken over
  # the COVERED industries; the economy-wide figure is reported separately below.
  .dcols   <- detect_sut_columns(den_raw)
  sup_full <- den_raw[, .(
    year          = as.integer(REF_DATE),
    industry      = trimws(as.character(get(.dcols$ind_label))),
    sut_item_code = trimws(sub("\\s*\\[[^][]+\\]\\s*$", "",
                               as.character(get(.dcols$prod_label)))),
    sup_val       = as.numeric(VALUE))][
      year %in% use_years &
        !(sut_item_code %in% CAN_SUPPLY_TOTAL_LABELS) & is.finite(sup_val)]
  va_prod_chk <- allocate_va_canada(va_ind, sup_full, ind_out)
  covered_ind <- unique(ind_out$industry)
  tot_econ <- va_ind[,                          sum(va_value, na.rm = TRUE)]  # whole economy
  tot_act  <- va_ind[industry %in% covered_ind, sum(va_value, na.rm = TRUE)]  # den_raw-covered industries
  tot_full <- va_prod_chk[,                     sum(va_value, na.rm = TRUE)]
  
  # Real-bug guard: a Supply-side make-industry whose label has no Use-side (VA)
  # match cannot be allocated and silently drops its agri-food VA.  This is the
  # Use-vs-Supply analogue of the Industry-normalization mismatch fixed above.
  unmatched_supply_ind <- setdiff(covered_ind, unique(va_ind$industry))
  if (length(unmatched_supply_ind) > 0L)
    warning(sprintf(
      "%d make-industry(ies) have a Supply-side label with NO matching Use-side (VA) label \u2014 their VA cannot be allocated (Use/Supply industry naming mismatch): %s",
      length(unmatched_supply_ind), paste(unmatched_supply_ind, collapse = " | ")),
      immediate. = TRUE)
  
  message(sprintf(
    "  Model-B invariant (allocate to ALL products of covered industries): industry \u03a3 = %.6e ; product \u03a3 = %.6e  (%.4f%% diff \u2014 expect ~0).",
    tot_act, tot_full,
    if (tot_act != 0) 100 * (tot_full - tot_act) / abs(tot_act) else NA_real_))
  message(sprintf(
    "  Denominator slice covers %d make-industry(ies) = %.1f%% of economy-wide VA; the rest make no concordance product and are outside the Canada pull by design.",
    length(covered_ind), if (tot_econ != 0) 100 * tot_act / tot_econ else NA_real_))
  tot_conc <- va_prod_all[, sum(va_value, na.rm = TRUE)]
  message(sprintf(
    "  Agri-food coverage: concordance products carry %.6e of %.6e total VA (%.1f%% of the economy).",
    tot_conc, tot_econ, if (tot_econ != 0) 100 * tot_conc / tot_econ else NA_real_))
  rm(.dcols, sup_full, va_prod_chk)
  
  # Per-industry leakage s_i = in-scope output / total output (header note).
  write_make_share_leakage(
    make_share_leakage(va_ind, sup_prep, ind_out, "industry"),
    cfg$source_tag, "industry")
  
  unmatched <- setdiff(conc_all$sut_item_code, unique(va_prod_all$sut_item_code))
  if (length(unmatched) > 0L)
    message(sprintf("  %d concordance CAN product(s) received NO VA (no Make/Use match): %s",
                    length(unmatched), paste(unmatched, collapse = ", ")))
  
  list(va_prod_all = va_prod_all, use_years = use_years)
}


# ============================================================================
# BRAZIL FRONT HALF — IBGE TRU workbooks (pure reader of files staged by 00_9)
# ============================================================================
#
# Reads one pair of .xls files per year from BRA_SUT_DIR:
#   68_tab1_<year>.xls  (Recursos / Supply) → sheet "producao"  (Make matrix)
#   68_tab2_<year>.xls  (Usos    / Use)     → sheet "VA"        (VA by activity)
# Activity VA → product VA via the Make-share (Model B) allocation.
#
# BRA_SUT_DIR holds the IBGE nivel-68 .xls pairs staged by
# R/00_9_prep_value_added.R (which downloads + unzips IBGE's archive). This
# builder is a pure path-based reader; it does no network access.

# ── Brazil config ────────────────────────────────────────────────────────────
# BRA_SUT_DIR and the .xls filename templates are the shared producer/consumer
# contract with R/00_9_prep_value_added.R, so they are defined ONCE in
# R/00_value_added_config.R — do NOT hard-code them here. The concordance, sheet
# names and VA labels below are private to this reader.
BRA_CONC_PATH       <- file.path(VA_CONCORDANCE_DIR, "concordance_items_brazil_sut_fabio.csv")
BRA_SUT_DIR         <- VA_BRA_SUT_DIR
BRA_SUPPLY_FILE_FMT <- VA_BRA_SUPPLY_FILE_FMT
BRA_USE_FILE_FMT    <- VA_BRA_USE_FILE_FMT
BRA_SUPPLY_SHEET    <- "producao"            # Make matrix (product x activity)
BRA_USE_SHEET       <- "VA"                  # value-added by activity

# IBGE VA component labels (column A of the "VA" sheet; PARENT rows only).
BRA_LABOUR_LABELS  <- c("Remunera\u00e7\u00f5es")                                  # Remunerações
BRA_CAPITAL_LABELS <- c("Excedente operacional bruto e rendimento misto bruto")
BRA_TLS_POS_LABELS <- c("Outros impostos sobre a produ\u00e7\u00e3o")              # Outros impostos sobre a produção
BRA_TLS_NEG_LABELS <- c("Outros subs\u00eddios \u00e0 produ\u00e7\u00e3o")         # Outros subsídios à produção
# IBGE stores subsidies NEGATIVE → both TLS rows are added as-is (sign +1).
BRA_TLS_SUBSIDY_SIGN <- 1
# Gross-output row label in the "VA" sheet (Make-share denominator cross-check).
BRA_OUTPUT_ROW_LABEL <- "Valor da produ\u00e7\u00e3o"                              # Valor da produção

#' Leading code token of a header cell (text before the first line break, trimmed).
lead_token <- function(x) trimws(sub("[\r\n].*$", "", as.character(x)))

#' Collapse internal whitespace and trim — robust label matching (e.g. "Soja  em grão").
norm_label <- function(x) trimws(gsub("[[:space:]]+", " ", as.character(x)))

resolve_sheet <- function(path, want) {
  sh  <- readxl::excel_sheets(path)
  hit <- sh[tolower(trimws(sh)) == tolower(trimws(want))]
  if (length(hit) == 0L)
    stop("Workbook ", basename(path), " has no sheet matching '", want,
         "'.  Sheets present: ", paste(sh, collapse = ", "))
  hit[1L]
}

read_sheet_matrix <- function(path, sheet) {
  df <- suppressMessages(readxl::read_excel(
    path, sheet = sheet, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"))
  as.matrix(df)
}

#' Locate the activity HEADER row (most n-digit activity codes); ndigit = 4 for IBGE.
find_activity_header <- function(mat, ndigit = 4L) {
  pat    <- sprintf("^[0-9]{%d}$", ndigit)
  counts <- apply(mat, 1L, function(r) sum(grepl(pat, lead_token(r)), na.rm = TRUE))
  hrow   <- which.max(counts)
  if (length(hrow) == 0L || counts[hrow] < 5L)
    stop("Could not locate the activity header row (expected many ", ndigit,
         "-digit codes in one row).")
  codes <- lead_token(mat[hrow, ])
  cols  <- which(grepl(pat, codes))
  list(row = as.integer(hrow), cols = cols, codes = codes[cols])
}

#' Locate the product-code column (most n-digit product codes); ndigit = 5 for IBGE.
find_code_col <- function(mat, ndigit = 5L) {
  pat    <- sprintf("^[0-9]{%d}$", ndigit)
  counts <- apply(mat, 2L, function(cc) sum(grepl(pat, lead_token(cc)), na.rm = TRUE))
  cc     <- which.max(counts)
  if (length(cc) == 0L || counts[cc] < 5L)
    stop("Could not locate the product-code column (expected many ", ndigit,
         "-digit codes in one column).")
  as.integer(cc)
}

#' Read VA-by-activity from one year's "VA" (Use) workbook → (activity_code,
#' va_account, year, va_value) in 10^6 BRL; attr "output" = gross-output row.
read_va_by_activity <- function(use_path, year) {
  sheet <- resolve_sheet(use_path, BRA_USE_SHEET)
  mat   <- read_sheet_matrix(use_path, sheet)
  hdr   <- find_activity_header(mat, 4L)
  colA  <- norm_label(mat[, 1L])
  numv  <- function(v) suppressWarnings(as.numeric(v))
  
  acct_spec <- list(
    list(acct = "LABOUR",  labels = BRA_LABOUR_LABELS,  sign = 1),
    list(acct = "CAPITAL", labels = BRA_CAPITAL_LABELS, sign = 1),
    list(acct = "TLS",     labels = BRA_TLS_POS_LABELS, sign = 1),
    list(acct = "TLS",     labels = BRA_TLS_NEG_LABELS, sign = BRA_TLS_SUBSIDY_SIGN))
  
  found  <- character(0); pieces <- list(); subsidy_vals <- numeric(0)
  for (sp in acct_spec) {
    targets <- norm_label(sp$labels)
    rws <- which(colA %in% targets)
    if (length(rws) == 0L) next
    found <- c(found, targets[targets %in% colA[rws]])
    vals  <- numv(mat[rws, hdr$cols, drop = FALSE])
    vals  <- matrix(vals, nrow = length(rws))
    peract <- colSums(vals, na.rm = TRUE) * sp$sign
    if (identical(sp$labels, BRA_TLS_NEG_LABELS)) subsidy_vals <- peract
    pieces[[length(pieces) + 1L]] <- data.table(
      activity_code = hdr$codes, va_account = sp$acct,
      year = as.integer(year), va_value = peract)
  }
  
  expect <- norm_label(c(BRA_LABOUR_LABELS, BRA_CAPITAL_LABELS,
                         BRA_TLS_POS_LABELS, BRA_TLS_NEG_LABELS))
  missing_lab <- setdiff(expect, unique(found))
  if (length(missing_lab) > 0L)
    warning(sprintf("[%d] VA component label(s) NOT found in sheet '%s' ", year, sheet),
            "(check spelling in the config block): ", paste(missing_lab, collapse = " | "))
  
  if (length(subsidy_vals) > 0L && BRA_TLS_SUBSIDY_SIGN == 1 &&
      any(subsidy_vals > 0, na.rm = TRUE))
    warning(sprintf("[%d] 'Outros subsidios a producao' has POSITIVE values after adding ", year),
            "as-is — IBGE usually stores subsidies negative.  If your vintage stores ",
            "them positive, set BRA_TLS_SUBSIDY_SIGN <- -1.")
  
  va_long <- rbindlist(pieces, use.names = TRUE)
  va_long <- va_long[, .(va_value = sum(va_value, na.rm = TRUE)),
                     by = .(activity_code, va_account, year)]
  
  orow <- which(colA %in% norm_label(BRA_OUTPUT_ROW_LABEL))
  if (length(orow) >= 1L) {
    ov <- numv(mat[orow[1L], hdr$cols])
    attr(va_long, "output") <- data.table(
      activity_code = hdr$codes, year = as.integer(year), va_output = ov)
  }
  va_long
}

#' Read the Make matrix from one year's "producao" (Supply) workbook → LONG
#' (sut_item_code, activity_code, year, sup_val) in 10^6 BRL over all products.
read_make_matrix <- function(supply_path, year) {
  sheet  <- resolve_sheet(supply_path, BRA_SUPPLY_SHEET)
  mat    <- read_sheet_matrix(supply_path, sheet)
  hdr    <- find_activity_header(mat, 4L)
  pcol   <- find_code_col(mat, 5L)
  codeA  <- lead_token(mat[, pcol])
  codeA  <- ifelse(grepl("^[0-9]{4}$", codeA), paste0("0", codeA), codeA)  # repair lost leading zero
  prows  <- which(grepl("^[0-9]{5}$", codeA))
  numv   <- function(v) suppressWarnings(as.numeric(v))
  
  vals <- numv(mat[prows, hdr$cols, drop = FALSE])
  vals <- matrix(vals, nrow = length(prows),
                 dimnames = list(codeA[prows], hdr$codes))
  long <- as.data.table(as.data.frame(as.table(vals)))
  setnames(long, c("sut_item_code", "activity_code", "sup_val"))
  long[, `:=`(sut_item_code = as.character(sut_item_code),
              activity_code = as.character(activity_code),
              sup_val       = as.numeric(sup_val),
              year          = as.integer(year))]
  long[is.finite(sup_val) & sup_val != 0]
}

#' Per-activity gross output = Make-matrix column sum over leaf products.
compute_activity_output <- function(make_long) {
  ind_out <- make_long[, .(ind_output = sum(sup_val, na.rm = TRUE)),
                       by = .(activity_code, year)]
  chk <- merge(
    make_long[, .(maxprod = max(sup_val, na.rm = TRUE)), by = .(activity_code, year)],
    ind_out, by = c("activity_code", "year"))
  n_diag <- chk[ind_output > 0 & maxprod / ind_output > 0.95, .N]
  message(sprintf(
    "  Make matrix near-diagonal: %d / %d activity\u00d7year columns are >95%% one product (expected for IBGE).",
    n_diag, nrow(chk)))
  ind_out
}

#' Make-share allocation (Model B): VA_product = Σ_activities VA_activity ×
#' Make[p,i] / ActivityOutput[i].
allocate_va_brazil <- function(va_ind, sup_prep, ind_out) {
  shares <- merge(sup_prep, ind_out, by = c("activity_code", "year"))
  shares[, make_share := fifelse(ind_output > 0, sup_val / ind_output, 0)]
  alloc <- merge(va_ind, shares, by = c("activity_code", "year"), allow.cartesian = TRUE)
  alloc[, va_share := va_value * make_share]
  va_prod <- alloc[, .(va_value = sum(va_share, na.rm = TRUE)),
                   by = .(sut_item_code, va_account, year)]
  va_prod[va_value != 0 | va_account %in% VA_ACCOUNTS]
}

# ── Brazil builder: concordances + cfg in, (va_prod_all, use_years) out ──────
build_product_va_brazil <- function(cfg, item_conc_a, item_conc_c, conc_all) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required for the Brazil front half but is not installed.")
  
  # F1. Brazil SUT .xls pairs are staged by R/00_9_prep_value_added.R; this
  #     builder is a pure reader (F2 errors clearly if a year pair is missing).
  
  # F2. Locate per-year workbook pairs; keep only years whose BOTH files exist.
  year_files <- lapply(cfg$years, function(y) list(
    year   = y,
    supply = file.path(BRA_SUT_DIR, sprintf(BRA_SUPPLY_FILE_FMT, y)),
    use    = file.path(BRA_SUT_DIR, sprintf(BRA_USE_FILE_FMT,    y))))
  present   <- Filter(function(f) file.exists(f$supply) && file.exists(f$use), year_files)
  use_years <- sort(vapply(present, function(f) f$year, integer(1)))
  missing_y <- setdiff(cfg$years, use_years)
  if (length(use_years) == 0L)
    stop("No year has BOTH '", BRA_SUPPLY_FILE_FMT, "' and '", BRA_USE_FILE_FMT,
         "' present in '", BRA_SUT_DIR, "'.  Check BRA_SUT_DIR / the filename templates.")
  if (length(missing_y) > 0L)
    message(sprintf("  Note: %d requested year(s) missing a workbook pair (skipped): %s",
                    length(missing_y), paste(missing_y, collapse = ", ")))
  message(sprintf("  Reading %d year(s): %s.",
                  length(use_years), paste(range(use_years), collapse = "\u2013")))
  
  # F3. Read VA-by-activity and the Make matrix for each available year.
  message("Reading IBGE SUT workbooks (VA by activity + Make matrix) ...")
  va_ind_list <- vector("list", length(present))
  make_list   <- vector("list", length(present))
  for (k in seq_along(present)) {
    f <- present[[k]]
    message(sprintf("  [%d] %s  +  %s", f$year, basename(f$use), basename(f$supply)))
    va_long  <- read_va_by_activity(f$use, f$year)
    make_lng <- read_make_matrix(f$supply, f$year)
    
    ocheck <- attr(va_long, "output")
    if (!is.null(ocheck)) {
      cs <- make_lng[, .(make_colsum = sum(sup_val, na.rm = TRUE)),
                     by = .(activity_code, year)]
      cmp <- merge(cs, ocheck, by = c("activity_code", "year"))
      bad <- cmp[is.finite(va_output) & va_output > 0 &
                   abs(make_colsum - va_output) / va_output > 0.01]
      if (nrow(bad) > 0L)
        message(sprintf(
          "    Note: %d activity column(s) differ >1%% between Make col-sum and ", nrow(bad)),
          "'Valor da producao' — usually fine (rounding), inspect if large.")
    }
    va_ind_list[[k]] <- va_long[, .(activity_code, va_account, year, va_value)]
    make_list[[k]]   <- make_lng
  }
  va_ind    <- rbindlist(va_ind_list, use.names = TRUE)
  make_long <- rbindlist(make_list,   use.names = TRUE)
  message(sprintf("  %d activity x account x year VA rows; accounts: %s.",
                  nrow(va_ind), paste(sort(unique(va_ind$va_account)), collapse = ", ")))
  
  # F4. Activity → product VA via Make shares.
  sup_prep <- make_long[sut_item_code %in% conc_all$sut_item_code,
                        .(activity_code, sut_item_code, year, sup_val)]
  ind_out  <- compute_activity_output(make_long)
  
  message("Allocating activity VA to products via Make shares (Model B) ...")
  va_prod_all <- allocate_va_brazil(va_ind, sup_prep, ind_out)
  message(sprintf("  %d product x account x year VA rows (%d BRA products covered).",
                  nrow(va_prod_all), uniqueN(va_prod_all$sut_item_code)))
  
  # Model-B invariant check (allocate to ALL products): Σ_p VA_p == Σ_i VA_i.
  sup_full    <- make_long[, .(activity_code, sut_item_code, year, sup_val)]
  va_prod_chk <- allocate_va_brazil(va_ind, sup_full, ind_out)
  tot_act  <- va_ind[,      sum(va_value, na.rm = TRUE)]
  tot_full <- va_prod_chk[, sum(va_value, na.rm = TRUE)]
  message(sprintf(
    "  Model-B invariant (allocate to ALL products): activity \u03a3 = %.6e ; product \u03a3 = %.6e  (%.4f%% diff \u2014 expect ~0).",
    tot_act, tot_full,
    if (tot_act != 0) 100 * (tot_full - tot_act) / abs(tot_act) else NA_real_))
  tot_conc <- va_prod_all[, sum(va_value, na.rm = TRUE)]
  message(sprintf(
    "  Agri-food coverage: concordance products carry %.6e of %.6e total VA (%.1f%% of the economy).",
    tot_conc, tot_act, if (tot_act != 0) 100 * tot_conc / tot_act else NA_real_))
  rm(sup_full, va_prod_chk)
  
  # Per-activity leakage s_i = in-scope output / total output (header note).
  write_make_share_leakage(
    make_share_leakage(va_ind, sup_prep, ind_out, "activity_code"),
    cfg$source_tag, "activity")
  
  unmatched <- setdiff(conc_all$sut_item_code, unique(va_prod_all$sut_item_code))
  if (length(unmatched) > 0L)
    message(sprintf("  %d concordance BRA product(s) received NO VA (no Make/VA match): %s",
                    length(unmatched), paste(unmatched, collapse = ", ")))
  
  list(va_prod_all = va_prod_all, use_years = use_years)
}


# ============================================================================
# COUNTRY REGISTRY + DRIVER
# ============================================================================

COUNTRY_CONFIG <- list(
  CAN = list(
    iso3        = "CAN",
    years       = VA_KEEP_YEARS,   # follows FABIO; builder clamps to CANSIM-available years
    unit_suffix = " [1000 CAD]",
    src_abbr    = "can",
    source_tag  = "CanadaSUT",
    source_name = "Canada-SUT",
    conc_path   = CAN_CONC_PATH,
    conc_code   = "CAN_SUT_code",
    conc_item   = "CAN_SUT_item",
    builder     = build_product_va_canada
  ),
  BRA = list(
    iso3        = "BRA",
    # Lower bound follows FABIO (VA_KEEP_YEARS); upper bound is the last year
    # Brazil publishes a national SUT (VA_BRA_SUT_MAX_YEAR, an upstream data
    # fact, not derivable from FABIO). The builder additionally self-trims to
    # years whose workbook pair is present on disk.
    years       = min(VA_KEEP_YEARS):VA_BRA_SUT_MAX_YEAR,
    unit_suffix = " [10^6 BRL]",
    src_abbr    = "bra",
    source_tag  = "BrazilSUT",
    source_name = "Brazil-SUT",
    conc_path   = BRA_CONC_PATH,
    conc_code   = "BRA_SUT_code",
    conc_item   = "BRA_SUT_item",
    builder     = build_product_va_brazil
  )
)

run_country <- function(iso3) {
  cfg <- COUNTRY_CONFIG[[iso3]]
  if (is.null(cfg)) stop("Unknown country '", iso3, "'.")
  
  message(sprintf("\n%s", RULE2))
  message(sprintf("  %s  (%s)  \u2014  years %s", cfg$source_name, cfg$iso3,
                  paste(range(cfg$years), collapse = "\u2013")))
  message(RULE2)
  
  # F1. Concordances per ISIC level.
  message(sprintf("Loading %s \u2194 FABIO item concordance ...", cfg$source_name))
  item_conc_a <- load_item_conc(cfg$conc_path, "A", cfg$conc_code, cfg$conc_item, out_code = "sut_item_code", out_item = "sut_item")
  item_conc_c <- load_item_conc(cfg$conc_path, "C", cfg$conc_code, cfg$conc_item, out_code = "sut_item_code", out_item = "sut_item")
  message(sprintf("  ISIC-A: %d mappings (%d products \u2192 %d FABIO items).",
                  nrow(item_conc_a), uniqueN(item_conc_a$sut_item_code),
                  uniqueN(item_conc_a$fabio_item_code)))
  message(sprintf("  ISIC-C: %d mappings (%d products \u2192 %d FABIO items).",
                  nrow(item_conc_c), uniqueN(item_conc_c$sut_item_code),
                  uniqueN(item_conc_c$fabio_item_code)))
  
  # Double-mapped FABIO items = present at BOTH ISIC levels → ISIC-C step-8
  # zeroing carve-out (their ISIC-C TPO inherits the ISIC-A primary quantity).
  double_mapped_items <- sort(intersect(item_conc_a$fabio_item_code,
                                        item_conc_c$fabio_item_code))
  message(sprintf("  Double-mapped FABIO items (in BOTH ISIC levels): %d",
                  length(double_mapped_items)))
  
  conc_all <- unique(rbindlist(list(item_conc_a, item_conc_c))[, .(sut_item_code, sut_item)])
  
  # FRONT HALF (country-specific) → canonical product VA + the years we have.
  fh          <- cfg$builder(cfg, item_conc_a, item_conc_c, conc_all)
  va_prod_all <- fh$va_prod_all
  use_years   <- fh$use_years
  
  # FABIOv2 total values per ISIC level, restricted to this country + years.
  message("Loading FABIOv2 total values (ISIC-A) ...")
  fv_pack_a <- prepare_fv(FABIO_TV_PATH_A)
  fv_a <- fv_pack_a$fv; value_col_a <- fv_pack_a$value_col; output_col_a <- fv_pack_a$output_col
  message("Loading FABIOv2 total values (ISIC-C) ...")
  fv_pack_c <- prepare_fv(FABIO_TV_PATH_C)
  fv_c <- fv_pack_c$fv; value_col_c <- fv_pack_c$value_col; output_col_c <- fv_pack_c$output_col
  
  n_a_pre <- nrow(fv_a); n_c_pre <- nrow(fv_c)
  fv_a <- fv_a[iso3c == cfg$iso3 & year %in% use_years]
  fv_c <- fv_c[iso3c == cfg$iso3 & year %in% use_years]
  message(sprintf(
    "Restricting FABIO to iso3c=='%s', years [%s]:  ISIC-A %d/%d rows, ISIC-C %d/%d rows.",
    cfg$iso3, paste(range(use_years), collapse = "\u2013"),
    nrow(fv_a), n_a_pre, nrow(fv_c), n_c_pre))
  if (nrow(fv_a) == 0L || nrow(fv_c) == 0L)
    stop("FABIO grid is empty after the ", cfg$iso3, "/year restriction \u2014 check that ",
         "iso3c == '", cfg$iso3, "' exists in the total_values RDS.")
  
  # BACK HALF — once per ISIC level.
  result_a <- process_isic_level(
    isic_level = "A", item_conc = item_conc_a,
    fv = fv_a, value_col = value_col_a, output_col = output_col_a,
    va_prod_all = va_prod_all,
    source_tag = cfg$source_tag, source_name = cfg$source_name,
    unit_suffix = cfg$unit_suffix, src_abbr = cfg$src_abbr
    # preserve_items defaults to integer(0): at ISIC-A every FABIO row's TPO IS
    # the right ISIC-A quantity, so NA/0 TPO genuinely means no production.
  )
  result_c <- process_isic_level(
    isic_level = "C", item_conc = item_conc_c,
    fv = fv_c, value_col = value_col_c, output_col = output_col_c,
    va_prod_all = va_prod_all,
    source_tag = cfg$source_tag, source_name = cfg$source_name,
    unit_suffix = cfg$unit_suffix, src_abbr = cfg$src_abbr,
    preserve_items = double_mapped_items   # ISIC-C double-mapped carve-out (see step 8)
  )
  
  invisible(list(A = result_a, C = result_c))
}


# ============================================================================
# RUN
# ============================================================================

COUNTRIES_TO_RUN <- c("CAN", "BRA")   # edit to run a single country

results <- list()
for (.iso in COUNTRIES_TO_RUN) results[[.iso]] <- run_country(.iso)

message("\nDone.")