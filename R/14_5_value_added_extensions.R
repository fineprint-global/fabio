# ==============================================================================
# 14_5_value_added_extensions.R
#
# "Last mile" of the value-added pipeline -> FABIO extensions.
#
# Reads the COMBINED value-added tables produced by
# 14_4_value_added_FABIO_v2_synthesis.R and emits TWELVE CBS-level FABIO
# extensions: the three value-added strands x two ISIC levels, built ONCE PER
# BASE (GLORIA and EXIOBASE), with the base baked into the Stressor name:
#
#   GLORIA base:
#     VA_wages_isic_a_gloria    VA_capital_isic_a_gloria    VA_tls_isic_a_gloria
#     VA_wages_isic_c_gloria    VA_capital_isic_c_gloria    VA_tls_isic_c_gloria
#   EXIOBASE base:
#     VA_wages_isic_a_exiobase  VA_capital_isic_a_exiobase  VA_tls_isic_a_exiobase
#     VA_wages_isic_c_exiobase  VA_capital_isic_c_exiobase  VA_tls_isic_c_exiobase
#
# 14_4 already produces a parallel COMBINED product per base
# (FABIOv2_COMBINED_GLORIA_... and FABIOv2_COMBINED_EXIOBASE_...); this script
# now consumes BOTH, so the two bases coexist in E instead of overwriting each
# other.  NOTE: the _gloria and _exiobase rows are ALTERNATIVE estimates of the
# SAME value-added quantity (different upstream MRIO base), NOT additive — a
# footprint must pick one base and must never sum a _gloria row with its
# _exiobase counterpart.
#
# Each is written to data/extensions/cbs/<Stressor>.rds in FABIO's standard
# extension shape (a year-keyed list of 1 x (regions*commodities) row-matrices,
# columns named <iso3c>_<comm_code>), via FABIO's own format_extension() so the
# column layout is byte-identical to every other extension and to X.
#
# CBS-ONLY by design: no data/extensions/sua/ counterparts are written.  The
# parity check in 15_extensions_main.R is patched to tolerate exactly these
# twelve names (see `cbs_only` there); the SUA / v2_525 E is compiled without
# them.
#
# After running this, run 15_extensions_main.R to compile E.rds and ex_labels.csv
# for the v2 tree.  inst/E_labels_initial.csv must already contain the twelve
# matching rows (see inst_E_labels_va_rows.csv).
# ==============================================================================

library(data.table)
source("R/00_system_variables.R")     # years, output_dir, na_sum
source("R/01_tidy_functions.R")       # format_extension
source("R/00_value_added_config.R")   # VA_VALUE_ADDED_OUTPUT_DIR, VA_* paths

# ── Configuration ────────────────────────────────────────────────────────────

# Bases to build extensions for.  Key (lower-case) -> file_tag baked into the
# COMBINED file names by 14_4 ("GLORIA_" / "EXIOBASE_").  The key becomes the
# Stressor-name suffix (VA_<strand>_isic_<level>_<key>), so the two bases write
# to distinct files and coexist in E instead of clobbering each other.
#   12 extensions = 2 bases x 2 ISIC levels x 3 strands.
ALL_BASES <- list(
  gloria   = "GLORIA_",
  exiobase = "EXIOBASE_"
)

# Optional restriction: VA_BASE_KEYS may name a comma-separated subset (e.g.
# "gloria" to rebuild only the GLORIA side).  Default = both bases.
.requested <- trimws(strsplit(Sys.getenv("VA_BASE_KEYS", unset = ""), ",")[[1]])
.requested <- tolower(.requested[nzchar(.requested)])
if (length(.requested) == 0L) {
  BASES <- ALL_BASES
} else {
  bad <- setdiff(.requested, names(ALL_BASES))
  if (length(bad) > 0L)
    stop("VA_BASE_KEYS names unknown base(s): ", paste(bad, collapse = ", "),
         ". Known: ", paste(names(ALL_BASES), collapse = ", "), ".")
  BASES <- ALL_BASES[.requested]
}

# Where the COMBINED outputs live.  Defaults to the in-repo stage-2 output dir
# (output/value_added) defined in R/00_value_added_config.R, which is exactly
# where 14_4_value_added_FABIO_v2_synthesis.R writes them.  Override with the
# VA_COMBINED_DIR env var if the files live elsewhere.
VA_COMBINED_DIR <- Sys.getenv(
  "VA_COMBINED_DIR",
  unset = VA_VALUE_ADDED_OUTPUT_DIR
)

ISIC_LEVELS <- c("A", "C")

# strand key -> source column in the COMBINED table.  Keys feed the Stressor
# name: VA_<key>_isic_<level>.  (Total `value_added [USD]` is deliberately NOT
# exported — the three strands sum to it, and footprints can sum the rows.)
# Same map the writers/synthesis use; sourced from R/00_value_added_helpers.R.
STRAND_COL <- STRAND_TO_COL

stressor_name <- function(key, level, base_key)
  sprintf("VA_%s_isic_%s_%s", key, tolower(level), base_key)

CBS_OUT_DIR <- "data/extensions/cbs"

# ── FABIO label tables that drive format_extension ───────────────────────────
# format_extension()'s reg/itms default to the globals `regions`/`items`; set
# them to the CBS universe used to build X so columns align 1:1 with X.
regions <- fread("inst/regions_full.csv")[current == TRUE]
items   <- fread("inst/items_full_123.csv")

stopifnot(all(c("code", "iso3c") %in% names(regions)),
          "comm_code" %in% names(items))

# ── Read one COMBINED ISIC level (rds preferred, csv fallback) ───────────────

read_combined <- function(level, base_tag) {
  base <- va_combined_output_basename(base_tag, level)   # shared with 14_4 writer
  rds  <- file.path(VA_COMBINED_DIR, paste0(base, ".rds"))
  csv  <- file.path(VA_COMBINED_DIR, paste0(base, ".csv"))
  if (file.exists(rds)) {
    dt <- as.data.table(readRDS(rds))
  } else if (file.exists(csv)) {
    dt <- fread(csv)
  } else {
    stop("COMBINED ", base_tag, "ISIC-", level, " not found:\n  ", rds,
         "\n  ", csv, "\nRun 14_4_value_added_FABIO_v2_synthesis.R, or set ",
         "VA_COMBINED_DIR.")
  }
  need <- c("fabio_area_code", "comm_code", "year", unname(STRAND_COL))
  miss <- setdiff(need, names(dt))
  if (length(miss) > 0L)
    stop("COMBINED ISIC-", level, " is missing column(s): ",
         paste(miss, collapse = ", "), ".")
  dt
}

# ── Reshape one (level, strand) into the FABIO extension shape ───────────────
# Aggregates to unique (area_code, comm_code, year) FIRST: format_extension uses
# match() (first-row-wins, NOT additive), so any commodity that received more
# than one COMBINED row at a given level would otherwise be silently truncated.

build_strand_extension <- function(dt, level, key, base_key) {
  col <- STRAND_COL[[key]]
  d <- data.table(
    area_code = as.integer(dt$fabio_area_code),
    comm_code = as.character(dt$comm_code),
    year      = as.integer(dt$year),
    value     = as.numeric(dt[[col]])
  )
  # Uniqueness guard (no-op when already unique, as ISIC-A is).  Negatives are
  # legitimate (tls = taxes - subsidies; capital includes net mixed income) and
  # are preserved.
  d <- d[, .(value = sum(value, na.rm = TRUE)),
         by = .(area_code, comm_code, year)]
  
  ext <- format_extension(d, yrs = years, reg = regions, itms = items,
                          value_col = "value")
  
  # Sanity: column count must equal regions*commodities (== ncol(X)).
  ncol_expected <- nrow(regions) * nrow(items)
  if (ncol(ext[[as.character(years[1])]]) != ncol_expected)
    stop("Column count for ", stressor_name(key, level, base_key), " is ",
         ncol(ext[[as.character(years[1])]]), ", expected ", ncol_expected, ".")
  ext
}

# ── Build & write all (up to) twelve ─────────────────────────────────────────

va_ensure_dir(CBS_OUT_DIR)

message(sprintf("Building value-added extensions for base(s): %s (dir: %s)",
                paste(names(BASES), collapse = ", "), VA_COMBINED_DIR))

n_written <- 0L
for (base_key in names(BASES)) {
  base_tag <- BASES[[base_key]]
  message(sprintf("\n-- base %s (tag %s) --", base_key, base_tag))
  for (level in ISIC_LEVELS) {
    dt <- read_combined(level, base_tag)
    for (key in names(STRAND_COL)) {
      nm  <- stressor_name(key, level, base_key)
      ext <- build_strand_extension(dt, level, key, base_key)
      saveRDS(ext, file.path(CBS_OUT_DIR, paste0(nm, ".rds")))
      n_written <- n_written + 1L
      
      tot <- sum(vapply(ext, sum, numeric(1)))    # USD across all years/cells
      message(sprintf("  %-28s  %d year-slices, Sum = %.3e USD -> %s.rds",
                      nm, length(ext), tot, nm))
    }
  }
}

message(sprintf(
  "\nDone. %d CBS value-added extension(s) written to %s.\n%s\nThen run R/15_extensions_main.R.",
  n_written, CBS_OUT_DIR,
  "Ensure inst/E_labels_initial.csv carries the matching rows (12 for both bases)."))