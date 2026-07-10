# ==============================================================================
# 14_5_value_added_extensions.R
#
# "Last mile" of the value-added pipeline -> a standalone FABIO V block.
#
# Reads the COMBINED value-added tables produced by
# 14_4_value_added_FABIO_v2_synthesis.R and builds TWELVE CBS-level value-added
# strands: the three value-added strands x two ISIC levels, once per base
# (GLORIA and EXIOBASE), with the base baked into the Stressor name:
#
#   GLORIA base:
#     VA_wages_isic_a_gloria    VA_capital_isic_a_gloria    VA_tls_isic_a_gloria
#     VA_wages_isic_c_gloria    VA_capital_isic_c_gloria    VA_tls_isic_c_gloria
#   EXIOBASE base:
#     VA_wages_isic_a_exiobase  VA_capital_isic_a_exiobase  VA_tls_isic_a_exiobase
#     VA_wages_isic_c_exiobase  VA_capital_isic_c_exiobase  VA_tls_isic_c_exiobase
#
# The _gloria and _exiobase rows are ALTERNATIVE estimates of the same
# value-added quantity (different upstream MRIO base), NOT additive — a footprint
# must pick one base and must never sum a _gloria row with its _exiobase
# counterpart.
#
# The twelve strands are compiled into a single value-added block and written to:
#
#     <output_dir>/V.rds        year-keyed list of (12 x regions*commodities)
#                               matrices, row order == v_labels$Stressor, columns
#                               named <iso3c>_<comm_code> (same layout as E and X,
#                               via FABIO's own format_extension()).
#     <output_dir>/v_labels.csv the twelve matching label rows, in V's row order.
#
# Value added ships as its own V block, independent of the E extensions compiled
# by 15_extensions_main.R. Label source: inst/v_labels_initial.csv must contain
# exactly the twelve rows above.
# ==============================================================================

library(data.table)
source("R/00_system_variables.R")     # years, output_dir, na_sum
source("R/01_tidy_functions.R")       # format_extension, compile_extension
source("R/00_value_added_config.R")   # VA_VALUE_ADDED_OUTPUT_DIR, VA_* paths

# ── Configuration ────────────────────────────────────────────────────────────

# Bases to build strands for.  Key (lower-case) -> file_tag baked into the
# COMBINED file names by 14_4 ("GLORIA_" / "EXIOBASE_").  The key becomes the
# Stressor-name suffix (VA_<strand>_isic_<level>_<key>), so the two bases coexist
# as distinct rows of V.
#   12 strands = 2 bases x 2 ISIC levels x 3 strands.
ALL_BASES <- list(
  gloria   = "GLORIA_",
  exiobase = "EXIOBASE_"
)

# Optional restriction: VA_BASE_KEYS may name a comma-separated subset (e.g.
# "gloria" to rebuild only the GLORIA side).  Default = both bases.  A partial
# subset yields a partial V; v_labels is filtered to match, so V and v_labels
# stay in sync.
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

# Compiled block + labels (v2 tree), and the label source.
V_OUT        <- paste0(output_dir, "V.rds")
V_LABELS_OUT <- paste0(output_dir, "v_labels.csv")
V_LABELS_SRC <- "inst/v_labels_initial.csv"

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

# ── Build all (up to) twelve strands, collecting into one named list ──────────

message(sprintf("Building value-added block for base(s): %s (dir: %s)",
                paste(names(BASES), collapse = ", "), VA_COMBINED_DIR))

va_list <- list()    # nm -> single-strand extension object, in build order
for (base_key in names(BASES)) {
  base_tag <- BASES[[base_key]]
  message(sprintf("\n-- base %s (tag %s) --", base_key, base_tag))
  for (level in ISIC_LEVELS) {
    dt <- read_combined(level, base_tag)
    for (key in names(STRAND_COL)) {
      nm  <- stressor_name(key, level, base_key)
      ext <- build_strand_extension(dt, level, key, base_key)
      va_list[[nm]] <- ext
      
      tot <- sum(vapply(ext, sum, numeric(1)))    # USD across all years/cells
      message(sprintf("  %-28s  %d year-slices, Sum = %.3e USD",
                      nm, length(ext), tot))
    }
  }
}

# ── Compile the strands into a single V block ────────────────────────────────
# compile_extension stacks in list order and takes row names from `files`, so we
# feed the stressor names as pseudo-filenames -> rownames == names(va_list).
V <- compile_extension(va_list, files = paste0(names(va_list), ".rds"))
va_names <- names(va_list)
stopifnot(all(rownames(V[[as.character(years[1])]]) == va_names))

# ── Build v_labels, filtered & ordered to V's rows ───────────────────────────
if (!file.exists(V_LABELS_SRC))
  stop("Label source not found: ", V_LABELS_SRC, ".")

v_labels_all <- fread(V_LABELS_SRC)
if (!"Stressor" %in% names(v_labels_all))
  stop(V_LABELS_SRC, " has no 'Stressor' column.")

missing_v <- setdiff(va_names, v_labels_all$Stressor)
if (length(missing_v) > 0L)
  stop("v_labels source is missing row(s) for: ", paste(missing_v, collapse = ", "),
       ".\nAdd them to ", V_LABELS_SRC, ".")

v_labels <- v_labels_all[Stressor %in% va_names][order(match(Stressor, va_names)), ]
if (!all(v_labels$Stressor == va_names))
  stop("v_labels ordering does not match V row order.")

# ── Write V.rds + v_labels.csv (v2 tree) ─────────────────────────────────────
saveRDS(V, V_OUT)
fwrite(v_labels, V_LABELS_OUT)

message(sprintf(
  "\nDone. V block: %d strand(s) x %d year-slices -> %s\n  labels -> %s",
  length(va_names), length(V), V_OUT, V_LABELS_OUT))