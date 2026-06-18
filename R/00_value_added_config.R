# ==============================================================================
# Sourcing this file also sources R/00_value_added_helpers.R, so scripts get the
# helpers from this one source() call.
# ==============================================================================


# ==============================================================================
# 1. ROOTS  (environment-overridable; defaults reproduce the canonical layout)
# ==============================================================================
#
# Resolve an environment variable to a path, falling back to a default and
# expanding a leading "~". Empty string (the Sys.getenv default for an unset
# variable) is treated as "unset" so FABIO_ROOT="" behaves like absent.
.va_env_path <- function(var, default) {
  val <- Sys.getenv(var, unset = "")
  if (!nzchar(val)) val <- default
  path.expand(val)
}

# The FABIO repo root: holds R/, inst/, data/, input/, output/. This single
# root replaces the former VA_ROOT (project tree), FABIO_CODE_ROOT (utilities)
# and FABIO_DATA_ROOT (tidy data / inst tables), which all now point at the same
# folded repo. Override with FABIO_ROOT; legacy FABIO_DATA_ROOT is honoured as a
# fallback so existing environment setups keep working.
FABIO_ROOT <- .va_env_path("FABIO_ROOT",
                           Sys.getenv("FABIO_DATA_ROOT", unset = "~/fabio"))

# Shared NFS scratch holding the raw upstream databases (GLORIA, EXIOBASE, and
# the compiled FABIO v2 X / labels). Override with FINEPRINT_ROOT.
FINEPRINT_ROOT <- .va_env_path("FINEPRINT_ROOT", "/mnt/nfs_fineprint/tmp")


# ==============================================================================
# 2. PROJECT DIRECTORIES  (derived from FABIO_ROOT)
# ==============================================================================

# Shared concordances, relocated into the FABIO inst/ tree.
VA_CONCORDANCE_DIR <- file.path(FABIO_ROOT, "inst", "value_added")

# Stage-1 raw inputs. The only stage-1 raw inputs are FAOSTAT bulk files, which
# FABIO already downloads in R/00_1_prep_fao.R into input/fao/ (with rm = FALSE,
# so the extracted CSVs persist). We read from there to avoid a second copy.
# FABIO downloads the *Normalized* (long) archives, and the value-added readers
# now consume that layout directly: 13_1_FAOstat reshapes the price/exchange
# files to wide on read (read_fao_normalized_wide()), and 13_3 reads the
# production file long. The exchange-rate Normalized archive was added to the
# FABIO prep step's download list so all three live in input/fao/ with no
# separate wide download. (Previously the readers wanted the wide "_E_All_Data"
# layout and FABIO fetched a redundant set of wide CSVs at the end of the prep
# step; that block has been removed.)
VA_TOTAL_VALUE_INPUT_DIR  <- file.path(FABIO_ROOT, "input", "fao")

# Stage-2 inputs (national SUT sheets, FSDN, etc.), relocated here.
VA_VALUE_ADDED_INPUT_DIR  <- file.path(FABIO_ROOT, "input", "value_added")

# Stage-1 outputs (producer prices / total values) and stage-2 outputs (value
# added + the COMBINED_* files consumed by R/14_5_value_added_extensions.R).
VA_TOTAL_VALUE_OUTPUT_DIR <- file.path(FABIO_ROOT, "data", "total_value")
VA_VALUE_ADDED_OUTPUT_DIR <- file.path(FABIO_ROOT, "data", "value_added")

# Per-stage diagnostics live under each stage's output dir.
VA_TOTAL_VALUE_DIAG_DIR   <- file.path(VA_TOTAL_VALUE_OUTPUT_DIR, "diagnostics")
VA_VALUE_ADDED_DIAG_DIR   <- file.path(VA_VALUE_ADDED_OUTPUT_DIR, "diagnostics")

# Where the price-stage scripts (13_1_FAOstat, 13_2_clean) write their handoff
# files. Defaults to the stage-1 output dir so 01 -> 03 and 02 -> 03 join up
# deterministically. Override with VA_PRICE_OUTPUT_DIR if desired.
VA_PRICE_OUTPUT_DIR <- .va_env_path("VA_PRICE_OUTPUT_DIR", VA_TOTAL_VALUE_OUTPUT_DIR)
VA_PRICE_DIAG_DIR   <- file.path(VA_PRICE_OUTPUT_DIR, "diagnostics")


# ==============================================================================
# 3. RAW UPSTREAM DATABASE DIRECTORIES  (derived from FINEPRINT_ROOT)
# ==============================================================================

VA_FABIO_V2_DIR  <- file.path(FINEPRINT_ROOT, "fabio", "v2")
VA_GLORIA_DIR    <- file.path(FINEPRINT_ROOT, "gloria", "v060-compiled")
VA_EXIOBASE_DIR  <- file.path(FINEPRINT_ROOT, "exiobase", "v3.10")

# Named entry points within those trees (the rest are built per-year inside the
# adapters via file.path(VA_*_DIR, ...)).
VA_FABIO_V2_IO_LABELS_CSV <- file.path(VA_FABIO_V2_DIR, "io_labels.csv")
VA_FABIO_V2_X_RDS         <- file.path(VA_FABIO_V2_DIR, "X.rds")
VA_GLORIA_README_XLSX     <- file.path(VA_GLORIA_DIR, "labels", "GLORIA_ReadMe_060.xlsx")
VA_GLORIA_V_DIR           <- file.path(VA_GLORIA_DIR, "IOTs_producer_prices", "V")
VA_GLORIA_X_DIR           <- file.path(VA_GLORIA_DIR, "IOTs_producer_prices", "X")


# ==============================================================================
# 4. SHARED HANDOFF FILES  (named because >1 script touches them)
# ==============================================================================
#
# Only files that cross a script boundary are named here. Files that a single
# script writes and nobody else reads (most diagnostic CSVs) keep their basename
# inside that script and are rooted at the directory constants above.
#
# NOTE: the staged national/regional SUT inputs below ARE such a boundary —
# R/00_9_prep_value_added.R is the WRITER and 14_3 (Brazil, Canada) / 14_4
# (OECD, Eurostat) are the READERS. Their basenames and filename templates used
# to be hand-copied in both the writer and the reader(s); they now live here
# once so the producer/consumer contract cannot silently drift.

# -- Stage-1 inputs (Normalized FAOSTAT bulk CSVs, in input/fao/) --------------
# These are the long-format "(Normalized)" archives FABIO downloads in
# R/00_1_prep_fao.R (with rm = FALSE, so the extracted CSVs persist).
# 13_1_FAOstat reshapes the price/exchange files to wide on read via
# read_fao_normalized_wide(); 13_3 reads the production file long directly.
VA_PRICES_RAW_CSV              <- file.path(VA_TOTAL_VALUE_INPUT_DIR, "Prices_E_All_Data_(Normalized).csv")
VA_EXCHANGE_RATE_CSV           <- file.path(VA_TOTAL_VALUE_INPUT_DIR, "Exchange_rate_E_All_Data_(Normalized).csv")
VA_PRODUCTION_CROPS_LIVESTOCK_CSV <- file.path(VA_TOTAL_VALUE_INPUT_DIR,
                                               "Production_Crops_Livestock_E_All_Data_(Normalized).csv")

# -- Stage-1 handoff outputs (price pipeline) ---------------------------------
VA_PRICES_USD_CSV                          <- file.path(VA_PRICE_OUTPUT_DIR, "Prices_E_All_Data_with_USD.csv")
VA_BILATERAL_TRADE_PRICES_RDS              <- file.path(VA_PRICE_OUTPUT_DIR, "bilateral_trade_prices.rds")
VA_BILATERAL_TRADE_PRICES_CBS_OVERRIDE_RDS <- file.path(VA_PRICE_OUTPUT_DIR, "bilateral_trade_prices_cbs_override.rds")

# -- FABIO v2 producer total values (13_3_FABIO_v2_price_extension OUT; MRIOTs / FSDN /
#    national_SUTs IN). The single most important handoff — these MUST resolve
#    to the same file for every script, which is the whole point of this file.
VA_FABIO_TV_ISIC_A_RDS <- file.path(VA_TOTAL_VALUE_OUTPUT_DIR, "FABIOv2_producer_total_values_isic_a.rds")
VA_FABIO_TV_ISIC_A_CSV <- file.path(VA_TOTAL_VALUE_OUTPUT_DIR, "FABIOv2_producer_total_values_isic_a.csv")
VA_FABIO_TV_ISIC_C_RDS <- file.path(VA_TOTAL_VALUE_OUTPUT_DIR, "FABIOv2_producer_total_values_isic_c.rds")
VA_FABIO_TV_ISIC_C_CSV <- file.path(VA_TOTAL_VALUE_OUTPUT_DIR, "FABIOv2_producer_total_values_isic_c.csv")

# -- Stage-2 STAGED national / regional SUT inputs --------------------------- 
#    WRITTEN by R/00_9_prep_value_added.R; READ by 14_3 (Brazil, Canada, REQUIRED)
#    and 14_4 (OECD, Eurostat, OPTIONAL overlays). Writer and reader(s) must agree
#    on these exact paths/templates, so they are named here once rather than
#    re-declared in each script.

# Brazil — IBGE nivel-68 TRU .xls pairs (one Supply + one Use workbook per year).
VA_BRA_SUT_DIR         <- file.path(VA_VALUE_ADDED_INPUT_DIR, "Brazil_SUTs")
VA_BRA_SUPPLY_FILE_FMT <- "68_tab1_%d.xls"   # Tabela 1 — Recursos (Supply / Make)
VA_BRA_USE_FILE_FMT    <- "68_tab2_%d.xls"   # Tabela 2 — Usos     (Use / VA)

# Canada — StatCan Detail SUT (cansim 36-10-0478-01) sliced to plain CSVs by 00_9,
# read with no cansim/network dependency by 14_3.
VA_CAN_SUT_DIR   <- file.path(VA_VALUE_ADDED_INPUT_DIR, "Canada_SUTs")
VA_CAN_USE_CSV   <- file.path(VA_CAN_SUT_DIR, "cansim_canada_use_va_basic.csv")
VA_CAN_NUM_CSV   <- file.path(VA_CAN_SUT_DIR, "cansim_canada_supply_make_numerators.csv")
VA_CAN_DEN_CSV   <- file.path(VA_CAN_SUT_DIR, "cansim_canada_supply_make_denominators.csv")
VA_CAN_AVAIL_CSV <- file.path(VA_CAN_SUT_DIR, "cansim_canada_avail_years.csv")

# OECD SUT (table T1600) and Eurostat NAMA (nama_10_a64) — optional 14_4 overlays.
# (These two basenames were previously duplicated AGAIN as OECD_SUT_STAGED_FILE /
#  EU_NAMA_STAGED_FILE in 00_value_added_helpers.R; those are now gone and the
#  loaders default to the constants below.)
VA_OECD_SUT_CSV      <- file.path(VA_VALUE_ADDED_INPUT_DIR, "oecd_sut_use_valueadded.csv")
VA_EUROSTAT_NAMA_CSV <- file.path(VA_VALUE_ADDED_INPUT_DIR, "eurostat_nama_10_a64.csv")

# -- Shared concordances (the two used by >1 script; script-private ones are
#    built as file.path(VA_CONCORDANCE_DIR, "...") in their own script). -------
VA_CONC_BTD_CBS_ISIC <- file.path(FABIO_ROOT, "inst", "conc_btd-cbs.csv")
VA_CONC_GLORIA_ITEMS <- file.path(VA_CONCORDANCE_DIR, "concordance_items_gloria_fabio.csv")
# Canada SUT item concordance: read by BOTH 00_9 (to scope the cansim staging
# slices) and 14_3 (the reader), so it is named here. Brazil's SUT concordance
# is read only by 14_3 and stays local in that script per the rule above.
VA_CONC_CANADA_SUT   <- file.path(VA_CONCORDANCE_DIR, "concordance_items_canada_sut_fabio.csv")

# -- FABIO core data + utilities (now in-repo, under FABIO_ROOT) --------------
VA_FABIO_REGIONS_CSV      <- file.path(FABIO_ROOT, "inst", "regions_full.csv")
VA_FABIO_ITEMS_CSV        <- file.path(FABIO_ROOT, "inst", "items_full_123.csv")
VA_FABIO_BTD_SUA_TIDY_RDS <- file.path(FABIO_ROOT, "data", "tidy", "btd_sua_tidy.rds")
VA_FABIO_BACI_TIDY_RDS    <- file.path(FABIO_ROOT, "data", "tidy", "baci_tidy.rds")
VA_FABIO_SUA_TIDY_RDS     <- file.path(FABIO_ROOT, "data", "tidy", "sua_tidy.rds")
FABIO_TIDY_FUNCTIONS_PATH <- file.path(FABIO_ROOT, "R", "01_tidy_functions.R")


# ==============================================================================
# 5. SHARED NUMERIC / IDENTIFIER CONSTANTS  (previously duplicated per script)
# ==============================================================================
#
# Only constants that were genuinely the SAME across scripts are centralised.
# WINSOR_MAD_K is deliberately NOT here: it is 2.5 in MRIOTs (and in the price
# scripts 13_1 / 13_2) but 3.5 in national_SUTs / FSDN by design, so its VALUE
# stays a local, per-script decision. Its NAME, however, is now uniform: every
# script declares `WINSOR_MAD_K <- <value>` (the old `WINSOR_Z` alias in 14_1 /
# 14_2 / 14_3 and the hidden `threshold = 2.5` default in 13_1 are gone), so the
# same identifier means the same thing everywhere even though the number differs.
# Per-script values, for reference:
#     13_1 FAOstat            WINSOR_MAD_K = 2.5
#     13_2 clean_bilateral    WINSOR_MAD_K = 2.5
#     14_1 MRIOTs             WINSOR_MAD_K = 2.5
#     14_2 FSDN               WINSOR_MAD_K = 3.5
#     14_3 national_SUTs      WINSOR_MAD_K = 3.5
#
# WINSOR_MIN_OBS, by contrast, IS centralised here, because unlike WINSOR_MAD_K
# its value is the SAME everywhere by design. It is the minimum number of pooled
# observations a group needs before a MAD winsorization band (or an IHS theta
# fit) is built; groups below it pass through uncapped.
WINSOR_MIN_OBS <- 8L   # min pooled obs per group to build a MAD band / IHS theta fit

# Likewise the Hampel PASS COUNT varies by design and stays local: 13_2 runs the
# two-pass `hampel_filter_iterate(n_passes = HAMPEL_PASSES = 2L)` because adjacent
# spikes are common in bilateral trade series and a second pass catches spikes
# masked by a neighbour in the first; 13_1 and 14_1 use the single-pass
# `hampel_filter()` / `hampel_by_series()`. Like WINSOR_MAD_K this is a documented
# per-caller choice, named locally rather than baked in as a bare literal.
#
# Note on the former Hampel naming clash, now ELIMINATED: 13_1_FAOstat used to
# call the threshold HAMPEL_K and the half-window HAMPEL_WINDOW, while MRIOTs
# called the threshold HAMPEL_Z and the half-window HAMPEL_K — so the same name
# HAMPEL_K meant opposite things in the two scripts. Every Hampel call site now
# uses the names below verbatim (HAMPEL_THRESHOLD / HAMPEL_HALF_WINDOW), which
# match 00_value_added_helpers.R::hampel_filter()'s own argument names. There is
# no HAMPEL_K / HAMPEL_Z alias left anywhere, so the same identifier means the
# same thing in every script and the values can never drift apart.
VA_HAMPEL_THRESHOLD   <- 3      # robust-z spike cutoff
VA_HAMPEL_HALF_WINDOW <- 3L     # rolling-median half-window (full window = 2*hw+1 = 7)

# -- Year coverage (derived from R/00_system_variables.R) ---------------------
#
# FABIO's year range is declared exactly once, as `years` in
# R/00_system_variables.R. The value-added pipeline tracks it automatically so
# that changing the FABIO year range needs no edits here or in the 13_/14_
# scripts. R/00_system_variables.R is NOT modified for this.
#
# We obtain `years` without re-running R/00_system_variables.R's side effects
# (it calls git to pick a branch -- which stop()s on an unrecognised one -- and
# creates an output directory). Two-step, in order of preference:
#   1. If `years` is already in scope (the FABIO core sourced
#      R/00_system_variables.R before us), just use it.
#   2. Otherwise read the single `years <- ...` line out of the file and
#      evaluate only that. This is side-effect-free and keeps this config's
#      "no side effects at source time" guarantee intact, so a standalone VA
#      run does not trigger the git/branch logic.
# NOTE: `mode = "numeric"` is load-bearing, not decorative. A bare
# exists("years", inherits = TRUE) also matches lubridate::years() -- a FUNCTION
# attached by every library(tidyverse) in the repo -- which makes this guard
# think `years` is "already in scope", skip the file read below, and leave
# `years` bound to a closure. as.integer(years) then dies with
# "cannot coerce type 'closure' to vector of type 'integer'". Requiring numeric
# mode skips the function and only treats a real year *vector* as "in scope".
if (!exists("years", inherits = TRUE, mode = "numeric")) {
  .va_sysvars <- file.path(FABIO_ROOT, "R", "00_system_variables.R")
  if (!file.exists(.va_sysvars)) .va_sysvars <- "R/00_system_variables.R"
  if (file.exists(.va_sysvars)) {
    .va_year_line <- grep("^\\s*years\\s*<-",
                          readLines(.va_sysvars, warn = FALSE),
                          value = TRUE)
    if (length(.va_year_line))
      years <- eval(parse(text = .va_year_line[[1L]]))
    rm(.va_year_line)
  }
  rm(.va_sysvars)
}
if (!exists("years", inherits = TRUE, mode = "numeric"))
  stop("Could not obtain `years`. It is declared in R/00_system_variables.R; ",
       "source that first, or run from the FABIO repo root so this file can ",
       "read the `years <- ...` line from it.")

# Output/keep window: the years actually written by the pipeline. Mirrors the
# FABIO core `years`.
VA_KEEP_YEARS <- as.integer(years)

# Hampel buffer: the years carried ALONGSIDE VA_KEEP_YEARS through the Hampel
# pass purely to give the filter a full +/- VA_HAMPEL_HALF_WINDOW of context at
# both edges of the keep window, then dropped. Derived from the keep window and
# the half-window exactly as 14_1_value_added_FABIO_v2_MRIOTs.R derives its own
# buffer_years from fabio_years, so the two pipelines stay in lock-step.
#
# NOTE: this is symmetric (HAMPEL_HALF_WINDOW years on each side). The old 13_1
# literal was c(2005:2009, 2024:2025) -- an asymmetric 5 low / 2 high margin.
# 3 each side is exactly enough for a full Hampel window at the edges; the extra
# low-side years were only spare margin. To restore extra margin, widen the
# seq() lower bound below (e.g. min(VA_KEEP_YEARS) - (VA_HAMPEL_HALF_WINDOW + 2L)).
VA_BUFFER_YEARS <- c(
  seq(min(VA_KEEP_YEARS) - VA_HAMPEL_HALF_WINDOW, min(VA_KEEP_YEARS) - 1L),
  seq(max(VA_KEEP_YEARS) + 1L,                    max(VA_KEEP_YEARS) + VA_HAMPEL_HALF_WINDOW)
)

# Union of keep + buffer: the working window the price pipeline operates on from
# the year-restriction step through the Hampel pass.
VA_WORKING_YEARS <- sort(union(VA_BUFFER_YEARS, VA_KEEP_YEARS))

# Upstream data limit that is NOT derivable from FABIO: Brazil's national SUT
# files (read by 14_3_value_added_FABIO_v2_national_SUTs.R) are published only
# through this year. R/00_9_prep_value_added.R STAGES the IBGE nivel-68 archive
# and clamps the staged per-year .xls pairs to min(VA_KEEP_YEARS):this cap, so
# this is the reviewed CEILING, not a hard file-staging requirement.
# It is deliberately manual: a new IBGE vintage can revise the activity/product
# breakdown or rename the VA-sheet row labels, which would silently corrupt the
# allocation, so raising the cap should follow a concordance/label review.
# Canada needs no such cap (its builder clamps to the years CANSIM returns).
# 00_9's stage_brazil_suts() messages at run time when IBGE publishes past this
# cap, so you'll see when the FTP has run ahead of the reviewed ceiling.
VA_BRA_SUT_MAX_YEAR <- 2021L

# FAOSTAT exchange-rate selectors. Germany's EUR/USD SLC row is the single
# currency source of truth shared by the EXIOBASE adapter (14_1, MRIOTs) and the
# synthesis script (14_4): USD = native / rate.
VA_FX_ELEMENT_CODE   <- "SLC"
VA_GERMANY_AREA_CODE <- 79L

# Synthetic "global median" area code used in the FAO producer-price CSV
# (written by 13_1_FAOstat, read by 13_3_FABIO_v2_price_extension).
VA_GLOBAL_MEDIAN_AREA_CODE <- 5000L


# -- Own-series-median gap-fill (price pipeline) ------------------------------
#
# Controls the own-series-median rung shared by 13_2 (main + cbs_override grids)
# and 13_3 (FAO producer-price side). For a missing (area, item, year) cell, the
# fill ladder is: trade_direct -> own_series_median -> cross-sectional median
# (item-year, then item / area-5000 global). The own-series median is the median
# of that (area, item) series' own direct observations (post Hampel/winsor),
# reused for every missing year of the series; it carries the country-level price
# level that a cross-country median averages away. Implemented by
# own_series_median_fill() in 00_value_added_helpers.R.
#
# PRICE_PREFER_OWN_SERIES_MEDIAN is the master switch: FALSE reproduces the
# previous cross-sectional-only behaviour exactly (the rung never fires).
PRICE_PREFER_OWN_SERIES_MEDIAN <- TRUE

# Restrict the own-series rung to items that HAVE a winsor band (>= WINSOR_MIN_OBS
# pooled obs with a non-degenerate scale); band-less items route to the
# cross-sectional rung. A separate "own_med outside the winsor [lo,hi]" check is
# intentionally absent: a median of winsor-clipped prices is always in-band.
PRICE_REQUIRE_WINSOR_BAND <- TRUE


# ==============================================================================
# 6. SMALL UTILITIES
# ==============================================================================

#' Create a directory (and parents) if it does not exist; return it invisibly.
#' Replaces the repeated `dir.create(x, showWarnings = FALSE, recursive = TRUE)`
#' idiom at the top of every script.
va_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}


# ==============================================================================
# 7. LOAD SHARED HELPERS
# ==============================================================================
# Sourced here so every pipeline script gets the helpers by sourcing this one
# file. 00_value_added_helpers.R is pure-base / data.table only and has no side
# effects.
source("R/00_value_added_helpers.R")

# ==============================================================================
# End of 00_value_added_config.R
# ==============================================================================