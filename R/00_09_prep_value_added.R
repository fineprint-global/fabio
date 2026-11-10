# ============================================================================
# 00_9_prep_value_added.R
# ----------------------------------------------------------------------------
#
# Staged artifacts (all under input/value_added/ = VA_VALUE_ADDED_INPUT_DIR):
#   Brazil_SUTs/68_tab1_YYYY.xls   IBGE nivel-68 TRU Supply (Recursos) per year
#   Brazil_SUTs/68_tab2_YYYY.xls   IBGE nivel-68 TRU Use     (Usos)     per year
#   Canada_SUTs/cansim_canada_use_va_basic.csv
#   Canada_SUTs/cansim_canada_supply_make_numerators.csv
#   Canada_SUTs/cansim_canada_supply_make_denominators.csv
#   Canada_SUTs/cansim_canada_avail_years.csv
#   oecd_sut_use_valueadded.csv    OECD SDMX T1600 (year-bounded)
#   eurostat_nama_10_a64.csv       whole Eurostat NAMA (nama_10_a64) table
# ============================================================================

library(data.table)
source("R/00_value_added_config.R")

# Staged-input paths/templates are defined ONCE in R/00_value_added_config.R
# (the writer here and the readers in 14_3 / 14_4 share that single source of
# truth). Bound to the short local names below so the staging code reads cleanly.
BRA_SUT_DIR          <- VA_BRA_SUT_DIR
BRA_SUPPLY_FILE_FMT  <- VA_BRA_SUPPLY_FILE_FMT
BRA_USE_FILE_FMT     <- VA_BRA_USE_FILE_FMT

CAN_CACHE_DIR        <- VA_CAN_SUT_DIR
CAN_USE_CSV          <- VA_CAN_USE_CSV
CAN_NUM_CSV          <- VA_CAN_NUM_CSV
CAN_DEN_CSV          <- VA_CAN_DEN_CSV
CAN_AVAIL_CSV        <- VA_CAN_AVAIL_CSV

OECD_SUT_CSV         <- VA_OECD_SUT_CSV
EUROSTAT_NAMA_CSV    <- VA_EUROSTAT_NAMA_CSV


# ============================================================================
# 1. BRAZIL — IBGE nivel-68 TRU workbooks
# ============================================================================
# Discover the newest IBGE reference vintage carrying a nivel-68 TRU zip, then
# download + unpack the per-year .xls pairs into BRA_SUT_DIR (overwriting).

BRA_IBGE_SCN_BASE <- Sys.getenv(
  "BRA_IBGE_SCN_BASE",
  "https://ftp.ibge.gov.br/Contas_Nacionais/Sistema_de_Contas_Nacionais/")
BRA_IBGE_TRU_SUB  <- "tabelas_xls/tabelas_de_recursos_e_usos/"
BRA_IBGE_NIVEL    <- "68"

#' Read an Apache-style IBGE directory index and return its hrefs.
.bra_read_index <- function(u) {
  con  <- url(u, open = "r", encoding = "latin1")
  on.exit(close(con), add = TRUE)
  html <- tryCatch(paste(readLines(con, warn = FALSE), collapse = "\n"),
                   error = function(e) "")
  h <- regmatches(html, gregexpr('href="[^"]+"', html, perl = TRUE,
                                 useBytes = TRUE))[[1]]
  sub('^href="', "", sub('"$', "", h))
}

#' Discover the newest IBGE reference folder carrying a nivel-68 TRU zip.
discover_ibge_nivel68 <- function() {
  refs <- .bra_read_index(BRA_IBGE_SCN_BASE)
  refs <- sub("/$", "", refs[grepl("^[0-9]{4}/$", refs)])
  refs <- sort(as.integer(refs), decreasing = TRUE)
  for (ry in refs) {
    files <- .bra_read_index(paste0(BRA_IBGE_SCN_BASE, ry, "/", BRA_IBGE_TRU_SUB))
    zip68 <- files[grepl(sprintf("nivel_?%s.*\\.zip$", BRA_IBGE_NIVEL),
                         files, ignore.case = TRUE)]
    if (length(zip68)) {
      z   <- zip68[[1L]]
      yrs <- as.integer(unlist(regmatches(z, gregexpr("[0-9]{4}", z))))
      return(list(ref = ry,
                  zip_url  = paste0(BRA_IBGE_SCN_BASE, ry, "/", BRA_IBGE_TRU_SUB, z),
                  zip_name = z,
                  years    = if (length(yrs) >= 2) seq(min(yrs), max(yrs)) else yrs))
    }
  }
  NULL
}

#' EAGER Brazil stage: always re-download the IBGE archive and overwrite the
#' per-year .xls pairs in BRA_SUT_DIR (restricted to the FABIO-coupled range).
stage_brazil_suts <- function() {
  bra_years <- min(VA_KEEP_YEARS):VA_BRA_SUT_MAX_YEAR     # coupled to FABIO years
  va_ensure_dir(BRA_SUT_DIR)
  message(sprintf("  Brazil: fetching IBGE nivel-%s TRU archive for %s ...",
                  BRA_IBGE_NIVEL, paste(range(bra_years), collapse = "\u2013")))
  
  hit <- discover_ibge_nivel68()
  if (is.null(hit))
    stop("No nivel-", BRA_IBGE_NIVEL, " zip found under any IBGE reference folder; ",
         "IBGE may have changed its layout (check BRA_IBGE_SCN_BASE / BRA_IBGE_TRU_SUB).")
  message(sprintf("  Brazil: newest IBGE archive = reference %d, %s (years %s).",
                  hit$ref, hit$zip_name, paste(range(hit$years), collapse = "\u2013")))
  # FABIO-year check (informational): IBGE running past the reviewed ceiling.
  if (length(hit$years) && max(hit$years) > VA_BRA_SUT_MAX_YEAR)
    message(sprintf(paste0(
      "  Brazil NOTE: IBGE now publishes through %d but VA_BRA_SUT_MAX_YEAR is %d. ",
      "Review the concordance / VA-sheet labels for the new vintage before raising ",
      "the cap in 00_value_added_config.R."), max(hit$years), VA_BRA_SUT_MAX_YEAR))
  
  tmp <- tempfile(fileext = ".zip"); ex <- tempfile(); dir.create(ex)
  old_to <- getOption("timeout"); options(timeout = max(600, old_to))
  on.exit({ unlink(tmp); unlink(ex, recursive = TRUE); options(timeout = old_to) },
          add = TRUE)
  utils::download.file(hit$zip_url, tmp, mode = "wb", quiet = FALSE)
  utils::unzip(tmp, exdir = ex)
  
  found <- list.files(ex, pattern = "^68_tab[12]_[0-9]{4}\\.xls$",
                      recursive = TRUE, full.names = TRUE)
  if (!length(found))
    stop("Downloaded ", hit$zip_name, " but found no 68_tab[12]_YYYY.xls inside.")
  # Keep only the FABIO-coupled years; overwrite whatever is on disk.
  fyear <- as.integer(sub("^68_tab[12]_([0-9]{4})\\.xls$", "\\1", basename(found)))
  found <- found[fyear %in% bra_years]
  if (!length(found))
    stop("IBGE archive ", hit$zip_name, " covers none of the requested year(s): ",
         paste(range(bra_years), collapse = "\u2013"), ".")
  ok <- file.copy(found, file.path(BRA_SUT_DIR, basename(found)), overwrite = TRUE)
  message(sprintf("  Brazil: staged %d workbook(s) into %s.", sum(ok), BRA_SUT_DIR))
  
  staged_years <- sort(unique(as.integer(
    sub("^68_tab[12]_([0-9]{4})\\.xls$", "\\1", basename(found)))))
  missing <- setdiff(bra_years, staged_years)
  if (length(missing))
    message(sprintf("  Brazil NOTE: archive does not cover requested year(s): %s.",
                    paste(missing, collapse = ", ")))
  invisible(staged_years)
}


# ============================================================================
# 2. CANADA — StatCan Detail SUT (cansim 36-10-0478-01) -> plain slice CSVs
# ============================================================================
# Download the WHOLE table fresh into the cansim sqlite cache, run the three
# parameterized slices (Use VA / Supply numerators / Supply denominators) plus
# the year-availability list, and write each as a plain CSV that 14_3 reads
# with NO cansim dependency.
#
# The thin "pull" functions below (collect + normalize a lazy sqlite query) live
# HERE because they touch the network/DB. The downstream-shared, network-free
# parsers (nrm_label, clean_ioic_code, split_product_code, detect_sut_columns,
# prep_supply_numerators) live in 00_value_added_helpers.R so both this script
# and 14_3 use one definition.

CAN_CANSIM_TABLE  <- "36-10-0478-01"
CAN_CONC_PATH     <- VA_CONC_CANADA_SUT   # shared with 14_3; defined in config
CAN_USE_VALUE     <- "Use"
CAN_SUPPLY_VALUE  <- "Supply"
CAN_BASIC_PRICE   <- "Basic price"
CAN_GEO_VALUE     <- "Canada"
CAN_LABOUR_LABELS  <- c("Wages and salaries",
                        "Employers' social contributions",
                        "Employers\u2019 social contributions")
CAN_CAPITAL_LABELS <- c("Gross operating surplus", "Gross mixed income")
CAN_TLS_POS_LABELS <- c("Taxes on production")
CAN_TLS_NEG_LABELS <- c("Subsidies on production")

# collect a lazy cansim sqlite query and normalize values (across cansim vintages)
collect_norm <- function(q) {
  if ("collect_and_normalize" %in% getNamespaceExports("cansim")) {
    cansim::collect_and_normalize(q)
  } else {
    cansim::normalize_cansim_values(dplyr::collect(q))
  }
}

#' Distinct `Product` dimension, parsed once.
.can_prod_dim_env <- new.env(parent = emptyenv())
product_dimension <- function(con) {
  if (is.null(.can_prod_dim_env$dt)) {
    pd <- as.data.table(dplyr::collect(dplyr::distinct(con, Product)))
    .can_prod_dim_env$dt <- split_product_code(pd[[1L]])   # split_product_code: helpers
  }
  .can_prod_dim_env$dt
}
products_for_codes  <- function(con, codes)
  product_dimension(con)[ioic_code %in% codes, product_full]
products_for_labels <- function(con, labels) {
  pd <- product_dimension(con)
  pd[nrm_label(bare_label) %in% nrm_label(labels), product_full]   # nrm_label: helpers
}

#' Open (download/cache) the Detail SUT fresh and return a lazy dbplyr handle.
#' refresh = TRUE: EAGER re-download, overwriting the cansim sqlite cache.
open_sut <- function(table = CAN_CANSIM_TABLE) {
  message("  Canada: downloading cansim Detail SUT ", table,
          " (sqlite, fresh) ...")
  if ("get_cansim_connection" %in% getNamespaceExports("cansim")) {
    cansim::get_cansim_connection(table, format = "sqlite", refresh = TRUE)
  } else {
    cansim::get_cansim_sqlite(table)
  }
}

# ── Canada pull functions (collect + normalize a thin slice; no processing) ──
pull_use_va_raw <- function(con, year_chr) {
  va_labels  <- unique(c(CAN_LABOUR_LABELS, CAN_CAPITAL_LABELS,
                         CAN_TLS_POS_LABELS, CAN_TLS_NEG_LABELS))
  va_members <- products_for_labels(con, va_labels)
  if (length(va_members) == 0L)
    stop("No `Product` members matched the VA primary-input labels even after ",
         "stripping the bracketed IOIC suffix \u2014 inspect ",
         "product_dimension(con)$bare_label and the LABOUR/CAPITAL/TLS labels.")
  q <- con |>
    dplyr::filter(GEO == CAN_GEO_VALUE,
                  `Supply and use` == CAN_USE_VALUE,
                  Valuation        == CAN_BASIC_PRICE,
                  Product %in% va_members,
                  REF_DATE %in% year_chr)
  as.data.table(collect_norm(q))
}
pull_supply_by_products <- function(con, product_codes, year_chr) {
  prod_members <- products_for_codes(con, product_codes)
  if (length(prod_members) == 0L)
    stop("No `Product` members matched the concordance IOIC codes \u2014 check that ",
         "sut_item_code values (e.g. MPG327302) match product_dimension(con)$ioic_code.")
  q <- con |>
    dplyr::filter(GEO == CAN_GEO_VALUE,
                  `Supply and use` == CAN_SUPPLY_VALUE,
                  Valuation        == CAN_BASIC_PRICE,
                  Product %in% prod_members,
                  REF_DATE %in% year_chr) |>
    dplyr::mutate(product_raw = Product, industry_raw = Industry)
  as.data.table(collect_norm(q))
}
pull_supply_by_industries <- function(con, industries, year_chr) {
  q <- con |>
    dplyr::filter(GEO == CAN_GEO_VALUE,
                  `Supply and use` == CAN_SUPPLY_VALUE,
                  Valuation        == CAN_BASIC_PRICE,
                  Industry %in% industries,
                  REF_DATE %in% year_chr)
  as.data.table(collect_norm(q))
}

#' EAGER Canada stage: fresh DB download, three slices + availability -> CSV.
stage_canada_suts <- function() {
  if (!requireNamespace("cansim", quietly = TRUE))
    stop("Package 'cansim' is required to stage the Canada SUTs but is not installed.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required to stage the Canada SUTs but is not installed.")
  va_ensure_dir(CAN_CACHE_DIR)
  .can_prod_dim_env$dt <- NULL   # reset the parsed-dimension cache for a fresh DB
  
  requested_years <- VA_KEEP_YEARS                     # coupled to FABIO years
  con <- open_sut(CAN_CANSIM_TABLE)
  
  # Concordance product codes (same load the 14_3 driver uses).
  item_conc_a <- load_item_conc(CAN_CONC_PATH, "A", "CAN_SUT_code", "CAN_SUT_item",
                                out_code = "sut_item_code", out_item = "sut_item")
  item_conc_c <- load_item_conc(CAN_CONC_PATH, "C", "CAN_SUT_code", "CAN_SUT_item",
                                out_code = "sut_item_code", out_item = "sut_item")
  conc_all <- unique(rbindlist(list(item_conc_a, item_conc_c))[, .(sut_item_code, sut_item)])
  
  # Availability: which years the table actually publishes (local DB query).
  ay <- con |>
    dplyr::filter(GEO == CAN_GEO_VALUE) |>
    dplyr::distinct(REF_DATE) |>
    dplyr::collect()
  avail_years <- sort(unique(as.integer(ay$REF_DATE)))
  use_years   <- intersect(requested_years, avail_years)
  if (length(use_years) == 0L)
    stop("None of VA_KEEP_YEARS [", paste(range(requested_years), collapse = "\u2013"),
         "] are in the cansim table (available: ",
         paste(range(avail_years), collapse = "\u2013"), ").")
  if (!all(requested_years %in% avail_years))
    message(sprintf("  Canada NOTE: requested %s; cansim coverage %s \u2014 staging %s.",
                    paste(range(requested_years), collapse = "\u2013"),
                    paste(range(avail_years),     collapse = "\u2013"),
                    paste(range(use_years),       collapse = "\u2013")))
  year_chr <- as.character(use_years)
  fwrite(data.table(avail_year = avail_years), CAN_AVAIL_CSV)
  message(sprintf("  Canada: availability years \u2192 %s.", basename(CAN_AVAIL_CSV)))
  
  # (a) Use-table VA by industry.
  message("  Canada: pulling Use-table VA slice (Basic price) ...")
  use_raw <- pull_use_va_raw(con, year_chr)
  fwrite(use_raw, CAN_USE_CSV)
  message(sprintf("  Canada: %d Use rows \u2192 %s.", nrow(use_raw), basename(CAN_USE_CSV)))
  
  # (b) Make-matrix numerators for the concordance products.
  message("  Canada: pulling Supply (Make) numerator slice ...")
  num_raw <- pull_supply_by_products(con, conc_all$sut_item_code, year_chr)
  fwrite(num_raw, CAN_NUM_CSV)
  message(sprintf("  Canada: %d numerator rows \u2192 %s.", nrow(num_raw), basename(CAN_NUM_CSV)))
  
  # (c) Make-matrix denominators (industries that make the concordance products).
  #     needed_industries_raw is derived from the numerator slice via the shared
  #     prep_supply_numerators() (helpers) — the same derivation 14_3 uses.
  sup_prep <- prep_supply_numerators(num_raw, conc_all$sut_item_code,
                                     conc_all$sut_item, use_years)
  needed_industries_raw <- sort(unique(sup_prep$industry_raw))
  message(sprintf("  Canada: %d make-industry member string(s); pulling denominator slice ...",
                  length(needed_industries_raw)))
  den_raw <- pull_supply_by_industries(con, needed_industries_raw, year_chr)
  fwrite(den_raw, CAN_DEN_CSV)
  message(sprintf("  Canada: %d denominator rows \u2192 %s.", nrow(den_raw), basename(CAN_DEN_CSV)))
  
  invisible(use_years)
}


# ============================================================================
# 3. OECD — SUT "Use, Value added and its components by activity" (T1600)
# ============================================================================
# SDMX csvfilewithlabels export, year-bounded to range(VA_WORKING_YEARS).

.oecd_sut_url <- function(years = VA_WORKING_YEARS) {
  override <- Sys.getenv("OECD_SUT_URL", "")
  if (nzchar(override)) return(override)
  url <- paste0("https://sdmx.oecd.org/public/rest/data/",
                "OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,2.0/all",
                "?dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
  if (length(years))
    url <- sprintf("%s&startPeriod=%d&endPeriod=%d", url, min(years), max(years))
  url
}

#' EAGER OECD stage: always re-download the year-bounded SUT CSV (overwrite).
stage_oecd_sut <- function() {
  if (!requireNamespace("curl", quietly = TRUE))
    stop("Package 'curl' is required to stage the OECD SUT but is not installed.")
  va_ensure_dir(dirname(OECD_SUT_CSV))
  url <- .oecd_sut_url(VA_WORKING_YEARS)
  message("  OECD: fetching SUT from OECD SDMX ...\n    ", url)
  tmp <- tempfile(fileext = ".csv")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  h <- curl::new_handle(timeout = 1800L)
  curl::handle_setheaders(h, Accept = "application/vnd.sdmx.data+csv; charset=utf-8")
  curl::curl_download(url, tmp, handle = h, quiet = FALSE)
  if (file.size(tmp) < 1e5)
    stop("OECD SUT download looks truncated (", file.size(tmp), " bytes). ",
         "Check OECD_SUT_URL / the SDMX endpoint.")
  ok <- file.copy(tmp, OECD_SUT_CSV, overwrite = TRUE)
  if (!ok) stop("Could not write OECD SUT cache to ", OECD_SUT_CSV)
  message(sprintf("  OECD: written %s (%.0f MB).", basename(OECD_SUT_CSV),
                  file.size(OECD_SUT_CSV) / 1e6))
  invisible(OECD_SUT_CSV)
}


# ============================================================================
# 4. EUROSTAT — National accounts by activity (nama_10_a64), whole table
# ============================================================================
# 14_4 filters this table by na_item / nace_r2 / unit downstream, so the whole
# table is staged once. time_format="num" gives a numeric `time` year column,
# which 14_4 already maps to TIME_PERIOD on read.

EU_NAMA_TABLE <- "nama_10_a64"

#' EAGER Eurostat stage: always re-fetch the whole NAMA table (overwrite).
stage_eurostat_nama <- function() {
  if (!requireNamespace("eurostat", quietly = TRUE))
    stop("Package 'eurostat' is required to stage the Eurostat NAMA table but ",
         "is not installed. install.packages('eurostat') to enable the Eurostat ",
         "fallback fill / diagnostic in 14_4.")
  va_ensure_dir(dirname(EUROSTAT_NAMA_CSV))
  message("  Eurostat: fetching ", EU_NAMA_TABLE, " (whole table) ...")
  nama <- as.data.table(eurostat::get_eurostat(EU_NAMA_TABLE, time_format = "num"))
  if (!nrow(nama)) stop("Eurostat returned an empty ", EU_NAMA_TABLE, " table.")
  fwrite(nama, EUROSTAT_NAMA_CSV)
  message(sprintf("  Eurostat: written %s (%d rows).",
                  basename(EUROSTAT_NAMA_CSV), nrow(nama)))
  invisible(EUROSTAT_NAMA_CSV)
}


# ============================================================================
# DRIVER — run every stage eagerly; report a summary.
# ============================================================================
# Brazil and Canada are REQUIRED by 14_3 (a failure stops the run). OECD and
# Eurostat are OPTIONAL overlays in 14_4 (a failure is reported but does not
# stop staging the rest). Each runs regardless of the others' outcome.

message(sprintf("%s\n  Staging value-added network inputs (eager; overwrites) | FABIO years %s\n%s",
                strrep("=", 76),
                paste(range(VA_KEEP_YEARS), collapse = "\u2013"), strrep("=", 76)))

.stages <- list(
  list(name = "Eurostat NAMA", required = FALSE, fn = stage_eurostat_nama),
  list(name = "OECD SUT",      required = FALSE, fn = stage_oecd_sut),
  list(name = "Canada SUTs",   required = TRUE,  fn = stage_canada_suts),
  list(name = "Brazil SUTs",   required = TRUE,  fn = stage_brazil_suts)
)

.results <- lapply(.stages, function(s) {
  message(sprintf("\n[ %s ]", s$name))
  err <- tryCatch({ s$fn(); NULL },
                  error = function(e) conditionMessage(e))
  if (!is.null(err))
    message(sprintf("  !! %s FAILED: %s", s$name, err))
  list(name = s$name, required = s$required, ok = is.null(err), err = err)
})

message(sprintf("\n%s\n  Staging summary\n%s", strrep("-", 76), strrep("-", 76)))
for (r in .results)
  message(sprintf("  %-14s %s%s", r$name,
                  if (r$ok) "OK" else "FAILED",
                  if (r$required && !r$ok) "  (REQUIRED by 14_3)" else
                    if (!r$required && !r$ok) "  (optional; 14_4 will skip the overlay)" else ""))

.failed_required <- Filter(function(r) r$required && !r$ok, .results)
if (length(.failed_required))
  stop("Required value-added input(s) failed to stage: ",
       paste(vapply(.failed_required, `[[`, character(1), "name"), collapse = ", "),
       ". 14_3 cannot run without them. See the messages above.")

message("\nDone. 14_3 / 14_4 can now run as pure readers of input/value_added/.")