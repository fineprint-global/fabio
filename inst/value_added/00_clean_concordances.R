# ------------------------------------------------------------------
# Add identifier codes to the FABIOv2 concordance tables and write
# each of the nine sheets out as a separate CSV.
#
# Updated for the FLAGGED workbook: the former "ISIC A" / "ISIC C"
# sheet pairs (BioSAM, GLORIA) are now single "Items ..." sheets that
# carry an `ISIC` column (values "A", "C", or blank). EXIOBASE, whose
# A and C sheets used to be identical, is now likewise a single "Items"
# sheet with the corrected per-row ISIC allocation. The ISIC column
# (and any `comments` column) is preserved untouched and carried into
# the output CSVs.
#
# Also handles the Canada SUT <-> FABIO item concordance
# ("Item CAN SUTs FABIOv2"), the US SUT <-> FABIO item concordance
# ("Item USA SUTs FABIOv2") and the Brazil SUT <-> FABIO item
# concordance ("Item BRA SUTs FABIOv2"). Each national side carries its
# own SUT code column (`CAN_SUT_code` / `USA_SUT_code` / `BRA_SUT_code`,
# transferred from the respective national Supply table), so on top of
# that only the FABIO codes (item code + commodity code) are added; the
# national SUT code, `ISIC` and `comments` columns flow through untouched.
#
# Also handles the Japanese Input-Output Table <-> FABIO item
# concordances ("Item JPN IOT2011/2015/2020 FABIOv2"), one sheet per
# benchmark IOT year. They follow the same pattern as the national SUT
# sheets: the Japanese side carries its own item and code columns
# (`JPN_IOT{year}_item` / `JPN_IOT{year}_code`, basic sector
# classification column codes, kept as text to preserve leading zeros),
# so only the FABIO codes are added; `ISIC` and `comments` flow through
# untouched. Because the three sheets are identical apart from the year
# in the column names, they are processed in a loop.
# ------------------------------------------------------------------

library(readxl)
library(readr)
library(dplyr)
library(stringr)

# ---- File paths --------------------------------------------------
input_dir        <- path.expand("~/value_added_2/02_value_added_FABIO_v2/input")
output_dir       <- path.expand("~/value_added_2/concordances/")
concordance_xlsx <- file.path(output_dir, "FABIOv2_concordance_tables.xlsx")

fabio_items_csv   <- "/mnt/nfs_fineprint/tmp/fabio/v2/items.csv"
fabio_regions_csv <- "/mnt/nfs_fineprint/tmp/fabio/v2/regions.csv"
gloria_xlsx       <- "/mnt/nfs_fineprint/tmp/gloria/v060-compiled/labels/GLORIA_ReadMe_060.xlsx"
biosam_csv        <- file.path(input_dir,
                               "Dataset_JRC_-_BioSAMs_for_the_EU_Member_States_-_2010.csv")
fao_prod_csv      <- path.expand(
  "~/value_added_2/01_total_value_FABIO_v2/input/Production_Crops_Livestock_E_All_Data.csv"
)
exiobase_unit_pxp_rds <- "/mnt/nfs_fineprint/tmp/exiobase/v3.10/IOT_2022_pxp/unit.rds"
exiobase_unit_ixi_rds <- "/mnt/nfs_fineprint/tmp/exiobase/v3.10/IOT_2022_ixi/unit.rds"

# ---- Helpers -----------------------------------------------------
# Fix "mojibake": UTF-8 bytes that were misread as Latin-1 and
# re-encoded, producing e.g. "CÃ´te d'Ivoire" instead of "Côte d'Ivoire".
# The fix is to write the characters out as Latin-1 bytes and re-interpret
# those bytes as UTF-8. We only ACCEPT the converted result when it is
# valid UTF-8 and actually different from the original — otherwise the
# same conversion would corrupt clean strings (e.g. clean "Côte" bytes
# c3 b4 -> bytes f4, which is invalid UTF-8). This makes the function
# idempotent and safe to run on a mix of mojibake'd and clean inputs.
fix_mojibake <- function(x) {
  if (!is.character(x)) return(x)
  converted <- suppressWarnings(
    iconv(x, from = "UTF-8", to = "latin1", sub = NA)
  )
  Encoding(converted) <- "UTF-8"
  accept <- !is.na(x) & !is.na(converted) &
    validUTF8(converted) & converted != x
  out <- x
  out[accept] <- converted[accept]
  out
}

# After all reads + fixes, scrub any remaining invalid UTF-8 bytes so
# that downstream string ops (regex, adist, normalize_str) don't choke.
sanitize_utf8 <- function(x) {
  if (!is.character(x)) return(x)
  bad <- !is.na(x) & !validUTF8(x)
  if (any(bad)) {
    x[bad] <- iconv(x[bad], from = "UTF-8", to = "UTF-8", sub = "?")
  }
  x
}

# Apply fix_mojibake to every character column of a data frame, then
# sanitize any remaining invalid UTF-8 bytes.
fix_mojibake_df <- function(df) {
  df |> mutate(across(where(is.character), \(v) sanitize_utf8(fix_mojibake(v))))
}

# Normalise a string for fuzzy matching: fold diacritics to ASCII,
# standardise quotation marks, collapse whitespace, lowercase.
# Used ONLY for matching, never to modify the stored data.
normalize_str <- function(x) {
  if (!is.character(x)) return(x)
  x |>
    stringi::stri_trans_general("Any-Latin; Latin-ASCII") |>
    str_replace_all("[\u2018\u2019\u2032]", "'") |>   # curly / prime to straight
    str_replace_all("[\u201C\u201D]", '"') |>         # curly double quotes
    str_squish() |>
    tolower()
}

# Look up a code column by a name column. Tries exact match first,
# then falls back to a match on normalised forms so differences in
# diacritic spelling, quote style or whitespace don't block matches.
lookup_code <- function(x, tbl, name_col, code_col) {
  code <- tbl[[code_col]][match(x, tbl[[name_col]])]
  still_na <- is.na(code) & !is.na(x)
  if (any(still_na)) {
    code[still_na] <- tbl[[code_col]][
      match(normalize_str(x[still_na]),
            normalize_str(tbl[[name_col]]))
    ]
  }
  code
}

# ---- Reference lookup tables -------------------------------------
fabio_items <- read_csv(fabio_items_csv, show_col_types = FALSE) |>
  fix_mojibake_df() |>
  distinct(item, item_code, comm_code)

fabio_regions <- read_csv(fabio_regions_csv, show_col_types = FALSE) |>
  fix_mojibake_df() |>
  distinct(area, iso3c, area_code)

gloria_regions <- read_excel(gloria_xlsx, sheet = "Regions") |>
  fix_mojibake_df() |>
  distinct(Region_names, Region_acronyms)

gloria_sectors <- read_excel(gloria_xlsx, sheet = "Sectors") |>
  fix_mojibake_df() |>
  distinct(Sector_names, Lfd_Nr)

biosam_raw <- read_csv(biosam_csv, show_col_types = FALSE) |>
  fix_mojibake_df()

biosam_items <- biosam_raw |>
  mutate(spending_agent_clean = `Spending Agent` |>
           str_remove("\\s*\\(activity\\)\\s*$") |>
           str_squish()) |>
  distinct(spending_agent_clean, `Spending Agent (Code)`)

biosam_countries <- biosam_raw |>
  distinct(`Country (Harmonized)`, `Country (ISO2)`)

# FAO lookup comes solely from Production_Crops_Livestock
fao_prod <- read_csv(fao_prod_csv, show_col_types = FALSE) |>
  fix_mojibake_df()
fao_prod_items <- fao_prod |> distinct(Item, `Item Code`)
fao_prod_areas <- fao_prod |> distinct(Area, `Area Code`)

# EXIOBASE has no item codes of its own: take the position of each
# distinct sector in unit.rds as its code. Sectors appear in the same
# order for every region, so taking distinct sectors in order of first
# appearance gives a stable 1..N index that matches the row position
# of that sector within any region's block of the IOT.
# Product-by-product (pxp): sector column holds the 200 product names.
exiobase_units_pxp <- readRDS(exiobase_unit_pxp_rds) |>
  fix_mojibake_df()

exiobase_sectors_pxp <- exiobase_units_pxp |>
  distinct(sector) |>
  mutate(EXIOBASE_sector_code = row_number())

# Industry-by-industry (ixi): sector column holds the 163 industry names.
# Same construction, different unit file, so codes run 1..163.
exiobase_units_ixi <- readRDS(exiobase_unit_ixi_rds) |>
  fix_mojibake_df()

exiobase_sectors_ixi <- exiobase_units_ixi |>
  distinct(sector) |>
  mutate(EXIOBASE_sector_code = row_number())

# ---- Read all nine concordance sheets & normalise encoding -------
# The BioSAM, GLORIA and EXIOBASE item/sector concordances are now a
# single "Items ..." sheet each, carrying an `ISIC` flag column that
# replaces the old separate ISIC A / ISIC C sheets. The flag (and the
# `comments` column) is left untouched here and flows through to the
# output via `everything()` in the column-ordering step below.
sheets <- list(
  biosam_items     = "Items BioSAMs FABIOv2",
  biosam_areas     = "Area match BioSAMs FABIOv2",
  gloria_sectors   = "Items GLORIAv60 FABIOv2",
  gloria_areas     = "Area matching GLORIAv60 FABIOv2",
  fao_items        = "Item matching FAO FABIOv2",
  fao_areas        = "Area matching FAO FABIOv2",
  exiobase_sectors_pxp = "Items EXIOBv3.10 pxp FABIOv2",
  exiobase_sectors_ixi = "Items EXIOBv3.10 ixi FABIOv2",
  exiobase_areas   = "Area match EXIOBv3.10 FABIOv2",
  can_items        = "Item CAN SUTs FABIOv2",
  usa_items        = "Item USA SUTs FABIOv2",
  bra_items        = "Item BRA SUTs FABIOv2",
  jpn_items_2011   = "Item JPN IOT2011 FABIOv2",
  jpn_items_2015   = "Item JPN IOT2015 FABIOv2",
  jpn_items_2020   = "Item JPN IOT2020 FABIOv2"
)

# The three Japanese sheets share one structure; only the year embedded
# in the column names differs. Loops below use this to avoid repetition.
jpn_years <- c("2011", "2015", "2020")

# Workbook holding the Japanese sheets. By default the main concordance
# workbook; point this elsewhere if the JPN tables live in their own file.
jpn_xlsx <- concordance_xlsx

conc <- lapply(names(sheets), function(nm) {
  src <- if (startsWith(nm, "jpn_")) jpn_xlsx else concordance_xlsx
  read_excel(src, sheet = sheets[[nm]]) |> fix_mojibake_df()
})
names(conc) <- names(sheets)

# ---- Fill in the codes -------------------------------------------
# 1. BioSAM <-> FABIO items (single sheet; ISIC flag preserved)
conc$biosam_items <- conc$biosam_items |>
  mutate(
    BioSAM_item_code = lookup_code(BioSAM_item, biosam_items,
                                   "spending_agent_clean",
                                   "Spending Agent (Code)"),
    FABIO_item_code  = lookup_code(FABIO_item, fabio_items,
                                   "item", "item_code"),
    FABIO_comm_code  = lookup_code(FABIO_item, fabio_items,
                                   "item", "comm_code")
  )

# 2. BioSAM <-> FABIO areas
conc$biosam_areas <- conc$biosam_areas |>
  mutate(
    BioSAM_area_code = lookup_code(BioSAM_area, biosam_countries,
                                   "Country (Harmonized)",
                                   "Country (ISO2)"),
    FABIO_iso3c      = lookup_code(FABIO_area, fabio_regions,
                                   "area", "iso3c"),
    FABIO_area_code  = lookup_code(FABIO_area, fabio_regions,
                                   "area", "area_code")
  )

# 3. GLORIA sectors <-> FABIO items (single sheet; ISIC flag preserved)
conc$gloria_sectors <- conc$gloria_sectors |>
  mutate(
    GLORIA_sector_code = lookup_code(GLORIA_sector, gloria_sectors,
                                     "Sector_names", "Lfd_Nr"),
    FABIO_item_code    = lookup_code(FABIO_item, fabio_items,
                                     "item", "item_code"),
    FABIO_comm_code    = lookup_code(FABIO_item, fabio_items,
                                     "item", "comm_code")
  )

# 4. GLORIA regions <-> FABIO areas
conc$gloria_areas <- conc$gloria_areas |>
  mutate(
    FABIO_iso3c        = lookup_code(FABIO_area, fabio_regions,
                                     "area", "iso3c"),
    FABIO_area_code    = lookup_code(FABIO_area, fabio_regions,
                                     "area", "area_code"),
    GLORIA_region_code = lookup_code(GLORIA_region, gloria_regions,
                                     "Region_names", "Region_acronyms")
  )

# 5. FAO <-> FABIO items (only Production_Crops_Livestock)
conc$fao_items <- conc$fao_items |>
  mutate(
    FABIO_item_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "item_code"),
    FABIO_comm_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "comm_code"),
    FAO_item_code   = lookup_code(FAO_item, fao_prod_items,
                                  "Item", "Item Code")
  )

# 6. FAO <-> FABIO areas (only Production_Crops_Livestock)
conc$fao_areas <- conc$fao_areas |>
  mutate(
    FABIO_iso3c     = lookup_code(FABIO_area, fabio_regions,
                                  "area", "iso3c"),
    FABIO_area_code = lookup_code(FABIO_area, fabio_regions,
                                  "area", "area_code"),
    FAO_area_code   = lookup_code(FAO_area, fao_prod_areas,
                                  "Area", "Area Code")
  )

# 7. EXIOBASE sectors <-> FABIO items (one sheet per representation;
#    ISIC flag preserved). EXIOBASE_sector_code = position of the sector
#    in the matching unit.rds: the pxp product list (1..200) for the pxp
#    sheet, the ixi industry list (1..163) for the ixi sheet.
conc$exiobase_sectors_pxp <- conc$exiobase_sectors_pxp |>
  mutate(
    EXIOBASE_sector_code = lookup_code(EXIOBASE_sector, exiobase_sectors_pxp,
                                       "sector", "EXIOBASE_sector_code"),
    FABIO_item_code      = lookup_code(FABIO_item, fabio_items,
                                       "item", "item_code"),
    FABIO_comm_code      = lookup_code(FABIO_item, fabio_items,
                                       "item", "comm_code")
  )

conc$exiobase_sectors_ixi <- conc$exiobase_sectors_ixi |>
  mutate(
    EXIOBASE_sector_code = lookup_code(EXIOBASE_sector, exiobase_sectors_ixi,
                                       "sector", "EXIOBASE_sector_code"),
    FABIO_item_code      = lookup_code(FABIO_item, fabio_items,
                                       "item", "item_code"),
    FABIO_comm_code      = lookup_code(FABIO_item, fabio_items,
                                       "item", "comm_code")
  )

# 8. EXIOBASE regions <-> FABIO areas
#    EXIOBASE has no region names of its own; the country code IS the
#    identifier, so it's used directly with no lookup on the EXIOBASE side.
conc$exiobase_areas <- conc$exiobase_areas |>
  mutate(
    FABIO_iso3c     = lookup_code(FABIO_area, fabio_regions,
                                  "area", "iso3c"),
    FABIO_area_code = lookup_code(FABIO_area, fabio_regions,
                                  "area", "area_code")
  )

# 9. Canada SUT <-> FABIO items (single sheet; ISIC flag preserved)
#    The Canadian side already carries its own `CAN_SUT_code`, so only the
#    FABIO item code and commodity code need to be filled in here; the
#    existing Canadian code is left untouched and carried through.
conc$can_items <- conc$can_items |>
  mutate(
    FABIO_item_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "item_code"),
    FABIO_comm_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "comm_code")
  )

# 10. US SUT <-> FABIO items (single sheet; ISIC flag preserved)
#     The US side already carries its own `USA_SUT_code`, so only the
#     FABIO item code and commodity code need to be filled in here; the
#     existing US code is left untouched and carried through.
conc$usa_items <- conc$usa_items |>
  mutate(
    FABIO_item_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "item_code"),
    FABIO_comm_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "comm_code")
  )

# 11. Brazil SUT <-> FABIO items (single sheet; ISIC flag preserved)
#     The Brazilian side already carries its own `BRA_SUT_code`, so only
#     the FABIO item code and commodity code need to be filled in here;
#     the existing Brazilian code is left untouched and carried through.
conc$bra_items <- conc$bra_items |>
  mutate(
    FABIO_item_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "item_code"),
    FABIO_comm_code = lookup_code(FABIO_item, fabio_items,
                                  "item", "comm_code")
  )

# 12. Japanese IOT <-> FABIO items (one sheet per benchmark year;
#     ISIC flag preserved). The Japanese side already carries its own
#     `JPN_IOT{year}_code` (6-digit basic sector column code), so only
#     the FABIO item code and commodity code need to be filled in; the
#     existing Japanese code is left untouched and carried through —
#     forced to character so leading zeros survive even if a cell was
#     stored as a number in Excel.
for (yr in jpn_years) {
  key      <- paste0("jpn_items_", yr)
  code_col <- paste0("JPN_IOT", yr, "_code")
  conc[[key]] <- conc[[key]] |>
    mutate(
      across(all_of(code_col), as.character),
      FABIO_item_code = lookup_code(FABIO_item, fabio_items,
                                    "item", "item_code"),
      FABIO_comm_code = lookup_code(FABIO_item, fabio_items,
                                    "item", "comm_code")
    )
}

# ---- Manual overrides -------------------------------------------
# For names that don't match automatically, fill in codes by hand.
# Overrides take effect only where the lookup produced NA.
apply_overrides <- function(df, name_col, overrides) {
  for (code_col in setdiff(names(overrides), name_col)) {
    # 1) exact match
    manual <- overrides[[code_col]][match(df[[name_col]], overrides[[name_col]])]
    
    # 2) normalised match (diacritics, quote styles, whitespace)
    still_na <- is.na(manual) & !is.na(df[[name_col]])
    if (any(still_na)) {
      manual[still_na] <- overrides[[code_col]][
        match(normalize_str(df[[name_col]][still_na]),
              normalize_str(overrides[[name_col]]))
      ]
    }
    
    # 3) approximate match: tolerate up to 2 differing characters.
    #    catches cases where the string carries invalid UTF-8 bytes that
    #    normalize_str can't fold (e.g. "C<garbage>te d'Ivoire" vs the
    #    literal "Côte d'Ivoire" in the override table).
    still_na <- is.na(manual) & !is.na(df[[name_col]])
    if (any(still_na)) {
      x <- df[[name_col]][still_na]
      y <- overrides[[name_col]]
      d <- adist(x, y)              # rows = x, cols = y
      hit <- apply(d, 1, function(row) {
        i <- which(row <= 2)
        if (length(i)) i[1] else NA_integer_
      })
      manual[still_na] <- overrides[[code_col]][hit]
    }
    
    df[[code_col]] <- coalesce(df[[code_col]], manual)
  }
  df
}

fabio_area_manual <- tribble(
  ~FABIO_area,      ~FABIO_iso3c, ~FABIO_area_code,
  "Côte d'Ivoire",  "CIV",        107L
)

fao_area_manual <- tribble(
  ~FAO_area,        ~FAO_area_code,
  "Côte d'Ivoire",  107L,
  "Réunion",        182L,
  "Türkiye",        223L
)

fao_item_manual <- tribble(
  ~FAO_item,                       ~FAO_item_code,
  "Tea nes (herbal tea)",          674L,
  "Flax, processed but not spun",  773L
)

conc$gloria_areas   <- apply_overrides(conc$gloria_areas,   "FABIO_area", fabio_area_manual)
conc$exiobase_areas <- apply_overrides(conc$exiobase_areas, "FABIO_area", fabio_area_manual)
conc$fao_areas      <- apply_overrides(conc$fao_areas,      "FABIO_area", fabio_area_manual)
conc$fao_areas      <- apply_overrides(conc$fao_areas,      "FAO_area",   fao_area_manual)
conc$fao_items      <- apply_overrides(conc$fao_items,      "FAO_item",   fao_item_manual)

# ---- Report any unmatched names ----------------------------------
report_unmatched <- function(df, name_col, code_col) {
  miss <- df |>
    filter(!is.na(.data[[name_col]]) & is.na(.data[[code_col]])) |>
    pull(.data[[name_col]]) |>
    unique()
  if (length(miss)) {
    message("  Unmatched in ", code_col, ": ",
            paste(miss, collapse = "; "))
  }
}
message("Unmatched entries (if any):")
message("- biosam_items")
report_unmatched(conc$biosam_items, "BioSAM_item", "BioSAM_item_code")
report_unmatched(conc$biosam_items, "FABIO_item",  "FABIO_item_code")
message("- biosam_areas")
report_unmatched(conc$biosam_areas, "BioSAM_area", "BioSAM_area_code")
report_unmatched(conc$biosam_areas, "FABIO_area",  "FABIO_area_code")
message("- gloria_sectors")
report_unmatched(conc$gloria_sectors, "GLORIA_sector", "GLORIA_sector_code")
report_unmatched(conc$gloria_sectors, "FABIO_item",    "FABIO_item_code")
message("- gloria_areas")
report_unmatched(conc$gloria_areas, "FABIO_area",    "FABIO_area_code")
report_unmatched(conc$gloria_areas, "GLORIA_region", "GLORIA_region_code")
message("- fao_items")
report_unmatched(conc$fao_items, "FABIO_item", "FABIO_item_code")
report_unmatched(conc$fao_items, "FAO_item",   "FAO_item_code")
message("- fao_areas")
report_unmatched(conc$fao_areas, "FABIO_area", "FABIO_area_code")
report_unmatched(conc$fao_areas, "FAO_area",   "FAO_area_code")
message("- exiobase_sectors_pxp")
report_unmatched(conc$exiobase_sectors_pxp, "EXIOBASE_sector", "EXIOBASE_sector_code")
report_unmatched(conc$exiobase_sectors_pxp, "FABIO_item",      "FABIO_item_code")
message("- exiobase_sectors_ixi")
report_unmatched(conc$exiobase_sectors_ixi, "EXIOBASE_sector", "EXIOBASE_sector_code")
report_unmatched(conc$exiobase_sectors_ixi, "FABIO_item",      "FABIO_item_code")
message("- exiobase_areas")
report_unmatched(conc$exiobase_areas, "FABIO_area", "FABIO_area_code")
message("- can_items")
report_unmatched(conc$can_items, "FABIO_item", "FABIO_item_code")
message("- usa_items")
report_unmatched(conc$usa_items, "FABIO_item", "FABIO_item_code")
message("- bra_items")
report_unmatched(conc$bra_items, "FABIO_item", "FABIO_item_code")
for (yr in jpn_years) {
  key <- paste0("jpn_items_", yr)
  message("- ", key)
  report_unmatched(conc[[key]], "FABIO_item", "FABIO_item_code")
}

# ---- Put columns in a sensible order -----------------------------
# Source side first (name then code(s)), then FABIO side (name then
# codes). `everything()` at the end preserves any extra columns from
# the original sheet — notably the `ISIC` flag and `comments`.
conc$biosam_items <- conc$biosam_items |>
  select(BioSAM_item, BioSAM_item_code,
         FABIO_item,  FABIO_item_code,  FABIO_comm_code,
         everything())

conc$biosam_areas <- conc$biosam_areas |>
  select(BioSAM_area, BioSAM_area_code,
         FABIO_area,  FABIO_area_code,  FABIO_iso3c,
         everything())

conc$gloria_sectors <- conc$gloria_sectors |>
  select(GLORIA_sector, GLORIA_sector_code,
         FABIO_item,    FABIO_item_code, FABIO_comm_code,
         everything())

conc$gloria_areas <- conc$gloria_areas |>
  select(GLORIA_region, GLORIA_region_code,
         FABIO_area,    FABIO_area_code, FABIO_iso3c,
         everything())

conc$fao_items <- conc$fao_items |>
  select(FAO_item,   FAO_item_code,
         FABIO_item, FABIO_item_code, FABIO_comm_code,
         everything())

conc$fao_areas <- conc$fao_areas |>
  select(FAO_area,   FAO_area_code,
         FABIO_area, FABIO_area_code, FABIO_iso3c,
         everything())

conc$exiobase_sectors_pxp <- conc$exiobase_sectors_pxp |>
  select(EXIOBASE_sector, EXIOBASE_sector_code,
         FABIO_item,      FABIO_item_code, FABIO_comm_code,
         everything())

conc$exiobase_sectors_ixi <- conc$exiobase_sectors_ixi |>
  select(EXIOBASE_sector, EXIOBASE_sector_code,
         FABIO_item,      FABIO_item_code, FABIO_comm_code,
         everything())

# EXIOBASE_region is the code itself (no separate name/code pair).
conc$exiobase_areas <- conc$exiobase_areas |>
  select(EXIOBASE_region,
         FABIO_area, FABIO_area_code, FABIO_iso3c,
         everything())

# Canada SUT side: name then its SUT code; FABIO side gets name then codes.
conc$can_items <- conc$can_items |>
  select(CAN_SUT_item, CAN_SUT_code,
         FABIO_item, FABIO_item_code, FABIO_comm_code,
         everything())

# US SUT side: name then its SUT code; FABIO side gets name then codes.
conc$usa_items <- conc$usa_items |>
  select(USA_SUT_item, USA_SUT_code,
         FABIO_item, FABIO_item_code, FABIO_comm_code,
         everything())

# Brazil SUT side: name then its SUT code; FABIO side gets name then codes.
conc$bra_items <- conc$bra_items |>
  select(BRA_SUT_item, BRA_SUT_code,
         FABIO_item, FABIO_item_code, FABIO_comm_code,
         everything())

# Japanese IOT side: name then its IOT code; FABIO side gets name then
# codes. Column names carry the year, hence all_of() with built names.
for (yr in jpn_years) {
  key <- paste0("jpn_items_", yr)
  conc[[key]] <- conc[[key]] |>
    select(all_of(paste0("JPN_IOT", yr, c("_item", "_code"))),
           FABIO_item, FABIO_item_code, FABIO_comm_code,
           everything())
}

# ---- Write each sheet as its own CSV -----------------------------
# NOTE: the two former ISIC A/C item files for BioSAM and GLORIA are
# now one file each; the ISIC class lives in the `ISIC` column inside
# the file. The Canada, US and Brazil SUT <-> FABIO concordances and
# the three Japanese IOT <-> FABIO concordances (one per benchmark
# year) are written as additional CSVs.
out_files <- c(
  biosam_items     = "concordance_items_biosam_fabio.csv",
  biosam_areas     = "concordance_areas_biosam_fabio.csv",
  gloria_sectors   = "concordance_items_gloria_fabio.csv",
  gloria_areas     = "concordance_areas_gloria_fabio.csv",
  fao_items        = "concordance_items_fao_producer_prices_fabio.csv",
  fao_areas        = "concordance_areas_fao_producer_prices_fabio.csv",
  exiobase_sectors_pxp = "concordance_items_exiobase_pxp_fabio.csv",
  exiobase_sectors_ixi = "concordance_items_exiobase_ixi_fabio.csv",
  exiobase_areas   = "concordance_areas_exiobase_fabio.csv",
  can_items        = "concordance_items_canada_sut_fabio.csv",
  usa_items        = "concordance_items_usa_sut_fabio.csv",
  bra_items        = "concordance_items_brazil_sut_fabio.csv",
  jpn_items_2011   = "concordance_items_japan_iot2011_fabio.csv",
  jpn_items_2015   = "concordance_items_japan_iot2015_fabio.csv",
  jpn_items_2020   = "concordance_items_japan_iot2020_fabio.csv"
)

for (nm in names(conc)) {
  # drop rows where every cell is NA (trailing empty rows from Excel)
  df <- conc[[nm]] |> filter(if_any(everything(), ~ !is.na(.)))
  write_excel_csv(df, file.path(output_dir, out_files[[nm]]), na = "")
}

message("Wrote ", length(out_files), " CSVs to ", output_dir)