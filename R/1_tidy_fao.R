
# Tidying -----------------------------------------------------------------

library(data.table) # 1.12.0
source("R/1_tidy_functions.R")
regions <- fread("inst/regions_full.csv", encoding = "UTF-8")
years <- 1986:2013


# CBS --------------------------------------------------------------------

cbs_crop <- readRDS("input/fao/cbs_crop.rds")
cbs_live <- readRDS("input/fao/cbs_live.rds")
cbs_raw <- rbind(cbs_crop, cbs_live)
rm(cbs_crop, cbs_live)

# Remove country groups (>= 5000)
cat("Removing country groups from the cbs object.\n")
cbs_raw <- cbs_raw[cbs_raw[[1]] < 5000, ]

# Widening the data
cbs <- dcast(cbs_raw,
             `Area Code` + Area + `Item Code` + Item + Year ~ Element,
             value.var = "Value")
rm(cbs_raw)

cat("Setting NA values in the cbs object to 0.\n")
replace_dt(cbs, 0)

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Year" = "year",
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  # "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses" = "other",
  "Processing" = "processing"
)
cbs <- rename_cols(cbs, rename)unique(btd$item_code)

cat("Changing total_supply from 'prod + imp - exp' to 'prod + imp'.\n",
    "Equality of total_supply and formula: ",
    all.equal(cbs$total_supply,
              cbs$production + cbs$imports - cbs$exports + cbs$stock_withdrawal),
    ".\n", sep = "")
cbs[, total_supply := production + imports]
# cbs$total_supply <- cbs$production + cbs$imports

# Adjust stock variation from representing stock-withdrawals to stock-additions
cbs[, stock_addition := -stock_withdrawal]
# cbs$stock_addition <- -cbs$stock_withdrawal

cat("Found ", cbs[stock_addition > total_supply, .N],
    # sum(cbs$stock_addition > cbs$total_supply),
    " occurences of stock_addition exceeding total_supply.\n",
    "Capping out the values at total_supply.\n", sep = "")
cbs[stock_addition > total_supply, stock_addition := total_supply]
# cbs$stock_addition <- ifelse(cbs$stock_addition > cbs$total_supply,
#                              cbs$total_supply, cbs$stock_addition)

cat("Setting negative values in some cbs variables to 0.\n")
for(var in c("production", "imports", "exports", "total_supply", "losses",
             "food", "feed", "seed", "other", "processing"))
  set(cbs, which(cbs[[var]] < 0), var, 0)

cat("Rebalancing uses in cbs with total_supply and stock_additions given.\n")
uses <- c("exports", "food", "feed", "seed", "losses", "processing", "other")
denom <- (cbs[["total_supply"]] - cbs[["stock_addition"]]) / rowSums(cbs[, ..uses])
for(use in uses)
  set(cbs, j = use, value = cbs[[use]] / denom)

cat("Setting NaN values in cbs that occured after rebalancing to 0.\n")
replace_dt(cbs, 0, is.nan)

# Match country names
cbs$area <- regions$name[match(cbs$area_code, regions$code)]

# Merge variations of countries - currently limited to name changes
# merge_areas(cbs, orig = 206, dest = 276, "Sudan")
merge_areas(cbs, orig = 62, dest = 238, "Ethiopia")

# Detect missing years for countries
missing <- data.frame(years = years)
for(code in unique(cbs$area_code)) {
  missing_years <- which(!years %in% unique(cbs[area_code == code]$year))
  if(length(missing_years > 0))
    missing[missing_years, as.character(code)] <- TRUE
}
cat("Missing years found for the following regions:\n",
    paste(regions$name[match(names(missing)[-1], regions$code)],
          colSums(missing[, -1], na.rm = TRUE), collapse = ", "),
    ".\n", sep = "")
cat("Missing regions found for the following years:\n",
    paste(years, rowSums(missing[, -1], na.rm = TRUE), collapse = ", "),
    ".\n", sep = "")


# BTD ---------------------------------------------------------------------

btd_raw <- readRDS("input/fao/btd_raw.rds")

# Rename the columns
rename <- c(
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  "Item Code" = "item_code",
  "Item" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value"
)
btd <- rename_cols(btd_raw, rename)
rm(btd_raw) # >3GB

# Exclude the following items:
items_exclude <- structure(
  c(631, 769, 853, 1031, 1181, 1183, 1218, 1293, 1296),
  names = c(
  "Waters,ice etc", "Cotton waste", "Vitamins", "Hair, goat, coarse",
  "Beehives", "Beeswax", "Hair, fine", "Crude materials", "Waxes vegetable"
))
items <- unique(btd$item_code)
for(item in items_exclude) {
  cat(paste0("Item #", item, " to remove - found: ",
             any(grepl(item, items, fixed = TRUE))), "\n")
}
btd <- btd[!item_code %in% items_exclude, ]

# Exclude values of 0
cat("Subsetting to observations with value > 0.\n",
    "Dropping ", btd[!value > 0, .N], " obsevations.", sep = "")
btd <- btd[value > 0, ]

# Add column cutting from "Import/Export Quantity/Value"
btd$imex <- factor(gsub("^(Import|Export) (.*)$", "\\1", btd$element))

# Convert tonnes to primary equivalents
tcf_trade <- fread("inst/tcf_trade.csv", encoding = "UTF-8")

btd_ton <- btd[unit == "tonnes"]
btd_rest <- btd[unit != "tonnes"]

cat("Converting tonnes to primary equivalents using tcf_trade.\n")
btd_ton <- merge(btd_ton, tcf_trade,
                 by.x = "item_code", by.y = "code", all.x = TRUE)
# Check for missing tcf - Rice is expected to be
if(any(is.na(btd_ton$tcf))) {
  cat("Missing conversion factors found for: ",
      unique(btd_ton[is.na(btd_ton$tcf), item]),
      ".\nSetting missing values to 1.\n", sep = "")
  set(btd_ton, which(is.na(btd_ton[["tcf"]])), "tcf", 1)
}
btd_ton[, value := value / tcf]
btd <- rbind(btd_ton[, !"tcf"], btd_rest)
rm(btd_ton, btd_rest)

# Item aggregation to the level of CBS
item_conc <- fread("inst/items_btd-cbs.csv", encoding = "UTF-8")
item_match <- match(btd$item_code, item_conc$btd_item_code)
btd$item_code <- item_conc$cbs_item_code[item_match]
btd$item <- item_conc$cbs_item[item_match]
rm(item_match)
cat("Aggregating items to the level of cbs.\n",
    "Start at N = ", nrow(btd), ".\n", sep = "") # ~27 mio.
btd <- btd[, list(value = sum(value)),
           by = .(reporter_code, reporter, partner_code, partner,
                  item_code, item, year, imex, unit)]
cat("Arrived at N = ", nrow(btd), ".\n", sep = "") # ~16 mio.

cat("Removing ", sum(is.na(btd$unit)),
    " observations with missing unit values.\n", sep = "")
btd <- btd[!is.na(unit), ]

# Widening the data
btd <- dcast(btd,
             reporter_code + reporter + partner_code + partner +
             item_code + item + year + imex ~ unit, value.var = "value")
btd <- rename_cols(btd, c("1000 Head" = "k_cap", "1000 US$" = "k_usd",
                          "Head" = "cap", "tonnes" = "tonnes"), drop = FALSE)

cat("Setting NA values in the btd object to 0.\n")
replace_dt(btd, 0)

cat("Filling missing k_cap values with cap.\n")
btd[k_cap == 0 & cap != 0, k_cap := cap / 1000]
