
# Tidying -----------------------------------------------------------------

library("data.table") # 1.12.0
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
cbs <- rename_cols(cbs, rename)

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
missing_summary(cbs, years)

# Store object
saveRDS(cbs, "data/tidy/cbs_tidy.rds")


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
  names = c("Waters,ice etc", "Cotton waste", "Vitamins", "Hair, goat, coarse",
            "Beehives", "Beeswax", "Hair, fine", "Crude materials",
            "Waxes vegetable")
)
cat("Excluding the following items:\n",
    paste0(names(items_exclude), collapse = ";\n"), sep = "")
items <- unique(btd$item_code)
for(item in items_exclude) {
  cat(paste0("Item #", item, " to remove found: ",
             any(grepl(item, items, fixed = TRUE))), "\n")
}
btd <- btd[!item_code %in% items_exclude, ]

# Exclude values of 0
cat("Subsetting to observations with value > 0.\n",
    "Dropping ", btd[!value > 0, .N], " observations.", sep = "")
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

# Merge variations of countries - currently limited to name changes
# merge_areas(btd, orig = 206, dest = 276, "Sudan", col = "reporter")
# merge_areas(btd, orig = 206, dest = 276, "Sudan", col = "partner")
merge_areas(btd, orig = 62, dest = 238, "Ethiopia", col = "reporter")
merge_areas(btd, orig = 62, dest = 238, "Ethiopia", col = "partner")

# Store object
saveRDS(btd, "data/tidy/btd_tidy.rds")


# Forestry ----------------------------------------------------------------

forestry_raw <- readRDS("input/fao/for_raw.rds")

cat("Removing country groups from the cbs object.\n")
forestry_raw <- forestry_raw[forestry_raw[[1]] < 5000, ]

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
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
forestry <- rename_cols(forestry_raw, rename)
rm(forestry_raw)

cat("Subsetting to observations with value > 0.\n",
    "Dropping ", forestry[!value > 0, .N], " obsevations.", sep = "")
forestry <- forestry[value > 0, ]

# Exclude all items except the following:
items_include <- structure(
  c(1864, 1866, 1867),
  names = c("Wood fuel", "Industrial roundwood, coniferous",
            "Industrial roundwood, non-coniferous")
)
cat("Excluding everything but the following items:\n",
    paste0(names(items_include), collapse = ";\n"), sep = "")
items <- unique(forestry$item_code)
for(item in items_include) {
  cat(paste0("Item #", item, " to keep found: ",
             any(grepl(item, items, fixed = TRUE))), "\n")
}
forestry <- forestry[item_code %in% items_include, ]

cat("Dropping", forestry[unit != "m3", .N],
    "observations without m3 as unit.\n")
forestry <- forestry[unit == "m3", ]

cat("Excluding", forestry[area_code == 351, .N],
    "observations of China to avoid conflicts / double counting.\n")
forestry <- forestry[area_code != 351, ]

# Add column cutting from "Import/Export Quantity/Value"
forestry$imex <- factor(gsub("^(Import|Export)(.*)$", "\\1", forestry$element))

# Widening the data
forestry <- dcast(forestry,
                  area_code + area +
                  item_code + item + year ~ imex, value.var = "value")
forestry <- rename_cols(forestry, c("Export" = "exports", "Import" = "imports",
                                    "Production" = "production"), drop = FALSE)

cat("Setting NA values in the btd object to 0.\n")
replace_dt(forestry, 0)

# Merge variations of countries - currently limited to name changes
# merge_areas(forestry, orig = 206, dest = 276, "Sudan")
merge_areas(forestry, orig = 62, dest = 238, "Ethiopia")

# Detect missing years for countries
missing_summary(forestry, years)

# Store object
saveRDS(forestry, "data/tidy/for_tidy.rds")


# Forestry Trade ----------------------------------------------------------

for_trad_raw <- readRDS("input/fao/for_trad.rds")

cat("Removing country groups from the cbs object.\n")
for_trad_raw <- for_trad_raw[for_trad_raw[[1]] < 5000 | for_trad_raw[[3]] < 5000, ]

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
for_trad <- rename_cols(for_trad_raw, rename)
rm(for_trad_raw)

# Exclude all items except the following:
items_include <- structure(
  c(1651, 1657, 1670),
  names = c("Industrial roundwood, coniferous",
            "Industrial roundwood, non-coniferous tropical",
            "Industrial roundwood, non-coniferous non-tropical")
)

cat("Excluding everything but the following items:\n",
    paste0(names(items_include), collapse = ";\n"), sep = "")
items <- unique(for_trad$item_code)
for(item in items_include) {
  cat(paste0("Item #", item, " to keep found: ",
             any(grepl(item, items, fixed = TRUE))), "\n")
}
for_trad <- for_trad[item_code %in% items_include, ]

# Add column cutting from "Import/Export Quantity/Value"
for_trad$imex <- factor(gsub("^(Import|Export)(.*)$", "\\1", for_trad$element))

# Widening the data
for_trad <- dcast(for_trad,
                  reporter_code + reporter + partner_code + partner +
                    item_code + item + year ~ imex,
                  value.var = "value")
for_trad <- rename_cols(for_trad, c("Export" = "exports", "Import" = "imports",
                                    "Production" = "production"), drop = FALSE)


# Merge variations of countries - currently limited to name changes
# merge_areas(forestry, orig = 206, dest = 276, "Sudan")
merge_areas(for_trad, orig = 62, dest = 238, "Ethiopia")

# Detect missing years for countries
missing_summary(for_trad, years)

# Store object
saveRDS(for_trad, "data/tidy/for_trad_tidy.rds")


# Crop production ---------------------------------------------------------

crop_raw <- readRDS("input/fao/crop_raw.rds")

cat("Removing country groups from crop_raw.\n")
crop_raw <- crop_raw[crop_raw[[1]] < 5000, ]

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
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
crop <- rename_cols(crop_raw, rename)
rm(crop_raw)

cat("Removing NA values.\n")
crop <- crop[!is.na(value)]

cat("Removing values of 0.\n")
crop <- crop[value != 0]

item_conc <- fread("inst/items_crop-cbs.csv", encoding = "UTF-8")

cat("Converting tonnes to primary equivalents using tcf_trade.\n")
crop <- merge(crop, item_conc,
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
# Empty entries in items_crop-cbs.csv should be skipped
# To-do: move to explicitly excluding aggregates
if(any(is.na(crop$tcf))) {
  cat("No concordance found for: ",
      paste0(unique(crop[is.na(crop$tcf), item]), collapse = ", "),
      ".\nDropping missing values.\n", sep = "")
  crop <- crop[!is.na(tcf), ]
  # set(crop, which(is.na(crop[["tcf"]])), "tcf", 1)
}

crop[, value := value * tcf]

crop <- crop[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]


# Merge variations of countries - currently limited to name changes
# merge_areas(crop, orig = 206, dest = 276, "Sudan", col = "reporter")
# merge_areas(crop, orig = 206, dest = 276, "Sudan", col = "partner")
merge_areas(crop, orig = 62, dest = 238, "Ethiopia", col = "reporter")
merge_areas(crop, orig = 62, dest = 238, "Ethiopia", col = "partner")

# To-do: Add Production_Crops_Primary.csv

crop_prim <- readRDS("input/fao/crop_prim.rds")

foddercrops <- data.frame(
  Item.Code = c(rep(2000, 16)),
  Item = c(rep("Fodder crops", 16)),
  Prod.Code = c(636, 637, 638, 639, 640, 641, 642, 643, 644, 645, 646, 647,
                648, 649, 651, 655),
  Prod = c(
    "Forage and silage, maize",
    "Forage and silage, sorghum",
    "Forage and silage, rye grass",
    "Forage and silage, grasses nes",
    "Forage and silage, clover",
    "Forage and silage, alfalfa",
    "Forage and silage, green oilseeds",
    "Forage and silage, legumes",
    "Cabbage for fodder",
    "Turnips for fodder",
    "Beets for fodder",
    "Carrots for fodder",
    "Swedes for fodder",
    "Forage products",
    "Vegetables and roots fodder"))
Fodder <- Primary_raw[Primary_raw$ItemCode %in% foddercrops$Prod.Code,3:10]
names(Fodder) <- c("Country.Code","Country","Element.Code","Element","Prod.Code","Prod","Year","Value")
Fodder <- merge(Fodder, foddercrops[,1:3], by="Prod.Code", all.x=TRUE)
Fodder <- aggregate(Value ~ ., Fodder[,c(2,3,4,5,7,8,9,10)],sum)
Fodder <- Fodder[Fodder$Element!="Yield",]
Fodder$Unit <- "tonnes"
Fodder$Unit[Fodder$Element=="Area harvested"] <- "ha"
Fodder$Unit <- as.factor(Fodder$Unit)
Fodder <- Fodder[,c(1,2,6,7,3,4,5,9,8)]
crop <- rbind(crop,Fodder)
crop <- crop[crop$value>0,]
crop <- crop[!is.na(crop$Item.Code),]
rm(Fodder,foddercrops,Primary_raw)

# Live

prod_live_raw <- readRDS("input/fao/live_raw.rds")

cat("Removing country groups from crop_raw.\n")
prod_live_raw <- prod_live_raw[prod_live_raw[[1]] < 5000, ]

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
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
prod_live <- rename_cols(prod_live_raw, rename)
rm(prod_live_raw)

# Exclude the following items:
items_exclude <- structure(
  c(1057, 1068, 1079, 1072, 1083, 1746, 1749, 1181),
  names = c("Chickens", "Ducks", "Turkeys", "Geese and guinea fowls",
            "Pigeons, other birds", "Cattle and Buffaloes", "Sheep and Goats",
            "Beehives")
)
cat("Excluding the following items:\n",
    paste0(names(items_exclude), collapse = ";\n"), sep = "")
items <- unique(prod_live$item_code)
for(item in items_exclude) {
  cat(paste0("Item #", item, " to remove found: ",
             any(grepl(item, items, fixed = TRUE))), "\n")
}
prod_live <- prod_live[!item_code %in% items_exclude, ]

# Merge variations of countries - currently limited to name changes
# merge_areas(prod_live, orig = 206, dest = 276, "Sudan", col = "reporter")
# merge_areas(prod_live, orig = 206, dest = 276, "Sudan", col = "partner")
merge_areas(prod_live, orig = 62, dest = 238, "Ethiopia", col = "reporter")
merge_areas(prod_live, orig = 62, dest = 238, "Ethiopia", col = "partner")

# Remove China to prevent double-counting
prod_live <- prod_live[area_code != 351]

# Primary production
prim_live_raw <- readRDS("input/fao/live_prim.rds")


cat("Removing country groups from crop_raw.\n")
prim_live_raw <- prim_live_raw[prim_live_raw[[1]] < 5000, ]

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
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
prim_live <- rename_cols(prim_live_raw, rename)
rm(prim_live_raw)

# Merge variations of countries - currently limited to name changes
# merge_areas(prim_live, orig = 206, dest = 276, "Sudan", col = "reporter")
# merge_areas(prim_live, orig = 206, dest = 276, "Sudan", col = "partner")
merge_areas(prim_live, orig = 62, dest = 238, "Ethiopia", col = "reporter")
merge_areas(prim_live, orig = 62, dest = 238, "Ethiopia", col = "partner")

# Remove China to prevent double-counting
prim_live <- prim_live[area_code != 351]

# To-do: add butter


# Processing data --------------------------------------------------------

proc_crop <- readRDS("input/fao/crop_proc.rds")
proc_live <- readRDS("input/fao/live_proc.rds")
proc_raw <- rbind(proc_crop, proc_live)
rm(proc_crop, proc_live)

cat("Removing country groups from proc_raw.\n")
proc_raw <- proc_raw[proc_raw[[1]] < 5000, ]

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
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
proc <- rename_cols(proc_raw, rename)
rm(proc_raw)

cat("Removing NA values.\n")
proc <- proc[!is.na(value)]

cat("Removing values of 0.\n")
proc <- proc[value != 0]

item_conc <- fread("inst/items_proc-cbs.csv", encoding = "UTF-8")

cat("Converting tonnes to primary equivalents using tcf_trade.\n")
proc <- merge(proc, item_conc,
              by.x = "item_code", by.y = "proc_item_code", all.x = TRUE)
# Empty entries in items_crop-cbs.csv should be skipped
if(any(is.na(proc$tcf))) {
  cat("No concordance found for: ",
      paste0(unique(proc[is.na(proc$tcf), item]), collapse = ", "),
      ".\nDropping missing values.\n", sep = "")
  proc <- proc[!is.na(tcf), ]
  # set(proc, which(is.na(proc[["tcf"]])), "tcf", 1)
}

proc[, value := value * tcf]

proc <- proc[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]

# Merge variations of countries - currently limited to name changes
# merge_areas(proc, orig = 206, dest = 276, "Sudan", col = "reporter")
# merge_areas(proc, orig = 206, dest = 276, "Sudan", col = "partner")
merge_areas(proc, orig = 62, dest = 238, "Ethiopia", col = "reporter")
merge_areas(proc, orig = 62, dest = 238, "Ethiopia", col = "partner")

# Remove China to prevent double-counting
proc <- proc[area_code != 351]

# To-do: Maybe merge with prod
# to-do: Separate butter, i.e. "Butter, Ghee"
