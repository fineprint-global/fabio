
library("data.table")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full_123.csv")


# Colnames ----------------------------------------------------------------

rename <- c(
  "Area Code" = "area_code",
  "AreaCode" = "area_code",
  "Area" = "area",
  "AreaName" = "area",
  "Item Code" = "item_code",
  "ItemCode" = "item_code",
  "Item" = "item",
  "ItemName" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  "ElementName" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value",
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  # After casting
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Import quantity" = "imports",
  "Export quantity" = "exports",
  "Domestic supply quantity" = "supply",
  "Losses" = "losses",
  "Loss" = "losses",
  "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses (non-food)" = "other", # Other uses
  "Processing" = "processing",
  "Processed" = "processing",
  # Units
  # "1000 US$" = "k_usd",
  # "1000 Head" = "k_capita",
  "Head" = "head",
  "tonnes" = "tonnes",
  "Export" = "exports",
  "Import" = "imports",
  # Fish
  "COUNTRY" = "country",
  # "AREA" = "water_area",
  "SOURCE" = "source_code",
  "SPECIES" = "species",
  "YEAR" = "year",
  "UNIT" = "unit",
  "QUANTITY" = "value",
  "Months" = "months",
  "Tourist consumption" = "tourist",
  "Food" = "food",
  "Residuals" = "residuals",
  #"Geographic.Area" = "area",
  #"measuredElement" = "element_code",
  "COUNTRY.UN_CODE" = "country",
  "SPECIES.ALPHA_3_CODE" = "species",
  #"AREA.CODE" = "water_area",
  "PRODUCTION_SOURCE_DET.CODE" = "source_code",
  "MEASURE" = "unit",
  "PERIOD" = "year",
  "VALUE" = "value",
  "code" = "area_code",
  "name" = "area"
)

# CBS ---------------------------------------------------------------------

cat("\nTidying CBS.\n")

# food: transform to tonnes
cbs_food_old <- readRDS("input/fao/cbs_food_old.rds")[!Year %in% years,]
cbs_food_new <- readRDS("input/fao/cbs_food_new.rds")[Year %in% years,]
cbs_food <- rbindlist(list(cbs_food_old, cbs_food_new), use.names = TRUE, fill = TRUE)
cbs_food[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]
# Stock Variation seems to be defined wrongly, sign needs to be changed
# NOTE: this is inconsistently defined (sometimes correct, sometimes wrong), so it is corrected further below in the balancing section

# nonfood: remove items contained in food balances
cbs_nonfood <- readRDS("input/fao/cbs_nonfood_new.rds")
cbs_nonfood <- cbs_nonfood[Year %in% years, 1:(ncol(cbs_nonfood)-1)]
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]
cbs_nonfood <- merge(cbs_nonfood, cbs_food[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food"))
cbs_nonfood <- cbs_nonfood[is.na(Value.food) ,]
cbs_nonfood <- cbs_nonfood[,`:=` (Value.food = NULL)]

# bind
cbs <- rbind(cbs_food, cbs_nonfood, fill=TRUE)
cbs <- dt_rename(cbs, rename, drop = TRUE)
rm(cbs_nonfood, cbs_food, cbs_food_old, cbs_food_new)

# transform items that changed from old to new FBS method
# "Groundnuts (Shelled Eq)" in "Groundnuts"
cbs[item == "Groundnuts (Shelled Eq)", `:=` (item_code = 2552, item = "Groundnuts", value = 1/0.685 * value)]
# “Rice (milled equivalent)” into "Rice and products" via TCF
cbs[item == "Rice (Milled Equivalent)", `:=` (item_code = 2807, item = "Rice and products", value = 1/0.674 * value)]
# “Unmanufactured tobacco” into "Tobacco"
cbs[item_code == 826, `:=` (item_code = 2671, item = "Tobacco", value = value)]
# replace "other" with "Other" in c("Vegetables, other", "Fruits, other", "Cereals, other")
cbs[item %in% c("Vegetables, other", "Fruits, other", "Cereals, other"),
    item := sub("other", "Other", item)]
# Note: Sugar (Raw Equivalent) was also present in old FBS, so we don't need to transform it here

# remove unused elements
cbs <- cbs[! element %in% c("Food supply (kcal/capita/day)",
                            "Food supply quantity (kg/capita/yr)",
                            "Fat supply quantity (g/capita/day)",
                            "Protein supply quantity (g/capita/day)",
                            "Fat supply quantity (t)",
                            "Protein supply quantity (t)",
                            "Food supply (kcal)",
                            "Total Population - Both sexes")]
cbs[, element := ifelse(element=="Processed", "Processing", element)]
cbs[, element := ifelse(element=="Export Quantity", "Export quantity", element)]
cbs[, element := ifelse(element=="Import Quantity", "Import quantity", element)]

# change units to tonnes
cbs[, unit := ifelse(unit=="t", "tonnes", unit)]
cbs[unit=="1000 t", `:=`(unit="tonnes", value=value*1000)]

# Country / Area adjustments
cbs <- area_kick(cbs, code = 351, pattern = "China", groups = TRUE)
cbs <- area_merge(cbs, orig = 62, dest = 238, pattern = "Ethiopia")
cbs <- area_merge(cbs, orig = 206, dest = 276, pattern = "Sudan")
cbs <- area_fix(cbs, regions)

# Widen by element
cbs <- data.table::dcast(cbs, area_code + area + item_code + item + year ~ element,
                         value.var = "value") # fun.aggregate = sum, na.rm = TRUE sum is used her because remaining duplicates only contain NAs in food balance
cbs <- dt_rename(cbs, rename, drop = FALSE)

# Replace NA values with 0
cbs <- dt_replace(cbs, is.na, value = 0)
# Make sure values are not negative
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("imports", "exports", "feed", "food", "losses",
                           "other", "processing", "production", "seed"))

cat("Recoding 'supply' from",
    "'production + imports - exports + stock_withdrawal'", "to",
    "'production + imports'.\n")
cbs[, supply := na_sum(production, imports)]

# Add more intuitive 'stock_addition'
cbs[, stock_addition := -stock_withdrawal]


# Rebalance uses, with 'supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(supply,
                          -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other, -residuals, -tourist)] #

# correct mistakes in stock variation reporting: this was reported with inconsistent signs
cat("\nExchange stock_addition and stock_withdrawal in",
    cbs[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
          (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),.N],
    "out of", nrow(cbs),
    "cases where supply and use are balanced after this exchange.",
    "\nThis assumes a reporting mistake for",
    round(cbs[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
                (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),.N] / nrow(cbs) * 100),
    "% of the data entries.")
cbs[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
    (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),
    `:=`(stock_addition = -stock_addition,
       stock_withdrawal = -stock_withdrawal,
       balancing = balancing + 2*stock_addition,
       corr = TRUE,
       ratio = balancing/stock_addition)  ]

cbs[, `:=`(corr = NULL, ratio = NULL)]

# remove negatives in stock_addition and stock_withdrawal variables
# negative stock additions previously decreased use
cbs[, `:=`(stock_addition = ifelse(stock_addition < 0, 0, stock_addition))]
cbs[, `:=`(stock_withdrawal = ifelse(stock_withdrawal < 0, 0, stock_withdrawal))]
# Each country's supply is the sum of its production and stock withdrawals (i.e. where stock_addition < 0)
cbs[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]

# fix discrepancies of stock additions with 'supply'
# Note: residuals should capture such inconsistencies now
cat("Found ", cbs[stock_addition > supply, .N],
    " occurences of 'stock_addition' exceeding 'supply'.\n",
    "Keeping values as is.\n", sep = "")
# cbs[stock_addition > supply, stock_addition := supply]
# cbs[stock_addition > supply,
#    `:=` (stock_addition = ifelse(stock_addition + balancing < 0, 0, stock_addition + balancing),
#          balancing = ifelse(stock_addition + balancing < 0, balancing + stock_addition, 0))]

# rename 2 items in order to have identical name throughout the FAOSTAT data domains
cbs[, item := ifelse(item_code==2605,	"Vegetables, Other",
                     ifelse(item_code==2625, "Fruits, Other", item))]

# quick-fix for cocoa data error:
# NOTE: this is postponed now to a later point (use), where all un-allocated processing use 
# (i.e. when a supply chain is not further traced in FABIO) are put into a new final demand category
# in new fbs, only cocoa beans are in cocoa and products, while FAOSTAT forgot cocoa powder
# there is thus no food use, and most of use goes into processing, where it is not traced further
# simply moving processing use to food use is a bad fix
# cbs[(item == "Cocoa Beans and products" & year >= 2010), `:=`(food = processing, processing = 0)]
# TODO: this needs to be improved in the future (i.e. by using SUA or contacting FAO to correct data)

# Store
saveRDS(cbs, "data/tidy/cbs_tidy.rds")


# SUA ---------------------------------------------------------------------

cat("\nTidying SUA.\n")

sua <- readRDS("input/fao/sua.rds")
items_sua <- fread("inst/sua/items_sua.csv")

sua <- dt_rename(sua, rename = rename)

# keep only relevant elements
sua <- sua[element %in% c("Production", "Import quantity", "Export quantity",
                          "Processed", "Seed", "Feed", "Food supply quantity (tonnes)",
                          "Other uses (non-food)", "Loss", "Residuals", "Stock Variation", "Tourist consumption"),]

# Fix encoding of item strings 
sua[, item := iconv(item, from = "latin1", to = "UTF-8")]

# keep only relevant items
sua <- sua[item_code %in% items_sua$item_code & value != 0]


# Country / Area adjustments
sua <- area_kick(sua, code = 351, pattern = "China", groups = TRUE)
sua <- area_merge(sua, orig = 62, dest = 238, pattern = "Ethiopia")
sua <- area_merge(sua, orig = 206, dest = 276, pattern = "Sudan")
sua <- area_fix(sua, regions)

# Widen by element
sua <- data.table::dcast(sua, area_code + area + item_code + item + year ~ element,
                         value.var = "value") # fun.aggregate = sum, na.rm = TRUE sum is used her because remaining duplicates only contain NAs in food balance
sua <- dt_rename(sua, rename, drop = FALSE)

# Replace NA values with 0
sua <- dt_replace(sua, is.na, value = 0, cols = 6:ncol(sua))
# Make sure values are not negative
sua <- dt_replace(sua, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("imports", "exports", "feed", "food", "losses",
                           "other", "processing", "production", "seed"))

cat("\nRecoding 'supply' from",
    "'production + imports - exports + stock_withdrawal'", "to",
    "'production + imports'.\n")
sua[, supply := na_sum(production, imports)]

# Add more intuitive 'stock_addition'
sua[, stock_addition := -stock_withdrawal]


# Rebalance uses, with 'supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
sua[, balancing := na_sum(supply,
                          -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other, -residuals, -tourist)] #

# correct mistakes in stock variation reporting: this was reported with inconsistent signs
cat("\nExchange stock_addition and stock_withdrawal in",
    sua[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
          (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),.N],
    "out of", nrow(sua),
    "cases where supply and use are balanced after this exchange.",
    "\nThis assumes a reporting mistake for",
    round(sua[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
                (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),.N] / nrow(sua) * 100),
    "% of the data entries.")
sua[((balancing/stock_addition < -1.9) & is.finite(balancing/stock_addition)) |
    (data.table::between(-2*stock_addition, balancing - 1000, balancing + 1000) & abs(stock_addition) > 1000),
  `:=`(stock_addition = -stock_addition,
       stock_withdrawal = -stock_withdrawal,
       balancing = balancing + 2*stock_addition,
       corr = TRUE,
       ratio = balancing/stock_addition)  ]

sua[, `:=`(corr = NULL, ratio = NULL)]

# remove negatives in stock_addition and stock_withdrawal variables
# negative stock additions previously decreased use
sua[, `:=`(stock_addition = ifelse(stock_addition < 0, 0, stock_addition))]
sua[, `:=`(stock_withdrawal = ifelse(stock_withdrawal < 0, 0, stock_withdrawal))]
# Each country's supply is the sum of its production and stock withdrawals (i.e. where stock_addition < 0)
sua[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
sua[, `:=`(supply = na_sum(domestic_supply, imports))]
sua[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
sua[, `:=`(use = na_sum(domestic_use, exports))]

setnames(sua, "item_code", "item_code")

# Store
saveRDS(sua, "data/tidy/sua_tidy.rds")


# Match SUA items to corresponding (aggregate) FABIO item
conc <- fread("inst/conc_btd-cbs.csv")
sua <- merge(sua, conc[,.(item_code = btd_item_code, item = btd_item, cbs_item_code, cbs_item, tcf)], 
             by = c("item_code", "item"), all.x = TRUE)
sua <- sua[item_code != 1276]  # remove fatty acids to avoid double-counting
sua[, `:=`(item_code = NULL, item = NULL)]
setnames(sua, old = c("cbs_item_code", "cbs_item"), new = c("item_code", "item"))
# remove NAs
sua <- sua[!is.na(item_code)]
# Divide numeric columns by tcf to convert into the FABIO item
num_cols <- setdiff(names(sua)[sapply(sua, is.numeric)], c("area_code", "item_code", "year", "tcf"))
sua[, (num_cols) := lapply(.SD, function(x) x / tcf), .SDcols = num_cols]

# Aggregate
sua <- sua[, lapply(.SD, sum, na.rm = TRUE), by = .(area_code, area, item_code, item, year), .SDcols = num_cols]

# Store
saveRDS(sua, "data/tidy/sua_cbs_tidy.rds")

rm(sua)


# BTD ---------------------------------------------------------------------

cat("\nTidying BTD.\n")

btd <- readRDS("input/fao/btd_prod.rds")
btd <- dt_rename(btd, rename, drop = TRUE)

btd <- dt_filter(btd, value >= 0 & year %in% years)

# Country / Area adjustments
for(col in c("reporter_code", "partner_code")) {
  btd <- area_kick(btd, code = 351, pattern = "China", groups = TRUE, col = col)
  btd <- area_merge(btd, orig = 62, dest = 238, pattern = "Ethiopia", col = col)
  btd <- area_merge(btd, orig = 206, dest = 276, pattern = "Sudan", col = col)
  btd <- area_fix(btd, regions, col = col)
}

btd <- as.data.table(btd)
# btd[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]
set(btd, j = "imex", value = factor(gsub("^(Import|Export) (.*)$", "\\1", btd$element))) # does the same
btd[, element := NULL]

# Recode "1000 USD" to "usd"
btd[unit == "1000 USD", `:=`(value = value * 1000, unit = "usd")]
# Recode "t" to "tonnes"
btd[unit == "t", `:=`(unit = "tonnes")]

# Check Animal stock units
btd[item_code %in% items[unit=="1000 animals", item_code] & 
      unit == "An", `:=`(value = value / 1000, unit = "1000 An")]
btd[unit == "An", `:=`(unit = "An")]

# Change from reporting & partner country to receiving & supplying country
btd[, `:=`(from = ifelse(imex == "Import", partner, reporter),
           from_code = ifelse(imex == "Import", partner_code, reporter_code),
           to = ifelse(imex == "Import", reporter, partner),
           to_code = ifelse(imex == "Import", reporter_code, partner_code),
           reporter = NULL, reporter_code = NULL,
           partner = NULL, partner_code = NULL)]

# Give preference to reported export flows over import flows
btd <- flow_pref(btd, pref = "Export")
btd[, imex := NULL]

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)

#store full version for sua
saveRDS(btd, "data/tidy/btd_sua_tidy.rds")


# Apply TCF to observations with 'unit' == "tonnes"
btd <- merge(btd, fread("inst/tcf_btd.csv"),
             by = "item_code", all.x = TRUE)
if(btd[is.na(tcf), .N] != 0) {
  cat("Warning: There are", btd[is.na(tcf), .N], "cases where tcf is NA.\n")
  btd[unit != "tonnes", tcf := 1]
}
btd <- tcf_apply(btd, na.rm = FALSE, filler = 1, fun = `/`)

# Aggregate to CBS items
btd_conc <- fread("inst/conc_btd-cbs.csv")

cat("Aggregating BTD items to the level of CBS.\n")
item_match <- match(btd[["item_code"]], btd_conc[["btd_item_code"]])
btd[, `:=`(item_code = btd_conc$cbs_item_code[item_match],
           item = btd_conc$cbs_item[item_match])]
# remove items not included in btd_conc (mainly unclassified food preparations, crude organic material and food wastes)
btd <- btd[!is.na(item_code)]
btd <- btd[, list(value = na_sum(value)), by = .(from_code, from,
                                                 to_code, to, item_code, item, year, unit)]
cat("Aggregation from", length(item_match), "to", nrow(btd), "observations.\n")

# Store
saveRDS(btd, "data/tidy/btd_tidy.rds")
rm(btd, btd_conc, item_match)


# # Forestry ----------------------------------------------------------------
#
# cat("\nTidying forestry.\n")
#
# #
# # Production
# fore_prod <- readRDS("input/fao/fore_prod.rds")
#
# fore_prod <- dt_rename(fore_prod, rename, drop = TRUE)
#
# # Country / Area adjustments
# fore_prod <- area_kick(fore_prod, code = 351, pattern = "China", groups = TRUE)
# fore_prod <- area_merge(fore_prod, orig = 62, dest = 238, pattern = "Ethiopia")
# fore_prod <- area_merge(fore_prod, orig = 206, dest = 276, pattern = "Sudan")
# fore_prod <- area_fix(fore_prod, regions)
#
# # Cut down to certain products
# fore_prod <- dt_filter(fore_prod, item_code %in% c("Wood fuel" = 1864,
#   "Industrial roundwood, coniferous" = 1866,
#   "Industrial roundwood, non-coniferous" = 1867))
# fore_prod <- dt_filter(fore_prod, value >= 0)
# # fore_prod <- dt_filter(fore_prod, unit != "1000 US$")
# # Recode "1000 US$" to "usd"
# fore_prod[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]
#
# fore_prod[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]
#
# # Get this in the format of CBS
# fore_prod <- dt_filter(fore_prod, unit == "m3")
# fore_prod[, unit := NULL]
# fore_prod <- data.table::dcast(fore_prod,
#   area_code + area + item_code + item + year ~ imex, value.var = "value")
# fore_prod <- dt_rename(fore_prod, rename, drop = FALSE)
#
# # Store
# saveRDS(fore_prod, "data/tidy/fore_prod_tidy.rds")
# rm(fore_prod)
#
# #
# # Trade
# fore_trad <- readRDS("input/fao/fore_trad.rds")
#
# fore_trad <- dt_rename(fore_trad, rename)
#
# # Country / Area adjustments
# for(col in c("reporter_code", "partner_code")) {
#   fore_trad <- area_kick(fore_trad, code = 351, pattern = "China",
#     groups = TRUE, col = col)
#   fore_trad <- area_merge(fore_trad, orig = 62, dest = 238,
#     pattern = "Ethiopia", col = col)
#   fore_trad <- area_merge(fore_trad, orig = 206, dest = 276,
#     pattern = "Sudan", col = col)
#   fore_trad <- area_fix(fore_trad, regions, col = col)
# }
#
# # Cut down to certain products
# fore_trad <- dt_filter(fore_trad, item_code %in%
#   c("Industrial roundwood, coniferous" = 1651,
#     "Industrial roundwood, non-coniferous tropical" = 1657,
#     "Industrial roundwood, non-coniferous non-tropical" = 1670))
# # fore_trad <- dt_filter(fore_trad, unit != "m3")
# # Recode "1000 US$" to "usd"
# fore_trad[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]
#
# fore_trad[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]
#
# # Aggregate to forestry production items
# fore_conc <- fread("inst/conc_forestry.csv")
#
# cat("Aggregating forestry trade items to the level of forestry production.\n")
# item_match <- match(fore_trad[["item_code"]], fore_conc[["trad_item_code"]])
# fore_trad[, `:=`(item_code = fore_conc$prod_item_code[item_match],
#   item = fore_conc$prod_item[item_match])]
# fore_trad <- fore_trad[, list(value = na_sum(value)),
#   by = .(reporter_code, reporter, partner_code, partner,
#     item_code, item, year, imex, unit)]
# cat("Aggregation from", length(item_match), "to",
#   nrow(fore_trad), "observations.\n")
#
# # Store
# saveRDS(fore_trad, "data/tidy/fore_trad_tidy.rds")
# rm(fore_trad, fore_conc, item_match)
#

# Crops -------------------------------------------------------------------

# NOTE: crop and livestock production / trade are no longer reported separately, but in joint datasets
# for CBS: the distinction between crop and livestock is kept in the code, splitting the datasets in the beginning
# for SUA: the distinction is no longer kept


cat("\nTidying crops (cbs and sua) and livestock (sua).\n")

crop_conc <- fread("inst/conc_crop-cbs.csv")


# Production

prod <- readRDS("input/fao/prod.rds")
prod <- dt_rename(prod, rename, drop = TRUE)
prod <- prod[element %in% c("Area harvested","Production", "Yield", "Stocks", "Producing Animals/Slaughtered",                             
                            "Yield/Carcass Weight", "Milk Animals", "Laying", "Import quantity",                                           
                            "Export quantity", "Export value", "Import value"),     ]

# extrapolate production: after 2022, there is decreased data density from reporting gaps
prod_by_year <- prod[, .(n_year = .N), by = "year"]
setorder(prod_by_year, by = year)
prod_by_year

# fill them with simple extrapolation, using the last available value
# step 1:  get items that were reported at least for 20 years, with the last reporting after 2016
prod <- prod[!is.na(value)]
prod_count <- prod[, .(max_year = max(year),  n = .N), by = c("area_code", "area", "item_code", "item", "element", "unit")]
prod_rel <- prod_count[max_year >= 2017 & n > 20, .(area_code, area, item_code, item, element, unit)] # max_year < 2021
prod_extr <- as.data.table(reshape::expand.grid.df(prod_rel, data.table(year = 2018:max(years))))
prod_extr <- prod_extr[paste0(area_code,"_",year) %in% unique(paste0(cbs$area_code,"_",cbs$year)),]
# step 2: extrapolate missing values
prod <- merge(prod, prod_extr, by = names(prod_extr), all = TRUE)
prod[, value_interp := forecast::na.interp(value),
     by=.(area,item,element,unit)]
prod[is.na(value), value := value_interp]
prod[,value_interp := NULL]

# We stopped filtering items and keep all.
# prod <- prod[item_code %in% crop_conc$crop_item_code,]

# Country / Area adjustments
prod <- area_kick(prod, code = 351, pattern = "China", groups = TRUE)
prod <- area_merge(prod, orig = 62, dest = 238, pattern = "Ethiopia")
prod <- area_merge(prod, orig = 206, dest = 276, pattern = "Sudan")
prod <- area_fix(prod, regions)

trad <- readRDS("input/fao/trad.rds")
trad <- dt_rename(trad, rename, drop = TRUE)
trad <- trad[element %in% c("Area harvested","Production", "Yield", "Stocks", "Producing Animals/Slaughtered",                             
                            "Yield/Carcass Weight", "Milk Animals", "Laying", "Import quantity",                                           
                            "Export quantity", "Export value", "Import value"),     ]
# We stopped filtering items and keep all.
# crop_trad <- trad[item_code %in% crop_conc$crop_item_code,]
trad <- unique(trad)
trad <- area_kick(trad, code = 351, pattern = "China", groups = TRUE)
trad <- area_kick(trad, code = 265, pattern = "China*")
trad <- area_merge(trad, orig = 62, dest = 238, pattern = "Ethiopia")
trad <- area_merge(trad, orig = 206, dest = 276, pattern = "Sudan")
trad <- area_fix(trad, regions)


# create dataset with all crop and livestock production and trade data
prod_trad <- rbind(prod, trad)

# change 't' to 'tonnes'
prod_trad[, unit := ifelse(unit=="t", "tonnes", unit)]


## Primary/Fodder ------------
crop_prim <- readRDS("input/fao/crop_prim_14.rds")
crop_prim_19 <- readRDS("input/fao/crop_prim_19.rds")
crop_conc <- fread("inst/conc_crop-cbs.csv")

# bring new fodder data in same format as old one (different code nomenclatures are used)
m49_codes <- fread("inst/m49_codes.csv")
setnames(m49_codes, c("M49 Code", "ISO-alpha3 Code"), c("m49", "iso3c"))
cpc_codes_fodder <- fread("inst/cpc_fcl_fodder.csv")
crop_prim_19[, `:=`(m49 = as.integer(geographicAreaM49), cpc = as.numeric(measuredItemCPC))]
crop_prim_19 <- merge(crop_prim_19, m49_codes[,.(m49, iso3c)], by = "m49")
crop_prim_19 <- merge(crop_prim_19, regions[,.(code, iso3c, name)], by = "iso3c")
crop_prim_19 <- merge(crop_prim_19, cpc_codes_fodder, by = "cpc")
crop_prim_19[,`:=`(unit = gsub(".*[[]([^]]+)[]].*", "\\1", Element), element = sub(" [[].*", "", Element), Year = as.numeric(Year))]
crop_prim_19[element == "Area Harvested", element := "Area harvested"]
crop_prim_19[unit == "t", unit := "tonnes"]
crop_prim_19[, Year := as.numeric(Year)]
setnames(crop_prim_19, c("code", "name", "Value", "Year"), c("area_code", "area", "value", "year"))

# bind
crop_prim <- dt_rename(crop_prim, rename, drop = TRUE)
crop_prim_19 <- crop_prim_19[, names(crop_prim), with=FALSE]
crop_prim <- rbind(crop_prim, crop_prim_19)

# Country / Area adjustments
crop_prim <- crop_prim[!is.na(area_code), ]
crop_prim <- area_kick(crop_prim, code = 351, pattern = "China", groups = TRUE)
crop_prim <- area_merge(crop_prim, orig = 62, dest = 238, pattern = "Ethiopia")
crop_prim <- area_merge(crop_prim, orig = 206, dest = 276, pattern = "Sudan")
crop_prim <- area_fix(crop_prim, regions)
crop_prim <- dt_filter(crop_prim, element != "Yield")

# Only keep fodder crops
crop_prim <- merge(crop_prim[item_code %in% crop_conc[cbs_item_code==2000, crop_item_code]], crop_conc,
                   by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)

# inter/extrapolate:
# get relevant fodder crops and elements for each country
fod_country <- unique(crop_prim[,.(area_code, area, item_code, item, element, unit, crop_item, cbs_item_code, cbs_item, tcf)])
fod_country <- as.data.table(reshape::expand.grid.df(fod_country, data.table(year = years)))
cbs_years <- unique(cbs[,.(area_code, area, year)])
fod_country <- merge(fod_country, cbs_years) # to avoid years for countries that did not exist at the time (e.g. Belgium-Luxembourg)
crop_prim_test <- merge(crop_prim, fod_country, by = names(fod_country), all = TRUE)

# consider country-item-element combinations with only NAs or only one value (no inter/extrapolation possible)
crop_prim[, count := sum(is.finite(value)), by =.(area,item_code,element)]

# interpolate using linear interpolation (if more than 3 values are available for whole time series)
# the interpolation takes moving averages for data gaps between existing values, and takes the last available value as extrapolation for new years
crop_prim[count > 3, value_interp := forecast::na.interp(value),
          by=.(area,item_code,element)]
crop_prim[, value := ifelse(is.na(value), value_interp, value)][, `:=` (count = NULL, value_interp = NULL)]

# save non-aggregated version
saveRDS(crop_prim, "data/tidy/fodder_crop_non_agg_tidy.rds")

# aggregate
crop_prim <- crop_prim[, list(value = na_sum(value)),
                       by = .(area_code, area, element, unit, year, cbs_item_code, cbs_item)]
crop_prim <- dt_rename(crop_prim, drop = FALSE,
                       rename = c("cbs_item_code" = "item_code", "cbs_item" = "item"))
crop_prim <- dt_filter(crop_prim, value >= 0)



# Gap filling livestock products --------------------------------------
cat("\nGap-filling livestock products where stocks or slaughtered animals are 
    reported but no production.\n")

# create table for meat items and live animals to convert slaughtered animals/stocks to meat using averages
conv_meat <-  prod_trad[, .(area, year, element, unit, item, item_code, value)]
conv_meat <- prod_trad[, region := regions$region[match(area, regions$name)]]

# only include live animals and meat
conv_meat <- conv_meat[item_code %in% c(1127, 867, 1017, 977, 1035, 1097, 1141, 947, 1158, 1151, 
                                        1108, 1111, 1058, 1069, 1073, 1080, 1089, 1126, 866, 1016, 
                                        976, 1034, 1096, 1140, 946, 1157, 1150, 1107, 1110, 1057, 
                                        1068, 1072, 1079, 1083)]


# filter necessary elements
conv_meat <- conv_meat[element %in% c("Stocks", "Production", "Producing Animals/Slaughtered"
                                      ,"Yield/Carcass Weight") & (unit == "kg/An" |
                                                                    unit == "An"|
                                                                    unit == "tonnes"|
                                                                    unit == "g/An")]
# Define groups
item_group <- data.table(
  item_code = c(866, 867, 946, 947, 976, 977, 1016, 1017,
                1034, 1035, 1096, 1097, 1107, 1108, 1110, 1111,
                1126, 1127),
  group = c(rep("Cattle", 2),rep("Buffalo", 2), rep("Sheep", 2), rep("Goats", 2),
            rep("Swine / pigs", 2), rep("Horses", 2), rep("Asses", 2),
            rep("Mules and hinnies", 2), rep("Camels", 2)),
  type = rep(c("live animals", "meat"), times = 9))

conv_meat[, group := item_group$group[match(item_code, item_group$item_code)]]

conv_meat <- conv_meat[!is.na(group)]

# create wide table
conv_meat <- dcast(conv_meat, region + year + group + area  ~ element, value.var = "value")

#find average yield (kg/an) to calculate production (tonnes) from number of slaughtered animals
# -> for some data points, slaughtered animals are reported but yield and production 

# take average yield from other years in the same country where available
conv_meat[, average_yield := mean(`Yield/Carcass Weight`, na.rm = TRUE), by = .(
  area, group)]

#take regional average yields where country-specific yields are not available
conv_meat[is.na(average_yield) | is.nan(average_yield), 
          average_yield := mean(`Yield/Carcass Weight`, na.rm = TRUE), 
          by = .(region, group)]

# impute production by multiplying average production with slaughtered animals where possible
conv_meat[`Producing Animals/Slaughtered` > 0 & !is.na(`Producing Animals/Slaughtered`) &
            (Production == 0 | is.na(Production)),
          Production := `Producing Animals/Slaughtered` * average_yield / 10000]
conv_meat[, average_yield := NULL]

# for cases where there are no slaughtered animals reported but only stocks, 
# find regional average conversion factors from animal stocks to meat production
conv_meat[, conversion := Production / Stocks]
conv_meat[conversion == 0 | !is.finite(conversion), conversion := NA] 

# average by region 
conv_meat[, avg_conversion := mean(conversion, na.rm = TRUE), by = .(region, year, group)] 
conv_meat[is.nan(avg_conversion), avg_conversion := NA]

#average globally where no regional average is available
conv_meat[, global_conversion := mean(conversion, na.rm = TRUE), by = .(year, group)]
conv_meat[is.na(avg_conversion), avg_conversion := global_conversion][,
                                                                      `:=` (global_conversion = NULL, conversion = NULL)] 


# apply conversion factor to production values, where stocks are positive and production is 0
conv_meat <- conv_meat[Stocks > 0 & !is.na(Stocks) & (Production == 0 | is.na(Production)),
                       Production := Stocks * avg_conversion]


#match meat items to rows in conv_meat
meat_codes <- item_group[type == "meat"]

# Match by group
conv_meat[, item_code := meat_codes$item_code[match(group, meat_codes$group)]]


#add back to prod_trad
prod_trad <- merge(prod_trad, conv_meat[,.(year, item_code, area, Production)], 
                   by = c("year", "item_code", "area"),all.x = TRUE)
prod_trad[!is.na(Production) & element == "Production", value := Production ]
prod_trad[, Production := NULL]

# # do the same for milk -> easier here, because of milk animal columns
# # NOTE: These reporting gaps no longer exist
# conv_milk <- prod_trad[item_code %in% c(882, 951, 1130 ,1020, 982)]
# conv_milk <- conv_milk[element %in% c("Production", "Milk Animals")]
# conv_milk <- dcast(conv_milk, region + area + item + year ~ element, value.var = "value")
# conv_milk[, conversion := Production / `Milk Animals`]
# conv_milk[!is.finite(conversion) | conversion == 0,] <- NA
# conv_milk[, conversion := mean(conversion, na.rm = TRUE), by = .(region, year, item)]
# conv_milk[is.na(conversion), conversion := mean(conversion, na.rm = TRUE), by = .(year, item)]
# conv_milk <- conv_milk[`Milk Animals` > 0 & !is.na(`Milk Animals`) & (Production == 0 | is.na(Production)),][
#   ,Production := `Milk Animals` * conversion]
# 
# # merge with prod_trad
# prod_trad <- merge(prod_trad, conv_milk[, .(area, year, group, Production)], 
#               by = c("area", "year", "group"), all.x = TRUE)
# prod_trad <- prod_trad[!is.na(Production) & element == "Production", value := Production ]
# prod_trad[, `:=`(Production = NULL, group =NULL, region = NULL)]
# setcolorder(prod_trad, c("area_code", "area", "element", "year", "unit", "item_code",
#                     "item","value"))

prod_trad[, region := NULL]

# save before converting into primary equivalents
saveRDS(rbind(prod_trad, crop_prim), "data/tidy/prod_trad_full.rds")


# Build crop dataset
crop <- merge(prod_trad, unique(crop_conc[,.(crop_item_code, cbs_item_code, cbs_item, tcf)]),
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
crop <- tcf_apply(crop, fun = `*`, na.rm = TRUE)

crop <- crop[, list(value = na_sum(value)),
             by = .(area_code, area, element, year, unit, cbs_item_code, cbs_item)]
crop <- dt_rename(crop, drop = FALSE,
                  rename = c("cbs_item_code" = "item_code", "cbs_item" = "item"))
crop <- dt_filter(crop, value >= 0)


# Bind all parts & store
saveRDS(rbind(crop, crop_prim), "data/tidy/crop_tidy.rds")
rm(crop, crop_prim, crop_conc, cbs)


source("R/00_prep_functions.R")
path_fao <- "input/fao/"
# Land use --------------------------------------------------------------
# get FAO land use data
file <- c("land" = "Inputs_LandUse_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")

# TODO: what is this error? (Currently no solution, can be ignored)
fa_extract(path_in = path_fao, files = file, path_out=path_fao, 
           name = names(file))
land <- fread(paste0(path_fao,"Inputs_LandUse_E_All_Data_(Normalized).csv"))

# rename and filter
land <- dt_rename(land, rename = rename, drop = TRUE)
land <- land[ element == "Area",]

# clean up areas
land <- area_fix(land, regions = regions)  
#land <- land[area_code < 420]
land <- area_kick(land, code = 351, pattern = "China", groups = TRUE)
land <- area_merge(land, orig = 206, dest = 276, pattern = "Sudan")
land[, iso3c := regions$iso3c[match(area, regions$name)]]


saveRDS(land, "data/tidy/land_tidy.rds")

rm(land)

# Fertilizers --------------------------------------------------------------
# get FAO fertilizer data
file <- c("fert" = "Inputs_FertilizersNutrient_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")

# TODO: what is this error? (Currently no solution, can be ignored)
fa_extract(path_in = path_fao, files = file, path_out=path_fao, 
           name = names(file))
fert <- fread(paste0(path_fao,"Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv"))

# rename and filter

fert <- dt_rename(fert, rename = rename, drop = TRUE)
fert <- fert[year %in% years & element %in% "Agricultural Use",]

# clean up areas
fert <- area_fix(fert, regions = regions)
fert <- area_kick(fert, code = 351, pattern = "China", groups = TRUE)
fert <- area_merge(fert, orig = 206, dest = 276, pattern = "Sudan")
fert[, iso3c := regions$iso3c[match(area, regions$name)]]

#convert to kg
fert[, value := value * 1000]    

saveRDS(fert, "data/tidy/fert_tidy.rds")

rm(fert)

# Crop Nutrient Balances ---------------------------------------------------
#get files from FAO -> TODO: this should be in prep_FAO
file <- c("Environment_Cropland_nutrient_budget_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
cnb <- fread(paste0(path_fao,"Environment_Cropland_nutrient_budget_E_All_Data_(Normalized).csv"))

#clean up
cnb <- dt_rename(cnb, rename = rename, drop = TRUE)
cnb <- cnb[year %in% years, ]
cnb <- area_fix(cnb, regions = regions)
cnb <- area_kick(cnb, code = 351, pattern = "China", groups = TRUE)
cnb <- area_merge(cnb, orig = 206, dest = 276, pattern = "Sudan")
cnb[, iso3c := regions$iso3c[match(area, regions$name)]]

# aggregate countries not in fabio to RoW
cnb[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
cnb_sum <- cnb[unit == "t", .(area = unique(area), unit= unique(unit), 
                              value = sum(value, na.rm = TRUE)), 
               by = .(iso3c, item, year, element)]
cnb_mean <- cnb[unit %in% c("kg/ha" , "%"), .(area = unique(area), unit = unique(unit),
                                              value = mean(value, na.rm = TRUE)), 
                by = .(iso3c, item, year, element)]
cnb <- bind_rows(cnb_sum, cnb_mean)


# save
saveRDS(cnb, "data/tidy/cnb_tidy.rds")

rm(cnb, cnb_mean, cnb_sum)


# Manure -------------------------------------------------------------------
#get files from FAO
file <- c("Environment_LivestockManure_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
manure <- fread(paste0(path_fao,"Environment_LivestockManure_E_All_Data_(Normalized).csv"))

#clean up
manure <- dt_rename(manure, rename = rename, drop = TRUE)
manure <- manure[year %in% years, ]
manure <- area_fix(manure, regions = regions)
manure <- area_kick(manure, code = 351, pattern = "China", groups = TRUE)
manure <- area_merge(manure, orig = 206, dest = 276, pattern = "Sudan")
manure[, iso3c := regions$iso3c[match(area, regions$name)]]

# exclude stocks and duplicate categories
manure <- manure[element != "Stocks" & !item_code %in% c(1048, 1054, 1126, 
                                                         1177, 1749, 1755,  
                                                         1757, 1759, 2029)]

# aggregate countries not in fabio to RoW
manure[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
manure <- manure[, .(area = unique(area), area_code = unique(area_code),
                     item_code = unique(item_code), unit = unique(unit),
                     value = sum(value, na.rm = TRUE)), 
                 by = .(iso3c, item, year, element)]

# save
saveRDS(manure, "data/tidy/manure_tidy.rds")

# Emissions from crops -----------------------------------------------------------
#get files from FAO
file <- c("Emissions_crops_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
crop_emissions <- fread(paste0(path_fao,"Emissions_crops_E_All_Data_(Normalized).csv"))

#clean up
crop_emissions <- crop_emissions[ `Source Code` == 3050] # only FAO tier 1 emissions
crop_emissions <- dt_rename(crop_emissions, rename = rename, drop = TRUE)
crop_emissions <- crop_emissions[year %in% years, ]
crop_emissions <- area_fix(crop_emissions, regions = regions)
crop_emissions <- area_kick(crop_emissions, code = 351, pattern = "China", groups = TRUE)
crop_emissions <- area_merge(crop_emissions, orig = 206, dest = 276, pattern = "Sudan")
crop_emissions[, iso3c := regions$iso3c[match(area, regions$name)]]


# aggregate countries not in fabio to RoW
crop_emissions[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
crop_emissions <- crop_emissions[, .(area = unique(area), unit = unique(unit), 
                                     item_code = unique(item_code),
                                     value = sum(value, na.rm = TRUE)), 
                                 by = .(iso3c, item, year, element)]

# replace crop items with cbs items
conc <- fread("inst/conc_crop-cbs.csv")
crop_emissions[, `:=` (item_cbs = conc$cbs_item[match(item_code, conc$crop_item_code)],
                       item_code_cbs = conc$cbs_item_code[match(item_code, conc$crop_item_code)])]
crop_emissions[!is.na(item_cbs), `:=` (item = item_cbs, item_code = item_code_cbs)][
  , `:=` (item_cbs = NULL, item_code_cbs = NULL)]


# save
saveRDS(crop_emissions, "data/tidy/crop_emissions_tidy.rds")


# Emissions from drained organic soils ----------------------------------------
#get files from FAO
file <- c("Emissions_Drained_Organic_Soils_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
drain_emissions <- fread(paste0(path_fao,"Emissions_Drained_Organic_Soils_E_All_Data_(Normalized).csv"))

#clean up
drain_emissions <- drain_emissions[ `Source Code` == 3050] # only FAO tier 1 emissions
drain_emissions <- dt_rename(drain_emissions, rename = rename, drop = TRUE)
drain_emissions <- drain_emissions[year %in% years, ]
drain_emissions <- area_fix(drain_emissions, regions = regions)
drain_emissions <- area_kick(drain_emissions, code = 351, pattern = "China", groups = TRUE)
drain_emissions <- area_merge(drain_emissions, orig = 206, dest = 276, pattern = "Sudan")
drain_emissions[, iso3c := regions$iso3c[match(area, regions$name)]]


# aggregate countries not in fabio to RoW
drain_emissions[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
drain_emissions <- drain_emissions[, .(area = unique(area), unit= unique(unit), 
                                       item_code = unique(item_code),
                                       value = sum(value, na.rm = TRUE)), 
                                   by = .(iso3c, item, year, element)]

# save
saveRDS(drain_emissions, "data/tidy/drain_emissions_tidy.rds")

# Emissions from manure ----------------------------------------
#get files from FAO
file <- c("Emissions_livestock_E_All_Data_(Normalized).zip")
fa_dl(file = file, path = path_fao, link = "https://bulks-faostat.fao.org/production/")
fa_extract(path_in = path_fao, file = file, path_out=path_fao, name = names(file))
manure_emissions <- fread(paste0(path_fao,"Emissions_livestock_E_All_Data_(Normalized).csv"))

#clean up
manure_emissions <- manure_emissions[ `Source Code` == 3050] # only FAO tier 1 emissions
manure_emissions <- dt_rename(manure_emissions, rename = rename, drop = TRUE)
manure_emissions <- manure_emissions[year %in% years, ]
manure_emissions <- area_fix(manure_emissions, regions = regions)
manure_emissions <- area_kick(manure_emissions, code = 351, pattern = "China", groups = TRUE)
manure_emissions <- area_merge(manure_emissions, orig = 206, dest = 276, pattern = "Sudan")
manure_emissions[, iso3c := regions$iso3c[match(area, regions$name)]]


# aggregate countries not in fabio to RoW
manure_emissions[is.na(iso3c), `:=` (iso3c = "ROW", area = "RoW", area_code = 999)]
manure_emissions <- manure_emissions[, .(area = unique(area), unit = unique(unit),
                                         item_code = unique(item_code),
                                         value = sum(value, na.rm = TRUE)), 
                                     by = .(iso3c, item, year, element)]

# save
saveRDS(manure_emissions, "data/tidy/manure_emissions_tidy.rds")

# Livestock ---------------------------------------------------------------

cat("\nTidying livestock for cbs.\n")

live_conc <- fread("inst/conc_live-cbs.csv")

# aggregate chickens, turkeys, etc. into poultry
live <- prod[item_code %in% live_conc$live_item_code, ]
live_trad <- trad[item_code %in% live_conc$live_item_code,]

# live <- live[item_code != 1808,] # the Meat, poultry category is incomplete after 2017
# live[item_code %in% c(1058, 1069, 1080, 1084) , item_code := 1808]
# live_trad[item_code %in% c(1057, 1068, 1079, 1083) , item_code := 2029]

live <- rbind(live, live_trad)

# Country / Area adjustments
live <- area_kick(live, code = 351, pattern = "China", groups = TRUE)
live <- area_kick(live, code = 265, groups = FALSE)
live <- area_merge(live, orig = 62, dest = 238, pattern = "Ethiopia")
live <- area_merge(live, orig = 206, dest = 276, pattern = "Sudan")
live <- area_fix(live, regions)

live <- merge(live, live_conc,
              by.x = "item_code", by.y = "live_item_code", all.x = TRUE)
live <- dt_filter(live, !is.na(cbs_item_code))

# Aggregate
live <- live[, list(value = na_sum(value)),
             by = .(area_code, area, element, year, unit, cbs_item_code, cbs_item)]
live <- dt_rename(live, drop = FALSE,
                  rename = c("cbs_item_code" = "item_code", "cbs_item" = "item"))
live <- dt_filter(live, value >= 0)

# Recode units
# live[unit %in% c("1000 Head", "1000 An"), `:=`(value = value * 1000, unit = "An")]
# live[unit %in% c("Head", "An"), `:=`(unit = "An")]
live[unit %in% c("1000 US$", "1000 USD"), `:=`(value = value * 1000, unit = "usd")]
live[unit == "t", `:=`(unit = "tonnes")]

saveRDS(live, "data/tidy/live_tidy.rds")


# Prices -------------------------------------------------------------------

cat("\nTidying prices.\n")

crop_conc <- fread("inst/conc_crop-cbs.csv")
regions <- fread("inst/regions_full.csv")

prices <- readRDS("input/fao/prices.rds")
prices <- dt_rename(prices, rename, drop = TRUE)

# Country / Area adjustments
prices <- area_kick(prices, code = 351, pattern = "China", groups = TRUE)
prices <- area_merge(prices, orig = 62, dest = 238, pattern = "Ethiopia")
prices <- area_merge(prices, orig = 206, dest = 276, pattern = "Sudan")
prices <- area_fix(prices, regions)

prices <- merge(prices, crop_conc[, .(crop_item_code, cbs_item_code, cbs_item, tcf)],
                by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)

# Store
saveRDS(prices, "data/tidy/prices_tidy.rds")
rm(prices, crop_conc)


# Technical conversion factors -------------------------------------------------------------------

cat("\nTidying tcfs.\n")
tcf <- fread("inst/sua/tcf_rates_sua.csv") 

# filter out unneeded items and columns 
tcf <- tcf[!variable %in% c("seeding rates", "hatching eggs", "waste")]
tcf <- tcf[, .(country_tcf, item_tcf, item_group, variable, 
               value, unit)]

# average values for small island states for imputing micronesia and marshall islands later
# (which are in SUAs but not in TCFs)
tcf[country_tcf %in% c("american samoa", "cook islands", "guam", "niue"),
    small_island := TRUE]
tcf[small_island == TRUE, `:=`(value = mean(value, na.rm = TRUE),
                               country_tcf = "small pacific islands"), 
    by = .(item_tcf, variable)][,small_island := NULL]
tcf <- unique(tcf, by = c("country_tcf", "item_tcf", "variable"))


# country concordance with current SUAs
country_conc_sua_tcf <-fread("inst/sua/conc_country_sua_tcf.csv")
tcf[, country_sua := country_conc_sua_tcf$country_sua[match(country_tcf, country_conc_sua_tcf$country_tcf)]]

#item concordance with current suas
sua <- readRDS("data/tidy/sua_tidy.rds")
sua_conc <- fread("inst/sua/conc_sua_tcf.csv")
sua_conc[, names(sua_conc) := lapply(.SD, function(x) fifelse(x == "", NA, x))]
tcf[,item_sua := sua_conc$sua[match(item_tcf, sua_conc$tcf)]]

# creating "full" data table for gap-filling -> items that are in SUAs but not in TCAs
# are not included yet

# get unique item/variable/unit combinations
unique_combinations <- unique(tcf[, .(item_sua, variable, unit)])
unique_combinations <- unique_combinations[!is.na(item_sua)][, join_key := 1]
unique_countries <- unique(country_conc_sua_tcf[, .(country_sua)][, join_key := 1])

# Repeat the combinations of item_sua, variable, and unit for all countries
tcf_full <- merge(unique_combinations, unique_countries, by = "join_key", allow.cartesian = TRUE)
tcf_full[, join_key := NULL]

# add available tcf data to full table
tcf_full <- merge(tcf_full, tcf[ ,. (country_sua, item_sua, unit, variable, value)], 
                  by = c("country_sua", "item_sua", "variable", "unit"), all.x = TRUE)

tcf_full[] <- lapply(tcf_full, function(x) {
  if (is.character(x)) iconv(x, from = "", to = "UTF-8", sub = "byte") else x
})

#store
saveRDS(tcf_full, "data/tidy/tcf_sua_tidy.rds")


# Fish --------------------------------------------------------------------

cat("\nTidying fish.\n")

fish <- readRDS("input/fao/fish_prod.rds")
fish <- dt_rename(fish, rename, drop = TRUE)

fish[, source := ifelse(source_code == "CAPTURE", "Capture", "Aquaculture")] # see "CL_FI_PRODUCTION_SOURCE_DET.csv" in the "GlobalProduction_2022.1.0.zip" folder
fish[, unit := ifelse(unit == "Q_tlw", "t", "no")]

# Country / Area adjustments
country_match <- match(fish[["country"]], regions[["fish"]])
fish[, `:=`(area = regions$name[country_match],
            area_code = regions$code[country_match], country = NULL)]

fish <- dt_filter(fish, !is.na(area))

fish <- area_kick(fish, code = 351, pattern = "China", groups = TRUE)
fish <- area_merge(fish, orig = 62, dest = 238, pattern = "Ethiopia")
fish <- area_merge(fish, orig = 206, dest = 276, pattern = "Sudan")
fish <- area_fix(fish, regions)

# Store
saveRDS(fish, "data/tidy/fish_tidy.rds")
rm(fish, country_match)
