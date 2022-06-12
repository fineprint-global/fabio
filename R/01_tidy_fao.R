
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


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
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  #"Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses (non-food)" = "other", # Other uses
  "Processing" = "processing",
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

#cbs_v2 <- rbind(readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/cbs_crop.rds"),
#   readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/cbs_live.rds"))

# food: filter post-2010 values from old balances and transform to tonnes
cbs_food_old <- readRDS("input/fao/cbs_food_old.rds")[Year < 2010,]
cbs_food_old[, `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]
cbs_food_new <- readRDS("input/fao/cbs_food_new.rds")
cbs_food_new[, `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]

# nonfood: remove items contained in food balances
cbs_nonfood <- readRDS("input/fao/cbs_nonfood.rds")
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]

cbs_nonfood <- merge(cbs_nonfood, cbs_food_old[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food_old"))
cbs_nonfood <- merge(cbs_nonfood, cbs_food_new[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food_new"))
cbs_nonfood <- cbs_nonfood[is.na(Value.food_old) & is.na(Value.food_new) ,]
cbs_nonfood <- cbs_nonfood[,`:=` (Value.food_old = NULL, Value.food_new = NULL )]


# bind
cbs <- rbind(cbs_food_old, cbs_food_new, cbs_nonfood)
#rm(cbs_nonfood, cbs_food_old, cbs_food_new)
cbs <- dt_rename(cbs, rename, drop = TRUE)

# transform items that changed from old to new FBS method
# "Groundnuts (Shelled Eq)" in "Groundnuts"
cbs[item == "Groundnuts (Shelled Eq)", `:=` (item_code = 2552, item = "Groundnuts", value = 1/0.7 * value)]
# “Rice (milled equivalent)” into "Rice and products" via TCF
cbs[item == "Rice (Milled Equivalent)", `:=` (item_code = 2807, item = "Rice and products", value = 1/0.67 * value)]
# Note: Sugar (Raw Equivalent) was also present in old FBS

# aggregate tourist consumption and residuals into other uses and drop unused elements
cbs[element %in% c("Tourist consumption"), element := "Other uses (non-food)"]
cbs <- cbs[,.(value = sum(value)), by = setdiff(names(cbs), "value")]
cbs <- cbs[! element %in% c("Food supply (kcal/capita/day)",
                            "Food supply quantity (kg/capita/yr)",
                            "Fat supply quantity (g/capita/day)",
                            "Protein supply quantity (g/capita/day)",
                            "Total Population - Both sexes")]

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

cat("Recoding 'total_supply' from",
    "'production + imports - exports + stock_withdrawal'", "to",
    "'production + imports'.\n")
cbs[, total_supply := na_sum(production, imports)]

# Add more intuitive 'stock_addition' and fix discrepancies with 'total_supply'
cbs[, stock_addition := -stock_withdrawal]
cat("Found ", cbs[stock_addition > total_supply, .N],
    " occurences of 'stock_addition' exceeding 'total_supply'.\n",
    "Keeping values as is.\n", sep = "")
# cbs[stock_addition > total_supply, stock_addition := total_supply]

# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
                          -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other)] # tourist
# remove residuals, which are now contained in balancing
cbs[, residuals := NULL]

# rename 2 items in order to have identical name throughout the FAOSTAT data domains
cbs[, item := ifelse(item_code==2605,	"Vegetables, Other",
                     ifelse(item_code==2625, "Fruits, Other", item))]

# TODO: imbalances in the data need to be double-checked! there are a lot of them!

# Store
saveRDS(cbs, "data/tidy/cbs_tidy.rds")


# SUA (not used yet) ---------------------------------------------------------------------

sua <- readRDS("input/fao/sua.rds")
sua <- dt_rename(sua, rename = rename)

# we only use molasses for now
sua <- sua[item == "Molasses",]
sua[, item_code := as.numeric(item_code)]

# Store
saveRDS(sua, "data/tidy/sua_tidy.rds")
rm(sua)


# BTD ---------------------------------------------------------------------

cat("\nTidying BTD.\n")

btd <- readRDS("input/fao/btd_prod.rds")
#btd_v2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/btd_prod.rds")

btd <- dt_rename(btd, rename, drop = TRUE)


# Country / Area adjustments
for(col in c("reporter_code", "partner_code")) {
  btd <- area_kick(btd, code = 351, pattern = "China", groups = TRUE, col = col)
  btd <- area_merge(btd, orig = 62, dest = 238, pattern = "Ethiopia", col = col)
  btd <- area_merge(btd, orig = 206, dest = 276, pattern = "Sudan", col = col)
  btd <- area_fix(btd, regions, col = col)
}

# Cut down on the size / remove items not used
btd <- dt_filter(btd, !item_code %in% c("Waters,ice etc" = 631,
                                        "Cotton waste" = 769, "Vitamins" = 853, "Hair, goat, coarse" = 1031,
                                        "Beehives" = 1181, "Beeswax" = 1183, "Hair, fine" = 1218,
                                        "Crude materials" = 1293, "Waxes vegetable" = 1296,
                                        "Hides, horse, dry salted" = 1104,
                                        "Hides, camel, wet salted" = 1134,
                                        "Hides and skins nes, fresh" =	1213,
                                        "Hides nes" =	1216
))

btd <- dt_filter(btd, value >= 0)

btd[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

# Apply TCF to observations with 'unit' == "tonnes"
btd <- merge(btd, fread("inst/tcf_btd.csv"),
             by = "item_code", all.x = TRUE)
cat("Applying TCF to trade data, where `unit == 'tonnes'` applies.\n")
btd[unit != "tonnes", tcf := 1]
btd <- tcf_apply(btd, na.rm = FALSE, filler = 1, fun = `/`)

# Aggregate to CBS items
btd_conc <- fread("inst/conc_btd-cbs.csv")

cat("Aggregating BTD items to the level of CBS.\n")
item_match <- match(btd[["item_code"]], btd_conc[["btd_item_code"]])
btd[, `:=`(item_code = btd_conc$cbs_item_code[item_match],
           item = btd_conc$cbs_item[item_match])]
# remove items not included in btd_conc (mainly food wastes and by-products for feed)
btd <- btd[!is.na(item_code)]
btd <- btd[, list(value = na_sum(value)), by = .(reporter_code, reporter,
                                                 partner_code, partner, item_code, item, year, imex, unit)]
cat("Aggregation from", length(item_match), "to", nrow(btd), "observations.\n")

# Recode "1000 Head" to "head"
btd[unit == "1000 Head", `:=`(value = value * 1000, unit = "Head")]
btd[unit == "Head", `:=`(unit = "head")]
# Recode "1000 US$" to "usd"
btd[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

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
#
# Crops -------------------------------------------------------------------

# NOTE: crop and livestock production / trade are no longer reported separately, but in joint datasets
# nevertheless. the distinction between crop and livestock is kept in the code, splitting the datasets in the beginning

cat("\nTidying crops.\n")

crop_conc <- fread("inst/conc_crop-cbs.csv")

#
# Production

#crop_v2 <- rbind(readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/crop_prod.rds"),
#                 readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/crop_proc.rds"))

prod <- readRDS("input/fao/prod.rds")
prod <- dt_rename(prod, rename, drop = TRUE)
crop <- prod[item_code %in% crop_conc$crop_item_code,]
crop <- unique(crop)

# Country / Area adjustments
crop <- area_kick(crop, code = 351, pattern = "China", groups = TRUE)
crop <- area_merge(crop, orig = 62, dest = 238, pattern = "Ethiopia")
crop <- area_merge(crop, orig = 206, dest = 276, pattern = "Sudan")
crop <- area_fix(crop, regions)

trad <- readRDS("input/fao/trad.rds")
trad <- dt_rename(trad, rename, drop = TRUE)
crop_trad <- trad[item_code %in% crop_conc$crop_item_code,]
crop_trad <- unique(crop_trad)
crop_trad <- area_kick(crop_trad, code = 351, pattern = "China", groups = TRUE)
crop_trad <- area_kick(crop_trad, code = 265, pattern = "China*")
crop_trad <- area_merge(crop_trad, orig = 62, dest = 238, pattern = "Ethiopia")
crop_trad <- area_merge(crop_trad, orig = 206, dest = 276, pattern = "Sudan")
crop_trad <- area_fix(crop_trad, regions)

crop <- rbind(crop, crop_trad)

# save before converting into primary equivalents
saveRDS(crop, "data/tidy/crop_full.rds")

crop <- merge(crop, crop_conc,
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
crop <- tcf_apply(crop, fun = `*`, na.rm = TRUE)

# Aggregate
crop <- crop[, list(value = na_sum(value)),
             by = .(area_code, area, element, year, unit, cbs_item_code, cbs_item)]
crop <- dt_rename(crop, drop = FALSE,
                  rename = c("cbs_item_code" = "item_code", "cbs_item" = "item"))
crop <- dt_filter(crop, value >= 0)

#
# Primary
#crop_prim <- readRDS("input/fao/crop_prim.rds")
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
crop_prim <- merge(crop_prim, crop_conc,
                   by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
crop_prim <- dt_filter(crop_prim, cbs_item_code == 2000)
crop_prim[, item := crop_item]

# inter/extrapolate:
# get relevant fodder crops and elements for each country
fod_country <- unique(crop_prim[,.(area_code, area, item_code, item, element, unit, crop_item, cbs_item_code, cbs_item, tcf)])
fod_country <- as.data.table(reshape::expand.grid.df(fod_country, data.table(year = 1986:2019)))
cbs_years <- unique(cbs[,.(area_code, area, year)])
fod_country <- merge(fod_country, cbs_years)
crop_prim <- merge(crop_prim, fod_country, by = names(fod_country), all = TRUE)

# consider country-item-element combinations with only NAs or only one value (no inter/extrapolation possible)
crop_prim[, count := sum(is.finite(value)), by =.(area,item,element)]

# interpolate using linear interpolation (if more than 3 values are available for whole time series)
crop_prim[count > 3, value_interp := forecast::na.interp(value),
          by=.(area,item,element)
]
crop_prim[, value := ifelse(is.na(value), value_interp, value)][, `:=` (count = NULL, value_interp = NULL)]

# aggregate
crop_prim <- crop_prim[, list(value = na_sum(value)),
                       by = .(area_code, area, element, unit, year, cbs_item_code, cbs_item)]
crop_prim <- dt_rename(crop_prim, drop = FALSE,
                       rename = c("cbs_item_code" = "item_code", "cbs_item" = "item"))
crop_prim <- dt_filter(crop_prim, value >= 0)

#
# Bind all parts & store
saveRDS(rbind(crop, crop_prim), "data/tidy/crop_tidy.rds")
rm(crop, crop_prim, crop_conc, cbs)


# Livestock ---------------------------------------------------------------

cat("\nTidying livestocks.\n")

live_conc <- fread("inst/conc_live-cbs.csv")

#live_trad_v2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/live_trad.rds")
#live_trad_v2 <- live_trad_v2[`Item Code` %in% live_conc$live_item_code,]

live_trad <- trad[item_code %in% live_conc$live_item_code,]
# aggregate chickens, turkeys, etc. into poultry
live_trad[item_code %in% c(1057, 1068, 1079, 1083) , item_code := 2029]

live <- prod[item_code %in% live_conc$live_item_code, ]
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

# Recode "1000 Head" to "head"
live[unit == "1000 Head", `:=`(value = value * 1000, unit = "Head")]
live[unit == "Head", `:=`(unit = "head")]
# Recode "1000 US$" to "usd"
live[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]


# Store
saveRDS(live, "data/tidy/live_tidy.rds")
rm(live, live_conc)


# Prices -------------------------------------------------------------------

cat("\nTidying prices.\n")

crop_conc <- fread("inst/conc_crop-cbs.csv")

#prices_v2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/prices.rds")
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


# Fish --------------------------------------------------------------------

cat("\nTidying fish.\n")

#fish_v2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/fao/fish_prod.rds")
fish <- readRDS("input/fao/fish_prod.rds")
fish <- dt_rename(fish, rename, drop = TRUE)

#fish_v2[, source := ifelse(source_code == 4, "Capture", "Aquaculture")]
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
