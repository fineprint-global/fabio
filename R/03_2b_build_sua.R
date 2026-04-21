library(data.table)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items_sua <- fread("inst/sua/items_sua.csv")
use_structure <- fread("inst/sua/use_structure_sua.csv")
prod_trad <- readRDS("data/tidy/prod_trad_full.rds")

#TODO: domestic supply should be equal to production, not production + stock withdrawal
# instead: stock addition as final demand

# SUA ---------------------------------------------------------------------

cat("\nBuilding full SUA.\n")

sua <- readRDS("data/tidy/sua_tidy.rds")

# Add CBS data ---------------------------------------
cbs <- readRDS("data/tidy/cbs_tidy.rds")
cbs <- cbs[year %in% years & !item %in% sua$item & item %in% items_sua$item]

sua <- rbind(sua, cbs)


# Prepare BTD data --------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/tidy/btd_sua_tidy.rds")
btd <- btd[year %in% years, ]

# Adapt units for small animals
# TODO: What's up here? -> no need to change anything anymore after Martin's update, but this code does not seem right
btd[item_code %in% prod_trad[unit == "1000 An" & element == "Stocks", unique(item_code)],
                                 `:=` (value = value/1000 , unit = "1000 head")]


cat("\nGiving preference to units in the following order:\n",
    "\t 'head' > 'tonnes'\n", "Dropping 'usd'.\n", sep = "")

# Imports
imps <- btd[!unit %in% c("usd", "No") , list(value = na_sum(value)),
            by = list(to_code, to, item_code, item, year, unit)]
imps[unit == "Head", unit := "head"]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
                          value.var = "value")
imps[, value := ifelse(!is.na(head), head,
                       ifelse(!is.na(`1000 head`), `1000 head`, t))][, `:=` 
                        (head = NULL, `1000 head`= NULL, t = NULL)]
imps$item <- iconv(imps$item, from = "", to = "UTF-8")

# Exports
exps <- btd[!unit %in% c("usd", "No") , list(value = na_sum(value)),
            by = list(from_code, from, item_code, item, year, unit)]
exps[ unit == "Head", unit := "head"]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
                          value.var = "value")
exps[, value := ifelse(!is.na(head), head,
                       ifelse(!is.na(`1000 head`), `1000 head`, t))][, `:=` 
                     (head = NULL, `1000 head`= NULL, t = NULL)]
exps$item <- iconv(exps$item, from = "", to = "UTF-8")


# Add live animals -------------------------

# NOTE: this creates sua for live animals not contained in sua

cat("\nFilling missing livestock data.\n")
tgt_item <- c(1126, 866, 1016, 976, 1034, 1096, 1140,
              946, 1157, 1150, 1107, 1110, 1057, 1068, 1072, 1079, 1083)
tgt_name <- c("Camels", "Cattle", "Goats", "Sheep", "Swine / pigs",
              "Horses", "Rabbits and hares", "Buffalo", "Other camelids",
              "Other rodents", "Asses", "Mules and hinnies", "Chickens", "Ducks", "Geese",
              "Turkeys", "Other birds")

# Map "Meat, ..." items to SUA items
src_item <- c(1127, 867, 1017, 977, 1035, 1097, 1141,
              947, 1158, 1151, 1108, 1111, 1058, 1069, 1073, 1080, 1089)

live <- prod_trad[year %in% years & element == "Producing Animals/Slaughtered" , ]

conc <- match(live$item_code, src_item)
live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live <- live[!is.na(item_code), ]

# Add trade data
live_trad <- prod_trad[year %in% years,]

live_imp <- live_trad[element == "Import quantity" & (unit == "An" | unit == "1000 An"), ] 
live_exp <- live_trad[element == "Export quantity" & (unit == "An" | unit == "1000 An"), ]

live <- merge(live[, .(area_code, area, item_code, item, year, production = value)],
              live_imp[, .(area_code, area, item_code, item, year, imports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all.x = TRUE)
live <- merge(live,
              live_exp[, .(area_code, area, item_code, item, year, exports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all.x = TRUE)

# add trade values from btd in case they are missing in live_trad
live <- merge(
  live,
  imps[item_code %in% unique(live[, item_code]) & value > 0,  # these are in heads as well
       c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
live[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
live <- merge(
  live,
  exps[item_code %in% live[, item_code] & value > 0,
       c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
live[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]

# estimate livestock raised in a country (slaughtered + exported - imported)
live[, production := na_sum(production, exports, -imports)]
live[production < 0, production := 0]

# compute total supply
live[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
live[, exports := ifelse(exports > total_supply, total_supply, exports)]
# all uses are assumed to go to processing
live[, processing := na_sum(production, imports, -exports)]
#live <- live[total_supply > 0, ]
setnames(live, "item_code", "item_code")

# add live animals to sua 
sua <- dplyr::bind_rows(sua, live)
sua[, names(sua) := lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Fill production where missing -------------------------------------

cat("\nAdding production data.\n")
# prod_trad <- readRDS("data/tidy/prod_trad_full.rds")
prod_trad[, c("item", "area") := lapply(.SD, iconv, from ="", to = "UTF-8"), .SDcols = c("item", "area")]

# filter relevant data points
prod_trad <- prod_trad[year %in% years, ]
prod <- prod_trad[element == "Production" & unit == "tonnes", ]
prod[, `:=`(element = NULL, unit = NULL)]
setkey(prod, year, area_code, item_code)

prod <- prod[item_code %in% items_sua$item_code]

# rename
setnames(prod, "item_code", "item_code")

# Add production to SUA where is.na or is zero ---
sua <- merge(sua, prod,
             by = c("area_code", "area", "item_code", "item", "year"), all.x = TRUE)
cat("\nFilling missing sua production with production data. Items:\n",
    paste0(unique(sua[(is.na(production) | production == 0) & !is.na(value), item]), collapse = "; "),
    ".\n", sep = "")
sua[!is.na(value) & (is.na(production) | production == 0), production := value]
sua[, value := NULL]


# Add rows for item country combinations that are missing completely ---------------------
# Filter datapoints that are not yet in SUA
addsua <- dt_filter(prod,
                    !paste(area_code,item_code,year) %in% paste(sua$area_code,sua$item_code,sua$year))
addsua[, names(addsua) := lapply(.SD, function(x) if (is.character(x)) iconv(x, from = "", to = "UTF-8") else x)]
setnames(addsua, "value", "production")

# Add imports and exports to addsua 
# seed cotton from BACI? ( item code 520210)
addsua <- merge(addsua, prod_trad[element == "Import Quantity", .(year, area_code, item_code = item_code, imports = value)],
                by = c("area_code", "item_code", "year"), all.x = TRUE)
addsua <- merge(addsua, prod_trad[element == "Export Quantity", .(year, area_code, item_code = item_code, exports = value)],
                by = c("area_code", "item_code", "year"), all.x = TRUE)


# add missing imports and exports from btd
addsua[, exports_btd := exps$value[match(paste(addsua$year, addsua$area_code, addsua$item_code),
                                     paste(exps$year, exps$from_code, exps$item_code))]]
addsua[is.na(exports), exports := exports_btd]

addsua[, imports_btd := imps$value[match(paste(addsua$year, addsua$area_code, addsua$item_code),
                                     paste(imps$year, imps$to_code, imps$item_code))]]
addsua[is.na(imports), imports := imports_btd]

# set missing values to 0 
addsua[is.na(imports), imports := 0]
addsua[is.na(exports), exports := 0]
addsua[, `:=` (imports_btd = NULL, exports_btd = NULL)]

# add total supply
addsua[, total_supply := na_sum(production, imports)]

# find cases where exports exceed total supply and reduce exports
addsua[exports > total_supply, exports := total_supply]

# create sua table with gap-filled production to use as input for processing
sua_prelim <- rbindlist(list(sua, addsua), fill = TRUE)

# base processing on production of processed (child) items and conversion factors 
# Technical conversion factors to impute processing ---
tcf_crop_sua <- readRDS("data/sua/tcf_sua_final.rds") 
proc_sua <- fread("inst/sua/proc_sua.csv")


# only keep rows with parent items that are needed in addsua 
tcf_crop_sua[, parent_code_fcl := items_sua$item_code[match(parent, items_sua$item)]]
tcf_crop_sua <- tcf_crop_sua[paste(area, parent_code_fcl) %in% paste(addsua$area, addsua$item_code)]

# only keep needed columns
tcf_crop_sua[, `:=` (proc = NULL, proc_code = NULL, child_code = NULL, parent_code = NULL,
                     min = NULL, max = NULL)]

# add years and child production from sua_prelim
tcf_crop_sua[, child_code_fcl := items_sua$item_code[match(child, items_sua$item)]]
tcf_crop_sua <- merge(sua_prelim[paste(area, item_code) %in% paste(tcf_crop_sua$area, tcf_crop_sua$child_code_fcl),
                             .(area, year, item_code, child_production = production)], 
                           tcf_crop_sua, 
                      by.x = c("area", "item_code"),
                      by.y = c("area", "child_code_fcl"),allow.cartesian = TRUE)
tcf_crop_sua <- tcf_crop_sua[child_production > 0]


# some child items have several possible inputs, making it more difficult to derive 
# its parent's processing from its production
# -> here it will be assumed that the amount going to processing will be the amount 
# that would go to processing if this was the only parent * its share of the total parent production
# i.e. the proportion in which parent items are available is the proportion that the child item
# is assumed to consist of

# Find parent -> child structures
structure <- use_structure[ child %in% tcf_crop_sua$child]

# merge the structure with supply data from sua_prelim 
parent_supply <- sua_prelim[item %in% structure$parent, 
                            .(area, year, parent_code_fcl = item_code, item, 
                              supply = na_sum(production, imports, stock_withdrawal, -exports ))]
parent_supply <- merge(structure, parent_supply, by.x = c("parent"), 
                       by.y = c("item"), allow.cartesian = TRUE)

# set supply to 0 where it is negative (exports exceed total supply)
parent_supply[supply < 0, supply := 0]

# sum the supply of all parent items that could go into one child item by country and year
parent_supply[, total_parent_supply := sum(supply, na.rm = TRUE), by = .(area, year, child)]

# get shares of parent item of total supply
parent_supply[, parent_share := supply/total_parent_supply]
parent_supply[, parent_share := ifelse(!is.finite(parent_share),0,parent_share)]


# add parent_share to tcf_crop_sua
tcf_crop_sua[, parent_share := parent_supply$parent_share[match(paste0(area, year, parent, child),
                                                                paste0(parent_supply$area,
                                                                       parent_supply$year,
                                                                       parent_supply$parent,
                                                                       parent_supply$child))]]

# find processing from child production
tcf_crop_sua[, processing := (child_production / extraction_rate) * parent_share]
tcf_crop_sua[!is.finite(processing), processing := 0]

# add processes and average by process to avoid double-counting in coupled processes

tcf_crop_sua[, proc := proc_sua$proc[match(item_code, proc_sua$item_code)]]
tcf_crop_sua <- tcf_crop_sua[, .(processing = mean(processing, na.rm = TRUE)), 
                     by = .(area, year, proc, parent, parent_code_fcl)]


# sum processing from different child items
tcf_crop_sua[, processing := sum(processing, na.rm = TRUE), 
             by = .(area, year, parent_code_fcl)]


# Get unique processing values for each parent item
tcf_crop_sua <- unique(tcf_crop_sua[, .(area, year, parent, parent_code_fcl, processing)])


# merge back to addsua
addsua[, processing := tcf_crop_sua$processing[match(paste(area, year, item_code), 
                                                     paste(tcf_crop_sua$area, tcf_crop_sua$year, tcf_crop_sua$parent_code_fcl))]]

outliers <- addsua[(processing + exports) > total_supply] 
outliers[, rel_dev := ((processing + exports)/ total_supply)-1]
outliers[, rel_dev := round(rel_dev, 2)]
bad_outliers <- outliers[rel_dev >0.2]

# cap processing at total_supply - exports
addsua[, processing := ifelse(processing > (total_supply - exports), 
                              (total_supply - exports), 
                              processing)]


# Allocate rest to food (for non-food items this is reversed in the following)
addsua[, food := na_sum(total_supply, - processing, - exports)]


# Allocate 'Seed cotton, unginned' and 'Hop cones', 'green coffee', palm kernels',
# cocoa beans, 'molasses'  and ' Castor oil seeds' supply fully to processing
addsua[item_code %in% c(165, 265, 328, 656, 661, 677, 256), 
       `:=`(processing = na_sum(total_supply, -exports, -food), food = 0)]

# add the following items fully to other:
# "Unmanufactured tobacco", "Cotton lint, ginned",  "Kenaf, and other textile bast fibres, raw or retted",
# "Other fibre crops, raw, n.e.c.", "True hemp, raw or retted", "Jute, raw or retted", "Coir, raw",
# "Natural rubber in primary forms", "Pyrethrum, dried flowers", "Ramie, raw or retted",
# "Agave fibres, raw, n.e.c.", and "Kapok fibre, raw", "Sisal, raw", "Safflower seed" to other uses,
# "Raw hides and skins of cattle", "Raw hides and skins of sheep or lambs", "Raw hides and skins of goats or kids",
# "Raw hides and skins of buffaloes", "Beeswax", "Shorn wool, greasy, including fleece-washed shorn wool"

addsua[item_code %in% c(280,754, 767, 777, 778, 789, 780, 782, 788, 
                            800, 813, 821, 826, 836, 919, 957, 987, 995, 1025, 1183), 
       `:=` (other = na_sum(total_supply, -exports, -processing), food = 0)]

# Allocate 'Fodder crops', 'vetches' and left-over cotton seed supply to feed
addsua[item_code %in% c(2000, 205, 329), `:=` 
       (feed = na_sum(total_supply, -processing, -exports), food = 0)]

# add back to sua
sua <- dplyr::bind_rows(sua, addsua)
sua[, names(sua) := lapply(.SD, function(x) ifelse(is.na(x), 0, x))]

# Tidy
rm(outliers, parent_supply, proc_sua, tcf_crop_sua, addsua, sua_prelim, prod_trad, prod)


# Fill Ethanol -----------------------------

cat("\nAdding ethanol production data to SUA.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")
eth <- eth[year %in% years]
# Keep one unit and recode for merging
eth <- eth[, `:=`(unit = NULL,
                  item = "Alcohol, Non-Food", item_code = 2659)]

eth_sua <- merge(sua[item == "Alcohol, Non-Food", ], eth, all = TRUE,
                 by = c("area_code", "area", "year", "item", "item_code"))

cat("Using EIA/IEA ethanol production values where FAO's",
    "CBS are not (or under-) reported.\n")
eth_sua[production < value | is.na(production), production := value]
eth_sua[, value := NULL]

# compute total supply
eth_sua[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
eth_sua[(exports + other) > (total_supply + stock_withdrawal), exports := total_supply + stock_withdrawal - other]
eth_sua[exports < 0, exports := 0]
# all supply is assumed to go to other uses
eth_sua[, other := na_sum(production, imports, -exports, -stock_addition)]
# rebalance
eth_sua[, balancing := na_sum(production, imports, -exports, -stock_addition, -other)]

sua <- rbindlist(list(sua[item_code != 2659, ], eth_sua), use.names = TRUE)
rm(eth, eth_sua)



# Add BTD data ------------------------------------------------------------

cat("\nAdding missing export and import data to SUA from BTD.\n")
sua <- merge(
  sua, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
sua[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
sua <- merge(
  sua, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
sua[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
rm(imps, exps)


# Create RoW --------------------------------------------------------------

# Aggregate RoW countries in sua
sua <- replace_RoW(sua, codes = regions[current == TRUE, code])
sua <- sua[, lapply(.SD, na_sum),
           by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
                   codes = c(regions[current == TRUE, code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
                                         "to_code", "to","element", "item_code", 
                                         "item", "unit", "year")]

# Remove ROW-internal trade from sua
intra <- btd[from_code==to_code & unit!="usd", sum(value),
             by=c("from_code","from","item_code","item","year")]
sua <- merge(sua, intra,
             by.x = c("area_code", "area", "item_code", "item", "year"),
             by.y = c("from_code", "from", "item_code", "item", "year"),
             all.x = TRUE)
sua[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
                     imports = na_sum(imports,-V1))]
sua[, V1 := NULL]
rm(intra)

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)


# Rebalance columns -------------------------------------------------------

# TODO: this could be improved! Decide how to use residuals here.

cat("\nRebalance sua.\n")

# Round to 0 digits
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "tourist")
sua[, (cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = cols]

# Replace negative and NA values with '0'
sua <- sua[, (cols) := lapply(.SD, function(x) ifelse(x < 0 | is.na(x), 0, x)), 
           .SDcols = cols]

# Rebalance table
sua_test <- sua[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals)]

# Note: the sum of balancing and residuals should never be <0 in the end
# as long as we do not introduce an "unknown source" region, we need to adapt the other sua use elements accordingly


# TODO
# a) For countries where trade is reported but production is zero and balancing or residuals is negative,
# assume the missing supply coming from domestic production.
# b) USA and CHE buttermilk needs to be adapted manually
# c) For any n.e.c. or nes items, export use exceeding supply will be redistributed among the other items in that group, 
# e.g. Cereal brans, n.e.c. to other cereal brans, in proportion to the production of other cereal brans.
# d) For Soybeans in Brazil (years 2019-2022), ask Liza to search for official statistics on production, trade, processing (soybean oil and cake production).
# e) Manually correct other most important cases of negative residuals + balancing.
# f) then run the following steps





# # neutralize negative balancing via other items
# cat("\nAdjust 'residuals' for ", sua[balancing < 0 &
#                                      !is.na(residuals) & residuals >= -balancing*0.99, .N],
#     " observations, where `balancing < 0` and `residuals >= -balancing` to ",
#     "`residuals = residuals - balancing`.\n", sep = "")
# sua[balancing < 0 & !is.na(residuals) & residuals >= -balancing*0.99,
#     `:=`(residuals = na_sum(residuals, balancing),
#          balancing = 0)]


cat("\nAdjust 'exports' for ", sua[na_sum(balancing, residuals) < 0 &
                                     !is.na(exports) & exports >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `exports >= -na_sum(balancing, residuals)` to ",
    "`exports = exports - na_sum(balancing, residuals)`.\n", sep = "")
sua[na_sum(balancing, residuals) < 0 & !is.na(exports) & exports >= na_sum(balancing, residuals)*0.99,
    `:=`(exports = na_sum(exports, balancing, residuals),
         balancing = 0, residuals = 0)]
sua[exports < 0, `:=`(balancing = balancing + exports,
                      exports = 0)]

cat("\nAdjust 'processing' for ", sua[na_sum(balancing, residuals) < 0 &
                                        !is.na(processing) & processing >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `processing >= -na_sum(balancing, residuals)` to ",
    "`processing = processing + na_sum(balancing, residuals)`.\n", sep = "")
sua[na_sum(balancing, residuals) < 0 & !is.na(processing) & processing >= -na_sum(balancing, residuals)*0.99,
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0, residuals = 0)]
sua[processing < 0, `:=`(balancing = balancing + processing,
                         processing = 0)]

cat("\nAdjust 'other' for ", sua[na_sum(balancing, residuals) < 0 &
                                   !is.na(other) & other >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `other >= -na_sum(balancing, residuals)` to ",
    "`other = other + na_sum(balancing, residuals)`.\n", sep = "")
sua[na_sum(balancing, residuals) < 0 & !is.na(other) & other >= -na_sum(balancing, residuals)*0.99,
    `:=`(other = na_sum(other, balancing, residuals),
         balancing = 0, residuals = 0)]
sua[other < 0, `:=`(balancing = balancing + other,
                    other = 0)]

# cat("\nAdjust 'tourist' for ", sua[balancing < 0 &
#     !is.na(tourist) & tourist >= -balancing, .N],
#     " observations, where `balancing < 0` and `tourist >= -balancing` to ",
#     "`tourist = tourist + balancing`.\n", sep = "")
# sua[balancing < 0 & !is.na(other) & other >= -balancing,
#     `:=`(other = na_sum(other, balancing),
#          balancing = 0)]

# cat("\nAdjust 'unspecified' for ", sua[na_sum(balancing, residuals) < 0 &
#                                          !is.na(unspecified) & unspecified >= -na_sum(balancing, residuals)*0.99, .N],
#     " observations, where `na_sum(balancing, residuals) < 0` and `unspecified >= -na_sum(balancing, residuals)` to ",
#     "`unspecified = unspecified + na_sum(balancing, residuals)`.\n", sep = "")
# sua[na_sum(balancing, residuals) < 0 & !is.na(unspecified) & unspecified >= -na_sum(balancing, residuals)*0.99,
#     `:=`(unspecified = na_sum(unspecified, balancing, residuals),
#          balancing = 0, residuals = 0)]
# sua[unspecified < 0, `:=`(balancing = balancing + unspecified,
#                           unspecified = 0)]

cat("\nAdjust 'stock_addition' for ", sua[na_sum(balancing, residuals) < 0 &
                                            !is.na(stock_addition) & stock_addition >= -na_sum(balancing, residuals)*0.99, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `stock_addition >= -na_sum(balancing, residuals)` to ",
    "`stock_addition = stock_addition + na_sum(balancing, residuals)`.\n", sep = "")
sua[na_sum(balancing, residuals) < 0 & !is.na(stock_addition) & stock_addition >= -na_sum(balancing, residuals)*0.99,
    `:=`(stock_addition = na_sum(stock_addition, balancing, residuals),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing, -residuals),
         balancing = 0, residuals = 0)]

cat("\nAdjust uses proportionally for ", sua[na_sum(balancing, residuals) < 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0`", sep = "")
sua[, divisor := na_sum(exports, other, processing, seed, food, feed, stock_addition, tourist)]
sua[na_sum(balancing, residuals) < 0 & divisor >= -na_sum(balancing, residuals),
    `:=`(stock_addition = round(na_sum(stock_addition, (na_sum(balancing, residuals) / divisor * stock_addition))),
         processing = round(na_sum(processing, (na_sum(balancing, residuals) / divisor * processing))),
         exports = round(na_sum(exports, (na_sum(balancing, residuals) / divisor * exports))),
         other = round(na_sum(other, (na_sum(balancing, residuals) / divisor * other))),
         seed = round(na_sum(seed, (na_sum(balancing, residuals) / divisor * seed))),
         food = round(na_sum(food, (na_sum(balancing, residuals) / divisor * food))),
         feed = round(na_sum(feed, (na_sum(balancing, residuals) / divisor * feed))),
         tourist = round(na_sum(tourist, (na_sum(balancing, residuals) / divisor * tourist))),
         balancing = 0, residuals = 0)]
sua[, `:=`(stock_withdrawal = -stock_addition,
           divisor = NULL)]

# Rebalance table
sua[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals)]

# Attribute rest (resulting from rounding differences) to stock changes
sua[na_sum(balancing, residuals) < 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0, residuals = 0)]
sua[balancing < 0, `:=`(residuals = residuals + balancing, balancing = 0)]
sua[residuals < 0, `:=`(balancing = residuals + balancing, residuals = 0)]

# Adjust 'total_supply' to new production values
sua[, total_supply := na_sum(production, imports)]

cat("\nSkip capping 'exports', 'seed' and 'processing' at",
    "'total_supply + stock_withdrawal'.\n")


# # Balance sua imports and exports -------------------------------------------------------
# # --> This balancing step was moved to script 05_balance.R
#
# # Adjust sua to have equal export and import numbers per item per year
# # This is very helpful for the iterative proportional fitting of bilateral trade data
# sua_bal <- sua[, list(exp_t = sum(exports, na.rm = TRUE), imp_t = sum(imports, na.rm = TRUE)),
#                by = c("year", "item_code", "item")]
# sua_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
#                area_code = 999, area = "RoW")]
# # Absorb the discrepancies in "RoW"
# sua <- merge(sua, sua_bal,
#              by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
# sua[area_code == 999, `:=`(
#   exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
#   imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
# sua[, diff := NULL]
#
# rm(sua_bal); gc()
#
# # Rebalance RoW
# sua[, balancing := na_sum(production, imports, stock_withdrawal,
#                           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]
#
# sua[, divisor := na_sum(other, processing, seed, food, feed, stock_addition, unspecified)]
# sua[balancing < 0 & divisor >= -balancing,
#     `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
#          unspecified = round(na_sum(unspecified, (balancing / divisor * unspecified))),
#          processing = round(na_sum(processing, (balancing / divisor * processing))),
#          other = round(na_sum(other, (balancing / divisor * other))),
#          seed = round(na_sum(seed, (balancing / divisor * seed))),
#          food = round(na_sum(food, (balancing / divisor * food))),
#          feed = round(na_sum(feed, (balancing / divisor * feed))),
#          balancing = 0)]
# sua[, `:=`(stock_withdrawal = -stock_addition, divisor = NULL,
#            balancing = na_sum(production, imports, stock_withdrawal,
#           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified))]
# sua[balancing < 0,
#     `:=`(production = na_sum(production, -balancing),
#          balancing = 0)]
# sua[, total_supply := na_sum(production, imports)]



cat("\nAllocate remaining supply from 'unspecified' and 'balancing' to uses.\n")

cat("\nHops, oil palm fruit, palm kernels, sugar crops and live animals to 'processing'.\n")
sua[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
                     1126, 1157, 1140, 1150, 1171, 2536, 2537, 2562) & na_sum( balancing, residuals) > 0,
    `:=`(processing = na_sum(processing, balancing, residuals),
         unspecified = 0, balancing = 0, residuals = 0)]

cat("\nNon-food crops to 'other'.\n")
sua[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
                     1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(unspecified, balancing, residuals, processing) > 0,
    `:=`(other = na_sum(other,  balancing, residuals, processing),
         balancing = 0, residuals = 0, processing = 0)]

cat("\nFeed crops to 'feed'.\n")
sua[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
                     2595, 2596, 2597, 2598, 2749) & na_sum( balancing, residuals) > 0,
    `:=`(feed = na_sum(feed, balancing, residuals),
          balancing = 0, residuals = 0)]

cat("\nRest (mostly 'food', 'feed' and 'processing') remains in 'unspecified', 'balancing' and 'residuals'.\n")


sua <- sua[year %in% years, ]


# Save --------------------------------------------------------------------

saveRDS(sua, "data/sua_full.rds")






