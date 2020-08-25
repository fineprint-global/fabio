
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

# CBS ---------------------------------------------------------------------

cat("\nBuilding full CBS.\n")

cbs <- readRDS("data/tidy/cbs_tidy.rds")

cat("Removing items from CBS that are not used in the FAO-MRIO:\n\t",
  paste0(unique(cbs[!item_code %in% items$item_code, item]),
    sep = "", collapse = "; "), ".\n", sep = "")
# Particularly fish and aggregates
cbs <- dt_filter(cbs, item_code %in% items$item_code)


# Prepare BTD data --------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/tidy/btd_full_tidy.rds")

cat("\nGiving preference to units in the following order:\n",
  "\t 'm3' > 'head' > 'tonnes'\n", "Dropping 'usd'.\n", sep = "")

# Imports
imps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
  value.var = "value")
imps[, `:=`(value = ifelse(!is.na(m3), m3, ifelse(!is.na(head), head, tonnes)),
  m3 = NULL, head = NULL, tonnes = NULL)]

# Exports
exps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
  value.var = "value")
exps[, `:=`(value = ifelse(!is.na(m3), m3, ifelse(!is.na(head), head, tonnes)),
  m3 = NULL, head = NULL, tonnes = NULL)]


# Forestry ----------------------------------------------------------------

cat("\nAdding forestry production data.\n")

fore <- readRDS("data/tidy/fore_prod_tidy.rds")

fore[, `:=`(total_supply = na_sum(production, imports),
  other = na_sum(production, imports, -exports),
  stock_withdrawal = 0, stock_addition = 0,
  feed = 0, food = 0, losses = 0, processing = 0,
  seed = 0, balancing = 0)]
fore[other < 0, `:=`(balancing = other, other = 0)]

cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
rm(fore)


# Fill crop production where missing -------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]
setkey(crop_prod, year, area_code, item_code)

# Add production to CBS where is.na or is zero ---
cbs <- merge(cbs, crop_prod,
             by = c("area_code", "area", "item_code", "item", "year"), all.x = TRUE)
cat("\nFilling missing cbs production with crop production data. Items:\n",
    paste0(unique(cbs[(is.na(production) | production == 0) & !is.na(value), item]), collapse = "; "),
    ".\n", sep = "")
cbs[!is.na(value) & (is.na(production) | production == 0), production := value]
cbs[, value := NULL]



# Estimate cbs for items/countries not included -------------------------------------

# Filter countries and items that are not yet in CBS
addcbs <- dt_filter(crop_prod, ! paste(area_code,item_code,year) %in% paste(cbs$area_code,cbs$item_code,cbs$year))

# Technical conversion factors to impute processing ---
tcf_crop <- fread("inst/tcf_crop.csv")

C <- data.table::dcast(tcf_crop, item_code ~ source_code, fill = 0, value.var = "tcf")
tcf_codes <- list(C[, item_code], as.integer(colnames(C[, -1])))
C <- as(C[, -1], "Matrix")
dimnames(C) <- tcf_codes

tcf_data <- addcbs[item_code %in% unlist(tcf_codes),
  .(year, area_code, item_code, production = value)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_data$area_code))

# Base processing on production + imports - exports
tcf_data[imps[, .(year, area_code = to_code, item_code, imports = value)],
  on = c("area_code", "item_code", "year"), imports := imports]
tcf_data[exps[, .(year, area_code = from_code, item_code, exports = value)],
  on = c("area_code", "item_code", "year"), exports := exports]

# Production of items
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[1]])))]
output[, `:=`(value = production,
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[2]])))]
input[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(input, function(x) {`<`(x, 0)}, value = 0, cols = "value")
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
setkey(results, year, area_code, item_code)
results[, `:=`(value = NA_real_,
  production = NULL, imports = NULL, exports = NULL)]

# Fill during a loop over years and areas
for(x in years) {
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    results[year == x & area_code == y,
      value := fill_tcf(y = output_y, z = input_y, C = C, cap = FALSE)]
  }
}
results <- results[!is.na(value), .(year, area_code, item_code, processing = value)]

# Allocate uses
addcbs[, `:=`(production = value, value = NULL)]
addcbs[, exports := exps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(exps$year, exps$from_code, exps$item_code))]]
addcbs[, imports := imps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(imps$year, imps$to_code, imps$item_code))]]
addcbs[, total_supply := na_sum(production,imports)]
# allocate processing inputs to 'processing'
addcbs[, processing := results$processing[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
                                                paste(results$year, results$area_code, results$item_code))]]
# Allocate 'Seed cotton', 'Oil, palm fruit', 'Hops', 'Sugar cane' and 'Palm kernels' supply to processing
addcbs[item_code %in% c(254, 328, 677, 2536, 2562), processing := na_sum(total_supply, -exports)]
# allocate rest to 'unspecified'
addcbs[, unspecified := na_sum(total_supply,-processing,-exports)]
addcbs[, balancing := 0]
addcbs[unspecified < 0, `:=`(balancing = unspecified, unspecified = 0)]

# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkip filling cbs seed.",
  "Apparently data on seed is not reported in newer faostat versions.\n")

# Add to CBS ---
cat("\nAdding ", nrow(addcbs), " missing cbs accounts.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, addcbs)

rm(crop, crop_prod, addcbs,
  tcf_crop, tcf_codes, tcf_data, input, output, results, years, areas,
  C, input_x, output_x, input_y, output_y)


# Fill livestock and ethanol production -----------------------------

cat("\nFilling missing livestock data.\n")

live <- readRDS("data/tidy/live_tidy.rds")

live <- live[element == "Production" & unit == "head", ]
live[, `:=`(element = NULL, unit = NULL)]

# Map "Meat indigenous, ..." items to CBS items
src_item <- c(1137, 944, 1032, 1012, 1775, 1055, 1120, 1144,
  972, 1161, 1154, 1122, 1124)
tgt_item <- c(1126, 866, 1016, 976, 2029, 1034, 1096, 1140,
  946, 1157, 1150, 1107, 1110)
tgt_name <- c("Camels", "Cattle", "Goats", "Sheep", "Poultry Birds", "Pigs",
  "Horses", "Rabbits and hares", "Buffaloes", "Camelids, other",
  "Rodents, other", "Asses", "Mules")
conc <- match(live$item_code, src_item)
live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live <- live[!is.na(item_code), ]

# Map "Meat, ..." items to CBS items
src_item_alt <- c(1127, 867, 1017, 977, 1808, 1035, 1097, 1141,
  947, 1158, 1151, 1108, 1111)

live_alt <- readRDS("data/tidy/live_tidy.rds")
live_alt <- live_alt[element == "Producing Animals/Slaughtered" &
  unit == "head", ]
live_alt[, `:=`(element = NULL, unit = NULL)]

conc <- match(live_alt$item_code, src_item_alt)
live_alt[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live_alt <- live_alt[!is.na(item_code), ]

live <- merge(live, live_alt,
  by = c("area_code", "area", "year", "item_code", "item"), all = TRUE)
live[ , `:=`(value =
  ifelse(is.na(value.x), value.y, ifelse(value.x == 0, value.y, value.x)),
  value.x = NULL, value.y = NULL)]

# Add trade data
live_trad <- readRDS("data/tidy/live_tidy.rds")

live_imp <- live_trad[element == "Import Quantity" & unit == "head", ]
live_exp <- live_trad[element == "Export Quantity" & unit == "head", ]

live <- merge(live[, .(area_code, area, item_code, item, year, production = value)],
              live_imp[, .(area_code, area, item_code, item, year, imports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)
live <- merge(live,
              live_exp[, .(area_code, area, item_code, item, year, exports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)

# add trade values from btd in case they are missing in live_trad
live <- merge(
  live,
  imps[item_code %in% live[, item_code] & value > 0,
       c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
live <- merge(
  live,
  exps[item_code %in% live[, item_code] & value > 0,
       c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]

live[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply
live[, exports := ifelse(exports > total_supply, total_supply, exports)]
live[, processing := na_sum(production, imports, -exports)]

cbs <- dplyr::bind_rows(cbs, live)

rm(src_item, tgt_item, tgt_name, conc, live, live_trad, live_alt,
  live_imp, live_exp)



# Ethanol ---

cat("\nAdding ethanol production data to CBS.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")

# Keep one unit and recode for merging
eth <- eth[, `:=`(unit = NULL,
  item = "Alcohol, Non-Food", item_code = 2659)]

eth_cbs <- merge(cbs[item_code == 2659, ], eth, all = TRUE,
  by = c("area_code", "area", "year", "item", "item_code"))

cat("Using EIA/IEA ethanol production values where FAO's",
  "CBS are not (or under-) reported.\n")
eth_cbs[production < value | is.na(production), production := value]
eth_cbs[, value := NULL]

cbs <- rbindlist(list(cbs[item_code != 2659, ], eth_cbs), use.names = TRUE)
rm(eth, eth_cbs)



# Add BTD data ------------------------------------------------------------

cat("\nAdding missing export and import data to CBS from BTD.\n")
cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
rm(imps, exps)


# Create RoW --------------------------------------------------------------

# remove regions "Unspecified" and "Others (adjustment)"
cbs <- cbs[! area %in% c("Unspecified", "Others (adjustment)")]

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[cbs == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[cbs == TRUE, code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "comm_code", "item_code", "item", "unit", "year")]

# Remove ROW-internal trade from CBS
intra <- btd[from_code==to_code & unit!="usd", sum(value), by=c("from_code","from","item_code","item","year")]
cbs <- merge(cbs, intra,
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
  imports = na_sum(imports,-V1))]
cbs[, V1 := NULL]
rm(intra)

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)


# Rebalance columns -------------------------------------------------------

cat("\nRebalance CBS.\n")

# Round to whole numbers
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "balancing", "unspecified")
cbs[, (cols) := lapply(.SD, round), .SDcols = cols]

# Replace negative values with '0'
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
  cols = c("imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed"))

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
        -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]

cat("\nAdjust 'exports' for ", cbs[balancing < 0 &
    !is.na(exports) & exports >= balancing*0.99, .N],
    " observations, where `balancing < 0` and `exports >= -balancing` to ",
    "`exports = exports - balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(exports) & exports >= -balancing*0.99,
    `:=`(exports = na_sum(exports, balancing),
         balancing = 0)]
cbs[exports < 0, `:=`(balancing = exports,
                      exports = 0)]

cat("\nAdjust 'processing' for ", cbs[balancing < 0 &
    !is.na(processing) & processing >= -balancing, .N],
    " observations, where `balancing < 0` and `processing >= -balancing` to ",
    "`processing = processing + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(processing) & processing >= -balancing,
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0)]

cat("\nAdjust 'other' for ", cbs[balancing < 0 &
    !is.na(other) & other >= -balancing, .N],
    " observations, where `balancing < 0` and `other >= -balancing` to ",
    "`other = other + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(other) & other >= -balancing,
    `:=`(other = na_sum(other, balancing),
         balancing = 0)]

cat("\nAdjust 'other' for ", cbs[balancing < 0 &
    !is.na(unspecified) & unspecified >= -balancing, .N],
    " observations, where `balancing < 0` and `unspecified >= -balancing` to ",
    "`unspecified = unspecified + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(unspecified) & unspecified >= -balancing,
    `:=`(unspecified = na_sum(unspecified, balancing),
         balancing = 0)]

cat("\nAdjust 'stock' for ", cbs[balancing < 0 &
    !is.na(stock_addition) & stock_addition >= -balancing, .N],
    " observations, where `balancing < 0` and `stock_addition >= -balancing` to ",
    "`stock_addition = stock_addition + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(stock_addition) & stock_addition >= -balancing,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

cat("\nAdjust uses proportionally for ", cbs[balancing < 0, .N],
    " observations, where `balancing < 0` to", sep = "")
cbs[, divisor := na_sum(exports, other, processing, seed, food, feed, stock_addition)]
cbs[balancing < 0 & divisor >= -balancing,
    `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
         processing = round(na_sum(processing, (balancing / divisor * processing))),
         exports = round(na_sum(exports, (balancing / divisor * exports))),
         other = round(na_sum(other, (balancing / divisor * other))),
         seed = round(na_sum(seed, (balancing / divisor * seed))),
         food = round(na_sum(food, (balancing / divisor * food))),
         feed = round(na_sum(feed, (balancing / divisor * feed))),
         balancing = 0)]
cbs[, `:=`(stock_withdrawal = -stock_addition,
           divisor = NULL)]

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]

# Attribute rest (resulting from rounding differences) to stock changes
cbs[balancing < 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

# Adjust 'total_supply' to new production values
cbs[, total_supply := na_sum(production, imports)]

cat("\nSkip capping 'exports', 'seed' and 'processing' at",
  "'total_supply + stock_withdrawal'.\n")


# Balance CBS imports and exports -------------------------------------------------------

# Adjust CBS to have equal export and import numbers per item per year
# This is very helpful for the iterative proportional fitting of bilateral trade data
cbs_bal <- cbs[, list(exp_t = sum(exports, na.rm = TRUE), imp_t = sum(imports, na.rm = TRUE)),
               by = c("year", "item_code", "item")]
cbs_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
               area_code = 999, area = "RoW")]
# Absorb the discrepancies in "RoW"
cbs <- merge(cbs, cbs_bal,
             by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
cbs[area_code == 999, `:=`(
  exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
  imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
cbs[, diff := NULL]

rm(cbs_bal); gc()

# Rebalance RoW
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]

cbs[, divisor := na_sum(other, processing, seed, food, feed, stock_addition, unspecified)]
cbs[balancing < 0 & divisor >= -balancing,
    `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
         unspecified = round(na_sum(unspecified, (balancing / divisor * unspecified))),
         processing = round(na_sum(processing, (balancing / divisor * processing))),
         other = round(na_sum(other, (balancing / divisor * other))),
         seed = round(na_sum(seed, (balancing / divisor * seed))),
         food = round(na_sum(food, (balancing / divisor * food))),
         feed = round(na_sum(feed, (balancing / divisor * feed))),
         balancing = 0)]
cbs[, `:=`(stock_withdrawal = -stock_addition, divisor = NULL,
           balancing = na_sum(production, imports, stock_withdrawal,
          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified))]
cbs[balancing < 0,
    `:=`(production = na_sum(production, -balancing),
         balancing = 0)]
cbs[, total_supply := na_sum(production, imports)]



cat("\nAllocate remaining supply from 'unspecified' and 'balancing' to uses.\n")

cat("\nHops, oil palm fruit and live animals to 'processing'.\n")
cbs[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
  1126, 1157, 1140, 1150, 1171, 2562) & na_sum(unspecified, balancing) > 0,
  `:=`(processing = na_sum(processing, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
  1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(unspecified, balancing) > 0,
  `:=`(other = na_sum(other, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
  2595, 2596, 2597, 2598, 2749) & na_sum(unspecified, balancing) > 0,
  `:=`(feed = na_sum(feed, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nRest (mostly 'food', 'feed' and 'processing') remains in 'unspecified'.\n")


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
