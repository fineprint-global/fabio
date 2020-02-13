
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

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
    "\t'head' | 'm3' > 'tonnes'\n",
    "Dropping 'usd' and 'litres'.\n", sep = "")

# Imports
imps <- btd[unit != "usd", list(value = na_sum(value)),
            by = list(to_code, item_code, year, unit)]
imps <- dcast(imps, to_code + item_code + year ~ unit,
              value.var = "value")
imps[, `:=`(value = ifelse(!is.na(head), head, ifelse(!is.na(m3), m3, tonnes)),
  head = NULL, litres = NULL, m3 = NULL, tonnes = NULL)]

# Exports
exps <- btd[unit != "usd", list(value = na_sum(value)),
            by = list(from_code, item_code, year, unit)]
exps <- dcast(exps, from_code + item_code + year ~ unit,
              value.var = "value")
exps[, `:=`(value = ifelse(!is.na(head), head, ifelse(!is.na(m3), m3, tonnes)),
  head = NULL, litres = NULL, m3 = NULL, tonnes = NULL)]


# Forestry ----------------------------------------------------------------

cat("\nAdding forestry production data.\n")

fore <- readRDS("data/tidy/fore_prod_tidy.rds")

fore[, `:=`(total_supply = production + imports,
            other = production + imports - exports,
            stock_withdrawal = 0, stock_addition = 0,
            feed = 0, food = 0, losses = 0, processing = 0,
            seed = 0, balancing = 0)]
fore[other < 0, `:=`(stock_addition = -other,
                     stock_withdrawal = other, other = 0)]

cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
rm(fore)


# Fill crop production and processing -------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]
cat("Add grazing item.\n")
graze <- crop_prod[item_code == 2000, ]
crop_prod <- rbindlist(list(
  crop_prod, graze[, `:=`(item = "Grazing", item_code = 2001, value = 0)]))
setkey(crop_prod, year, area_code, item_code)

# Technical conversion factors to impute processing ---
tcf_crop <- fread("inst/tcf_crop.csv")

C <- dcast(tcf_crop, item_code ~ source_code, fill = 0, value.var = "tcf")
tcf_codes <- list(C[, item_code], as.integer(colnames(C[, -1])))
C <- as(C[, -1], "Matrix")
dimnames(C) <- tcf_codes

tcf_data <- crop_prod[item_code %in% unlist(tcf_codes),
  .(year, area_code, item_code, production = value)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_data$area_code))

# Base processing on production + imports - exports
tcf_data[imps[, .(year, area_code = to_code, item_code, imports = value)],
  on = c("area_code", "item_code", "year"), imports := imports]
tcf_data[exps[, .(year, area_code = from_code, item_code, exports = value)],
  on = c("area_code", "item_code", "year"), exports := exports]
tcf_data[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
tcf_data <- dt_replace(tcf_data, function(x) {`<`(x, 0)},
  value = 0, cols = "value")


# Production of items
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[1]]))]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
setkey(results, year, area_code, item_code)
results[, value := NA]

# Fill during a loop over years and areas (maybe vectorise)
for(x in years) {
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    results[year == x & area_code == y,
      value := calc_processing(y = output_y, z = input_y, C = C, cap = FALSE)]
  }
}
results <- results[!is.na(value), .(year, area_code, item_code, value2 = value)]
crop_prod <- crop_prod[results][!is.na(value), ]

# Add to CBS ---
cbs <- merge(cbs, crop_prod,
  by = c("area_code", "area", "item_code", "item", "year"), all = TRUE)
cat("\nFilling missing cbs production with crop production data. Items:\n",
  paste0(unique(cbs[is.na(production) & !is.na(value), item]), collapse = "; "),
  ".\n", sep = "")
cbs[is.na(production), `:=`(production = value,
  processing = na_sum(processing, value2))]

cbs[, `:=`(value = NULL, value2 = NULL)]
rm(crop, crop_prod, graze,
  tcf_crop, tcf_codes, tcf_data, input, output, result,
  C, input_x, output_x, input_y, output_y)


# Fill seed, livestock and ethanol production -----------------------------

# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkipped filling cbs seed.",
    "Apparently data is dropped in newer versions. See Issue #19.\n")


cat("\nFilling missing livestock data.\n") # See Issues #22, #23. #24 and #49.

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

cbs <- merge(cbs, live,
  by = c("area_code", "area", "year", "item_code", "item"), all = TRUE)
cbs[!is.na(value), production := value]

cbs[, value := NULL]
rm(src_item, tgt_item, tgt_name, conc, live)

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
  cbs, imps[item_code %in% cbs$item_code,
    c("to_code", "item_code", "year", "value")],
  by.x = c("area_code", "item_code", "year"),
  by.y = c("to_code", "item_code", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[item_code %in% cbs$item_code,
    c("from_code", "item_code", "year", "value")],
  by.x = c("area_code", "item_code", "year"),
  by.y = c("from_code", "item_code", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
rm(imps, exps)


# Create RoW --------------------------------------------------------------

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[cbs == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = regions[cbs == TRUE, code])
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from", "to_code", "to",
  "item_code", "item", "unit", "year")]


# Adjusting TCF crops -----------------------------------------------------

tcf_cbs <- fread("inst/tcf_cbs.csv")

C <- dcast(tcf_cbs, area_code + item_code ~ source_code, fill = 0,
  fun.aggregate = na_sum, value.var = "tcf")
tcf_codes <- list(C[, area_code], C[, item_code],
  as.integer(colnames(C[, c(-1, -2)])))
C <- as(C[, c(-1, -2)], "Matrix")
dimnames(C) <- list(paste0(tcf_codes[[1]], "-", tcf_codes[[2]]), tcf_codes[[3]])

tcf_data <- cbs[area_code %in% tcf_codes[[1]] &
  item_code %in% c(tcf_codes[[2]], tcf_codes[[3]]),
  .(year, area_code, item_code, production, processing, imports, exports)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_prod$area_code))

# Base processing on production + imports - exports, cap at 0
tcf_data[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
tcf_data <- dt_replace(tcf_data, function(x) {`<`(x, 0)},
  value = 0, cols = "value")

# Production of items
output <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[3]]))]
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[3]]))]
setkey(results, year, area_code, item_code)
results[, value := NA]

for(x in years) {
  output_x <- output[year == x, value]
  input_x <- input[year == x, value]
  # Skip if no data is available
  if(all(output_x == 0) || all(input_x == 0)) {next}
  results[year == x,
    value := calc_processing(y = output_x, z = input_x, C = C, cap = TRUE)]
}

merge(cbs, results[!is.na(value)],
  by = c("year", "area_code", "item_code"), all.x = TRUE)
cbs[!is.na(value), processing := value]

cbs[, value := NULL]
rm(tcf_cbs, tcf_codes, tcf_data, input, output, result, C, input_x, output_x)



# Rebalance columns -------------------------------------------------------

cat("\nRebalance CBS.\n")

cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
  cols = c("total_supply", "imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed"))

cat("\nAdjust ", cbs[total_supply != na_sum(production, imports), .N],
    " observations of 'total_supply' to ",
    "`total_supply = production + imports`.\n", sep = "")
cbs[, total_supply := na_sum(production, imports)]

cat("\nCap out 'exports' (N = ", cbs[exports > total_supply, .N],
    "), 'processing' (N = ", cbs[processing > total_supply, .N],
    ") and 'seed' (N = ", cbs[seed > total_supply, .N],
    ") uses exceeding 'total_supply'.\n", sep = "")
cbs[exports > total_supply, exports := total_supply]
cbs[processing > total_supply, processing := total_supply]
cbs[seed > total_supply, seed := total_supply]

cat("\nSkipped capping out 'exports' at `total_supply - seed - processing`.\n")

cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
  -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other)]

cat("\nAllocate remaining supply from 'balancing' to uses.\n")
cat("\nHops and live animals to 'processing'.\n")
cbs[item_code %in% c(677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
                     1126, 1157, 1140, 1150, 1171) & balancing > 0,
    `:=`(processing = na_sum(processing, balancing), balancing = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
                     2661, 2746, 2748, 2747) & balancing > 0,
    `:=`(other = na_sum(other, balancing), balancing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2536, 2537, 2555, 2559, 2544, 2590, 2591, 2592,
                     2593, 2594, 2595, 2596, 2597, 2598) & balancing > 0,
    `:=`(feed = feed + balancing, balancing = 0)]
cat("\nRest to 'food'.\n")
cbs[balancing > 0, `:=`(food = na_sum(food, balancing), balancing = 0)]


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
