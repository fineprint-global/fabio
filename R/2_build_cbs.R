
library(data.table)
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

years <- 1986:2013


# CBS ---------------------------------------------------------------------

cat("\nBuilding full CBS.\n")

cbs <- readRDS("data/tidy/cbs_tidy.rds")

cat("Removing items from CBS that are not used in the FAO-MRIO:\n\t",
    paste0(unique(cbs[!item_code %in% items$item_code, item]),
           sep = "", collapse = ", "), ".\n", sep = "")
# Particularly fish and aggregates
cbs <- dt_filter(cbs, item_code %in% items$item_code)


# Forestry ----------------------------------------------------------------

cat("\nAdding forestry production data.\n")

fore <- readRDS("data/tidy/fore_prod_tidy.rds")

fore[, `:=`(total_supply = production + imports,
            other = production + imports - exports,
            stock_withdrawal = 0, stock_addition = 0,
            feed = 0, food = 0, losses = 0, processing = 0,
            seed = 0, balancing = 0)]

cbs <- rbindlist(list(cbs, fore), use.names = TRUE)


# Fill production ---------------------------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]
cbs <- merge(cbs, crop_prod,
  by = c("area_code", "area", "item_code", "item", "year"), all.x = TRUE)

cat("\nFilling missing cbs production with crop production data. Items:\n",
    paste0(unique(cbs[is.na(production) & !is.na(value), item]),
           collapse = ", "),
    ".\n", sep = "")
cbs[is.na(production), production := value]
cbs[, value := NULL]


# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkipped filling cbs seed.",
    "Apparently data is dropped in newer versions. See Issue #19.\n")


# cat("\nfilling missing livestock data.\n")
# live <- readRDS("data/tidy/live_tidy.rds")
# live <- live[element == "Production" & unit == "head", ]
# setkey(live, area, area_code, item, item_code, year)
# Only "Meat indigenous, ..." is missing. These need not be filled.
# Some items are available as stocks instead of production.
cat("Skipped filling cbs livestock. See Issues #22, #23 and #24.")


cat("\nAdding ethanol production data to CBS.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")

# Keep one unit and recode for merging
eth <- eth[, `:=`(unit = NULL,
                  item = "Alcohol, Non-Food", item_code = 2659)]

eth_cbs <- merge(cbs[item_code == 2659, ], eth, all = TRUE,
                 by = c("area_code", "area", "year", "item", "item_code"))

# eth_cbs <- dt_replace(eth_cbs, is.na, 0,
#                       cols = c("total_supply", "exports",  "imports",
#                                "processing", "production",
#                                "feed", "food", "losses", "other", "seed",
#                                "stock_withdrawal", "stock_addition"))

cat("Using EIA/IEA ethanol production values where FAO's",
    "CBS are not (or under-) reported.\n")
eth_cbs[production < value | is.na(production), production := value]
eth_cbs[, value := NULL]

cbs <- rbindlist(list(cbs[item_code != 2659, ], eth_cbs), use.names = TRUE)


# Add BTD data ------------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/btd_full.rds")

cat("\nGiving preference to units in the following order:\n",
    "\t'head' | 'm3' > 'tonnes'\n",
    "We drop 'usd' and 'litres'.\n", sep = "")

# Imports
imps <- btd[unit != "usd" & item_code %in% cbs$item_code,
            list(value = sum(value, na.rm = TRUE)),
            by = list(to_code, to, item_code, item, year, unit)]
imps <- dcast(imps, to_code + to + item_code + item + year ~ unit,
              value.var = "value")
imps[, value := ifelse(!is.na(head), head, ifelse(!is.na(m3), m3, tonnes))]

# Exports
exps <- btd[unit != "usd" & item_code %in% cbs$item_code,
            list(value = sum(value, na.rm = TRUE)),
            by = list(from_code, from, item_code, item, year, unit)]
exps <- dcast(exps, to_code + to + item_code + item + year ~ unit,
              value.var = "value")
exps[, value := ifelse(!is.na(head), head, ifelse(!is.na(m3), m3, tonnes))]


cat("\nAdding missing export and import data to CBS from BTD.\n")
cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]


# Ethanol -----------------------------------------------------------------

# 2019-10-14: We do not aggregate over all years anymore, so we can treat
# ethanol trade in the steps above.

# cat("\nAdding ethanol production data.\n")

# eth <- readRDS("data/tidy/eth_tidy.rds")

# # Keep one unit and recode for merging
# eth <- eth[, `:=`(unit = NULL,
#                   item = "Alcohol, Non-Food", item_code = 2659)]

# eth_cbs <- merge(cbs[item_code == 2659, ], eth, all = TRUE,
#                  by = c("area_code", "area", "year", "item", "item_code"))

# eth_cbs <- dt_replace(eth_cbs, is.na, 0,
#                       cols = c("total_supply", "exports",  "imports",
#                                "processing", "production",
#                                "feed", "food", "losses", "other", "seed",
#                                "stock_withdrawal", "stock_addition"))

# cat("Using EIA/IEA ethanol production values where FAO's",
#     "CBS are not (or under-) reported.\n")
# eth_cbs[production < value | is.na(production), production := value]
# eth_cbs[, value := NULL]

# cat("\nUse national aggregates for ethanol trade.\n")
# eth_imps <- btd[item_code == 2659 & unit == "tonnes",
#                 list(value = sum(value, na.rm = TRUE)),
#                 by = list(to_code, year, item_code)]
# eth_exps <- btd[item_code == 2659 & unit == "tonnes",
#                 list(value = sum(value, na.rm = TRUE)),
#                 by = list(from_code, year, item_code)]

# eth_cbs <- merge(eth_cbs, eth_imps,
#                  by.x = c("area_code", "year", "item_code"),
#                  by.y = c("to_code", "year", "item_code"), all.x = TRUE)
# eth_cbs <- merge(eth_cbs, eth_exps,
#                  by.x = c("area_code", "year", "item_code"),
#                  by.y = c("from_code", "year", "item_code"), all.x = TRUE)

# cat("\nOverwrite CBS imports and exports of Ethanol with BTD data.\n")
# eth_cbs[, `:=`(imports = value.x, exports = value.y,
#                value.x = NULL, value.y = NULL)]
# eth_cbs <- dt_replace(eth_cbs, is.na, 0, cols = c("exports", "imports"))

# cat("\nReduce exports where they surpass total_supply.\n")
# eth_cbs[, `:=`(total_supply = production + imports,
#                other = total_supply - exports - stock_addition)]
# eth_cbs[other < 0, `:=`(exports = exports + other, other = 0)]

# Kick original cbs[item_code == 2659, ] and integrate this instead
# cbs <- rbindlist(list(cbs[item_code != 2659], eth_cbs), use.names = TRUE)


# Estimate missing CBS ----------------------------------------------------

# # Apply TCF
# tcf <- fread("inst/cbs_tcf.csv")
# # Items with multiple sources
# dupe_i <- tcf$item_code[duplicated(tcf$item_code)]
# # Sources with multiple items
# dupe_s <- tcf$source_code[duplicated(tcf$source_code)]

# setkey(crop_prod, item_code, item)
# setkey(tcf, item_code, item)
# tcf <- merge(tcf, crop_prod, all.x = TRUE)

# cat("\nApplying TCF to crop production.\n")
# tcf[, processing := production / tcf]

# Estimate gaps and shares of co-products and co-processes
cat("\nSkipped derivation of processing from supply and TCFs.\n")


# Rebalance columns -------------------------------------------------------

cat("\nRebalance CBS.\n")

cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("total_supply", "imports", "exports", "feed",
                           "food", "losses", "other", "processing",
                           "production", "seed"))

cat("\nAdjust ", cbs[total_supply != production + imports, .N],
    " observations of 'total_supply' to ",
    "`total_supply = production + imports`.\n", sep = "")
cbs[, total_supply := production + imports]

cat("\nCap out 'exports' (N = ", cbs[exports > total_supply, .N],
    "), 'processing' (N = ", cbs[processing > total_supply, .N],
    ") and 'seed' (N = ", cbs[seed > total_supply, .N],
    ") uses exceeding 'total_supply'.\n", sep = "")
cbs[exports > total_supply, exports := total_supply]
cbs[processing > total_supply, processing := total_supply]
cbs[seed > total_supply, seed := total_supply]

cat("\nSkipping capping out 'exports' at `total_supply - seed - processing`.\n")

cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := total_supply - stock_addition -
        (exports + food + feed + seed + losses + processing + other)]

cat("\nAllocate remaining supply to uses.\n")
cat("\nHops and live animals to 'processing'.\n")
cbs[item_code %in% c(677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
                     1126, 1157, 1140, 1150, 1171) & balance > 0,
    `:=`(processing = balance, balance = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
                     2661, 2746, 2748, 2747) & balance > 0,
    `:=`(other = balance, balance = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2536, 2537, 2555, 2559, 2544, 2590, 2591, 2592, 2593,
                     2594, 2595, 2596, 2597, 2598) & balance > 0,
    `:=`(feed = balance, balance = 0)]
cat("\nRest to 'food'.\n")
cbs[balance > 0, `:=`(food = balance, balance = 0)]


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
