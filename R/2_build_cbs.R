
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


# Add BTD data ------------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/btd_full.rds")

# Imports
imps <- btd[unit != "usd", # tonnes, head, m3 and litres remain
            list(value = sum(value, na.rm = TRUE)),
            by = list(to_code, to, item_code, item, year, unit)]
imps <- dcast(imps, to_code + to + item_code + item + year ~ unit,
              value.var = "value")
# We prefer head or m3 if available, otherwise tonnes
imps[, value := ifelse(!is.na(head), head, ifelse(!is.na(m3), m3, tonnes))]

# Exports
exps <- btd[unit != "usd", # tonnes, head, m3 and litres remain
            list(value = sum(value, na.rm = TRUE)),
            by = list(from_code, from, item_code, item, year, unit)]

cat("\nAdding export and import data for missing CBS from BTD.\n")
cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports),
           value = NULL)]
cbs <- merge(
  cbs, exps[,
    c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports),
           value = NULL)]
cat("\nAdding missing livestock exports to CBS production from BTD.\n")
cbs <- merge(
  cbs, exps[unit == "head",
            c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE, all.y = TRUE)
cbs[, `:=`(production = ifelse(is.na(value), production, production + value),
           value = NULL)]


# Ethanol -----------------------------------------------------------------

cat("\nAdding ethanol production data.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")

# Keep one unit and recode for merging
eth <- eth[, `:=`(unit = NULL,
                  item = "Alcohol, Non-Food", item_code = 2659)]
eth_cbs <- dt_filter(cbs, item_code == 2659)

eth_cbs <- merge(eth_cbs, eth, all = TRUE,
                 by = c("area_code", "area", "year", "item", "item_code"))

eth_cbs <- dt_replace(eth_cbs, is.na, 0,
                      cols = c("total_supply", "exports",  "imports",
                               "processing", "production",
                               "feed", "food", "losses", "other", "seed",
                               "stock_withdrawal", "stock_addition"))

cat("Using EIA/IEA ethanol production values where FAO's",
    "CBS are not (or under-) reported.\n")
eth_cbs[production < value, production := value]
eth_cbs[, value := NULL]

cat("\nUse national aggregates for ethanol trade.\n")
eth_imps <- btd[item_code == 2659 & unit == "tonnes",
                list(value = sum(value, na.rm = TRUE)),
                by = list(to_code, year, item_code)]
eth_exps <- btd[item_code == 2659 & unit == "tonnes",
                list(value = sum(value, na.rm = TRUE)),
                by = list(from_code, year, item_code)]

eth_cbs <- merge(eth_cbs, eth_imps,
                 by.x = c("area_code", "year", "item_code"),
                 by.y = c("to_code", "year", "item_code"), all.x = TRUE)
eth_cbs <- merge(eth_cbs, eth_exps,
                 by.x = c("area_code", "year", "item_code"),
                 by.y = c("from_code", "year", "item_code"), all.x = TRUE)

cat("\nOverwrite CBS imports and exports of Ethanol with BTD data.\n")
eth_cbs[, `:=`(imports = value.x, exports = value.y,
               value.x = NULL, value.y = NULL)]
eth_cbs <- dt_replace(eth_cbs, is.na, 0, cols = c("exports", "imports"))

cat("\nReduce exports where they surpass total_supply.\n")
eth_cbs[, `:=`(total_supply = production + imports,
               other = total_supply - exports - stock_addition)]
eth_cbs[other < 0, `:=`(exports = exports + other, other = 0)]

# Kick original cbs[item_code == 2659, ] and integrate this instead
cbs <- rbindlist(list(cbs[item_code != 2659], eth_cbs), use.names = TRUE)

# Rebalance columns -------------------------------------------------------


# Estimate missing CBS ----------------------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(production = value, value = NULL, element = NULL, unit = NULL)]

# Add trade data
crop_imps <- btd[item_code %in% crop_prod$item_code & unit == "tonnes",
                 list(value = sum(value, na.rm = TRUE)),
                 by = list(to_code, year, item_code)]
crop_exps <- btd[item_code %in% crop_prod$item_code & unit == "tonnes",
                 list(value = sum(value, na.rm = TRUE)),
                 by = list(from_code, year, item_code)]

crop_prod <- merge(crop_prod, crop_imps,
                   by.x = c("area_code", "year", "item_code"),
                   by.y = c("to_code", "year", "item_code"), all.x = TRUE)
crop_prod <- merge(crop_prod, crop_exps,
                   by.x = c("area_code", "year", "item_code"),
                   by.y = c("from_code", "year", "item_code"), all.x = TRUE)
cat("\nAdd trade data from BTD to crop_prod.\n")
crop_prod[, `:=`(imports = value.x, exports = value.y,
                 value.x = NULL, value.y = NULL)]
crop_prod <- dt_replace(crop_prod, is.na, 0, cols = c("exports", "imports"))


cat("\nFilling missing cbs production with crop production data. Items:\n",
    paste0(unique(cbs[is.na(production) & !is.na(value), item]),
           collapse = ", "),
    ".\n", sep = "")
cbs[is.na(production), production := value]
cbs[, value := NULL]

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
#
cat("\nSkipped derivation of processing from supply and TCFs.\n")


# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkipped filling cbs seed.",
    "Apparently data is dropped in newer versions. See Issue #19.\n")

# cat("\nAdding livestock data.\n")
# live <- readRDS("data/tidy/live_tidy.rds")
# live <- live[element == "Production" & unit == "head", ]
# setkey(live, area, area_code, item, item_code, year)
# Only "Meat indigenous, ..." is missing. These need not be filled.
# Some items are available as stocks instead of production.
cat("Skipped filling cbs livestock. See Issues #22, #23 and #24.")


# Allocate supply to uses -------------------------------------------------

cat("\nAllocating supply to uses.\n")

cbs_ext <- rbindlist(list(...))

# Make sure production, allow.cartesian = TRUE and trade are not negative
cbs_ext <- dt_replace(cbs_ext, function(x) {`<`(x, 0)}, value = 0,
                      cols = c("production", "processing",
                               "imports", "exports", "total_supply",
                               "food", "feed", "seed", "losses", "other"))

# Estimate pet food production
cbs_ext[item_code == 843 & exports > total_supply,
        production := processing + exports + food + feed + seed + losses + other]

# Calculate total supply
cbs_ext[, total_supply := production + imports]

# Balance CBS where exports exceed total_supply
cbs_ext[exports > total_supply,
        balancing := total_supply - (processing + exports +
          food + feed + seed + losses + other)]

# For cotton, oil palm, hops and livestock:
# Processing use = Production + Imports - Exports - Balancing
cbs_ext[item_code %in% c(328, 254, 677, 866, 946, 976, 1016, 1034, 2029,
                         1096, 1107, 1110, 1126, 1157, 1140, 1150, 1171),
        processing := total_supply - (processing + exports +
          food + feed + seed + losses + other + balancing)]

# For pet food:
# Food use = Production + Imports - Exports - Balancing
cbs_ext[item_code == 843,
        food := total_supply - (processing + exports + food + feed +
                                  seed + losses + other + balancing)]

# For fodder crops:
# Feed use = Production + Imports - Exports - Balancing
cbs_ext[item_code == 2000,
        feed := total_supply - (processing + exports + food + feed +
                                  seed + losses + other + balancing)]

# Integrate
rbindlist(list(cbs, cbs_ext))


# Estimate missing CBS ----------------------------------------------------

# Estimate required processing inputs for processed products
  # use TCF_prod.csv and apply it to crop production:
    # tcf$processing <- crop$production / tcf
    # tcf$production <- crop$production
  # Estimate share of sugar beet and cane in sugar and molasses production
    # sugar <- tcf[item_code %in% c(2544, 2818), ]
    # sugar$production[item == a <- crop$production[item == a]
    # sugar$production[item == b] <- crop$production[item == b]
    # sugar$processing[item == a] <- sugar$processing[item == a] /
    #                                  (crop$production[item == a] + crop$production[item == b]) * crop$production[item == a]
    # Overwrite tcf$processing
    # Adapt tcf$production

# Estimate gaps for co-products

# Maybe allocate supply to uses here

# Weird check
