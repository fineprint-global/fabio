
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
imports <- btd[item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                976, 1016, 1034, 2029, 1096, 1107, 1110,
                                1126, 1157, 1140, 1150, 1171, 843),
               list(value = sum(value, na.rm = TRUE)),
               by = list(to_code, to, item_code, item, year, unit)]

# Exports
exports <- btd[item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                976, 1016, 1034, 2029, 1096, 1107, 1110,
                                1126, 1157, 1140, 1150, 1171, 843),
               list(value = sum(value, na.rm = TRUE)),
               by = list(from_code, from, item_code, item, year, unit)]

cat("\nAdding export and import data from BTD.\n")

# Add this, also add to production


# Ethanol -----------------------------------------------------------------

# National aggregates of ethanol imports
eth_imp <- aggregate(value ~ to_code + to, FUN = sum,
                     data = btd[unit == "tons" & item_code == 2659])
# National aggregates of ethanol exports
eth_exp <- aggregate(value ~ from_code + from, FUN = sum,
                     data = btd[unit == "tons" & item_code == 2659])

cat("\nAdding ethanol production data.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")
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

# Use national aggregates for imports / exports
eth_cbs <- merge(eth_cbs, eth_exp[, c("from_code", "value")],
                 by.x = "area_code", by.y = "from_code", all.x = TRUE)
eth_cbs <- merge(eth_cbs, eth_imp[, c("to_code", "value")],
                 by.x = "area_code", by.y = "to_code", all.x = TRUE)

cat("Overwrite CBS imports and exports of Ethanol with BTD data.")
eth_cbs[, `:=`(exports = value.x, imports = value.y,
               value.x = NULL, value.y = NULL)]
eth_cbs <- dt_replace(eth_cbs, is.na, 0, cols = c("exports",  "imports"))

eth_cbs[, `:=`(total_supply = production + imports,
               other = total_supply - exports - stock_addition)]

# Balance CBS
eth_cbs[other < 0, `:=`(exports = exports + other, other = 0)]

# Kick cbs[item_code == 2659, ] and integrate this instead


# Estimate missing CBS ----------------------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]

# Do TCF on this?

cat("\nJoining cbs and crop production using keys.\n")
setkey(cbs, area, area_code, item, item_code, year)
setkey(crop_prod, area, area_code, item, item_code, year)
cbs <- merge(cbs, crop_prod, all.x = TRUE, all.y = TRUE)

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

# cat("\nAdding livestock data.\n")
# live <- readRDS("data/tidy/live_tidy.rds")
# live <- live[element == "Production" & unit == "head", ]
# setkey(live, area, area_code, item, item_code, year)
# Only "Meat indigenous, ..." is missing. These need not be filled.
# Some items are available as stocks instead of production.
cat("Skipped filling cbs livestock. See Issues #22, #23 and #24.")

# Estimate required processing inputs for processed products
  # use TCF_prod.csv
  # estimate share of sugar beet and cane in sugar and molasses production

tcf <- fread("inst/cbs_tcf.csv")
# Items with multiple sources
dupe_i <- tcf$item_code[duplicated(tcf$item_code)]
# Sources with multiple items
dupe_s <- tcf$source_code[duplicated(tcf$source_code)]

setkey(crop_prod, item_code, item)
setkey(tcf, item_code, item)
tcf <- merge(tcf,
             crop_prod[, c("item_code", "item", "area_code", "area", "year",
                       "value")],
             all.x = TRUE)
tcf[, `:=`(processing = ifelse(is.na(tcf), processing, production / tcf))]

tcf[item_code %in% dupe_i, denom := sum(production),
    by = list(item_code, area_code, year)]
tcf[item_code %in% dupe_i, processing := processing / denom * production]

x[source_code %in% dupe_i, denom := sum(production),
    by = list(item_code, area_code, year)]
x[source_code %in% dupe_s, processing := processing / denom * production]


sugar <- tcf[item_code %in% c(2544, 2818)]


# Estimate gaps for co-products

# Maybe allocate supply to uses here



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

