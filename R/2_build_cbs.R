
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
            feed = 0, food = 0, losses = 0, processing = 0, seed = 0)]

cbs <- rbindlist(list(cbs, fore), use.names = TRUE)


# Estimate for missing commodities ----------------------------------------

cat("\nAdding crop and livestock data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop <- crop[element == "Production", ]
# Add this


# Add crop[element == "Seed", ] # Apparently dropped in newer versions, see #19

live <- readRDS("data/tidy/live_tidy.rds")

live <- live[element == "Production" & unit == "head", ]
cat("Removing items that also appear in the aggregated item",
    "'Meat indigenous, poultry'.\n")
live <- dt_filter(live, !item_code %in%
                    c("Duck" = 1070, "Geese" = 1077, "Bird" = 1084,
                      "Turkey" = 1087, "Chicken" = 1094))
# Add this


# Add BTD data ------------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/btd_full.rds")

# Fodder imports
aggregate(value ~ to_code + to + item_code + item + year, FUN = sum,
          data = btd[unit == "tonnes" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this to cbs$imports

# Fodder exports
aggregate(value ~ from_code + from + item_code + item + year, FUN = sum,
          data = btd[unit == "tonnes" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this to cbs$exports

# Livestock imports
aggregate(value ~ to_code + to + item_code + item + year, FUN = sum,
          data = btd[unit == "head" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this to cbs$imports

# Livestock exports
aggregate(value ~ from_code + from + item_code + item + year, FUN = sum,
          data = btd[unit == "head" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this to cbs$exports and do cbs$production <- ++


# Ethanol -----------------------------------------------------------------

# National aggregates of ethanol imports
eth_imp <- aggregate(value ~ to_code + to, FUN = sum,
                     data = btd[unit == "tons" & item_code == 2659])
# National aggregates of ethanol exports
eth_exp <- aggregate(value ~ from_code + from, FUN = sum,
                     data = btd[unit == "tons" & item_code == 2659])

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

# Kick original cbs[item_code == 2659, ] and integrate this instead


# Allocate supply to uses -------------------------------------------------

cat("\nAllocating supply to uses.\n")

cbs_ext <- rbindlist(list(...))

# Make sure production and trade are not negative
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
