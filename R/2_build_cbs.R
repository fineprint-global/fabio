
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

fore <- readRDS("data/tidy/fore_prod_tidy.rds")

fore[, `:=`(total_supply = production + imports,
            other = production + imports - exports,
            stock_withdrawal = 0, stock_addition = 0,
            feed = 0, food = 0, losses = 0, processing = 0, seed = 0)]

cbs <- rbindlist(list(cbs, fore), use.names = TRUE)


# Estimate for missing commodities ----------------------------------------

crop <- readRDS("data/tidy/crop_tidy.rds")

crop <- crop[element == "Production", ]
# Add this


# Add crop[element == "Seed", ] # Apparently dropped in newer versions, see #19

live <- readRDS("data/tidy/live_tidy.rds")

live <- live[element == "Production" & unit == "head"]
cat("Removing items that appear in the aggregated 'Meat indigenous, poultry'.\n")
live <- dt_filter(live, !item_code %in%
                    c("Duck" = 1070, "Geese" = 1077, "Bird" = 1084,
                      "Turkey" = 1087, "Chicken" = 1094))
# Add this


# Add BTD data ------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")

# Fodder imports
aggregate(value ~ to_code + to + item_code + item + year, FUN = sum,
          data = btd[unit == "tonnes" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this

# Fodder exports
aggregate(value ~ from_code + from + item_code + item + year, FUN = sum,
          data = btd[unit == "tonnes" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this

# Livestock imports
aggregate(value ~ to_code + to + item_code + item + year, FUN = sum,
          data = btd[unit == "head" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this

# Livestock exports
aggregate(value ~ from_code + from + item_code + item + year, FUN = sum,
          data = btd[unit == "head" &
                       item_code %in% c(328, 254, 677, 2000, 2001, 866, 946,
                                        976, 1016, 1034, 2029, 1096, 1107, 1110,
                                        1126, 1157, 1140, 1150, 1171, 843)])
# Add this, also add to production


# Allocate supply to uses -------------------------------------------------

# Make sure production and trade are not negative
cbs_ext <- dt_replace(cbs_ext, function(x) {`<`(x, 0)}, value = 0,
                      cols = c("production", "processing",
                               "imports", "exports", "total_supply",
                               "food", "feed", "seed", "losses", "other"))

cbs_ext[, total_supply := production + imports]

# Estimate pet food production
cbs_ext[item_code == 843 & exports > total_supply,
        production := processing + exports + food + feed + seed + losses + other]
