
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

years <- 1986:2017


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
btd_est <- readRDS("data/btd_est.rds")
cbs <- readRDS("data/cbs_full.rds")

areas <- unique(cbs$area_code)
items <- unique(cbs$item_code)

# Adjust CBS to have equalt export and import numbers per item per year
cbs_bal <- cbs[, list(exp_t = na_sum(exports), imp_t = na_sum(imports)),
  by = c("year", "item_code", "item")]
cbs_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
  area_code = 999, area = "Rest of World")]
# Absorb discrepancies in "RoW"
cbs <- merge(cbs, cbs_bal,
  by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
cbs[area_code == 999, `:=`(
  exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
  imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
cbs[, diff := NULL]

rm(cbs_bal); gc()

# Rebalance supply and use
cat("\nAdjust ", cbs[total_supply != na_sum(production, imports), .N],
    " observations of 'total_supply' to ",
    "`total_supply = production + imports`.\n", sep = "")
cbs[, total_supply := na_sum(production, imports)]
# To-do: Check whether we should just do this for "RoW"
cat("\nUpdate 'balancing' column with discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
  -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other)]


# Only keep relevant units in BTD
btd <- btd[unit %in% c("tonnes", "head", "m3"), ]
btd_est <- btd_est[year %in% years & item_code %in% items, ]

# Get data on target (CBS) trade
exp <- btd[, list(value = na_sum(value)),
  by = c("year", "from_code", "item_code")]
imp <- btd[, list(value = na_sum(value)),
  by = c("year", "to_code", "item_code")]

# Create info on target trade
exp <- cbs[, c("year", "area_code", "item_code", "exports")]
imp <- cbs[, c("year", "area_code", "item_code", "imports")]

target <- merge(exp, imp, by = c("year", "item_code", "area_code"), all = TRUE)

# Create array mapping importers to exporters per item per year
mapping <- as.data.table(array(NA,
  dim = c(length(areas), length(areas), length(items), length(years)),
  dimnames = list(areas, areas, items, years)))

# Fill this fucker with >=0 values from btd, then from btd_est
# Then RAS it per year per item (using `target`)
