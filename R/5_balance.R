
library("data.table")
library("mipfp")

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


# Prep btd

# Have RoW start always start at least at 1
btd[(from_code == 999 | to_code == 999) & value == 0, value := 1]

# Create structure to map imports to exports per item
# Create array mapping importers to exporters per item
mapping <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  item_code = rep(items, each = length(areas) ^ 2))
constraint <- data.table(
  area_code = rep(areas, each = length(items)),
  item_code = rep(items, times = length(areas)))


# Fill this array per year with (1) btd values, (2) estimated values
# Then do some iterative proportional fitting
for(y in years) {
  # Add btd values
  mapping <- merge(mapping,
    btd[year == y, c("from_code", "to_code", "item_code", "value")],
    all.x = TRUE, by = c("from_code", "to_code", "item_code"))
  # Add btd_est values
  mapping <- merge(mapping,
    btd_est[year == y, .(from_code, to_code, item_code, val_est = value)],
    all.x = TRUE, by = c("from_code", "to_code", "item_code"))
  mapping[, `:=`(value = ifelse(is.na(value), val_est, value), val_est = NULL)]
  # Prepare constraints for RAS
  constraint <- merge(constraint,
    target[year == y, c("area_code", "item_code", "imports", "exports")],
    by = c("area_code", "item_code"), all.x = TRUE)
  # Replace NAs with 0
  constraint[, `:=`(imports = ifelse(is.na(imports), 0, imports),
    exports = ifelse(is.na(exports), 0, exports))]

  # Change mapping into an ugly array
  mapping_arr <- array(as.numeric(unlist(
    lapply(split(mapping, by = "item_code"), function(x) {
      out <- dcast(x, from_code ~ to_code)[, -"from_code"]
      dt_replace(out, is.na, 0, verbose = FALSE)}))),
    dim = c(length(areas), length(areas), length(items)),
    dimnames = list(areas, areas, items))

  for(i in as.character(items)) {
    mapping_arr[, , i] <- Ipfp(mapping_arr[, , i],
      target.list = list(1, 2),
      target.data = constraint[item_code == i, c("exports", "imports")])$x.hat
  }
}



filling_arr <- array(as.numeric(
  unlist(lapply(split(x, by = "item_code"), dcast, from_code ~ to_code))),
  dim = c(length(areas), length(areas), length(items)),
  dimnames = list(areas, areas, items))


mapping <- as.data.table(array(NA,
  dim = c(length(areas), length(areas), length(items)),
  dimnames = list(areas, areas, items)))

mapping <- data.table(
  from_code = rep(areas, times = length(areas) * length(items) * length(years)),
  to_code = rep(areas, each = length(areas), times = length(items) * length(years)),
  item_code = rep(items, each = length(areas) ^ 2, times = length(years)),
  year = rep(years, each = length(areas) ^ 2 * length(items)))

# Fill this fucker with >=0 values from btd, then from btd_est
# Then RAS it per year per item (using `target`)

x <- data.table(
  from_code = rep(areas, each = length(areas), times =  length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  item_code = rep(items, each = length(areas) ^ 2),
  value = rnorm(length(areas) ^ 2 * length(items))
)
x

x <- array(as.numeric(unlist(lapply(split(x, by = c("item")), dcast, area1 ~ area2))),
  dim = c(length(areas), length(areas), length(items)),
  dimnames = list(areas, areas, items))

for(i in items) {
  print(
    diff(unlist(lapply(target[item_code == i, .(exports, imports)], na_sum))) <= 0.1
  )
}