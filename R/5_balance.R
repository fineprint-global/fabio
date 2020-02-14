
library("data.table")
library("Matrix")
library("mipfp")

source("R/1_tidy_functions.R")

years <- 1986:2017


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
btd_est <- readRDS("data/btd_est.rds")
cbs <- readRDS("data/cbs_full.rds")

areas <- unique(cbs$area_code)
items <- unique(cbs$item_code)

# Adjust CBS to have equal export and import numbers per item per year
# This is very helpful for the iterative proportional fitting
cbs_bal <- cbs[, list(exp_t = na_sum(exports), imp_t = na_sum(imports)),
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

# Rebalance supply and use
cat("\nAdjust ", cbs[total_supply != na_sum(production, imports), .N],
    " observations of 'total_supply' to ",
    "`total_supply = production + imports`.\n", sep = "")
cbs[, total_supply := na_sum(production, imports)]
# To-do: Check whether we should just do this for "RoW"
cat("\nUpdate 'balancing' column with discrepancies.\n")
cbs[, balancing := na_sum(total_supply,
  -stock_addition, -exports, -food, -feed, -seed, -losses, -processing, -other)]


# Prepare for creating balanced BTD sheets --------------------------------

# Subset to only keep relevant units, years and items
btd <- btd[unit %in% c("tonnes", "head", "m3"), ]
btd_est <- btd_est[year %in% years & item_code %in% items, ]

# Have RoW start at least at 1 so it can always be scaled
btd[(from_code == 999 | to_code == 999) & value == 0, value := 1]
# Then kick out values <= 0 and use estimates for those
btd <- btd[value > 0, ]

# Get info on target trade from CBS
exp <- cbs[, c("year", "area_code", "item_code", "exports")]
imp <- cbs[, c("year", "area_code", "item_code", "imports")]
target <- merge(exp, imp, by = c("year", "item_code", "area_code"), all = TRUE)


# To-do: Rework merging and structures
# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  item_code = rep(items, each = length(areas) ^ 2))
constr_templ <- data.table(
  area_code = rep(areas, each = length(items)),
  item_code = rep(items, times = length(areas)))

# Fill this structure per year with (1) btd values, (2) estimated values
# Then do some iterative proportional fitting to approximate target values
# Note that we loop this over years, so memory requirements can easily be
# reduced if necessary.
btd_bal <- vector("list", length(years))
names(btd_bal) <- years

for(i in seq_along(years)) {
  y <- years[i]
  # Add BTD values to the template
  mapping <- merge(mapping_templ,
    btd[year == y, c("from_code", "to_code", "item_code", "value")],
    by = c("from_code", "to_code", "item_code"), all.x = TRUE)
  # Add estimates
  mapping <- merge(mapping,
    btd_est[year == y, .(from_code, to_code, item_code, val_est = value)],
    by = c("from_code", "to_code", "item_code"), all.x = TRUE)
  mapping[, `:=`(val_est = NULL,
    value = ifelse(is.na(value), ifelse(is.na(val_est), 0, val_est), value))]
  # Prepare target-constraints for RAS
  constraint <- merge(constr_templ,
    target[year == y, c("area_code", "item_code", "imports", "exports")],
    by = c("area_code", "item_code"), all.x = TRUE)
  # Replace NA constraints with 0
  constraint[, `:=`(imports = ifelse(is.na(imports), 0, imports),
    exports = ifelse(is.na(exports), 0, exports))]

  # Restructure in a list with matrices per item
  mapping_ras <- lapply(
    split(mapping, by = "item_code", keep.by = FALSE),
    function(x) {
      out <- dcast(x, from_code ~ to_code,
        fun.aggregate = sum, value.var = "value")[, -"from_code"]
      as(out, "Matrix")})

  # Run iterative proportional fitting per item
  # To-do: This could be parallelised
  for(j in as.character(items)) {
    mapping_ras[[j]] <- Ipfp(mapping_ras[[j]],
      target.list = list(1, 2), iter = 100,
      target.data = constraint[item_code == j, .(exports, imports)])$x.hat
  }
  # ras_data <- parLapply(cl, as.character(items), function(j, map_list, constr) {
  #   library("Matrix")
  #   library("mipfp")
  #   Ipfp(map_list[[j]], target.list = list(1, 2), iter = 100,
  #     target.data = constr[item_code == j, .(exports, imports)])$x.hat
  # }, mapping_ras, constraint)
  # names(ras_data) <- as.character(items)

  btd_bal[[i]] <- lapply(names(mapping_ras), function(name) {
    out <- mapping_ras[[name]]
    out <- data.table(from_code = colnames(out), as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code")
    out[, .(item_code = name, from_code = as.integer(from_code),
      to_code = as.integer(to_code), value)]
  })
  # btd_bal[[i]] <- parLapply(cl, names(ras_data), function(name, data) {
  #   out <- data[[name]]
  #   out <- data.table(from_code = colnames(out), as.matrix(out))
  #   out <- melt(out, id.vars = c("from_code"), variable.name = "to_code")
  #   out[, .(item_code = name, from_code = as.integer(from_code),
  #     to_code = as.integer(to_code), value)]
  # })

  cat("Calculated year ", y, ".\n", sep = "")
}

# One datatable per year
btd_bal <- lapply(btd_bal, rbindlist)
# One datatable
btd_bal <- rbindlist(btd_bal)


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_bal, "data/btd_bal.rds")
saveRDS(cbs, "data/cbs_bal.rds")
