
library("data.table")
library("Matrix")
library("mipfp")

source("R/01_tidy_functions.R")

years <- 1986:2013


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
btd_est <- readRDS("data/btd_est.rds")
cbs <- readRDS("data/cbs_full.rds")

areas <- unique(cbs$area_code)
items <- unique(cbs$item_code)


# Prepare for creating balanced BTD sheets --------------------------------

# Subset to only keep relevant units, years and items
btd <- btd[unit %in% c("tonnes", "head", "m3") & item_code %in% items &
             from_code %in% areas & to_code %in% areas, ]
btd_est <- btd_est[year %in% years & item_code %in% items &
                     from_code %in% areas & to_code %in% areas, ]

# Start values are defined in btd_est
# # Have RoW start at least at 1 so it can always be scaled
# btd[((from_code == 999 & to_code != 999) | (from_code != 999 & to_code == 999)) &
#   value == 0, value := 1]

# Get info on target trade from CBS
target <- cbs[year %in% years, c("year", "area_code", "item_code", "exports", "imports")]

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
  # Prepare target-constraints for RAS
  constraint <- merge(constr_templ,
    target[year == y, c("area_code", "item_code", "imports", "exports")],
    by = c("area_code", "item_code"), all.x = TRUE)
  # Replace NA constraints with 0
  constraint[, `:=`(imports = ifelse(is.na(imports), 0, imports),
    exports = ifelse(is.na(exports), 0, exports))]

  # Eliminate estimates where data exist
  mapping[, val_est := ifelse(is.na(value), val_est, NA)]
  # Calculate totals for values and estimates per exporting country and item
  mapping[, `:=`(value_sum = na_sum(value), val_est_sum = na_sum(val_est)),
          by = c("from_code","item_code")]
  # Add export target
  mapping[, val_target := constraint$exports[match(paste(mapping$from_code, mapping$item_code),
                                                   paste(constraint$area_code, constraint$item_code))]]
  # Calculate export gap
  mapping[, gap := na_sum(val_target, -value_sum)]
  # Downscale export estimates in order not to exceed the total gap between reported exports and target values
  mapping[, val_est := ifelse(gap > 0, ifelse(gap < val_est_sum, val_est / val_est_sum * gap, val_est), NA)]

  # Assign estimates to value column with a weight of 50%
  mapping[, `:=`(
    value = ifelse(is.na(value), ifelse(is.na(val_est), 0, val_est * 0.5), value),
    val_est = NULL)]


  # Restructure in a list with matrices per item
  mapping_ras <- lapply(
    split(mapping, by = "item_code", keep.by = FALSE),
    function(x) {
      out <- data.table::dcast(x, from_code ~ to_code,
        fun.aggregate = sum, value.var = "value")[, -"from_code"]
      as(out, "Matrix")})

  # Run iterative proportional fitting per item
  for(j in as.character(items)) {
    mapping_ras[[j]] <- Ipfp(mapping_ras[[j]],
      target.list = list(1, 2), iter = 100, tol.margins = 1E5,
      target.data = constraint[item_code == j, .(round(exports), round(imports))])$x.hat
  }

  btd_bal[[i]] <- lapply(names(mapping_ras), function(name) {
    out <- mapping_ras[[name]]
    out <- data.table(from_code = colnames(out), as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
    out[, .(year = y, item_code = as.integer(name),
      from_code = as.integer(from_code), to_code = as.integer(to_code), value)]
  })

  cat("Calculated year ", y, ".\n", sep = "")
}

# One datatable per year
btd_bal <- lapply(btd_bal, rbindlist)
# One datatable
btd_bal <- rbindlist(btd_bal)
# Add commodity codes
items <- fread("inst/items_full.csv")
btd_bal[, comm_code := items$comm_code[match(btd_bal$item_code, items$item_code)]]


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_bal, "data/btd_bal.rds")
