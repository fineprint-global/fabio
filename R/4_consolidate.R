
library("data.table")

regions
items
cbs
btd


# Fix up countries -----

# Aggregate RoW countries in CBS
cbs[!area_code %in% regions$area_code, `:=`(
  area_code = 999, area = "Rest of World")]
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Remove unspecified and adjustment countries from the BTD
btd <- btd[!from_code %in% c(252, 254) & !to_code %in% c(252, 254), ]

# Aggregate RoW countries in BTD
btd[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]


# Other fixes -----

btd <- btd[item_code %in% items$item_code, ]

btd[value < 0, value := 0]

# Rest done in cbs script
