
library(data.table)

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")
supply <- fread("inst/items_supply.csv")

cat("Allocate production to supplying processes (incl. double-counting).\n")
# Check whether this should really be production (see Issue #37)
sup <- merge(
  cbs[, c("area_code", "area", "year", "item_code", "item", "production")],
  supply, all = TRUE,
  by = c("item_code", "item"), allow.cartesian = TRUE)


cat("Calculate supply shares for livestock products.\n")
shares <- fread("inst/items_supply-shares.csv")
live <- readRDS("data/tidy/live_tidy.rds")

shares <- merge(shares[source == "live"], live[element == "Production"],
  by.x = c("base_code", "base"), by.y = c("item_code", "item"), all.x = TRUE)

# Aggregate values
shares <- shares[, list(value = sum(value, na.rm = TRUE)),
                 by = list(area_code, area, year, proc_code, proc,
                           comm_code, item_code, item)]
# Add totals
shares <- merge(
  shares, all.x = TRUE,
  shares[, list(total = sum(value, na.rm = TRUE)),
         by = list(area_code, area, year, comm_code, item_code, item)])

shares[, share := value / total]

sup <- merge(
  sup, shares[, c("area_code", "year", "comm_code", "proc_code", "share")],
  by = c("area_code", "year", "comm_code", "proc_code"), all.x = TRUE)

cat("Applying livestock shares to",
  sup[comm_code %in% shares$comm_code, .N], "observations.\n")
sup[is.na(share) & comm_code %in% shares$comm_code, production := 0]
sup[!is.na(share) & comm_code %in% shares$comm_code,
    production := production * share]


shares_meat <- sup[comm_code %in% c("c115", "c116", "c117", "c118", "c119"),
                   list(area_code, year, proc,
                        share = production / sum(production, na.rm = TRUE))]

sup[comm_code %in% c("c120", "c121", "c122", "c123", "c124"),
    share]

# Calculate supply shares for other animal products based on meat shares
# Calculate supply shares for oil cakes

# Fill prices using BTD
