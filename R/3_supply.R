
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
  supply, by = c("item_code", "item"), all.x = TRUE, allow.cartesian = TRUE)


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

cat("Applying meat shares to",
  sup[comm_code %in% c("c120", "c121", "c122", "c123", "c124"), .N],
  "observations of animal products.\n")
shares_m <- sup[comm_code %in% c("c115", "c116", "c117", "c118", "c119"),
                list(proc, share_m = production / sum(production, na.rm = TRUE)),
                by = list(area_code, year)]
shares_m[is.nan(share_m), share_m := 0]

sup <- merge(sup, shares_m, by = c("area_code", "year", "proc"), all.x = TRUE)
# Check whether results are correct -
# Shares of "Hides and skins" are often available and applied beforehand
sup[is.na(share) & !is.na(share_m) &
    comm_code %in% c("c120", "c121", "c122", "c123", "c124"),
    `:=`(production = production * share_m, share_m = NULL)]

cat("Applying oil extraction shares to",
  sup[comm_code %in% c("c090"), .N],
  "observations of oilseed cakes.\n")
shares_o <- sup[comm_code %in% c("c079", "c080", "c081"),
                list(proc, share_o = production / sum(production, na.rm = TRUE)),
                by = list(area_code, year)]
shares_o[is.nan(share_o), share_o := 0]

sup <- merge(sup, shares_o, by = c("area_code", "year", "proc"), all.x = TRUE)
sup[is.na(share) & !is.na(share_o) &
    comm_code %in% c("c120", "c121", "c122", "c123", "c124"),
    `:=`(production = production * share_o, share_o = NULL)]




# Calculate supply shares for other animal products based on meat shares
# Calculate supply shares for oil cakes

# Fill prices using BTD
