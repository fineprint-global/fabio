
library(data.table)

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
shares <- fread("inst/items_supply-shares.csv")
supply <- fread("inst/items_supply.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")

# Allocate production to supplying processes (incl. double-counting)
for(year) {for(region) {for(item){
sup[region & item & year] <- cbs[region & item & year, production]
}}}

# Calculate supply shares
shares <- merge(shares[source == "live"], live[element == "Production"])
shares <- shares[, list(value = sum(value, na.rm = TRUE)), list(...)]
basis <- shares[, list(value = sum(value, na.rm = TRUE)), list(...)]

sup <- sup * shares

# Calculate supply shares for other animal products based on meat shares
# Calculate supply shares for oil cakes

# Fill prices using BTD
