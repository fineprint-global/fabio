
library("data.table")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_bal.rds")
sup <- fread("inst/items_supply.csv")

cat("Allocate production to supplying processes (incl. double-counting).\n")

# Add grazing placeholder to the CBS
grazing <- unique(cbs[, c("year", "area", "area_code")])
grazing[, `:=`(item = "Grazing", item_code = 2001)]
cbs <- rbindlist(list(cbs, grazing), use.names = TRUE, fill = TRUE)

# Kick "Pet food" and "Live animals, other", and "Grazing"
# First two are not present in the CBS, latter is imputed from use
sup <- sup[!item_code %in% c(2001, 1171, 843)]

sup <- merge(
  cbs[, c("area_code", "area", "year", "item_code", "item", "production")],
  sup, by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)


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

sup <- merge(sup,
  shares[, c("area_code", "area", "year", "comm_code", "proc_code", "share")],
  by = c("area_code", "area", "year", "comm_code", "proc_code"), all.x = TRUE)

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
  `:=`(production = production * share_m)]
sup[, share_m := NULL]

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
  `:=`(production = production * share_o)]
sup[, share_o := NULL]

sup[, share := NULL]


# Fill prices using BTD ---------------------------------------------------

prices <- dcast(btd, from + from_code + to + to_code +
  item + item_code + year ~ unit, value.var = "value")
prices <- prices[!is.na(usd) & usd > 0,
  list(usd = sum(usd, na.rm = TRUE), head = sum(head, na.rm = TRUE),
    tonnes = sum(tonnes, na.rm = TRUE), m3 = sum(m3, na.rm = TRUE)),
    by = list(from, from_code, item_code, item, year)]

prices[, price := ifelse(tonnes != 0 & !is.na(tonnes), usd / tonnes,
  ifelse(head != 0 & !is.na(head), usd / head, ifelse(m3 != 0, usd / m3, NA)))]

# Originally prices were capped at 20% / 500% of the world average
# Quantiles might be more robust - we'll go for the 5th and 95th one.
# We might want to add a yearly element.
caps <- prices[, list(price_q95 = quantile(price, .95, na.rm = TRUE),
  price_q50 = quantile(price, .50, na.rm = TRUE),
  price_q05 = quantile(price, .05, na.rm = TRUE)),
  by = list(item)]
prices <- merge(prices, caps, by = "item", all.x = TRUE)

cat("Capping ", prices[price > price_q95 | price < price_q05, .N],
  " prices at the specific item's 95th and 5th quantiles.\n", sep = "")
prices[, price := ifelse(price > price_q95, price_q95,
  ifelse(price < price_q05, price_q05, price))]

# Get worldprices to fill gaps
na_sum <- function(x) {ifelse(all(is.na(x)), NA_real_, sum(x, na.rm = TRUE))}
prices_world <- prices[!is.na(usd), list(usd = na_sum(usd),
  tonnes = na_sum(tonnes), head = na_sum(head),
  m3 = na_sum(m3)), by = list(item, item_code, year)]
prices_world[, price_world := ifelse(head != 0, usd / head,
  ifelse(tonnes != 0, usd / tonnes, usd / m3))]
prices <- merge(
  prices, prices_world[, c("year", "item_code", "item", "price_world")],
  by = c("year", "item_code", "item"), all.x = TRUE)

cat("Filling ", prices[is.na(price) & !is.na(price_world), .N],
  " missing prices with worldprices.\n", sep = "")
prices[is.na(price), price := price_world]

cat("Filling ", prices[is.na(price) & !is.na(price_q50), .N],
  " missing prices with median item prices.\n", sep = "")
prices[is.na(price), price := price_q50]

sup <- merge(sup, all.x = TRUE,
  prices[, c("from_code", "from", "item", "item_code", "year", "price")],
  by.x = c("area_code", "area", "item", "item_code", "year"),
  by.y = c("from_code", "from", "item", "item_code", "year"))


# Store results -----------------------------------------------------------

saveRDS(sup, "data/sup.rds")
