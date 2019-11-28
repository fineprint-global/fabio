
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


# Fill prices using BTD ---------------------------------------------------

sup_usd <- sup

prices <- btd[, list(value = sum(value, na.rm = TRUE)),
              by = list(from, from_code, item_code, item, unit, year)]
prices <- dcast(prices,
      from + from_code + item + item_code + year ~ unit, value.var = "value")
# Prices of item Alcohol are problematic.
# Estimation just based on available values of tonnes does not seem to work.
# See Issue #42, for now I'll assume alcohol weighs 1kg per liter and fill
# tonnes accordingly, unless no liter value is available.
prices[item_code == 2659 & litres > 0, `:=`(tonnes = litres / 1000)]
prices[, price := ifelse(tonnes != 0 & !is.na(tonnes), usd / tonnes,
                    ifelse(head != 0 & !is.na(head), usd / head, usd / m3))]

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
prices_world <- prices[, list(usd = na_sum(usd),
                              tonnes = na_sum(tonnes), head = na_sum(head),
                              litres = na_sum(litres), m3 = na_sum(m3)),
                       by = list(item, item_code, year)]
prices_world[, price_world := ifelse(tonnes != 0, usd / tonnes,
                                ifelse(head != 0, usd / head, usd / m3))]
prices <- merge(
  prices, prices_world[, c("year", "item_code", "item", "price_world")],
  by = c("year", "item_code", "item"), all.x = TRUE)

cat("Filling ", prices[is.na(price) & !is.na(price_world), .N],
  " missing prices with worldprices.\n", sep = "")
prices[is.na(price), price := price_world]

cat("Filling ", prices[is.na(price) & !is.na(price_q50), .N],
  " missing prices with median item prices.\n", sep = "")
prices[is.na(price), price := price_q50]

sup_usd <- merge(sup_usd, all.x = TRUE,
  prices[, c("from_code", "from", "item", "item_code", "year", "price")],
  by.x = c("area_code", "area", "item", "item_code", "year"),
  by.y = c("from_code", "from", "item", "item_code", "year"))


# Store results -----------------------------------------------------------

saveRDS(sup_usd, "data/sup.rds")
