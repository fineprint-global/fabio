
library("data.table")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# Supply ------------------------------------------------------------------

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")
sup <- fread("inst/items_supply.csv")


cat("Allocate production to supplying processes.\n")

# Add grazing placeholder to the CBS
grazing <- unique(cbs[, c("year", "area", "area_code")])
grazing[, `:=`(item = "Grazing", item_code = 2001)]
cbs <- rbindlist(list(cbs, grazing), use.names = TRUE, fill = TRUE)

# Allocate production to supplying processes including double-counting
sup <- merge(
  cbs[, c("area_code", "area", "year", "item_code", "item", "production")],
  sup[item_code %in% unique(cbs$item_code)],
  by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)

# correct comm_codes
sup$comm_code <- items$comm_code[match(sup$item_code, items$item_code)]

# Downscale double-counted production
cat("Calculate supply shares for livestock products.\n")
shares <- fread("inst/items_supply-shares.csv")
live <- readRDS("data/tidy/live_tidy.rds")

# correct comm_codes
shares$comm_code <- items$comm_code[match(shares$item_code, items$item_code)]

shares <- merge(shares[source == "live"], live[element == "Production"],
  by.x = c("base_code", "base"), by.y = c("item_code", "item"),
  all.x = TRUE, allow.cartesian=TRUE)

# Add regions to RoW if not included in CBS
shares[, `:=`(area = ifelse(!area_code %in% regions$code[regions$current], "RoW", area),
              area_code = ifelse(!area_code %in% regions$code[regions$current], 999, area_code))]

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

cat("Applying oil extraction shares to",
  sup[item_code %in% c(2598), .N],
  "observations of Oilseed Cakes, Other with shares from Ricebran Oil, Maize Germ Oil and Oilcrops Oil, Other\n")
shares_o <- sup[item_code %in% c(2581, 2582, 2586),
  list(proc, share_o = production / sum(production, na.rm = TRUE)),
  by = list(area_code, year)]

sup <- merge(sup, shares_o, by = c("area_code", "year", "proc"), all.x = TRUE)
sup[is.na(share_o), share_o := 0]
sup[is.na(share) & item_code %in% c(2598),  # 2598 = "Oilseed Cakes, Other"
  `:=`(production = production * share_o)]
sup[, share_o := NULL]

sup[, share := NULL]


# Fill prices using BTD ---------------------------------------------------

prices <- as.data.table(data.table::dcast(btd, from + from_code + to + to_code +
  item + item_code + year ~ unit, value.var = "value"))
prices <- prices[!is.na(usd) & usd > 0 & sum(head, tonnes, na.rm = TRUE) > 0 &
                   (!is.na(head) | !is.na(tonnes)),
  list(usd = sum(usd, na.rm = TRUE), head = sum(head, na.rm = TRUE),
    tonnes = sum(tonnes, na.rm = TRUE)),
    by = list(from, from_code, item_code, item, year)]

prices[, price := ifelse(tonnes != 0 & !is.na(tonnes), usd / tonnes,
  ifelse(head != 0 & !is.na(head), usd / head, NA))]

# Cap prices at 10th and 90th quantiles.
# We might want to add a yearly element.
caps <- prices[, list(price_q95 = quantile(price, .95, na.rm = TRUE),
  price_q90 = quantile(price, .90, na.rm = TRUE),
  price_q50 = quantile(price, .50, na.rm = TRUE),
  price_q10 = quantile(price, .10, na.rm = TRUE),
  price_q05 = quantile(price, .05, na.rm = TRUE)),
  by = list(item)]
prices <- merge(prices, caps, by = "item", all.x = TRUE)

cat("Capping ", prices[price > price_q90 | price < price_q10, .N],
  " prices at the specific item's 90th and 10th quantiles.\n", sep = "")
prices[, price := ifelse(price > price_q90, price_q90,
  ifelse(price < price_q10, price_q10, price))]

# Get worldprices to fill gaps
na_sum <- function(x) {ifelse(all(is.na(x)), NA_real_, sum(x, na.rm = TRUE))}
prices_world <- prices[!is.na(usd), list(usd = na_sum(usd),
  tonnes = na_sum(tonnes), head = na_sum(head)),
  by = list(item, item_code, year)]
prices_world[, price_world := ifelse(head != 0, usd / head,
  ifelse(tonnes != 0, usd / tonnes, NA))]
prices_world_all <- prices_world[, list(price_world = mean(price_world, na.rm = TRUE)),
  by = list(item, item_code)]
prices <- merge(
  prices, prices_world[, c("year", "item_code", "item", "price_world")],
  by = c("year", "item_code", "item"), all.x = TRUE)
prices <- merge(
  prices, prices_world_all[, .(item_code, item, price_average = price_world)],
  by = c("item_code", "item"), all.x = TRUE)

cat("Filling ", prices[is.na(price) & !is.na(price_world), .N],
  " missing prices with world prices per year.\n", sep = "")
prices[is.na(price), price := price_world]
cat("Filling ", prices[is.na(price) & !is.na(price_average), .N],
    " missing prices with world average prices.\n", sep = "")
prices[is.na(price), price := price_average]


cat("Filling ", prices[!is.finite(price) & !is.na(price_q50), .N],
  " missing prices with median item prices.\n", sep = "")
prices[!is.finite(price), price := price_q50]
prices[!is.finite(price_world), price := price_q50]

sup <- merge(sup, all.x = TRUE,
  prices[, c("from_code", "from", "item", "item_code", "year", "price")],
  by.x = c("area_code", "area", "item", "item_code", "year"),
  by.y = c("from_code", "from", "item", "item_code", "year"))

# apply world average price where price is NA
sup <- merge(sup, all.x = TRUE,
  prices_world[, c("item", "item_code", "year", "price_world")],
  by = c("item", "item_code", "year"))
sup <- merge(sup, all.x = TRUE,
  prices_world_all[, .(item, item_code, price_average = price_world)],
  by = c("item", "item_code"))
sup[, `:=`(price = ifelse(is.na(price), ifelse(is.na(price_world), price_average, price_world), price),
           price_world = NULL, price_average = NULL)]

# estimate the price of palm kernels at 60% of the price of palm oil
sup <- merge(sup, all.x = TRUE,
  sup[item == "Palm Oil", .(year, area_code, price_oil = price)],
  by = c("area_code", "year"))
sup[, `:=`(price = ifelse(item == "Palm kernels", price_oil * 0.6, price),
  price_oil = NULL)]

# # fill missing prices for oils and cakes with global averages
# price_oil <- prices[grepl(" Oil", item),
#   list(price_oil = na_sum(usd) / na_sum(tonnes)),
#   by = list(year)]
# price_oil <- merge(price_oil, all.x = TRUE,
#   prices[grepl("Cake", item),
#   list(price_cake = na_sum(usd) / na_sum(tonnes)),
#   by = list(year)],
#   by = "year")
# sup <- merge(sup, all.x = TRUE,
#   price_oil[, .(year, price_oil, price_cake)],
#   by = "year")
# sup[grepl("Cake", item), `:=`(price = ifelse(is.na(price), price_cake, price))]
# sup[grepl(" Oil", item), `:=`(price = ifelse(is.na(price), price_oil, price))]

# fill in milk prices
prices <- readRDS("data/tidy/prices_tidy.rds")
prices <- prices[grepl("milk", item) & months == "Annual value" &
  unit == "USD", .(item, area_code, area, year, value)]
prices[, item := stringr::word(item, -1)]
prices <- dcast(prices, area_code + area + year ~ item, value.var = "value") # tidyr::spread(prices, item, value)
prices[, milk := rowMeans(.SD, na.rm = TRUE),
  by = c("area_code", "area", "year"),
  .SDcols = c("buffalo", "camel", "goats", "sheep")]
prices[!is.na(cattle), milk := cattle]
sup <- merge(sup, all.x = TRUE,
  prices[, .(year, area_code, milk)],
  by = c("area_code", "year"))
# use latest available price
prices_earliest <- prices[, .SD[which.min(year)], by = area]
sup <- merge(sup, all.x = TRUE,
  prices_earliest[, .(area_code, milk_earliest = milk)],
  by = c("area_code"))
# use global average, where no value available
prices_avg <- prices[, list(milk = mean(milk, na.rm = TRUE)),
  by = list(year)]
sup <- merge(sup, all.x = TRUE,
  prices_avg[, .(year, milk_avg = milk)],
  by = c("year"))
sup[is.na(milk_avg), milk_avg := prices_avg[which.min(year), milk]]
sup[item == "Milk - Excluding Butter", price := ifelse(!is.na(milk),
  milk, ifelse(!is.na(milk_earliest), milk_earliest, milk_avg))]
sup[, `:=`(milk = NULL, milk_earliest = NULL, milk_avg = NULL)]



# Store results -----------------------------------------------------------

saveRDS(sup, "data/sup.rds")
