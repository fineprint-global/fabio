
library("data.table")
source("R/00_system_variables.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full_123.csv")


# read data ------------------------------------------------------------------

cbs <- readRDS("data/cbs_intermediate.rds")
btd <- readRDS("data/tidy/btd_full_tidy.rds")


# # Identify outliers -------------------------------------------------------
# 
# detect_big_jumps <- function(dt, 
#                              vars = c("food", "feed", "seed", "losses", "processing", "other", "production", "tourist"), 
#                              ratio_thresholds = c(10, 20, 20, 20, 20, 20, 10, 20), 
#                              min_value = 1000, 
#                              rel_supply = 0.5, 
#                              zero_trigger = 1e6) {
#   
#   if (length(vars) != length(ratio_thresholds)) {
#     stop("Length of 'vars' and 'ratio_thresholds' must be the same.")
#   }
#   
#   outlier_keys <- list()
#   
#   for (i in seq_along(vars)) {
#     v <- vars[i]
#     ratio_threshold <- ratio_thresholds[i]
#     
#     dt[, paste0(v, "_jump") := {
#       x <- get(v)
#       ts <- supply
#       prev_x <- shift(x)
#       diff <- abs(x - prev_x)
#       
#       # Compute safe ratio
#       ratio <- ifelse(prev_x == 0,
#                       ifelse(abs(x) >= zero_trigger, Inf, 1),
#                       x / prev_x)
#       
#       is_jump <- ratio >= ratio_threshold | ratio <= (1 / ratio_threshold)
#       
#       # Relevance filters
#       is_jump[diff <= min_value] <- FALSE
#       is_jump[x <= (rel_supply * ts)] <- FALSE
#       
#       is_jump[is.na(ratio) | is.na(ts)] <- FALSE
#       
#       is_jump
#     }, by = .(area_code, item_code)]
#     
#     # Store keys where at least one jump is detected
#     outlier_keys[[v]] <- dt[get(paste0(v, "_jump")) == TRUE,
#                             .(area_code, item_code)]
#   }
#   
#   # Combine all detected outlier keys
#   all_keys <- unique(rbindlist(outlier_keys))
#   
#   # Return full time series for those keys
#   outlier_series <- merge(dt, all_keys, by = c("area_code", "item_code"))
#   
#   return(outlier_series[order(area_code, item_code, year)])
# }
# 
# 
# # Identify outliers
# 
# big_jumps_total <- detect_big_jumps(dt = cbs, 
#                                     vars = "supply",
#                                     ratio_thresholds = 10,
#                                     min_value = 10000,
#                                     rel_supply = 0,
#                                     zero_trigger = 1e7)
# 
# vars_to_check <- c("food", "feed", "seed", "losses", "processing", "other", "production", "tourist")
# thresholds <- c(10, 20, 20, 20, 20, 20, 10, 20)
# 
# big_jumps <- detect_big_jumps(dt = big_jumps_total, 
#                               vars = vars_to_check,
#                               ratio_thresholds = thresholds,
#                               min_value = 10000,
#                               rel_supply = 0.5,
#                               zero_trigger = 1e7)
# 
# 
# big_jumps_trade <- detect_big_jumps(dt = cbs, 
#                                     vars = c("imports", "exports"),
#                                     ratio_thresholds = c(100, 100),
#                                     min_value = 10000,
#                                     rel_supply = 0.5,
#                                     zero_trigger = 1e7)


# Handle outliers ----------------------------------------------------------

txt <- "
  area_code	item_code	area	item	year	btd correction	factor
  2	1096	Afghanistan	Horses	2017	imp	1000
  2	1096	Afghanistan	Horses	2018	imp	1000
  2	1096	Afghanistan	Horses	2019	imp	1000
  2	1096	Afghanistan	Horses	2022	imp	1000
  28	1096	Myanmar	Horses	2014	imp	1000
  28	1096	Myanmar	Horses	2017	imp	10000
  91	1096	Guyana	Horses	2016	imp	1000
  99	1096	Iceland	Horses	2015	exp	100
  99	1096	Iceland	Horses	2016	exp	100
  150	1096	Netherlands	Horses	2015	imp	100
  150	1096	Netherlands	Horses	2015	exp	100
  159	1096	Nigeria	Horses	2010	imp	100
  159	1096	Nigeria	Horses	2011	imp	100
  101	1107	Indonesia	Asses	2017	imp	10000
  131	1107	Malaysia	Asses	2021	exp	10000
  137	1107	Mauritius	Asses	2019	imp	0
  137	1107	Mauritius	Asses	2020	imp	0
  202	1107	South Africa	Asses	2019	exp	10000
  202	1107	South Africa	Asses	2020	exp	1000
  231	1107	United States of America	Asses	2021	exp	1000
  231	1107	United States of America	Asses	2021	imp	100
  231	1107	United States of America	Asses	2022	imp	100
  231	1107	United States of America	Asses	2022	exp	1000
  26	1110	Brunei Darussalam	Mules	2017	imp	100
  26	1110	Brunei Darussalam	Mules	2018	imp	100
  26	1110	Brunei Darussalam	Mules	2019	imp	100000
  54	1110	Denmark	Mules	2017	imp	10
  54	1110	Denmark	Mules	2017	exp	100
  54	1110	Denmark	Mules	2018	imp	1000
  54	1110	Denmark	Mules	2018	imp	100
  54	1110	Denmark	Mules	2018	exp	10
  54	1110	Denmark	Mules	2019	imp	10
  54	1110	Denmark	Mules	2019	exp	100
  54	1110	Denmark	Mules	2020	imp	100
  54	1110	Denmark	Mules	2020	exp	10
  54	1110	Denmark	Mules	2021	imp	10
  54	1110	Denmark	Mules	2022	exp	10
  79	1110	Germany	Mules	2015	exp	1000
  79	1110	Germany	Mules	2018	imp	1000
  79	1110	Germany	Mules	2018	exp	1000
  131	1110	Malaysia	Mules	2018	exp	1000
  131	1110	Malaysia	Mules	2019	exp	1000
  199	1110	Slovakia	Mules	2016	exp	1000
  199	1110	Slovakia	Mules	2022	imp	1000
  52	1016	Azerbaijan	Goats	2013	imp	100
  52	1016	Azerbaijan	Goats	2014	imp	100
  10	1126	Australia	Camels	2010	exp	1000
  112	1126	Jordan	Camels	2012	imp	1000
  118	1126	Kuwait	Camels	2018	imp	10
  118	1126	Kuwait	Camels	2018	exp	100
  250	2848	Democratic Republic of the Congo	Milk - Excluding Butter	2022	imp	100
  250	2848	Democratic Republic of the Congo	Milk - Excluding Butter	2023	imp	100
  216	1150	Thailand	Rodents, other	2020	exp	10000
  209	2746	Eswatini	Wool (Clean Eq.)	2022	imp	1000
  96	2596	China, Hong Kong SAR	Copra Cake	2014	imp	1000
  96	2596	China, Hong Kong SAR	Copra Cake	2018	imp	10
  134	866	Malta	Cattle	2019	imp	100
  171	1096	Philippines	Horses	2019	imp	100
  189	866	Saint Lucia	Cattle	2021	imp	1000
  194	946	Saudi Arabia	Buffaloes	2016	imp	100
  194	946	Saudi Arabia	Buffaloes	2019	imp	10
  194	946	Saudi Arabia	Buffaloes	2023	imp	10
  203	1110	Spain	Mules	2017	imp	10
  229	1107	United Kingdom	Asses	2018	imp	1000
  20	1034	Botswana	Pigs	2016	imp	10
"

outliers <- fread(txt)

# case <- outliers[47]

# big_jumps[outliers[, .(area_code, item_code)], on = .(area_code, item_code), nomatch = 0] %>% View()
# big_jumps_total[outliers[, .(area_code, item_code)], on = .(area_code, item_code), nomatch = 0] %>% View()
# big_jumps_trade[outliers[, .(area_code, item_code)], on = .(area_code, item_code), nomatch = 0] %>% View()

cbs[area_code==194 & item_code==1096 & year==2022, `:=`(production = 60000, other = other + 60000)]

for(i in 1:nrow(outliers)){
  case <- outliers[i]
  if(case$`btd correction`=="imp"){
    btd[to_code==case$area_code & item_code==case$item_code & year==case$year & unit!="usd" & value > case$factor, 
        value := round(value / case$factor)]
    if(case$factor == 0) btd[to_code==case$area_code & item_code==case$item_code & year==case$year & unit!="usd", value := 0] 
  } else {
    btd[from_code==case$area_code & item_code==case$item_code & year==case$year & unit!="usd" & value > case$factor, 
        value := round(value / case$factor)]
    if(case$factor == 0) btd[from_code==case$area_code & item_code==case$item_code & year==case$year & unit!="usd", value := 0]
  }
}


# Replace the corrected trade data in CBS and rebalance ----

# Imports
imps <- btd[!unit %in% c("usd") & paste(to_code,item_code,year) %in% paste(outliers$area_code,outliers$item_code,outliers$year), 
            list(value = na_sum(value)),
            by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
                          value.var = "value")
imps[, `:=`(value = round(na_sum(An, `1000 An`, tonnes)),
            An = NULL, tonnes = NULL, `1000 An` = NULL)]

# Exports
exps <- btd[!unit %in% c("usd") & paste(from_code,item_code,year) %in% paste(outliers$area_code,outliers$item_code,outliers$year), 
            list(value = na_sum(value)),
            by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
                          value.var = "value")
exps[, `:=`(value = round(na_sum(An, `1000 An`, tonnes)),
            An = NULL, tonnes = NULL, `1000 An` = NULL)]

# Replace values in CBS
cbs <- merge(
  cbs,
  imps[, .(to_code, item_code, year, value)],
  by.x = c("area_code", "item_code", "year"),
  by.y = c("to_code", "item_code", "year"),
  all.x = TRUE)
cbs[!is.na(value) & item %in% c("Goats", "Rodents, other", "Cattle", "Buffaloes") & value < imports, 
    `:=`(processing = na_sum(processing, -imports, value))]
cbs[!is.na(value) & item %in% c("Horses", "Asses", "Mules", "Camels") & value < imports, 
    `:=`(other = na_sum(other, -imports, value))]
cbs[!is.na(value) & item %in% c("Wool (Clean Eq.)") & value < imports, 
    `:=`(other = na_sum(other, -imports, value))]
cbs[!is.na(value) & item %in% c("Milk - Excluding Butter") & value < imports, 
    `:=`(food = na_sum(food, -imports, value))]
cbs[, `:=`(imports = ifelse(!is.na(value), value, imports), value = NULL)]

cbs <- merge(
  cbs,
  exps[, .(from_code, item_code, year, value)],
  by.x = c("area_code", "item_code", "year"),
  by.y = c("from_code", "item_code", "year"),
  all.x = TRUE)
cbs[!is.na(value) & item %in% c("Goats", "Rodents, other", "Cattle", "Buffaloes") & value < exports, 
    `:=`(processing = na_sum(processing, exports, -value))]
cbs[!is.na(value) & item %in% c("Horses", "Asses", "Mules", "Camels") & value < exports, 
    `:=`(other = na_sum(other, exports, -value))]
cbs[!is.na(value) & item %in% c("Milk - Excluding Butter") & value < exports, 
    `:=`(food = na_sum(food, exports, -value))]
cbs[, `:=`(exports = ifelse(!is.na(value), value, exports), value = NULL)]



# merge residuals with balancing
cbs[, `:=`(balancing = residuals + balancing, residuals = 0)]

# rebalance corrected items
key_cols <- c("area_code", "item_code", "year")
keys <- unique(outliers[, do.call(paste, .SD), .SDcols = key_cols])
idx  <- cbs[, do.call(paste, .SD), .SDcols = key_cols] %chin% keys
cbs[idx & item == "Copra Cake", 
    balancing := na_sum(production, imports, stock_withdrawal,
                        -exports, -food, -feed, -seed, -losses, -processing, 
                        -other, -tourist, -residuals, -stock_addition)]

items_other <- c("Horses", "Camels", "Mules", "Asses", "Wool (Clean Eq.)")
items_proc <- c("Pigs", "Goats", "Cattle", "Buffaloes", "Rodents, other")

cbs[idx & item %in% c(items_proc, items_other),
    `:=`(production = ifelse(na_sum(exports, -imports) > 0, na_sum(exports, -imports), 0))]
cbs[idx & item %in% items_proc,
    `:=`(processing = na_sum(imports, production, -exports))]
cbs[idx & item %in% items_other,
    `:=`(other = na_sum(imports, production, -exports))]


# Re-calculate supply, use and balancing
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals, -stock_addition)]
cbs[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]


# Adjust exports where -balancing == exports and domestic supply <= exports
cbs[round(na_sum(-balancing, -residuals)) == round(exports) & balancing < 0 & na_sum(production, imports) <= exports,
    `:=`(exports = 0, balancing = 0, residuals = 0)]



# Balance CBS imports and exports -------------------------------------------------------

# Adjust CBS to have equal export and import numbers per item per year
# This is very helpful for the iterative proportional fitting of bilateral trade data
cbs_bal <- cbs[, .(
  exp_total      = sum(exports, na.rm = TRUE),
  imp_total      = sum(imports, na.rm = TRUE),
  bal_negative   = sum(pmin(balancing, 0), na.rm = TRUE),
  exp_when_neg   = sum(exports[balancing < 0], na.rm = TRUE)
), by = .(year, item_code, item)]
# cbs_bal[, diff := imp_total - exp_total]
# cbs_bal[, diff_share := round(diff / na_sum(imp_total, exp_total) * 100)]
# hist(cbs_bal$diff_share)

cbs <- merge(cbs, cbs_bal,
             by = c("year", "item_code", "item"), all = TRUE)

# 1. Start with adjusting exports
cat("\nAdjust exports for ", cbs[na_sum(balancing, residuals) < 0 & !is.na(exports) & exp_total > imp_total & 
                                   bal_negative <0 & !is.na(exports), .N],
    " observations, where balancing < 0 and total exports > total imports.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(exports) & exp_total > imp_total, 
    `:=`(exports = pmax(round(exports - (pmin((exp_total - imp_total), -bal_negative) / exp_when_neg * exports)), 0))]
cbs[, `:=`(imp_total = NULL, exp_total = NULL, bal_negative = NULL, exp_when_neg = NULL)]


# 2. Continue with imports and exports
cbs_bal <- cbs[, .(
  exp_total      = sum(exports, na.rm = TRUE),
  imp_total      = sum(imports, na.rm = TRUE)
), by = .(year, item_code, item)]
# cbs_bal[, diff := imp_total - exp_total]
# cbs_bal[, diff_share := round(diff / na_sum(imp_total, exp_total) * 100)]
# hist(cbs_bal$diff_share)

cbs <- merge(cbs, cbs_bal,
             by = c("year", "item_code", "item"), all = TRUE)

cbs[, diff := imp_total - exp_total]

# Calculate weight based on domestic use
cbs[, weight := {
  total_du <- sum(domestic_use, na.rm = TRUE)
  if (total_du > 0) domestic_use / total_du else rep(1/.N, .N)
}, by = .(year, item_code, item)]

# Compute adjustment factors for exports and imports
cbs[, `:=`(
  # Half of the difference goes to each side
  exports = fifelse(diff > 0,
                       # Imports > Exports -> upscale exports according to weights
                       exports + diff/2 * weight,
                       # Exports > Imports -> downscale exports proportionally
                       exports + diff/2 * exports / exp_total),
  imports = fifelse(diff > 0,
                       # Imports > Exports -> downscale imports according to weights
                       imports - diff/2 * weight,
                       # Exports > Imports -> upscale imports proportionally
                       imports - diff/2 * imports / imp_total)
)]

# Secure positive rounded values
cbs[, `:=`(
  exports = pmax(0, round(exports)),
  imports = pmax(0, round(imports))
)]

# Clean up helper columns
cbs[, c("diff","weight","exp_total","imp_total") := NULL]


# check balances
cbs_bal <- merge(cbs_bal, cbs[, .(
  exp_total_new      = sum(exports, na.rm = TRUE),
  imp_total_new      = sum(imports, na.rm = TRUE)
  ), by = .(year, item_code, item)],
  by = c("year", "item_code", "item"))


# # Spread the discrepancies over all countries proportionally by down-scaling to the lower of the two
# cbs[, `:=`(
#   imports = ifelse(exp_total < imp_total, round(imports / imp_total * exp_total), imports),
#   exports = ifelse(imp_total < exp_total, round(exports / exp_total * imp_total), exports),
#   imp_total = NULL, exp_total = NULL)]

rm(cbs_bal)

# Re-balance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals, -stock_addition)]

# Re-calculate supply and use
cbs[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]



# Neutralize negative balancing via stock_addition where possible -----------------------------
# Note: the sum of balancing and residuals should never be <0 in the end
# as long as we do not introduce an "unknown source" region, we need to adapt the other cbs use elements accordingly

cat("\nAdjust 'stock_addition' for ", 
    cbs[na_sum(balancing, residuals) < 0 & !is.na(stock_addition) & stock_addition > 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `stock_addition > 0`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(stock_addition) & stock_addition > 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing, residuals),
         balancing = 0, residuals = 0)]
cbs[stock_addition < 0, `:=`(balancing = stock_addition, stock_addition = 0)]




# Neutralize negative balancing via other and processing -----------------------------
# rest proportionally via all uses
cat("\nAdjust 'other' for ", cbs[na_sum(balancing, residuals) < 0 &
                                   !is.na(other) & other > 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `other > 0` to ",
    "`other = other + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(other),
    `:=`(other = na_sum(other, balancing, residuals),
         balancing = 0, residuals = 0)]
cbs[other < 0, `:=`(balancing = balancing + other,
                    other = 0)]

cat("\nAdjust 'processing' for ", cbs[na_sum(balancing, residuals) < 0 &
                                        !is.na(processing) & processing > 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0` and `processing > 0` to ",
    "`processing = processing + na_sum(balancing, residuals)`.\n", sep = "")
cbs[na_sum(balancing, residuals) < 0 & !is.na(processing),
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0, residuals = 0)]
cbs[processing < 0, `:=`(balancing = balancing + processing,
                         processing = 0)]

cat("\nAdjust uses proportionally for ", cbs[na_sum(balancing, residuals) < 0, .N],
    " observations, where `na_sum(balancing, residuals) < 0`", sep = "")
cbs[, divisor := na_sum(other, processing, seed, food, feed, losses, stock_addition, tourist)]
cbs[na_sum(balancing, residuals) < 0,
    `:=`(stock_addition = pmax(round(na_sum(stock_addition, (na_sum(balancing, residuals) / divisor * stock_addition))), 0),
         processing = pmax(round(na_sum(processing, (na_sum(balancing, residuals) / divisor * processing))), 0),
         other = pmax(round(na_sum(other, (na_sum(balancing, residuals) / divisor * other))), 0),
         seed = pmax(round(na_sum(seed, (na_sum(balancing, residuals) / divisor * seed))), 0),
         losses = pmax(round(na_sum(losses, (na_sum(balancing, residuals) / divisor * losses))), 0),
         food = pmax(round(na_sum(food, (na_sum(balancing, residuals) / divisor * food))), 0),
         feed = pmax(round(na_sum(feed, (na_sum(balancing, residuals) / divisor * feed))), 0),
         tourist = pmax(round(na_sum(tourist, (na_sum(balancing, residuals) / divisor * tourist))), 0),
         balancing = 0, residuals = 0)]
cbs[, `:=`(divisor = NULL)]
# Re-balance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals, -stock_addition)]


# re-balance remaining negatives by increasing stock_withdrawal
cbs[balancing < 0, `:=`(stock_withdrawal = na_sum(stock_withdrawal, -balancing), balancing = 0)]



# Allocate remaining positive balancing to all uses --------------------------------
cat("\nAllocate remaining supply from 'balancing' to uses.\n")

cat("\nHops, oil palm fruit, palm kernels, sugar crops and live animals to 'processing'.\n")
cbs[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
                     1126, 1157, 1140, 1150, 1171, 2536, 2537, 2562) & na_sum(balancing, residuals) > 0,
    `:=`(processing = na_sum(processing, balancing, residuals),
         balancing = 0, residuals = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
                     1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(balancing, residuals, processing) > 0,
    `:=`(other = na_sum(other, balancing, residuals, processing),
         balancing = 0, residuals = 0, processing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
                     2595, 2596, 2597, 2598, 2749) & na_sum(balancing, residuals) > 0,
    `:=`(feed = na_sum(feed, balancing, residuals),
         balancing = 0, residuals = 0)]

cat("\nRest is allocate to 'food'.\n")
cbs[na_sum(balancing, residuals) > 0,
    `:=`(food = na_sum(food, balancing, residuals),
         balancing = 0, residuals = 0)]


# Re-calculate supply and use
cbs[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]



# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
