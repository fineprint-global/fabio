
library("data.table")
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
tcf <- fread("inst/tcf_use.csv")

cbs <- readRDS("data/cbs_bal.rds")
btd <- readRDS("data/btd_bal.rds")
sup <- readRDS("data/sup.rds")

use <- fread("inst/items_use.csv")


# Use ---------------------------------------------------------------------

# Create long use table
use <- merge(
  cbs, # cbs[, c("area_code", "area", "year", "item_code", "item")],
  use[item_code != 843, ],
  by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]

# 100% processes
cat("Allocating crops going directly to a process. Applies to items:\n\t",
  paste0(unique(use[type == "100%", item]), collapse = "; "),
  ".\n", sep = "")
use[type == "100%", `:=`(use = processing, processing = 0)]

# Slaughtering
cat("Allocating live animals to slaughtering use. Applies to items:\n\t",
  paste0(unique(use[type == "slaughtering", item]), collapse = "; "),
  ".\n", sep = "")
use[type == "slaughtering", `:=`(use = processing, processing = 0)]

# Crop TCF ---

cat("Allocating part of the TCF crops to TCF use. Applies to items:\n\t",
  paste0(unique(use[type == "TCF", item]), collapse = "; "),
  ".\n", sep = "")

tcf_cbs <- fread("inst/tcf_cbs.csv")

C <- dcast(tcf_cbs, area_code + item_code ~ source_code, fill = 0,
  fun.aggregate = na_sum, value.var = "tcf")
tcf_codes <- list(C[, area_code], C[, item_code],
  as.integer(colnames(C[, c(-1, -2)])))
C <- as(C[, c(-1, -2)], "Matrix")
dimnames(C) <- list(paste0(tcf_codes[[1]], "-", tcf_codes[[2]]), tcf_codes[[3]])

tcf_data <- use[area_code %in% tcf_codes[[1]] &
  item_code %in% c(tcf_codes[[2]], tcf_codes[[3]]),
  .(year, area_code, item_code, production, processing, imports, exports)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_prod$area_code))

# Base processing on production + imports - exports, cap at 0
tcf_data[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
tcf_data <- dt_replace(tcf_data, function(x) {`<`(x, 0)},
  value = 0, cols = "value")

# Production of items
output <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[3]]))]
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_prod[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[3]]))]
setkey(results, year, area_code, item_code)
results[, value := NA]

for(x in years) {
  output_x <- output[year == x, value]
  input_x <- input[year == x, value]
  # Skip if no data is available
  if(all(output_x == 0) || all(input_x == 0)) {next}
  results[year == x,
    value := calc_processing(y = output_x, z = input_x, C = C, cap = TRUE)]
}

merge(use, results[!is.na(value), ],
  by = c("year", "area_code", "item_code"), all.x = TRUE)
use[!is.na(value), `:=`(use = value, processing = processing - value)]

cbs[, value := NULL]
rm(tcf_cbs, tcf_codes, tcf_data, years, areas,
  input, output, result, C, input_x, output_x)


# Ethanol production ------------------------------------------------------

eth_tcf <- fread("inst/tcf_eth.csv")

eth <- cbs[item_code %in% eth_tcf$item_code, ]
eth <- merge(eth, eth_tcf[area_code == 231, c("item_code", "tcf")],
  by = "item_code", all.x = TRUE)
eth <- merge(eth, eth_tcf[, c("area_code", "item_code", "value")],
  by = c("area_code", "item_code"), all = TRUE)

eth[, `:=`(pot_oth = other * tcf, pot_proc = processing * tcf)]

eth_total <- eth[,
  list(pot_oth_t = na_sum(pot_oth),
       pot_proc_t = na_sum(pot_proc)),
  by = c("area_code", "year")]
eth <- merge(eth, eth_total, by = c("area_code", "year"), all.x = TRUE)
eth[, `:=`(share_oth = pot_oth / pot_oth_t,
           share_proc = pot_proc / pot_proc_t)]
rm(eth_total)

cat("Country-specific TCFs available for:\n\t",
  paste0(unique(eth_tcf[, area]), collapse = "; "), ".\n", sep = "")
cat("Using only information for the US and Brazil.\n")
eth[!area %in% c("United States of America", "Brazil"), value := NA]
# Overwrite shares
eth[!is.na(value), `:=`(share_oth = value, share_proc = value)]
eth[, value := NULL]

# Allocate according to ethanol production
eth <- merge(eth,
  cbs[item_code == 2659, .(area_code, year, eth_prod = production)],
  by = c("area_code", "year"), all.x = TRUE)
# First use up `other`, then `processing` for ethanol production
eth[, `:=`(
  eth_oth = eth_prod * share_oth / tcf,
  eth_proc = ifelse(eth_prod > pot_oth_t,
    (eth_prod - pot_oth_t) * share_proc / tcf, 0))]
# Cap values at available `other` and `processing`
eth[, `:=`(
  eth_oth = ifelse(eth_oth > other, other, eth_oth),
  eth_proc = ifelse(eth_proc > processing, processing, eth_proc))]
# Reduce `other` and `processing` and allocate ethanol specifics to `use`
eth[, `:=`(
  use = eth_oth + eth_proc,
  other = other - eth_oth, processing = processing - eth_proc
)]

eth <- eth[!is.na(use), .(area_code, item_code, year, proc_code = "p084",
  processing_eth = processing, other_eth = other, use_eth = use)]
use <- merge(use, eth,
  by = c("area_code", "item_code", "year", "proc_code"), all.x = TRUE)
use[!is.na(use_eth), `:=`(
  use = use_eth, other = other_eth, processing = processing_eth)]
use[, `:=`(use_eth = NULL, other_eth = NULL, processing_eth = NULL)]
rm(eth)


# Feed use ----------------------------------------------------------------

# Use animal stocks
live <- readRDS("data/tidy/live_tidy.rds")

# Feed supply
feed_sup <- cbs[feed > 0 | item_code %in% c(2000),
  c("area_code", "item_code", "year", "feed")]
# Convert to dry matter
feed_sup <- merge(feed_sup, items[, c("item_code", "moisture", "feedtype")],
  by = c("item_code"), all.x = TRUE)
feed_sup[, dry := feed * (1 - moisture)]

# Requirements -----

# Estimates from Krausmann et al. (2008)
conv_k <- fread("inst/conv_krausmann.csv")

feed_req_k <- merge(conv_k,
  live[, c("area_code", "area", "item_code", "year", "value")],
  by = c("item_code"), all.x = TRUE)
feed_req_k[, `:=`(
  total = value * conversion, value = NULL, conversion = NULL,
  area = NULL, item = NULL, proc = NULL, # Kick these
  crops = 0, residues = 0, grass = 0, fodder = 0, scavenging = 0, animals = 0)]

# Estimates from Bouwman et al. (2013)
conv_b <- fread("inst/conv_bouwman.csv")
conc_b <- fread("inst/conc_bouwman.csv")

# Add process-information
conv_b <- merge(conv_b, conc_b,
  by = "item", all.x = TRUE, allow.cartesian = TRUE)
# Add country-information
conv_b <- merge(conv_b,
  regions[, .(area_code = code, area = name, region_code)],
  by = c("region_code"), all.x = TRUE, allow.cartesian = TRUE)

# Extend estimates to full timeline (weighted)
conv_b <- rbindlist(lapply(unique(use$year), function(y, conv_b) {
  years_avail <- unique(conv_b$year)
  if(y %in% years_avail) { # No need to interpolate
    return(conv_b[year == y,
        .(area_code, item, year, proc_code, feedtype, type, conversion)])}
  # Get closest years and weigh their conversions
  years <- years_avail[-which.max(abs(years_avail - y))] # Works with three
  weights <- abs(years - y) / sum(abs(years - y))
  conversions <- conv_b$conversion[conv_b$year == years[1]] * weights[1] +
    conv_b$conversion[conv_b$year == years[2]] * weights[2]
  conv_b[year == years[1], .(area_code, item, year = y,
    proc_code, feedtype, type, conversion = conversions)]
}, conv_b = conv_b))

cat("Calculating feed demand from supply for the following items:\n\t",
  paste0(collapse = "; ", unique(sup[item_code %in%
    c(2848, 2731, 2732, 2733, 2734, 2735, 2736, 2737, 2748, 2749, 843), item])),
  ".\n", sep = "")
# feed_req_b <- sup[item_code %in%
#   c(2848, 2731, 2732, 2733, 2734, 2735, 2736, 2737, 2748, 2749, 843),
#   c("area_code", "year", "proc_code", "item_code", "comm_code", "production")]

# Get indigenous livestock production in tonnes
live_b <- live[element == "Production" & unit == "tonnes", ]
live_b[, `:=`(element = NULL, unit = NULL)]

src_code <- c(944, 972, 1012, 1032, 1055, 1775, 882, 951, 982, 1020, 1130)
tgt_code <- c(866, 946, 976, 1016, 1034, 2029, 2848, 2848, 2848, 2848, 2848)
tgt_proc <- c("p085", "p086", "p087", "p088", "p089", "p090", "p099", "p100",
  "p101", "p102", "p103")
tgt_item <- c("Beef cattle", "Beef cattle", "Sheep and goats",
  "Sheep and goats", "Pigs", "Poultry", "Dairy cattle", "Dairy cattle",
  "Dairy cattle", "Dairy cattle", "Dairy cattle")
conc <- match(live_b$item_code, src_code)
live_b[, `:=`(item_code = tgt_code[conc],
  proc_code = tgt_proc[conc], item = tgt_item[conc])]
live_b <- live_b[!is.na(item_code), ]

feed_req_b <- live_b[, .(area_code, year, item_code, item, proc_code,
  production = value)]
rm(conc, live_b)

feed_req_b[, type := ifelse(item_code == 2848, "milk", "meat")]

cat("Recoding processes,",
  "e.g. from 'Cattle slaughtering' to 'Cattle husbandry'.\n")
proc_source <- c("p104","p105","p106","p107","p108","p109")
proc_target <- c("p085","p086","p087","p088","p089","p090")
feed_req_b[, proc_code := vsub(proc_source, proc_target, proc_code)]
rm(proc_source, proc_target)


feed_req_b <- merge(all.x = TRUE, allow.cartesian = TRUE,
  feed_req_b,
  conv_b[, c("area_code", "year", "proc_code", "type", "item",
    "feedtype", "conversion")],
  by = c("area_code", "year", "proc_code", "type", "item"))

# Consider estimating requirements of fodder crops, see Issue #60
feed_req_b[, converted := production * conversion]

feed_req_b <- dcast(feed_req_b, value.var = "converted", fun.aggregate = na_sum,
  area_code + year + proc_code + item_code + item + type ~ feedtype)
# Kick unwanted columns
feed_req_b[, `:=`(type = NULL, `NA` = NULL, item_code = 0)]
# Aggregate over items (all now 0)
feed_req_b <- feed_req_b[, list(
  animals = na_sum(animals), crops = na_sum(crops), grass = na_sum(grass),
  residues = na_sum(residues), scavenging = na_sum(scavenging)),
  by = list(area_code, year, proc_code, item, item_code)]

# Create column for fodder crops from residues
feed_req_b[, `:=`(
  fodder = ifelse(item == "Pigs", residues * 0.2, # 0.2 for pigs
    ifelse(item == "Poultry", 0, residues * 0.8)))] # 0.8 for ruminants
feed_req_b[, `:=`(residues = residues - fodder, item = NULL)]

# Create total
feed_req_b[, total := na_sum(animals, crops, grass, fodder,
  residues, scavenging)]

# Original subsets to >0
feed_req_b <- feed_req_b[!is.na(total)]
feed_req_b[, lapply(.SD, na_sum),
  by = list(area_code, year, proc_code, item_code)]

# Integrate Bouwman and Krausmann feed requirements
feed_req <- rbind(feed_req_b, feed_req_k)
rm(feed_req_k, feed_req_b)


# Allocate total feed demand from Krausmann to the Bouwman split -----
feed_alloc <- feed_req[item_code == 0,
  lapply(list(animals, crops, grass, fodder, residues, scavenging, total),
    na_sum),
  by = list(area_code, year)]
feed_alloc <- feed_alloc[V7 > 0, list(area_code, year,
  animals_f = V1 / V7, crops_f = V2 / V7, grass_f = V3 / V7,
  fodder_f  = V4 / V7, residues_f = V5 / V7, scavenging_f = V6 / V7)]

feed_req <- merge(feed_req, feed_alloc,
  by = c("area_code", "year"), all.x = TRUE)
# Use a 1 - 2 - 1 - 1 - 1 - 1 split if no info is available
feed_req[, `:=`(
  animals_f = ifelse(is.na(animals_f), 1 / 7, animals_f),
  crops_f = ifelse(is.na(crops_f), 2 / 7, crops_f),
  grass_f = ifelse(is.na(grass_f), 1 / 7, grass_f),
  fodder_f = ifelse(is.na(fodder_f), 1 / 7, fodder_f),
  residues_f = ifelse(is.na(residues_f), 1 / 7, residues_f),
  scavenging_f = ifelse(is.na(scavenging_f), 1 / 7, scavenging_f))]
feed_req[item_code != 0,
  `:=`(animals = total * animals_f, crops = total * crops_f,
       grass = total * grass_f, residues = total * residues_f,
       scavenging = total * scavenging_f)]
# Kick factors again
feed_req[, `:=`(animals_f = NULL, crops_f = NULL, grass_f = NULL,
  fodder_f = NULL, residues_f = NULL, scavenging_f = NULL)]


# Adapt feed-demand to available feed-supply -----
feed_bounds <- merge(
  dcast(feed_sup, value.var = "dry", fun.aggregate = na_sum,
    area_code + year ~ feedtype),
  feed_req[, list(animals_r = na_sum(animals), crops_r = na_sum(animals),
    grass_r = na_sum(grass), fodder_r = na_sum(fodder),
    residues_r = na_sum(residues), scavenging_r = na_sum(scavenging),
    total_r = na_sum(total)),
    by = c("area_code", "year", "proc_code")],
  by = c("area_code", "year"))

# Add totals over processes
feed_bounds <- merge(
  feed_bounds[, list(animals_o = na_sum(animals_r), crops_o = na_sum(crops_r),
    grass_o = na_sum(grass_r), fodder_o = na_sum(fodder_r),
    total_o = na_sum(total_r)),
    by = c("area_code", "year")],
  feed_bounds, by = c("area_code", "year"))

# Use up supply (process percentage), note that there is no grass supply per se
feed_bounds[, `:=`(
  crops_r = crops_r * crops / crops_o,
  animals_r = animals_r * animals / animals_o,
  fodder_r = fodder_r * fodder / fodder_o,
  grass_r = grass_r)]

feed_bounds[, `:=`(crops = NULL, animals = NULL, fodder = NULL)]
# "_r" is requirement per process
# "_o" is requirement over all processes


# Allocate feed-use -----

feed <- merge(
  use[type == "feed", list(use = na_sum(use)),
    by = c("area_code", "year", "item_code", "proc_code")],
  dcast(feed_sup[, !c("moisture"), with = FALSE],
    value.var = "dry", fun.aggregate = na_sum,
    area_code + year + item_code ~ feedtype),
  by = c("area_code", "year", "item_code"), all.x = TRUE)

# Add totals over items
feed <- merge(feed,
  feed[, list(animals_t = na_sum(animals),
    crops_t = na_sum(crops), fodder_t = na_sum(fodder)),
    by = c("area_code", "year", "proc_code")],
  by = c("area_code", "year", "proc_code"))
# "" is supply per item
# "_t" is supply over all items

feed <- merge(feed, feed_bounds, by = c("area_code", "year", "proc_code"))

# Set use to demand (process level) * supply (item level) / supply (all items)
feed[, `:=`(
  crops = crops * crops_r / crops_t,
  animals = animals * animals_r / animals_t,
  fodder = fodder * fodder_r / fodder_t)]

# Moisture
feed <- merge(feed, items[, c("item_code", "moisture")], by = "item_code")
feed[, `:=`(crops = crops / (1 - moisture),
  animals = animals / (1 - moisture), fodder = fodder / (1 - moisture))]

# Move to relevant column, note that grass is equal to the demand
feed[, use_feed := na_sum(crops, animals, fodder, grass_r)]

# Add feed use back to the use table
use <- merge(use,
  feed[, c("area_code", "year", "proc_code", "item_code", "use_feed")],
  by = c("area_code", "year", "item_code", "proc_code"), all.x = TRUE)
use[!is.na(use_feed), `:=`(use = use_feed)]
use[, `:=`(use_feed = NULL)]

# Add grazing (use_feed of item_code 2001 == grass_r) to supply and balances
grazing <- feed[item_code == 2001, list(grazing = na_sum(use_feed)),
    by = c("area_code", "year", "item_code")]

sup <- merge(sup, grazing,
  by = c("area_code", "year", "item_code"), all.x = TRUE)
sup[item_code == 2001, production := grazing]
sup[, grazing := NULL]

cbs <- merge(cbs, grazing,
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[item_code == 2001, `:=`(
  production = grazing, total_supply = na_sum(grazing, imports))]
cbs[, grazing := NULL]

# Clean up
rm(feed, grazing)


# Optimise feedstock allocation -----
# Allocate feedstocks to the production of alcoholic beverages and sweeteners
opt_tcf <- fread("inst/tcf_optim.csv")
opt_in <- fread("inst/optim_in.csv")
opt_out <- fread("inst/optim_out.csv")

# Add processing / production information from the balances
input <- merge(opt_in,
  cbs[, c("area_code", "year", "item_code", "processing")],
  by = "item_code", all.x = TRUE)

input <- input[is.finite(processing) & processing > 0]
output <- merge(opt_out,
  cbs[, c("area_code", "year", "item_code", "production")],
  by = "item_code", all.x = TRUE)
output <- output[is.finite(production) & production > 0]

# We have some NAs in the TCFs
opt_tcf <- opt_tcf[is.finite(value), ]

# Subset to needed TCFs (shold be all) and get weights (i.e. mean values)
# Weights are in-out ratio (to bypass e.g. high water contents of beer)
opt_tcf <- opt_tcf[inp_code %in% opt_in$item_code &
  out_code %in% opt_out$item_code, ]
weight_out <- opt_tcf[, list(weight = mean(value, na.rm = TRUE)),
  by = c("out_code", "area_code")]

# Subset to available TCF (should be all)
input <- input[item_code %in% opt_tcf$inp_code, ]
output <- output[item_code %in% opt_tcf$out_code, ]

# Set NAs to 0
input[is.na(processing), processing := 0]
output[is.na(production), production := 0]

# Optimise allocation -----
# This takes a very long time! Parallelisation should work by default.
results <- lapply(sort(unique(input$area_code)), function(x) {
  # Per area
  inp_x <- input[area_code == x, ]
  out_x <- output[area_code == x, ]
  tcf_x <- opt_tcf[area_code == x, ]
  wt_x <- weight_out[area_code == x, ]
  res <- lapply(sort(unique(input$year)), function(y) {
    # Per year
    inp_xy <- inp_x[year == y, ]
    out_xy <- out_x[year == y, ]
    # Skip optimisation if no data is available
    if(inp_xy[, .N] == 0 || out_xy[, .N] == 0) {return(NULL)}
    tcf_xy <- tcf_x[inp_code %in% inp_xy$item_code &
      out_code %in% out_xy$item_code, ]
    wt_xy <- wt_x[out_code %in% out_xy$item_code, ]
    # Optimise on country x & year y
    opt <- optim(par = rep(0, nrow(tcf_xy)), # To-do: vectorise further
      fn = function(par) {
        I <- tcf_xy[, .(inp_code, par = par)][,
          list(x = na_sum(par)), by = c("inp_code")]
        O <- tcf_xy[, .(out_code, par = par * value)][,
          list(x = na_sum(par)), by = c("out_code")]
        # Get errors (Ensuring correct order)
        prod_err <- out_xy$production[match(O$out_code, out_xy$item_code)] - O$x
        proc_err <- inp_xy$processing[match(I$inp_code, inp_xy$item_code)] - I$x
        # Production loss - sum of squared errors (weighted)
        prod_loss <- sum((prod_err / wt_xy$weight) ^ 2)
        # Processing loss - sum of squared negative and absolute positive errors
        proc_loss <- sum(proc_err[proc_err > 0], proc_err[proc_err < 0] ^ 2)
        return(prod_loss + proc_loss)
    }, method = "L-BFGS-B", lower = 0, upper = Inf)
    tcf_xy[, .(area_code, inp_code, out_code, value,
      result_in = opt$par, year = y)]
  })
  return(rbindlist(res))
})

results <- rbindlist(results)

results[, `:=`(result_out = result_in * value,
  item_code = inp_code, type = "optim")]
# Add process information
results[, proc_code := ifelse(out_code == 2658, "p083",
  ifelse(out_code == 2657, "p082", ifelse(out_code == 2656, "p081", "p066")))]

# Add optimisation results to use and balances
use <- merge(use,
  results[, c("area_code", "year", "item_code",
    "proc_code", "type", "result_in")],
  by = c("area_code", "year", "item_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(result_in) & type == "optim", use := result_in]
use[, result_in := NULL]

cbs <- merge(cbs,
  results[, c("area_code", "year", "item_code", "result_in")],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(result_in), processing := na_sum(processing, -result_in)]
cbs[, result_in := NULL]


# Allocation of seed -----

seed_sup <- merge(sup,
  sup[, list(total = na_sum(production)),
    by = c("area_code", "item_code", "year")],
  by = c("area_code", "item_code", "year"), all.x = TRUE)
seed_sup[, share := ifelse(total == 0, NA, production / total)]

# Add to seed-balances and filter to relevant ones
seed_sup <- merge(seed_sup,
  cbs[seed > 0, c("area_code", "item_code", "year", "seed")],
  by = c("area_code", "item_code", "year"), all.x = TRUE)
seed_sup[, share_s := seed * share]
seed_sup <- seed_sup[, list(share_s = na_sum(share_s)),
  by = c("area_code", "item_code", "proc_code", "year")]
seed_sup <- seed_sup[share_s > 0, ]

# Add seed use to the use table
use <- merge(use,
  seed_sup[, .(area_code, year, item_code, proc_code,
    type = "seedwaste", share_s)],
  by = c("area_code", "year", "item_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(share_s) & type == "seedwaste", use := share_s]
use[, share_s := NULL]

# Allocate the rest of processing to food balances -----
cbs[processing > 0, `:=`(food = na_sum(food, processing), processing = 0)]


# Allocate final demand from balances -----
use_fd <- cbs[, c("area_code", "item_code", "year",
  "food", "other", "stock_addition", "stock_withdrawal", "balancing")]


# Save -----

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
