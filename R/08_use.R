
library("data.table")
library("Matrix")
library("parallel")
source("R/01_tidy_functions.R")

# should the feedstock optimization be run or should previously stored results be used?
run_optim <- TRUE
regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

cbs <- readRDS("data/cbs_full.rds")
sup <- readRDS("data/sup.rds")

use_items <- fread("inst/items_use.csv")


# Use ---------------------------------------------------------------------

# Add grazing to the CBS (later filled with feed requirements)
grazing <- unique(cbs[, c("year", "area", "area_code")])
grazing[, `:=`(item = "Grazing", item_code = 2001, comm_code = "c062")]
cbs <- rbindlist(list(cbs, grazing), use.names = TRUE, fill = TRUE)

# Create long use table
use <- merge(
  cbs[, c("area_code", "area", "year", "item_code", "item", "production", "processing")],
  use_items[item_code %in% unique(cbs$item_code)],
  by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]

# correct comm_codes
use$comm_code <- items$comm_code[match(use$item_code, items$item_code)]

# 100% processes
cat("Allocating crops going directly to a process. Applies to items:\n\t",
  paste0(unique(use[type == "100%", item]), collapse = "; "),
  ".\n", sep = "")
use[type == "100%", `:=`(use = processing, processing = 0)]
# Reduce processing in CBS
cbs <- merge(cbs, use[type == "100%" & !is.na(use) & use > 0,
  c("area_code", "year", "item_code", "use")],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(use), processing := na_sum(processing, -use)]
cbs[, use := NULL]

# Slaughtering
cat("Allocating live animals to slaughtering use. Applies to items:\n\t",
  paste0(unique(use[type == "slaughtering", item]), collapse = "; "),
  ".\n", sep = "")
use[type == "slaughtering", `:=`(use = processing, processing = 0)]
# Reduce processing
cbs <- merge(cbs, use[type == "slaughtering" & !is.na(use) & use > 0,
  c("area_code", "year", "item_code", "use")],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(use), processing := na_sum(processing, -use)]
cbs[, use := NULL]


# Crop TCF ---

cat("Allocating part of the TCF crops to TCF use. Applies to items:\n\t",
  paste0(unique(use[type == "TCF", item]), collapse = "; "),
  ".\n", sep = "")

tcf_cbs <- fread("inst/tcf_cbs.csv")

tcf_codes <- list(sort(unique(cbs$area_code[cbs$area_code %in% tcf_cbs$area_code])), sort(unique(tcf_cbs$item_code)),
  sort(unique(tcf_cbs$source_code)))
Cs <- lapply(tcf_codes[[1]], function(x) {
  out <- data.table::dcast(tcf_cbs[area_code == x], item_code ~ source_code, fill = 0,
    fun.aggregate = na_sum, value.var = "tcf")
  setkey(out, item_code)
  out <- as(out[, -1], "Matrix")
})
Cs <- lapply(Cs, `dimnames<-`, list(tcf_codes[[2]], tcf_codes[[3]]))
names(Cs) <- tcf_codes[[1]]

tcf_data <- use[area_code %in% tcf_codes[[1]] &
  (item_code %in% c(tcf_codes[[2]]) | item_code %in% tcf_codes[[3]]),
  .(year, area_code, item_code, production, processing)]
tcf_data <- tcf_data[!duplicated(tcf_data), ] # Duplicates from proc_code
setkey(tcf_data, year, area_code, item_code)
years <- sort(unique(tcf_data$year))
areas <- tcf_codes[[1]]

# Production in processes
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
output[, `:=`(value = production, production = NULL, processing = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# output <- output[!duplicated(output), ] # Kick duplicates (from item_code)

# Processing of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[3]]))]
input[, `:=`(value = processing, production = NULL, processing = NULL)]
dt_replace(input, is.na, 0, cols = "value")
# input <- input[!duplicated(input), ] # Kick duplicates (from proc_code)

# Processing per process - to fill
results <- tcf_data[data.table(expand.grid(year = years, area_code = areas,
  item_code = tcf_codes[[3]], item_code_proc = tcf_codes[[2]]))]
setkey(results, item_code, item_code_proc)
results[, `:=`(value = 0, production = NULL, processing = NULL)]

for(x in years) {
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    out <- split_tcf(y = output_y, z = input_y,
      C = Cs[[as.character(y)]], cap = TRUE)
    if(length(out) == 1 && is.na(out)) {next}
    results[year == x & area_code == y &
      item_code_proc %in% out$item_code_proc, # item_code is always ordered
      value := out$value]
  }
}
results[, `:=`(proc_code =
  tcf_cbs[match(results$item_code_proc, tcf_cbs$item_code), proc_code],
  item_code_proc = NULL)]

# Add to use (per item and process)
use <- merge(use, results,
  by = c("year", "area_code", "proc_code", "item_code"), all.x = TRUE)
use[!is.na(value), `:=`(use = value)]
use[, value := NULL]

# Subtract from cbs processing (per item)
cbs <- merge(cbs, results[, list(value = na_sum(value)),
  by = c("year", "area_code", "item_code")],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(value), processing := round(na_sum(processing, -value))]
cbs[, value := NULL]


rm(tcf_cbs, tcf_codes, tcf_data, years, areas, out,
  results, Cs, input, output, input_x, output_x, input_y, output_y)


# Ethanol production ------------------------------------------------------

eth_tcf <- fread("inst/tcf_eth.csv")

eth <- cbs[item_code %in% eth_tcf$item_code, ]
eth <- merge(eth, unique(eth_tcf[, c("item_code", "tcf")]),
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
eth <- merge(eth[!is.na(area)],
  cbs[item_code == 2659, .(area_code, year, eth_prod = production)],
  by = c("area_code", "year"), all.x = TRUE)
# First use up other, then processing for ethanol production
eth[, `:=`(
  eth_oth = eth_prod * share_oth / tcf,
  eth_proc = ifelse(eth_prod > pot_oth_t,
    (eth_prod - pot_oth_t) * share_proc / tcf, 0))]
# Cap values at available other and processing
eth[, `:=`(
  eth_oth = ifelse(eth_oth > other, other, eth_oth),
  eth_proc = ifelse(eth_proc > processing, processing, eth_proc))]
eth[, `:=`(use = na_sum(eth_oth, eth_proc))]

# Reduce other and processing in cbs allocate ethanol use
cbs <- merge(cbs, eth[!is.na(use),
  c("year", "area_code", "item_code", "eth_oth", "eth_proc")],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[, `:=`(
  processing = na_sum(processing, -eth_proc), other = na_sum(other, -eth_oth))]
cbs[, `:=`(eth_proc = NULL, eth_oth = NULL)]

eth <- eth[!is.na(use), .(area_code, item_code, year, proc_code = "p084",
  use_eth = use)]
use <- merge(use, eth,
  by = c("area_code", "item_code", "year", "proc_code"), all.x = TRUE)
use[!is.na(use_eth), `:=`(use = use_eth)]
use[, `:=`(use_eth = NULL)]
rm(eth)


# Feed use ----------------------------------------------------------------

# Use animal stocks
live <- readRDS("data/tidy/live_tidy.rds")

# Feed supply
feed_sup <- cbs[feed > 0 | item_code %in% c(2000),
  c("area_code", "area", "item_code", "item", "year", "feed")]
# Convert to dry matter
feed_sup <- merge(feed_sup, items[, c("item_code", "moisture", "feedtype")],
  by = c("item_code"), all.x = TRUE)
feed_sup[, dry := feed * (1 - moisture)]

# Feed requirements -----

# Estimates from Krausmann et al. (2008)
# Own assumption for other camelids such as llamas and alpacas.
conv_k <- fread("inst/conv_krausmann.csv")

feed_req_k <- merge(conv_k,
  live[element == "Stocks", c("area_code", "area", "item_code", "year", "value")],
  by = c("item_code"), all.x = TRUE)
feed_req_k[, `:=`(
  total = value * conversion, value = NULL, conversion = NULL, item = NULL,
  original_value = NULL,
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
interpolate <- function(y, conv_b) {
  years_avail <- unique(conv_b$year)
  if(y %in% years_avail) { # No need to interpolate
    return(conv_b[year == y,
        .(area_code, area, item, year, proc_code, feedtype, type, conversion)])}
  # Get closest years and inter-/extrapolate their feed conversion rates
  years_min <- ifelse(y < min(years_avail), min(years_avail),
                      ifelse(y > max(years_avail), years_avail[2],
                      max(years_avail[(years_avail - y) < 0])))
  year_max <- ifelse(y > max(years_avail), max(years_avail),
                     ifelse(y < min(years_avail), years_avail[length(years_avail)-1],
                     min(years_avail[(years_avail - y) > 0])))
  difference <- (conv_b$conversion[conv_b$year == year_max] -
    conv_b$conversion[conv_b$year == years_min]) / (year_max - years_min) * (y - years_min)
  data <- conv_b[year == years_min]
  data[, .(area_code, area, item, year = y, proc_code, feedtype, type, conversion = conversion + difference)]
}
conv_b <- rbindlist(lapply(sort(unique(use$year)), interpolate, conv_b = conv_b))

cat("Calculating feed demand from supply for the following items:\n\t",
  paste0(collapse = "; ", unique(sup[item_code %in%
    c(2848, 2731, 2732, 2733, 2734, 2735), item])),
  ".\n", sep = "")

# Get indigenous livestock production in tonnes
live_b <- live[element == "Production" & unit == "tonnes", ]
live_b[, `:=`(element = NULL, unit = NULL)]

# src_code <- c(944, 972, 1012, 1032, 1055, 1775, 882, 951, 982, 1020, 1130)
src_code <- c(867, 947, 977, 1017, 1035, 1808, 882, 951, 982, 1020, 1130) # this regular meat and not indigenous!
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

feed_req_b <- live_b[, .(area_code, area, year, item_code, item, proc_code,
  production = value)]
rm(conc, live_b, tgt_code, tgt_proc, tgt_item, src_code)

feed_req_b <- merge(all.x = TRUE, allow.cartesian = TRUE,
  feed_req_b,
  conv_b[, c("area_code", "year", "proc_code", "item",
    "feedtype", "conversion")],
  by = c("area_code", "year", "proc_code", "item"))

# Calculate feed requirements
feed_req_b[, converted := round(production * conversion, 3)]

# Aggregate over processes, areas, items and years
feed_req_b <- data.table::dcast(feed_req_b, value.var = "converted", fun.aggregate = na_sum,
  area_code + area + year + item + proc_code ~ feedtype)

# Define share of fodder crops in residues
feed_req_b[, `:=`(
  fodder = ifelse(item == "Pigs", residues * 0.025, # 0.025 for pigs, 0 for poultry
  ifelse(item == "Poultry", 0, residues * 0.5)))] # 0.5 for ruminants
# Define share of oilcakes in residues
feed_req_b[, `:=`(
  residues = round(ifelse(item %in% c("Pigs", "Poultry", "Dairy cattle"), residues * 0.5,
  ifelse(item == "Beef cattle", residues * 0.1, residues * 0.05)), # 0.05 for sheep and goats
  3))]

# Add missing variables
processes <- unique(sup[, .(proc_code, proc)])
feed_req_b[, proc := processes$proc[match(feed_req_b$proc_code, processes$proc_code)]]

# Create total
feed_req_b[, `:=`(total = na_sum(animals, crops, grass, fodder,
  residues, scavenging), item_code = 0, item = NULL)]

# Integrate Bouwman and Krausmann feed requirements
feed_req <- rbind(feed_req_b[total > 0], feed_req_k[!is.na(total) & total > 0])
rm(feed_req_k, feed_req_b)


# Allocate total feed demand from Krausmann to the Bouwman split -----
feed_alloc <- feed_req[item_code == 0,
  lapply(list(animals, crops, grass, fodder, residues, scavenging, total),
    na_sum),
  by = list(area_code, year)]
feed_alloc <- feed_alloc[V7 > 0, list(area_code, year,
  animals_f = V1 / V7, crops_f = V2 / V7, grass_f = V3 / V7,
  fodder_f  = V4 / V7, residues_f = V5 / V7, scavenging_f = V6 / V7)]
feed_alloc[is.na(feed_alloc)] <- 0

feed_req <- merge(feed_req, feed_alloc,
  by = c("area_code", "year"), all.x = TRUE)
rm(feed_alloc)
# Use global average split if no info is available
feed_req[, `:=`(
  animals_f = ifelse(is.na(animals_f), mean(animals_f, na.rm = TRUE), animals_f),
  crops_f = ifelse(is.na(crops_f), mean(crops_f, na.rm = TRUE), crops_f),
  grass_f = ifelse(is.na(grass_f), mean(grass_f, na.rm = TRUE), grass_f),
  fodder_f = ifelse(is.na(fodder_f), mean(fodder_f, na.rm = TRUE), fodder_f),
  residues_f = ifelse(is.na(residues_f), mean(residues_f, na.rm = TRUE), residues_f),
  scavenging_f = ifelse(is.na(scavenging_f), mean(scavenging_f, na.rm = TRUE), scavenging_f))]
# This simple procedure assumes equal feed composition for
# Horses, Asses, Mules, Camels, Camelids, other
feed_req[item_code != 0 & !proc %in% c("Rabbits husbandry", "Rodents husbandry, other"),
  `:=`(animals = total * animals_f, crops = total * crops_f,
       grass = total * grass_f, fodder = total * fodder_f, residues = total * residues_f,
       scavenging = total * scavenging_f)]
# Rabbits and hares, Rodents, other
feed_req[item_code != 0 & proc %in% c("Rabbits husbandry", "Rodents husbandry, other"),
  `:=`(animals = total * animals_f, crops = total * (crops_f + grass_f),
       grass = 0, fodder = total * fodder_f, residues = total * residues_f,
       scavenging = total * scavenging_f)]

# Kick factors again
feed_req[, `:=`(animals_f = NULL, crops_f = NULL, grass_f = NULL,
  fodder_f = NULL, residues_f = NULL, scavenging_f = NULL)]

# Aggregate RoW countries in feed_req
feed_req <- replace_RoW(feed_req, codes = regions[cbs == TRUE, code])
feed_req <- feed_req[, lapply(.SD, na_sum),
           by = c("area_code", "area", "proc_code", "proc", "year")]


# Adapt feed-demand to available feed-supply -----
feed_sup[, total_dry := na_sum(dry), by=c("area_code","year","feedtype")]
feed_req <- data.table::melt(feed_req, id=c("area_code","area","year","proc_code","proc"),
  measure=c("crops","animals","residues","fodder","grass"),
  variable.name="feedtype", value.name="req")
feed_req[, total_req := na_sum(req), by = c("area_code", "year", "feedtype")]

feed <- merge(feed_sup[, .(area_code, area, year, item_code, item, feedtype, moisture,
  sup_dry = dry, total_sup_dry = total_dry, sup_fresh = feed)],
  feed_req, by=c("area_code", "area", "year", "feedtype"), all = TRUE, allow.cartesian = TRUE)

feed[feedtype != "grass", req := req / total_req * total_sup_dry]

# Allocate feed-use -----
feed[feedtype != "grass", req := req / total_sup_dry * sup_dry]
feed[feedtype == "grass", `:=`(item_code = 2001, item = "Grazing", moisture = 0.8)]

# Convert to fresh weight
feed[, feed_use := round(req / (1 - moisture),0)]

# Add feed use back to the use table
feed[, comm_code := items$comm_code[match(feed$item_code, items$item_code)]]
use <- merge(use,
  feed[!is.na(feed_use), .(area_code, area, year, proc_code, proc, item_code, item, feed_use)],
  by = c("area_code", "area", "year", "item_code", "item", "proc_code", "proc"), all = TRUE)
conc <- match(use$item_code, items$item_code)
use[, comm_code := items$comm_code[conc]]
use[!is.na(feed_use), `:=`(use = feed_use)]
use[, `:=`(feed_use = NULL)]

# Add grazing (feed_use of item_code 2001 == grass_r) to supply and balances
grazing <- feed[item_code == 2001, list(grazing = na_sum(feed_use)),
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
rm(feed, grazing, feed_req, feed_sup, live)


# Optimise feedstock allocation -----
# Allocate feedstocks to the production of alcoholic beverages and sweeteners
if (run_optim){
  opt_tcf <- fread("inst/tcf_optim.csv")
  opt_in <- fread("inst/optim_in.csv")
  opt_out <- fread("inst/optim_out.csv")

  # Add processing / production information from the balances
  input <- merge(opt_in,
    cbs[year > 1985, c("area_code", "year", "item_code", "processing")],
    by = "item_code", all.x = TRUE)

  input <- input[is.finite(processing) & processing > 0]
  output <- merge(opt_out,
    cbs[year > 1985, c("area_code", "year", "item_code", "production")],
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
  # results <- lapply(sort(unique(input$area_code)), function(x) {  # This takes ~20 hours.
  results <- mclapply(sort(unique(input$area_code)), function(x) {  # This takes ~1.5 hours.
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
          # Get absolute deviations from target (ensuring correct order, output deviations weighted)
          prod_tgt <- out_xy$production[match(O$out_code, out_xy$item_code)]
          prod_err <- abs(prod_tgt - O$x) / wt_xy$weight
          proc_tgt <- inp_xy$processing[match(I$inp_code, inp_xy$item_code)]
          proc_err <- abs(proc_tgt - I$x)
          # Get relative deviations from target weighted by maximum absolute error
          prod_err_rel <- abs(prod_tgt - O$x) / prod_tgt * max(prod_err)
          proc_err_rel <- abs(proc_tgt - I$x) / proc_tgt * max(proc_err)
          # Sum of squared absolute deviations + 50% of weighted relative deviation
          return(sum(prod_err^2) + sum(proc_err^2) + (sum(prod_err_rel^2) + sum(proc_err_rel^2)) / 2)
      }, method = "L-BFGS-B", lower = 0, upper = Inf, control = list(maxit = 500))
      # check results
      # data <- dplyr::mutate(tcf_xy, result_in = opt$par, year = y)
      if(opt$convergence != 0) cat("\n Warning: optimization did not converge for", unique(cbs[,.(area, area_code)])[area_code == x]$area, "in year",y, "within 500 iterations \n")
      tcf_xy[, .(area_code, inp_code, out_code, value,
        result_in = opt$par, year = y)]
    })
    return(rbindlist(res))
  #})
  }, mc.cores = 10)

  results <- rbindlist(results)
  results[, result_in := round(result_in)]
  saveRDS(results, paste0("./data/optim_results_",Sys.Date(),".rds"))

  } else {

  results <- readRDS("./data/optim_results_2023-01-05.rds")

}

# Add process information
results[, proc_code := ifelse(out_code == 2658, "p083",
  ifelse(out_code == 2657, "p082", ifelse(out_code == 2656, "p081", "p066")))]

# Add optimisation results to use (full detail) and cbs (item detail)
use <- merge(use,
  results[, .(area_code, year, item_code = inp_code, type = "optim",
    proc_code, result_in)],
  by = c("area_code", "year", "item_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(result_in) & type == "optim", use := result_in]
use[, result_in := NULL]

cbs <- merge(cbs,
  results[, list(result_in = na_sum(result_in)),
    by = .(area_code, year, item_code = inp_code)],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(result_in), processing := na_sum(processing, -result_in)]
cbs[, result_in := NULL]

# Allocate negatives in processing use resulting from feedstock optimization to balancing
# TODO: this is just a temporary solution. Ideally the opt algorithm should prevent such cases.
cbs[processing < 0, `:=`(balancing = na_sum(balancing, processing), processing = 0)]


# Allocation of seed -----
use <- merge(use,
  cbs[, .(area_code, year, item_code, type = "seedwaste", seed_use = seed)],
  by = c("area_code", "year", "item_code", "type"), all.x = TRUE)
use[!is.na(seed_use) & type == "seedwaste", use := seed_use]
use[, seed_use := NULL]


# add comm code
cbs[, comm_code := items$comm_code[match(cbs$item_code, items$item_code)]]



# Allocate final demand from balances -----

# The remainder of processing use (flows into supply chains that are not further tracked in FABIO) is interpreted as a new final demand category
use_fd <- cbs[, c("year", "comm_code", "area_code", "area", "item_code", "item",
  "food", "other", "losses", "stock_addition", "balancing", "unspecified", "tourist", "residuals", "processing")]


# Remove unneeded variables
use <- use[, c("year", "area_code", "area", "comm_code", "item_code", "item",
               "proc_code", "proc", "type", "use")]


# Save -----

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
saveRDS(use_fd, "data/use_fd_final.rds")
saveRDS(sup, "data/sup_final.rds")
