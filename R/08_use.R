
library("data.table")
library("Matrix")
library("parallel")
source("R/01_tidy_functions.R")

# should the feedstock optimization be run or should previously stored results be used?
run_optim <- FALSE
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

# correct milk processing
## butter production has a TCF of 3.5-6%, i.e. 100 t of milk are processed into 3.5-6 t of butter
## expressed the other way around, 1 t of butter needs between 16.67 and 28.57 t of milk
## if more than 28.57 t of milk are reported under 'processing' per tonne of butter produced,
## the excess quantity is move from processing to food
butter <- cbs[item=="Butter, Ghee", .(area_code,area,item_code,item,year,production)]
butter <- merge(butter, cbs[item=="Milk - Excluding Butter", .(area_code,area,year,processing)])
butter[, `:=`(max = production / 0.035,
              min = production / 0.06)]
cbs <- merge(cbs, butter[, .(area_code, area, year, item = "Milk - Excluding Butter", max)], all.x = TRUE)
cbs[!is.na(max) & max < processing, `:=`(food = food + processing - max, processing = max)]

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
source("R/08_use_feed.R")



# Optimise feedstock allocation -----
# Allocate feedstocks to the production of alcoholic beverages and sweeteners
if (run_optim){
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

  results <- readRDS("./data/optim_results_2023-12-12.rds")

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
