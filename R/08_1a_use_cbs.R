library("tidyverse")
library("data.table")
library("Matrix")
library("parallel")
library("lpSolve")
library("quadprog")
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

cbs <- readRDS("data/cbs_full.rds")
sup <- readRDS("data/sup.rds")
items <- fread("inst/items_full_123.csv")
use_items <- fread("inst/items_use.csv")


# Use ---------------------------------------------------------------------

# Add grazing to the CBS (later filled with feed requirements)
grazing <- unique(cbs[, c("year", "area", "area_code")])
grazing[, `:=`(item = "Grazing", item_code = 2001, comm_code = "c062")]
cbs <- rbindlist(list(cbs, grazing), use.names = TRUE, fill = TRUE)

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
cbs[, max := NULL]
cbs[, use := NULL]

# Create long use table
use <- merge(
  cbs[, c("area_code", "area", "year", "item_code", "item", "production", "processing")],
  use_items[item_code %in% unique(cbs$item_code)],
  by = c("item_code", "item"), all = TRUE, allow.cartesian = TRUE)
use[, use := NA_real_]

# correct comm_codes
use$comm_code <- items$comm_code[match(use$item_code, items$item_code)]

# Slaughtering
cat("Allocating live animals to slaughtering use. Applies to items:\n\t",
  paste0(unique(use[type == "slaughtering", item]), collapse = "; "),
  ".\n", sep = "")
use[type == "slaughtering", `:=`(use = processing, processing = 0)]
# Reduce processing
cbs <- merge(cbs, use[type == "slaughtering" & !is.na(use) & use > 0,
  .(area_code, year, item_code, use)],
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
yrs <- sort(unique(tcf_data$year))
areas <- tcf_codes[[1]]

# Production in processes
output <- tcf_data[data.table(expand.grid(year = yrs,
  area_code = areas, item_code = tcf_codes[[2]]))]
output[, `:=`(value = production, production = NULL, processing = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# output <- output[!duplicated(output), ] # Kick duplicates (from item_code)

# Processing of source items
input <- tcf_data[data.table(expand.grid(year = yrs,
  area_code = areas, item_code = tcf_codes[[3]]))]
input[, `:=`(value = processing, production = NULL, processing = NULL)]
dt_replace(input, is.na, 0, cols = "value")
# input <- input[!duplicated(input), ] # Kick duplicates (from proc_code)

# Processing per process - to fill
results <- tcf_data[data.table(expand.grid(year = yrs, area_code = areas,
  item_code = tcf_codes[[3]], item_code_proc = tcf_codes[[2]]))]
setkey(results, item_code, item_code_proc)
results[, `:=`(value = 0, production = NULL, processing = NULL)]

for(x in yrs) {
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


rm(tcf_cbs, tcf_codes, tcf_data, yrs, areas, out,
  results, Cs, input, output, input_x, output_x, input_y, output_y)



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
rm(eth, eth_tcf)



# Optimise feedstock allocation ----------------------------------------------------------------
# Allocate feedstocks to the production of alcoholic beverages and sweeteners
regions <- fread("inst/regions_tidy.csv")
opt_tcf <- fread("inst/tcf_optim.csv")
opt_in <- fread("inst/optim_in.csv")
opt_out <- fread("inst/optim_out.csv")
# Add processing / production information from the balances
input <- merge(opt_in,
               cbs[, c("area_code", "year", "item_code", "processing", "balancing", "feed", "food")],
               by = "item_code", all.x = TRUE)
# Sum up processing, balancing quantities, so that all of them can be used as inputs
input[, `:=`(processing = round(na_sum(processing, balancing),0),
             rest = round(na_sum(feed, food),0))]
input[, `:=`(balancing = NULL, feed = NULL, food = NULL)]
input <- input[(is.finite(processing) & processing > 0) | (is.finite(rest) & rest > 0)]
output <- merge(opt_out,
                cbs[, c("area_code", "year", "item_code", "production")],
                by = "item_code", all.x = TRUE)
output <- output[is.finite(production) & production > 0]

# We have some NAs in the TCFs
opt_tcf <- opt_tcf[is.finite(value), ]

# Subset to needed TCFs (shold be all) and get weights (i.e. mean values)
opt_tcf <- opt_tcf[inp_code %in% opt_in$item_code &
                     out_code %in% opt_out$item_code, ]

# Reduce the sugar cost of sweeteners, other (because this includes molasses which are a by-product from sugar production)
opt_tcf <- opt_tcf[inp_code==2542 & out_code==2543, value := 100]

# Subset to available TCF (should be all)
input <- input[item_code %in% opt_tcf$inp_code, ]
output <- output[item_code %in% opt_tcf$out_code, ]

# Allow a maximum of 1% of food and feed use to be used for processing
input[, processing := na_sum(processing, rest * 0.01)]

# Set NAs to 0
input[is.na(processing), processing := 0]
output[is.na(production), production := 0]

# Create all combinations
scenarios <- CJ(area_code = regions$area_code, year = years)
scenarios <- merge(scenarios, regions[, .(iso3c, area, area_code)], by = "area_code")

# Load solver function
source("R/08_solver_functions.R")

# Run optimization -----
results_list <- mclapply(1:nrow(scenarios), function(i) {
  yr <- scenarios[i]$year
  code <- scenarios[i]$area_code
  area <- scenarios[i]$area
  
  inp_i <- input[year==yr & area_code==code]
  out_i <- output[year==yr & area_code==code]
  tcf_i <- opt_tcf[area_code==code]
  
  solve_flow_qp(area_code = as.integer(code),
                area      = as.character(area),
                year      = as.integer(yr),
                inp       = inp_i,
                out       = out_i,
                tcf       = tcf_i)
}, mc.cores = 10)

# Combine results
flow_results     <- rbindlist(lapply(results_list, `[[`, "flow"),       fill = TRUE)
missing_outputs  <- rbindlist(lapply(results_list, `[[`, "missing"),    fill = TRUE)
oversupply       <- rbindlist(lapply(results_list, `[[`, "oversupply"), fill = TRUE)
overused_inputs  <- rbindlist(lapply(results_list, `[[`, "overused"),   fill = TRUE)
underused_inputs <- rbindlist(lapply(results_list, `[[`, "underused"),  fill = TRUE)


# Add process information
flow_results[, proc_code := ifelse(out_code == 2658, "p083",
  ifelse(out_code == 2657, "p082", ifelse(out_code == 2656, "p081", "p066")))]

# Add optimisation results to use (full detail) and cbs (item detail)
use <- merge(use,
  flow_results[, .(area_code, year, item_code = inp_code, type = "optim",
    proc_code, flow = round(flow))],
  by = c("area_code", "year", "item_code", "proc_code", "type"), all.x = TRUE)
use[!is.na(flow) & type == "optim", use := flow]
use[, flow := NULL]

cbs <- merge(cbs,
  flow_results[, list(flow = na_sum(flow)),
    by = .(area_code, year, item_code = inp_code)],
  by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[!is.na(flow), processing := round(na_sum(processing, -flow))]
cbs[, flow := NULL]

# Allocate negatives in processing use resulting from feedstock optimization to food and feed
cbs[processing < 0, 
    `:=`(food = food + round(processing / na_sum(food, feed) * food), 
         feed = feed + round(processing / na_sum(food, feed) * feed), 
         processing = 0)]
# no negatives are resulting from this step
# cbs[food < 0 , food := 0]
# cbs[feed < 0 , feed := 0]

# add comm code
cbs[, comm_code := items$comm_code[match(cbs$item_code, items$item_code)]]


# Allocation of seed -----
use <- merge(use,
  cbs[, .(area_code, year, item_code, type = "seedwaste", seed_use = seed)],
  by = c("area_code", "year", "item_code", "type"), all.x = TRUE)
use[!is.na(seed_use) & type == "seedwaste", use := seed_use]
use[, seed_use := NULL]




# Feed use ----------------------------------------------------------------
source("R/08_1b_use_cbs_feed.R")




# Allocate final demand from balances -----

# The remainder of processing use (flows into supply chains that are not further tracked in FABIO) is interpreted as a new final demand category
use_fd <- cbs[, .(year, area_code, area, item_code, item, comm_code, 
  food = food + processing, losses, other, 
  stock_addition = stock_addition - stock_withdrawal, tourist)]

# # replace Cyprus' tourist consumption data with more detailed data from SUAs
# tourist <- fread("input/Tourist_Cyprus.csv")
# use_fd <- merge(use_fd, tourist, by = c("item_code", "item", "year"), all.x = TRUE)
# use_fd[area=="Cyprus" & !is.na(value) & na_sum(food, tourist, -value) >= 0, `:=`(food = na_sum(food, tourist, -value), tourist = value)]
# use_fd[, value := NULL]

# Remove unneeded variables
use <- use[, .(year, area_code, area, comm_code, item_code, item,
               proc_code, proc, type, use)]


# Correct remaining imbalances between supply and use -----
a <- use %>% group_by(comm_code, item, year) %>% summarise(use_io = round(sum(use, na.rm = T))) %>% as.data.table()
c <- sup %>% group_by(comm_code, item, year) %>% summarise(supply_total = round(sum(supply, na.rm = T))) %>% as.data.table()
b <- use_fd %>% mutate(use_fd = na_sum(food, losses, other, stock_addition, tourist)) %>%
  group_by(comm_code, item, year) %>% summarise(use_fd = round(sum(use_fd, na.rm = T))) %>% as.data.table()
d <- merge(merge(a,b), c)[year %in% years]
d[, use_total := na_sum(use_io, use_fd)]
d[, diff      := na_sum(supply_total, -use_total)]
d[, `:=`(diff_share = diff / use_total,
         diff_io    = diff / use_io,
         diff_fd    = diff / use_fd)]
# check whether there are NA of Inf values
d[!is.finite(diff_share)]

# re-scale use and use_fd to absorb the differences
# in use_fd
use_fd <- merge(use_fd, d[, .(comm_code, year, diff_share)],
                by = c("comm_code", "year"), all.x = TRUE)
use_fd[, `:=`(
  food            = round((1 + diff_share) * food),
  losses          = round((1 + diff_share) * losses),
  other           = round((1 + diff_share) * other),
  stock_addition  = round((1 + diff_share) * stock_addition),
  tourist         = round((1 + diff_share) * tourist)
)]
use_fd[, `:=`(diff_share = NULL)]

# in use
use <- merge(use, d[, .(comm_code, year, diff_share)],
             by = c("comm_code", "year"), all.x = TRUE)
use[, `:=`(use        = round((1 + diff_share) * use))]
use[, `:=`(diff_share = NULL)]



# Save -----

saveRDS(cbs, "data/cbs_final.rds")
saveRDS(use, "data/use_final.rds")
saveRDS(use_fd, "data/use_fd_final.rds")
saveRDS(sup, "data/sup_final.rds")
