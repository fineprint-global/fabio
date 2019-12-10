
library(data.table)

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
tcf <- fread("inst/tcf_use.csv")

cbs <- readRDS("data/cbs_full.rds")
btd <- readRDS("data/btd_full.rds")
sup <- readRDS("data/sup.rds")

use <- fread("inst/items_use.csv")


# Use ---------------------------------------------------------------------

# Create long use table
use <- merge(
  cbs,
  # cbs[, c("area_code", "area", "year", "item_code", "item")],
  use, by = c("item_code", "item"), all.x = TRUE, allow.cartesian = TRUE)
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


# Crop TCF ----------------------------------------------------------------

# # FUCK >
# # Allocate TCF crops
# crop_tcf <- use[type == "TCF"]

# # Dirty - don't do this
# prcs <- u(crop_tcf$proc)
# i <- 3
# itms <- u(crop_tcf[proc == prcs[i], item])
# prod <- u(tcf[Process == prcs[i], Prod.Code])
# supply <- use[item_code == prod, production]

# # Pseudo, 1 item
# use$use[...] <- supply / tcf
# use[use > processing, use = processing]
# processing <- processing - use

# # 2 item
# tmp <- data.frame(
#   proc = use[area_code == 1 & year == 2005 & item %in% itms, processing],
#   item = use[area_code == 1 & year == 2005 & item %in% itms, item],
#   prod = use[area_code == 1 & year == 2005 & item_code == prod, production])
# # < THIS


# Ethanol production ------------------------------------------------------

eth_tcf <- fread("inst/tcf_eth.csv")

eth <- cbs[item_code %in% eth_tcf$item_code, ]
eth <- merge(eth, eth_tcf[area_code == 231, c("item_code", "tcf")],
  by = "item_code", all.x = TRUE)
eth <- merge(eth, eth_tcf[, c("area_code", "item_code", "value")],
  by = c("area_code", "item_code"), all = TRUE)

eth[, `:=`(pot_oth = other * tcf, pot_proc = processing * tcf)]

na_sum <- function(..., rowwise = TRUE) {
  dots <- list(...)
  if(length(dots) == 1) { # Base
    ifelse(all(is.na(dots[[1]])), NA_real_, sum(dots[[1]], na.rm = TRUE))
  } else { # Recurse
    if(rowwise) {
      x <- do.call(cbind, dots)
      return(apply(x, 1, na_sum))
    }
    return(na_sum(vapply(dots, na_sum, double(1L))))
  }
}
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
feed_sup <- cbs[feed > 0, c("area_code", "item_code", "year", "feed")]
# Convert to dry matter
feed_sup <- merge(feed_sup, items[, c("item_code", "moisture", "feedtype")],
  by = c("item_code"))
feed_sup[, dry := feed * (1 - moisture)]

# Requirements

# Estimates from Krausmann et al. (2008)
conv_k <- fread("inst/conv_krausmann.csv")

feed_req_k <- merge(conv_k,
  live[, c("area_code", "area", "item_code", "year", "value")],
  by = c("item_code"), all.x = TRUE)
feed_req_k[, `:=`(
  total = value * conversion, value = NULL, conversion = NULL,
  area = NULL, item = NULL, proc = NULL, # Kick these
  crops = 0, residues = 0, grass = 0, scavenging = 0, animals = 0)]

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
feed_req_b <- sup[item_code %in%
  c(2848, 2731, 2732, 2733, 2734, 2735, 2736, 2737, 2748, 2749, 843),
  c("area_code", "year", "proc_code", "item_code", "comm_code", "production")]

feed_req_b[, type := ifelse(item_code == 2848, "milk", "meat")]

cat("Recoding processes,",
  "e.g. from 'Cattle slaughtering' to 'Cattle husbandry'.\n")
vsub <- function(a, b, x) {
  stopifnot(length(a) == length(b))
  for(i in seq_along(a)) {x <- gsub(a[i], b[i], x)}
  return(x)
}
proc_source <- c("p104","p105","p106","p107","p108","p109")
proc_target <- c("p085","p086","p087","p088","p089","p090")
feed_req_b[, proc_code := vsub(proc_source, proc_target, proc_code)]
rm(proc_source, proc_target)

cat("Skipping estimation of domestic meat production.\n")
# Estimated export shares (2013) - Median: 0.000000; Mean: 0.028150

feed_req_b <- merge(all.x = TRUE, allow.cartesian = TRUE,
  feed_req_b,
  conv_b[, c("area_code", "year", "proc_code", "type", "feedtype", "conversion")],
  by = c("area_code", "year", "proc_code", "type"))

feed_req_b[, converted := production * conversion]

feed_req_b <- dcast(feed_req_b, value.var = "converted", fun.aggregate = na_sum,
  area_code + year + proc_code + item_code + comm_code + type ~ feedtype)
# Kick unwanted columns
feed_req_b[, `:=`(comm_code = NULL, type = NULL, `NA` = NULL, item_code = 0)]
# Aggregate over items (all now 0)
feed_req_b <- feed_req_b[, list(
  animals = na_sum(animals), crops = na_sum(crops), grass = na_sum(grass),
  residues = na_sum(residues), scavenging = na_sum(scavenging)),
  by = list(area_code, year, proc_code, item_code)]
feed_req_b[, `:=`(total = na_sum(animals, crops, grass, residues, scavenging))]

# Original subsets to >0
feed_req_b <- feed_req_b[!is.na(total)]
feed_req_b[, lapply(.SD, na_sum),
  by = list(area_code, year, proc_code, item_code)]

feed_req <- rbind(feed_req_b, feed_req_k)
rm(feed_req_k, feed_req_b)

# Allocate total feed demand from Krausmann
feed_alloc <- feed_req[item_code == 0,
  lapply(list(animals, crops, grass, residues, scavenging, total), na_sum),
  by = list(area_code, year)]
feed_alloc <- feed_alloc[total > 0, list(area_code, year,
  animals_f = V1 / V6, crops_f = V2 / V6, grass_f = V3 / V6,
  residues_f = V4 / V6, scavenging_f = V5 / V6)]

feed_req <- merge(feed_req, feed_alloc,
  by = c("area_code", "year"), all.x = TRUE)
# Use a 1 - 2 - 2 - 1 - 1 split if no info is available
feed_req[is.na(animals_f),
  `:=`(animals_f = 1 / 7, crops_f = 2 / 7, grass_f = 2 / 7,
       residues_f = 1 / 7, scavenging_f = 1 / 7)]
feed_req[item_code != 0,
  `:=`(animals = total * animals_f, crops = total * crops_f,
       grass = total * grass_f, residues = total * residues_f,
       scavenging = total * scavenging_f)]
# Kick factors again
feed_req[, `:=`(animals_f = NULL, crops_f = NULL, grass_f = NULL,
  residues_f = NULL, scavenging_f = NULL)]

# Adapt feed-crops to available crop-supply
feed_bounds <- feed_sup[, list(dry = na_sum(dry)),
  by = c("area_code", "year", "feedtype")]



