
library(data.table)

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
tcf <- fread("inst/use_tcf.csv", header = TRUE, sep = ";")

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

na_sum <- function(x) {ifelse(all(is.na(x)), NA_real_, sum(x, na.rm = TRUE))}
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
