
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

# Allocate crops that go directly to a process
use[type == "100%", `:=`(use = processing, processing = 0)]

# Allocate live animals to slaughtering
use[type == "slaughtering"]

# FUCK >
# Allocate TCF crops
crop_tcf <- use[type == "TCF"]

# Dirty - don't do this
prcs <- u(crop_tcf$proc)
i <- 3
itms <- u(crop_tcf[proc == prcs[i], item])
prod <- u(tcf[Process == prcs[i], Prod.Code])
supply <- use[item_code == prod, production]

# Pseudo, 1 item
use$use[...] <- supply / tcf
use[use > processing, use = processing]
processing <- processing - use

# 2 item
tmp <- data.frame(
  proc = use[area_code == 1 & year == 2005 & item %in% itms, processing],
  item = use[area_code == 1 & year == 2005 & item %in% itms, item],
  prod = use[area_code == 1 & year == 2005 & item_code == prod, production])
# < THIS

# Allocate crops to ethanol feedstocks
# We know TCFs and shares for some countries

eth$poten_o <- eth$other * eth$tcf
eth$poten_p <- eth$processing * eth$tcf
# Cap below at 0?
eth_total <- eth[,
  list(poten_o_t = sum(poten_o, na.rm = TRUE),
       poten_p_t = sum(poten_p, na.rm = TRUE)),
  by = "area_code"]
eth <- merge(eth, eth_total, by = "area_code" all.x = TRUE)

eth[, `:=`(share_o = poten_o / poten_o_t,
           share_p = poten_p / poten_p_t)]
# Kick NAs?
# Merge with CBS ethanol?
eth$feedstock_o <- eth$production * eth$share_o / eth$tcf
# cap at other
# Too little? Use processing use as well
if(poten_o_t < production)
  feedstock_p <- (production - poten_o_t) * share_p / tcf
# cap at processig








