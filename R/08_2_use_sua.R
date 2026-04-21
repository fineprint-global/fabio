library("tidyverse")
library("data.table")
library("readxl")
source("R/00_system_variables.R")

sua <- readRDS("data/sua_full.rds") # need to update this to sua_full
items_sua <- fread("inst/sua/items_sua.csv")
proc_sua <- fread("inst/sua/proc_sua.csv")
tcf <- readRDS("data/sua/tcf_sua_final.rds")
use_items <- readRDS("data/sua/use_items_sua.rds")

# create use table -------------

#model after these tables
cbs_use_final <- readRDS("~/fabio/data/use_final.rds")
cbs_use_items <- fread("inst/items_use.csv")
cbs_tcf <- fread("inst/tcf_cbs.csv")


# - can get seedwaste, feed, etc. from sua directly
# - can get processing structure completely from tcf_final (dividing up the sua columns
# "processing", "other", and "residual")

# define use types
use_items[is.na(parent), `:=` (parent = child, parent_code = child_code)]
use_items[, processed := items_sua$processed[match(child, items_sua$item)]]

# get items where all the item that is processed goes into one process

use_items[, type_1 := ifelse(processed == "processed" & .N == 1, "100%", NA_character_), by = parent]

# Get rid of "child items"
use_items[, `:=` (child = NULL, child_code = NULL)]
setnames(use_items, c("parent", "parent_code"), c("item", "comm_code"))

# get feed
feed_items <- sua[feed > 0 & is.finite(feed), unique(item)]
use_items[item %in% feed_items, type_2 := "feed"]

# get items where the item that is processed goes into several processes
use_items[ processed == "processed" & type_1 != "100%", type_3 := "tcf"]

# create long use_table

use_items <- melt(use_items, 
                id.vars = c("item", "comm_code", "proc", "proc_code", "processed"),
                measure.vars = patterns("^type_"),
                value.name = "type")
use_items <- use_items[!is.na(type)]
use_items[, variable := NULL]
use_items[, item_code_fcl := items_sua$item_code_fcl[match(item, items_sua$item)]]

# save use_items structure
saveRDS(use_items, "data/sua/use_items_final.rds")

# expand to get full table
use_sua <- merge(
  sua[, c("area_code", "area", "year", "item_code_fcl", "production", "processing")],
  use_items,
  by = c("item_code_fcl"), all = TRUE, allow.cartesian = TRUE)
use_sua[, use := NA_real_]

# start with fodder, seedwaste and 100%

# feed will be adapted in use_feed_cbs


# continue with tcf (first do oilseeds)
# have a look at split_tcf (incl your own notes) and fill_tcf (used to derive amount of 
# parent item going to processing in build_cbs where values are missing)


















