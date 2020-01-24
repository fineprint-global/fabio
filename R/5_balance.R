
library(data.table)
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# BTD ---------------------------------------------------------------------

cat("\nBuilding full BTD.\n")

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")

btd_start <- data.table(
  from_code, to_code, item_code, year, value)