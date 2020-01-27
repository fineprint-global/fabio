
library(data.table)
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# BTD ---------------------------------------------------------------------

cat("\nBalancing full BTD.\n")

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")

expand.grid(unique(btd[, c("item_code", "from_code", "to_code", "year", "unit")]))

btd_start <- data.table(
  from_code, to_code, item_code, year, value)



imp <- reshape2::dcast(CBS[,-7], Item.Code + Item + Year ~ Country.Code, value.var = "Imports")

exp <- reshape2::dcast(CBS[,-6], Item.Code + Item + Year ~ Country.Code, value.var = "Exports")