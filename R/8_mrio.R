
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

sup <- readRDS("data/sup_final.rds")

cbs <- readRDS("data/cbs_final.rds")
btd <- readRDS("data/btd_final.rds")
use <- readRDS("data/use_final.rds")


# Supply ---

years <- seq(1986, 2013)
areas <- unique(sup$area_code)
processes <- unique(sup$proc_code)
commodities <- unique(sup$comm_code)

template <- data.table(expand.grid(
  proc_code = processes, comm_code = commodities))
setkey(template, proc_code, comm_code)

mr_sup_m <- lapply(years, function(x) {
  matrices <- lapply(areas, function(y, sup_y) {
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, production)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, production = 0)]
    } else {template[sup_x]}
    out <- tryCatch(dcast(out, proc_code ~ comm_code,
      value.var = "production", fun.aggregate = na_sum, fill = 0),
      error = function(e) {stop("Issue at ", x, "-", y, ": ", e)})
    Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
      dimnames = list(out$proc_code, colnames(out)[-1]))
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, production)])
  bdiag(matrices)
})

mr_sup_p <- lapply(years, function(x) {
  matrices <- lapply(areas, function(y, sup_y) {
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, price)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, price = 0)]
    } else {template[sup_x]}
    out <- tryCatch(dcast(out, proc_code ~ comm_code,
      value.var = "price", fun.aggregate = na_sum, fill = 0),
      error = function(e) {stop("Issue at ", x, "-", y, ": ", e)})
    Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
      dimnames = list(out$proc_code, colnames(out)[-1]))
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, price)])
  bdiag(matrices)
})

names(mr_sup_m) <- names(mr_sup_p) <- years


# Use ---

# per year?

btd$comm_code <- code
dcast(btd, from_code + comm_code ~ to_code)

cbs$comm_code <- code
dcast(cbs, area_code + comm_code ~ area_code)

total <- cbs + btd

total_agg <- total[, list(value = na_sum(value)), by = "comm_code"]
total_agg[match(total$comm_code, total_agg$comm_code)]

# != 0
supply_shares <- total / total_agg


use <- dcast(use, comm_code ~ area_code + proc_code, value.var = "use")

mr_use <- use[match(total$comm_code, use$comm_code)]
Matrix(data.matrix(mr_use))

mr_use * supply_shares


mr_use_fd <- use_fd[match()]
Matrix(data.matrix(use_fd))

mr_use_fd * supply_shares

saveRDS()
