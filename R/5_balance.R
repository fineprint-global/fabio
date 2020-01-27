
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# BTD ---------------------------------------------------------------------

cat("\nBalancing full BTD.\n")

btd <- readRDS("data/btd_full.rds")
cbs <- readRDS("data/cbs_full.rds")

btd_start <- data.table(
  from_code, to_code, item_code, year, value)


cbs_imp <- dcast(cbs[, c("area_code", "year", "item_code", "imports")],
  year + item_code ~ area_code, value.var = "imports", fun.aggregate = na_sum)
cbs_exp <- dcast(cbs[, c("area_code", "year", "item_code", "exports")],
  year + item_code ~ area_code, value.var = "exports", fun.aggregate = na_sum)

# Check equivalence of year, item_code and areas.
stopifnot(all(cbs_exp[, c(1, 2)] == cbs_imp[, c(1, 2)]),
  all(names(cbs_exp) == names(cbs_imp)))
ids <- cbs_imp[, c("year", "item_code")]

cbs_imp <- as(cbs_imp[, c(-1, -2)], "Matrix")
cbs_exp <- as(cbs_exp[, c(-1, -2)], "Matrix")

spread_trade <- function(x, split_matr, inp_matr) {
  split_matr[, x] <- 0
  split_sums <- rowSums(split_matr, na.rm = TRUE)
  split_sums <- ifelse(split_sums == 0, NA, split_sums)
  split_matr / split_sums * inp_matr[, x]
}

build_back <- function(name, list, ids) {
  x <- list[[name]]
  row_na <- apply(x, 1, function(y) {all(is.na(y))})
  col_na <- apply(x, 2, function(y) {all(is.na(y))})
  melt(cbind(ids[!row_na], as.matrix(x[!row_na, !col_na])),
    id.vars = c("year", "item_code"), na.rm = TRUE,
    variable.name = "area_code", value.name = name)
}

est_exp <- lapply(colnames(cbs_imp), spread_trade, cbs_imp, cbs_exp)
names(est_exp) <- colnames(cbs_imp)
est_exp <- lapply(colnames(cbs_imp), build_back, est_exp, ids)
est_exp <- Reduce(function(x, y) {
  merge(x, y, by = c("year", "item_code"),
  sort = FALSE, all = TRUE, allow.cartesian = TRUE)}, est_exp)

est_imp <- lapply(colnames(cbs_exp), spread_trade, cbs_exp, cbs_imp)
names(est_imp) <- colnames(cbs_exp)
est_imp <- lapply(colnames(cbs_exp), build_back, est_imp, ids)
est_imp <- Reduce(function(x, y) {
  merge(x, y, by = c("year", "item_code"),
  sort = FALSE, all = TRUE, allow.cartesian = TRUE)}, est_imp)
