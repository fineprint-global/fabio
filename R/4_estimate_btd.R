
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")


# BTD ---------------------------------------------------------------------

cat("\nEstimating BTD from CBS.\n")

cbs <- readRDS("data/cbs_full.rds")


# Cast import and export columns
cbs_imp <- dcast(cbs[, c("area_code", "year", "item_code", "imports")],
  year + item_code ~ area_code, value.var = "imports", fun.aggregate = na_sum)
cbs_exp <- dcast(cbs[, c("area_code", "year", "item_code", "exports")],
  year + item_code ~ area_code, value.var = "exports", fun.aggregate = na_sum)

rm(cbs); gc()

# Check equivalence of year, item_code and areas.
stopifnot(all(cbs_exp[, c(1, 2)] == cbs_imp[, c(1, 2)]),
  all(names(cbs_exp) == names(cbs_imp)))
cbs_ids <- cbs_imp[, c("year", "item_code")]

# Use the Matrix package for efficient operations
cbs_imp <- as(cbs_imp[, c(-1, -2)], "Matrix")
cbs_exp <- as(cbs_exp[, c(-1, -2)], "Matrix")


spread_trade <- function(x, split_matr, inp_matr) {
  split_matr[, x] <- 0 # No internal "exports" / "imports"
  split_sums <- rowSums(split_matr, na.rm = TRUE)
  split_sums <- ifelse(split_sums == 0, NA, split_sums) # Avoid NaN
  split_matr / split_sums * inp_matr[, x]
}

build_estimates <- function(name, list, ids, kick_0 = TRUE) {
  x <- list[[name]]
  row_na <- apply(x, 1, function(y) {all(is.na(y))})
  col_na <- apply(x, 2, function(y) {all(is.na(y))})
  out <- melt(cbind(ids[!row_na], as.matrix(x[!row_na, !col_na])),
    id.vars = c("year", "item_code"), na.rm = TRUE,
    variable.name = "area_code")
  if(nrow(out) == 0) {return(NULL)}
  # Make sure encoding is right, to reduce memory load
  out[, `:=`(year = as.integer(year), item_code = as.integer(item_code),
    area_code = as.integer(area_code), inp_code = as.integer(name))]
  # Consider not carrying over 0 values
  if(kick_0) {out[value != 0, ]} else{out}
}


cat("\nPreparing to estimate trade shares. ",
  "More than 16GB of RAM may be required, and speeds up calculations.\n")

# Spread exports according to import shares
est_exp <- lapply(colnames(cbs_imp), spread_trade, cbs_imp, cbs_exp)
names(est_exp) <- colnames(cbs_imp)
est_exp <- lapply(colnames(cbs_imp), build_estimates, est_exp, cbs_ids)
est_exp <- rbindlist(est_exp)
est_exp <- est_exp[, .(year, item_code,
  to_code = area_code, from_code = inp_code, exp_spread = value)]

# Spread imports according to export shares
est_imp <- lapply(colnames(cbs_exp), spread_trade, cbs_exp, cbs_imp)
names(est_imp) <- colnames(cbs_exp)
est_imp <- lapply(colnames(cbs_exp), build_estimates, est_imp, cbs_ids)
est_imp <- rbindlist(est_imp)
est_imp <- est_imp[, .(year, item_code,
  from_code = area_code, to_code = inp_code, imp_spread = value)]

rm(cbs_exp, cbs_imp, cbs_ids); gc()

# Merge import-share and export-share based estimates
btd_est <- merge(est_exp, est_imp,
  by = c("year", "item_code", "from_code", "to_code"), all = TRUE)
rm(est_exp, est_imp); gc()

# Average the estimates - note that 0 estimates may be considered NA
btd_est[, `:=`(exp_spread = NULL, imp_spread = NULL,
  value = ifelse(is.na(exp_spread), imp_spread,
    ifelse(is.na(imp_spread), exp_spread,
      (imp_spread + exp_spread) / 2)))]


# Store result ----------------a--------------------------------------------
saveRDS(btd_est, "data/btd_est.rds")
