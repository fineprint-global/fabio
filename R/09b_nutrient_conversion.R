
library("Matrix")
library("parallel")
library(data.table)

# read data ---
sup <- readRDS("data/mr_sup_mass.rds")
use <- readRDS("data/mr_use.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
io_codes <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")
coeff <- fread("inst/nutrient_coefficients.csv")
io_codes$calorific <- coeff$kcal_per_kg[match(io_codes$item_code, coeff$item_code)]

# convert into calories
sup_e <- mclapply(sup, function(x) {
  out <- t(t(x) * io_codes$calorific)
  return(out)
}, mc.cores = 6)

use_e <- mclapply(use, function(x) {
  out <- x * io_codes$calorific
  return(out)
}, mc.cores = 6)

Y_e <- mclapply(Y, function(x) {
  out <- x * io_codes$calorific
  return(out)
}, mc.cores = 6)


# Store converted variables
saveRDS(sup_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/sup.rds")
saveRDS(use_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/use.rds")
saveRDS(Y_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Y.rds")
