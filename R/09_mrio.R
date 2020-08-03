
library("Matrix")


# MRIO Table ---

mr_sup_m <- readRDS("data/mr_sup_mass.rds")
mr_sup_v <- readRDS("data/mr_sup_value.rds")
mr_use <- readRDS("data/mr_use.rds")


# Mass
trans_m <- lapply(mr_sup_m, function(x) {
  out <- as.matrix(x / rowSums(x))
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
})

Z_m <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_m)

Z_m <- lapply(Z_m, round)
saveRDS(Z_m, "data/Z_mass.rds")


# Value
trans_v <- lapply(mr_sup_v, function(x) {
  out <- as.matrix(x / rowSums(x))
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
})

Z_v <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_v)

Z_v <- lapply(Z_v, round)
saveRDS(Z_v, "data/Z_value.rds")

