
library("Matrix")


# MRIO Table ---

mr_sup_m <- readRDS("data/mr_sup_wood_mass.rds")
mr_sup_v <- readRDS("data/mr_sup_wood_value.rds")
mr_use <- readRDS("data/mr_use_wood.rds")

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



# Rebalance row sums in Z and Y -----------------------------------------

library(data.table)
regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/wood/Y.rds")

# Rebalance row sums for each year
for(i in seq_along(Z_m)){

  X <- rowSums(Z_m[[i]]) + rowSums(Y[[i]])

  for(j in which(X < 0)){
    reg <- j %/% nrcom + 1
    Y[[i]][j, paste0(regions[reg, code], "_balancing")] <-
      Y[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
  }

}


# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_m, y = Y)



# Store X, Y, Z variables
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v2/wood/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v2/wood/Z_value.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v2/wood/Y.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v2/wood/X.rds")

