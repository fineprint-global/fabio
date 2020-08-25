
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




library(data.table)

# Remove items without data -----------------------------------------

Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/neu/Y.rds")
no_data <- c("Pet food", "Live animals, other")
items <- fread("inst/items_full.csv")
items <- items[!item %in% no_data]
regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]

for(i in seq_along(Z_m)){
  Z_m[[i]] <- Z_m[[i]][rownames(Z_m[[i]]) %in% items$comm_code,
                       rownames(Z_m[[i]]) %in% items$comm_code]
  Z_v[[i]] <- Z_v[[i]][rownames(Z_v[[i]]) %in% items$comm_code,
                       rownames(Z_v[[i]]) %in% items$comm_code]
  Y[[i]] <- Y[[i]][rownames(Y[[i]]) %in% items$comm_code,]
}


# Rebalance row sums in Z and Y -------------------------------------

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
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/neu/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/neu/Z_value.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/neu/Y.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/neu/X.rds")


