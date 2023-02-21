
library("Matrix")
library("parallel")
library(data.table)

# MRIO Table ---

sup_e <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/sup.rds")
use_e <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/use.rds")

# Mass
trans_e <- mclapply(sup_e, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 6)

Z_e <- mcmapply(function(x, y) {
  x %*% y
}, x = use_e, y = trans_e, mc.cores = 6)




# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y_e <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Y.rds")

# Rebalance row sums for each year
for(i in seq_along(Z_e)){

  X <- rowSums(Z_e[[i]]) + rowSums(Y_e[[i]])

  for(j in which(X < 0)){
    reg <- j %/% nrcom + 1
    Y_e[[i]][j, paste0(regions[reg, code], "_balancing")] <-
      Y_e[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
  }

}


# Derive total output X ---------------------------------------------

X_e <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_e, y = Y_e)



# Store X, Y, Z variables
saveRDS(Z_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Z.rds")
saveRDS(Y_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Y.rds")
saveRDS(X_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/X.rds")




# # redistribute balancing over all uses proportionally ---------------------------------------------
# regions <- fread("inst/regions_full.csv")
# regions <- regions[cbs==TRUE]
# items <- fread("inst/items_full.csv")
# nrcom <- nrow(items)
# nrreg <- nrow(regions)
# nrfd <- ncol(Y[[1]])/nrreg
# #i=28
# for(i in seq_along(Z_m)){
#   #reg=1
#   for(reg in seq_len(nrow(regions))){
#     z_range <- (nrcom*(reg-1)+1):(nrcom*reg)
#     y_range <- (nrfd*(reg-1)+1):(nrfd*reg)
#     Z_sum <- rowSums(Z_m[[i]][, z_range])
#     Y_sum <- rowSums(Y[[i]][, y_range])
#     balancing <- as.vector(Y[[i]][, grepl("balancing", colnames(Y[[i]]))][, reg])
#     balancing <- balancing / as.vector(Z_sum + Y_sum - balancing)
#     balancing[!is.finite(balancing)] <- 0
#     Z_m[[i]][, z_range] <- Z_m[[i]][, z_range] * (1 + balancing)
#     Z_v[[i]][, z_range] <- Z_v[[i]][, z_range] * (1 + balancing)
#     Y[[i]][, y_range] <- Y[[i]][, y_range] * (1 + balancing)
#   }
# }
#
# saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass_b.rds")
# saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value_b.rds")
# saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Y_b.rds")

