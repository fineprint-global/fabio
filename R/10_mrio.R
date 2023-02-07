
library("Matrix")
library("parallel")
library(data.table)

# MRIO Table ---

mr_sup_m <- readRDS("data/mr_sup_mass.rds")
mr_sup_v <- readRDS("data/mr_sup_value.rds")
mr_use <- readRDS("data/mr_use.rds")

# Mass
trans_m <- mclapply(mr_sup_m, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 6)

Z_m <- mcmapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_m, mc.cores = 6)

Z_m <- lapply(Z_m, round)


# Value
trans_v <- mclapply(mr_sup_v, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 6)

Z_v <- mcmapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_v, mc.cores = 6)

Z_v <- lapply(Z_v, round)



# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")

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
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds")




# redistribute balancing over all uses proportionally ---------------------------------------------
regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
nrreg <- nrow(regions)
nrfd <- ncol(Y[[1]])/nrreg
#i=28
for(i in seq_along(Z_m)){
  #reg=1
  for(reg in seq_len(nrow(regions))){
    z_range <- (nrcom*(reg-1)+1):(nrcom*reg)
    y_range <- (nrfd*(reg-1)+1):(nrfd*reg)
    Z_sum <- rowSums(Z_m[[i]][, z_range])
    Y_sum <- rowSums(Y[[i]][, y_range])
    balancing <- as.vector(Y[[i]][, grepl("balancing", colnames(Y[[i]]))][, reg])
    balancing <- balancing / as.vector(Z_sum + Y_sum - balancing)
    balancing[!is.finite(balancing)] <- 0
    Z_m[[i]][, z_range] <- Z_m[[i]][, z_range] * (1 + balancing)
    Z_v[[i]][, z_range] <- Z_v[[i]][, z_range] * (1 + balancing)
    Y[[i]][, y_range] <- Y[[i]][, y_range] * (1 + balancing)
  }
}

saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass_b.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value_b.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Y_b.rds")




# allocate ghg emissions to products --------------------------------------------------------------
ghg <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg.rds")
gwp <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_gwp.rds")
luh <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2.rds")

ghg_names <- ghg[[1]][,1]
gwp_names <- gwp[[1]][,1]
luh_names <- luh[[1]][,1]

write_csv(ghg_names, "/mnt/nfs_fineprint/tmp/fabio/v1.2/ghg_names.csv")
write_csv(gwp_names, "/mnt/nfs_fineprint/tmp/fabio/v1.2/gwp_names.csv")
write_csv(luh_names, "/mnt/nfs_fineprint/tmp/fabio/v1.2/luh_names.csv")

range <- rep(c(1:97,99:116,118:120),192)+rep(((0:191)*121), each=118)

ghg_m <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = ghg, y = trans_m[1:28])
gwp_m <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = gwp, y = trans_m[1:28])
luh_m <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = luh, y = trans_m[1:28])
ghg_v <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = ghg, y = trans_v[1:28])
gwp_v <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = gwp, y = trans_v[1:28])
luh_v <- mapply(function(x, y) { as.matrix(x[,-1][,range]) %*% y }, x = luh, y = trans_v[1:28])

saveRDS(ghg_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_mass.rds")
saveRDS(gwp_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_mass.rds")
saveRDS(luh_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_mass.rds")

saveRDS(ghg_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_value.rds")
saveRDS(gwp_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_value.rds")
saveRDS(luh_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_value.rds")

