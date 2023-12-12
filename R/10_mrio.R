
library(Matrix)
library(parallel)
library(data.table)
library(readr)

# MRIO Table ---

mr_sup_m <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_sup_mass.rds")
mr_sup_v <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_sup_value.rds")
mr_use <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_use.rds")

# Mass
trans_m <- mclapply(mr_sup_m, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 10)

Z_m <- mcmapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_m, mc.cores = 10)

Z_m <- lapply(Z_m, round)


# Value
trans_v <- mclapply(mr_sup_v, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 10)

Z_v <- mcmapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_v, mc.cores = 10)

Z_v <- lapply(Z_v, round)



# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_use_fd.rds")

# Rebalance row sums for each year
for(i in seq_along(Z_m)){

  X <- rowSums(Z_m[[i]]) + rowSums(Y[[i]])

  for(j in which(X < 0)){
    reg <- j %/% nrcom + 1
    print(paste0(regions[reg, name], " / ", X[j]))
    Y[[i]][j, paste0(regions[reg, code], "_balancing")] <-
      Y[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
  }

}

# Integrate balancing and residuals, food and processing
for(i in seq_along(Y)){
  
  Y[[i]][, grepl("balancing", colnames(Y[[i]]))] <-
    Y[[i]][, grepl("balancing", colnames(Y[[i]]))] +
    Y[[i]][, grepl("residuals", colnames(Y[[i]]))]
  
  Y[[i]][, grepl("food", colnames(Y[[i]]))] <-
    Y[[i]][, grepl("food", colnames(Y[[i]]))] +
    Y[[i]][, grepl("processing", colnames(Y[[i]]))]
  
  Y[[i]] <- Y[[i]][, !grepl("residuals", colnames(Y[[i]]))]
  Y[[i]] <- Y[[i]][, !grepl("processing", colnames(Y[[i]]))]
  
}

fd_codes <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/fd_codes.csv")
fd_codes <- fd_codes[!fd %in% c("residuals", "processing")]
fwrite(fd_codes, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/fd_codes.csv")


# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_m, y = Y)


# Store X, Y, Z variables
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds")




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




# create the losses version of fabio ---

years <- seq(1986, 2021)

# year <- 2019
for(year in years){
  
  print(year)
  
  # remove losses from Y
  Yi <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi[, grepl("losses", colnames(Yi))] <- 0
  Y[[as.character(year)]] <- Yi
  
  # remove this and instead subtract losses from output X:
  # # reshape losses for adding them later to the main diagonals of each submatrix of Z
  # ## Get the number of rows and columns in the data matrix
  # num_rows <- nrow(losses)
  # num_cols <- nrow(losses) / ncol(losses)
  # 
  # ## Define a function for reshaping
  # reshape_column <- function(v) {
  #   m <- matrix(0, ncol = num_cols, nrow = num_rows)
  #   indices <- ((seq_len(length(v)) - 1) %% num_cols) + 1
  #   m[cbind(seq_len(length(v)), indices)] <- v
  #   return(m)
  # }
  # 
  # ## Apply the reshape_column function to each column using lapply
  # matrix_list <- lapply(1:ncol(losses), function(i) {
  #   v <- losses[, i]
  #   reshape_column(v)
  # })
  # 
  # ## Combine the matrices in the list using cbind()
  # combined_matrix <- do.call(cbind, matrix_list)
  # combined_matrix <- as(combined_matrix, "dgCMatrix")
  # 
  # 
  # # add losses to the main diagonals of each submatrix of Z_m
  # Zi <- Z_m[[as.character(year)]]
  # Zi <- Zi + combined_matrix
  # Z_m[[as.character(year)]] <- Zi
  # 
  # # add losses to the main diagonals of each submatrix of Z_v
  # Zi <- Z_v[[as.character(year)]]
  # Zi <- Zi + combined_matrix
  # Z_v[[as.character(year)]] <- Zi
  
  X[,as.character(year)] <- X[,as.character(year)] - rowSums(losses)
  
}

saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/X.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds")
# saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Z_mass.rds")
# saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Z_value.rds")





# allocate ghg emissions to products --------------------------------------------------------------
ghg <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg.rds")
gwp <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_gwp.rds")
luh <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2.rds")

ghg_names <- ghg[[1]][,1]
gwp_names <- gwp[[1]][,1]
luh_names <- luh[[1]][,1]

write_csv(data.frame(ghg_names), "/mnt/nfs_fineprint/tmp/fabio/v1.2/ghg_names.csv")
write_csv(data.frame(gwp_names), "/mnt/nfs_fineprint/tmp/fabio/v1.2/gwp_names.csv")
write_csv(data.frame(luh_names), "/mnt/nfs_fineprint/tmp/fabio/v1.2/luh_names.csv")

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

