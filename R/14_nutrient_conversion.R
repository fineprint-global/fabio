
library("Matrix")
library("parallel")
library(data.table)

# read data ---
sup <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_sup_mass.rds")
use <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/mr_use.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
io_codes <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")
coeff <- fread("inst/nutrient_coefficients.csv")
io_codes$calorific <- coeff$kcal_per_kg[match(io_codes$item_code, coeff$item_code)]

# convert into calories
sup_e <- mclapply(sup, function(x) {
  out <- t(t(x) * io_codes$calorific)
  return(out)
}, mc.cores = 10)

use_e <- mclapply(use, function(x) {
  out <- x * io_codes$calorific
  return(out)
}, mc.cores = 10)

Y_e <- mclapply(Y, function(x) {
  out <- x * io_codes$calorific
  return(out)
}, mc.cores = 10)


# Store converted variables
saveRDS(sup_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/sup.rds")
saveRDS(use_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/use.rds")
saveRDS(Y_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Y.rds")



# MRIO Table ---

trans_e <- mclapply(sup_e, function(x) {
  #out <- as.matrix(x / rowSums(x))
  out <- x
  out@x <- out@x / rowSums(out)[(out@i+1)]
  out[!is.finite(out)] <- 0 # See Issue #75
  #return(as(out, "Matrix"))
  return(out)
}, mc.cores = 10)

Z_e <- mcmapply(function(x, y) {
  x %*% y
}, x = use_e, y = trans_e, mc.cores = 10)


# Rebalance row sums in Z and Y -----------------------------------------

regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y_e <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Y.rds")

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
saveRDS(Z_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Z.rds")
saveRDS(Y_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Y.rds")
saveRDS(X_e, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/X.rds")




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



# Leontief inverse ---

prep_solve <- function(year, Z, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {
  
  if(adj_X) {X <- X + 1e-10}
  # index_cotton <- which(names(X) == "c025" & X > 0)
  # X[index_cotton] <- X[index_cotton] + 1e-5
  
  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  #A <- Z
  #A@x <- A@x / rep.int(X, diff(A@p))
  
  if(adj_A) {A[A < 0] <- 0}
  
  if(adj_diag) {diag(A)[diag(A) >= 1] <- 1 - 1e-10}
  
  L <- .sparseDiagonal(nrow(A)) - A
  
  lu(L) # Computes LU decomposition and stores it in L
  
  #tryCatch({
  L_inv <- solve(L, tol = .Machine[["double.eps"]], sparse = TRUE)
  #}, error=function(e){cat("ERROR in ", year, "\n")})
  
  
  #L_inv[L_inv<0] <- 0
  
  return(L_inv)
}


years <- seq(1986, 2021)
years_singular <- years #c(1987, 1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2011)
# years_singular_losses <- c(1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2010, 2011, 2019)

Z <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Z.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/X.rds")


#year <- 2020
for(year in years){
  
  skip_to_next <- FALSE
  
  tryCatch({
    
    adjust <- ifelse(year %in% years_singular, TRUE, FALSE)
    
    L <- prep_solve(year = year, Z = Z[[as.character(year)]],
                    X = X[, as.character(year)], adj_diag = adjust) #, adj_X = adjust)
    
    L[L<0] <- 0
    
    saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/", year, "_L.rds"))
    
  }, error=function(e){skip_to_next <<- TRUE})
  
  if(!skip_to_next){
    cat(paste0("Sucessfully inverted matrix for ",year,".\n"))
  } else { next }
}



# create the losses version of fabio ---

#year <- 2019
for(year in years){

  print(year)
  
  # remove losses from Y
  Yi <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi[, grepl("losses", colnames(Yi))] <- 0
  Y[[as.character(year)]] <- Yi
  
  # subtract losses from X
  X[,as.character(year)] <- X[,as.character(year)] - rowSums(losses)
  
}

saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/losses/X.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/losses/Y.rds")



# derive L inverse -----------

years_singular_losses <- years
#year <- 2019
for(year in years){

  skip_to_next <- FALSE
  
  tryCatch({
    
    adjust <- ifelse(year %in% years_singular_losses, TRUE, FALSE)
    
    L <- prep_solve(year = year, Z = Z[[as.character(year)]],
                    X = X[, as.character(year)], adj_diag = adjust_losses)
    
    L[L<0] <- 0
    
    saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/calories/losses/", year, "_L.rds"))
    
  }, error=function(e){skip_to_next <<- TRUE})
  
  if(!skip_to_next){
    cat(paste0("Sucessfully inverted matrix for ",year,".\n"))
  } else { next }

}

