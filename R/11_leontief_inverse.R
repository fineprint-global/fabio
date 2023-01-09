library("data.table")
library("Matrix")


# Leontief inverse ---

prep_solve <- function(year, Z, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {

  if(adj_X) {X <- X + 1e-10}

  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  #A <- Z
  #A@x <- A@x / rep.int(X, diff(A@p))

  if(adj_A) {A[A < 0] <- 0}

  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}

  L <- .sparseDiagonal(nrow(A)) - A

  lu(L) # Computes LU decomposition and stores it in L

  #tryCatch({
  L_inv <- solve(L, tol = .Machine[["double.eps"]], sparse = TRUE)
  #}, error=function(e){cat("ERROR in ", year, "\n")})


  #L_inv[L_inv<0] <- 0

  return(L_inv)
}


years <- seq(1986, 2019)
years_singular <- c(1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2011) # 2013 #c(1994,2002,2009)
years_singular_losses <- c(1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2010, 2011, 2019) #  c(2013,2019) #c(1990,2010,2019) #c(1994,2002,2009)

Z_m <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass.rds")
Z_v <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds")


#year <- 2013
for(year in years){

  print(year)

  adjust <- ifelse(year %in% years_singular, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/", year, "_L_value.rds"))

}



# create the losses version of fabio ---

#year <- 2019
for(year in years){

  print(year)
  # add losses at the main diagonal of Z and remove from Y
  Yi <- Y[[as.character(year)]]
  losses <- rowSums(as.matrix(Yi[, grepl("losses", colnames(Yi))]))
  Yi[, grepl("losses", colnames(Yi))] <- 0
  Y[[as.character(year)]] <- Yi
  Zi <- Z_m[[as.character(year)]]
  diag(Zi) <- diag(Zi) + losses
  Z_m[[as.character(year)]] <- Zi
  Zi <- Z_v[[as.character(year)]]
  diag(Zi) <- diag(Zi) + losses
  Z_v[[as.character(year)]] <- Zi
}

saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/X.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds")
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Z_value.rds")



#year <- 2019
for(year in years){

  print(year)

  adjust_losses <- ifelse(year %in% years_singular_losses, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust_losses)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust_losses)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/", year, "_L_value.rds"))

}

