library("data.table")
library("Matrix")


# Leontief inverse ---

prep_solve <- function(year, Z, X,
                       adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {

  if(adj_X) {X <- X + 1e-10}

  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  if(adj_A) {A[A < 0] <- 0}

  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}
  L <- .sparseDiagonal(nrow(A)) - A

  lu(L) # Computes LU decomposition and stores it in L

  L_inv <- solve(L, tol = .Machine[["double.eps"]])

  L_inv[L_inv<0] <- 0

  return(L_inv)
}


years <- seq(1986, 2013)
years_singular <- c(1986,1994,2002,2009)

Z_m <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/Z_mass.rds")
Z_v <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/Z_value.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/X.rds")


for(year in years){

  print(year)

  adjust <- ifelse(year %in% years_singular, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/", year, "_L_value.rds"))


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

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/losses/", year, "_L_value.rds"))

}

saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v2/losses/X.rds")
saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v2/losses/Y.rds")
saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v2/losses/Z_mass.rds")
saveRDS(Z_v, "/mnt/nfs_fineprint/tmp/fabio/v2/losses/Z_value.rds")

