library("data.table")
library("Matrix")


# Leontief inverse ---

prep_solve <- function(year, Z, Y, X,
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

  return(L_inv)
}


years <- seq(1986, 2013)
years_singular <- c(1986,1994,2002,2009)

Z_m <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/wood/Z_mass.rds")
Z_v <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/wood/Z_value.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/wood/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/wood/X.rds")


for(year in years){

  print(year)

  adjust <- ifelse(year %in% years_singular, TRUE, FALSE)

  L <- prep_solve(year = year, Z = Z_m[[as.character(year)]],
                  Y = Y[[as.character(year)]], X = X[, as.character(year)],
                  adj_diag = adjust)
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/wood/", year, "_L_mass.rds"))

  L <- prep_solve(year = year, Z = Z_v[[as.character(year)]],
                  Y = Y[[as.character(year)]], X = X[, as.character(year)],
                  adj_diag = adjust)
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v2/wood/", year, "_L_value.rds"))

}


