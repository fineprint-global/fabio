library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

years <- seq(1986, 2013)

Z_m <- readRDS("/mnt/nfs_fineprint/tmp/fabio/neu/Z_mass.rds")
Z_v <- readRDS("/mnt/nfs_fineprint/tmp/fabio/neu/Z_value.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/neu/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/neu/X.rds")

fabio_inverse <- function(year){
  print(year)

  Z_m_y <- Z_m[[as.character(year)]]
  Z_v_y <- Z_v[[as.character(year)]]
  Y_y <- Y[[as.character(year)]]
  X_y <- X[[as.character(year)]]

  A <- t(t(Z_m_y) / X_y)
  A[!is.finite(A)] <- 0
  A[A < 0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10

  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-40)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/neu/", year, "_L_mass.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/FABIO data/neu/", year, "_L_mass.rds"))

  # invert Z_price
  A <- t(t(Z_v_y)/X)
  A[] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10

  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-40)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/neu/", year, "_L_price.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/FABIO data/neu/", year, "_L_value.rds"))
}



for(year in years){
  fabio_inverse(year=year)
}

