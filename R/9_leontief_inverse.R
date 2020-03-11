library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

years <- seq(1986, 2013)

Z_m <- readRDS("data/Z_mass.rds")
Z_v <- readRDS("data/Z_value.rds")
Y <- readRDS("data/mr_use_fd.rds")

names(Z_m) <- names(Z_v) <- names(Y) <- years

fabio_inverse <- function(year){
  print(year)

  Z_m_y <- Z_m[[as.character(year)]]
  Z_v_y <- Z_v[[as.character(year)]]
  Y_y <- Y[[as.character(year)]]
  X <- rowSums(Z_m_y) + rowSums(Y_y)

  A <- t(t(Z_m_y)/X)
  A[!is.finite(A)] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10

  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-40)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_mass.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/", year, "_L_mass.rds"))

  # invert Z_price
  A <- t(t(Z_v_y)/X)
  A[] <- 0
  A[A<0] <- 0
  diag(A)[diag(A)==1] <- 1 - 1e-10

  L <- diag(nrow(A))-A
  L <- solve(L, tol = 1.0e-40)
  L[L<0] <- 0
  saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/", year, "_L_price.rds"))
  # saveRDS(L, paste0("../wu_share/WU/Projekte/GRU/04_Daten/", year, "_L_price.rds"))
}

for(year in years){
  fabio_inverse(year=year)
}

