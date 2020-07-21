
library("Matrix")

years <- seq(1986, 2013)

Z_m <- readRDS("data/Z_mass.rds")
Z_v <- readRDS("data/Z_value.rds")
Y <- readRDS("data/mr_use_fd.rds")

names(Z_m) <- names(Z_v) <- names(Y) <- years # Todo: Do this during creation

prep_solve <- function(year,
  Z, Y,
  adj_X = FALSE, adj_A = TRUE, adj_diag = FALSE) {

  X <- rowSums(Z) + rowSums(Y)
  if(adj_X) {X <- X + 1e-10}

  A <- Matrix(0, nrow(Z), ncol(Z))
  idx <- X != 0
  A[, idx] <- t(t(Z[, idx]) / X[idx])
  if(adj_A) {A[A < 0] <- 0}

  if(adj_diag) {diag(A)[diag(A) == 1] <- 1 - 1e-10}
  L <- .sparseDiagonal(nrow(A)) - A

  lu(L) # Computes LU decomposition and stores it in L

  return(L)
}

L <- prep_solve(year = 2000, Z_m, Y)

E <- readRDS("data/E.rds") # Dummy, replace

footprint_lu <- function(L, E, Y,
  adj_F = TRUE
  tol = .Machine[["double_.eps"]]) {

  if(ncol(Y) == 1 || is.vector(Y)) {
    out <- E %*% solve(L, .sparseDiagonal(Y), tol = tol)
  } else {
    out <- apply(Y, 2, function(y) {
      E %*% solve(L, .sparseDiagonal(y), tol = tol)
    })
  }

  if(adj_F) {F[F < 0] <- 0}

  return(out)
}

F <- footprint_lu(L, E, Y)
