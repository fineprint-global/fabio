library("data.table")
library("Matrix")


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


years <- seq(1986, 2020)
years_singular <- years #c(1987, 1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2011)
# years_singular_losses <- c(1990, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2006, 2007, 2010, 2011, 2019)

Z <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Z.rds")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/Y.rds")
X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/X.rds")


#year <- 2020
for(year in years){

  skip_to_next <- FALSE

  tryCatch({

    adjust <- ifelse(year %in% years_singular, TRUE, FALSE)

    L <- prep_solve(year = year, Z = Z[[as.character(year)]],
                  X = X[, as.character(year)], adj_diag = adjust) #, adj_X = adjust)

    L[L<0] <- 0

    saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/", year, "_L.rds"))

  }, error=function(e){skip_to_next <<- TRUE})

  if(!skip_to_next){
    cat(paste0("Sucessfully inverted matrix for ",year,".\n"))
  } else { next }
}



# # create the losses version of fabio ---
#
# #year <- 2019
# for(year in years){
#
#   print(year)
#   # add losses at the main diagonal of Z and remove from Y
#   Yi <- Y[[as.character(year)]]
#   losses <- rowSums(as.matrix(Yi[, grepl("losses", colnames(Yi))]))
#   Yi[, grepl("losses", colnames(Yi))] <- 0
#   Y[[as.character(year)]] <- Yi
#   Zi <- Z[[as.character(year)]]
#   diag(Zi) <- diag(Zi) + losses
#   Z[[as.character(year)]] <- Zi
# }
#
# saveRDS(X, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/losses/X.rds")
# saveRDS(Y, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/losses/Y.rds")
# saveRDS(Z_m, "/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/losses/Z.rds")
#
#
#
# #year <- 2019
# for(year in years){
#
#   print(year)
#
#   adjust_losses <- ifelse(year %in% years_singular_losses, TRUE, FALSE)
#
#   L <- prep_solve(year = year, Z = Z[[as.character(year)]],
#                   X = X[, as.character(year)], adj_diag = adjust_losses)
#   L[L<0] <- 0
#   saveRDS(L, paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/cal/losses/", year, "_L.rds"))
#
# }

