library(Matrix)

# Matrices necessary
sup <- readODS::read_ods("inst/fabio-exiobase.ods", sheet = 1, skip = 1)
use <- readODS::read_ods("inst/fabio-exiobase.ods", sheet = 2, skip = 1)
conc <- readODS::read_ods("inst/fabio-exiobase.ods", sheet = 3)

conc$FABIO_code <- 1:nrow(conc)
# Move NAs to extra column to drop, allocate at some point
conc$EXIOBASE_code[is.na(conc$EXIOBASE_code)] <- 50

as_matrix <- function(x) {
  y <- as.matrix(x[5:ncol(x)])
  y_rowSums <- rowSums(y, na.rm = TRUE)
  if(!all(x[["Total"]] == y_rowSums)) stop()
  y[is.na(y)] <- 0
  dimnames(y) <- NULL
  Matrix(y)
}
Sup <- as_matrix(sup)
Use <- as_matrix(use)
Cou_NA <- sparseMatrix(i = conc$FABIO_code, j = conc$EXIOBASE_code) * 1
Cou <- Cou_NA[, 1:49] # Remove 50th column of countries missing in EXIOBASE
Y_all <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/v2/wood/Y.rds"))

# Function to create the hybrid part for a certain year
hybridise <- function(year, Sup, Use, Cou, Y_all) {

  require(Matrix) # Necessary for forked processes

  # Read EXIOBASE Z and FABIO Y
  Y <- Y_all[[as.character(year)]]
  if(year<1995){
    load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/1995_Z.RData"))
  } else {
    load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/", year, "_Z.RData"))
  }

  # Calculate Tech matrices for the 49 EXIO countries
  Tec <- vector("list", 49)
  for(i in 1:49) {
    tmp <- Matrix(0, nrow = 200, ncol = 200)
    for(j in 1:49)
      tmp <- tmp + Z[(1 + 200 * (j - 1)):(200 * j),
                     (1 + 200 * (i - 1)):(200 * i)]
    Tec[[i]] <- tmp
  }

  # Get the columns of Y containing the other use category to allocate
  Oth <- Y[, grep("other$", colnames(Y))]
  rm(Y, Z)

  # Match FABIO countries with EXIOBASE countries and restructure the Other use matrix
  Oth <- Oth %*% Cou

  # Create matrix for sector matching
  T <- vector("list", 49)
  for(i in 1:49) {
    T[[i]] <- Sup %*% Tec[[i]] * Use
    T[[i]] <- T[[i]] / rowSums(T[[i]])
    T[[i]][is.na(T[[i]])] <- 0
  }

  # Compute the hybrid part from Oth and T
  B <- Matrix(0, nrow = 192 * 125, ncol = 49 * 200, sparse = TRUE)
  for(i in 1:49) {
    B[, (1 + 200 * (i - 1)):(200 * i)] <-
      do.call(rbind, replicate(192, T[[i]], simplify = FALSE)) * Oth[, i]
  }

  B
}


# Execute -----------------------------------------------------------------

# Setup to process in parallel
library(parallel)
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)

# Years to calculate hybridised FABIO for
years <- 1986:2013

output <- parLapply(cl, years, hybridise, Sup, Use, Cou, Y_all)

for(i in seq_along(output)) {
  saveRDS(output[[i]],
          paste0("/mnt/nfs_fineprint/tmp/fabio/v2/wood/hybrid/", years[[i]], "_B.rds"))
}

stopCluster(cl)


# Alternatively run a loop
for(year in 2013:1986){
  saveRDS(hybridise(year, Sup, Use, Cou, Y_all),
          paste0("/mnt/nfs_fineprint/tmp/fabio/v2/wood/hybrid/", year, "_B.rds"))
}
