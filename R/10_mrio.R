
library(Matrix)
library(parallel)
library(data.table)
library(readr)

source("R/00_system_variables.R")
agg <- function(x) { as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)) }


# MRIO Table ---

mr_sup_m <- readRDS(file.path(output_dir,"mr_sup_mass.rds"))
mr_sup_v <- readRDS(file.path(output_dir,"mr_sup_value.rds"))
mr_use <- readRDS(file.path(output_dir,"mr_use.rds"))

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

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/items_full_123.csv")
nrcom <- nrow(items)
Y <- readRDS(file.path(output_dir,"mr_use_fd.rds"))

# # Rebalance row sums for each year
# for(i in seq_along(Z_m)){
# 
#   X <- rowSums(Z_m[[i]]) + rowSums(Y[[i]])
# 
#   for(j in which(X < 0)){
#     reg <- j %/% nrcom + 1
#     # print(paste0(regions[reg, name], " / ", X[j]))
#     Y[[i]][j, paste0(regions[reg, code], "_balancing")] <-
#       Y[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
#   }
# }
# 
# 
# 
# # Combine processing into food -----------------------------------------
# for (i in seq_along(Y)) {
#   print(years[i])
#   
#   Y[[i]][, which(grepl("food", colnames(Y[[i]])))] <- 
#     Y[[i]][, which(grepl("food", colnames(Y[[i]])))] + Y[[i]][, which(grepl("processing", colnames(Y[[i]])))]
#   
#   # Remove processing columns
#   Y[[i]] <- Y[[i]][, -which(grepl("processing", colnames(Y[[i]]))), drop = FALSE]
# }



# # Define function for spreading balancing -----------------------------------------
# balancing_correction <- function(Y_food, Y_other, Y_unspec, Y_bal) {
#   # Convert all to triplet format for coordinate access
#   Y_food <- as(Y_food, "TsparseMatrix")
#   Y_other <- as(Y_other, "TsparseMatrix")
#   Y_unspec <- as(Y_unspec, "TsparseMatrix")
#   Y_bal <- as(Y_bal, "TsparseMatrix")
#   
#   # Combine all indices
#   idx <- unique(paste(Y_bal@i, Y_bal@j, sep = "_"))
#   
#   parse_idx <- function(x) {
#     matrix(as.integer(do.call(rbind, strsplit(x, "_"))), ncol = 2)
#   }
#   
#   coords <- parse_idx(idx)
#   i <- coords[, 1] + 1
#   j <- coords[, 2] + 1
#   
#   # Extract corresponding values or 0 if not present
#   get_val <- function(mat) {
#     mat_val <- Matrix::sparseMatrix(i = mat@i + 1, j = mat@j + 1, x = mat@x, dims = dim(mat))
#     mat_val[cbind(i, j)]
#   }
#   
#   f <- get_val(Y_food)
#   o <- get_val(Y_other)
#   u <- get_val(Y_unspec)
#   b <- get_val(Y_bal)
#   
#   total <- f + o + u
#   valid <- total > 0
#   
#   # Proportional redistribution
#   f_add <- numeric(length(b))
#   o_add <- numeric(length(b))
#   u_add <- numeric(length(b))
#   
#   f_add[valid] <- b[valid] * f[valid] / total[valid]
#   o_add[valid] <- b[valid] * o[valid] / total[valid]
#   u_add[valid] <- b[valid] * u[valid] / total[valid]
#   
#   # Fallback: if total == 0, add all to unspecified
#   u_add[!valid] <- u_add[!valid] + b[!valid]
#   
#   dims <- dim(Y_food)
#   food_update <- sparseMatrix(i = i, j = j, x = f_add, dims = dims, dimnames = dimnames(Y_food))
#   other_update <- sparseMatrix(i = i, j = j, x = o_add, dims = dims, dimnames = dimnames(Y_food))
#   unspec_update <- sparseMatrix(i = i, j = j, x = u_add, dims = dims, dimnames = dimnames(Y_food))
#   
#   list(food = food_update, other = other_update, unspecified = unspec_update)
# }
# 
# 
# i=1
# # Spread balancing over food and other use
# for (i in seq_along(Y)) {
#   print(years[i])
#   
#   before <- sum(Y[[i]])
#   
#   corrections <- balancing_correction(
#     Y_food = Y[[i]][, grepl("food", colnames(Y[[i]]))],
#     Y_other = Y[[i]][, grepl("other", colnames(Y[[i]]))],
#     Y_unspec = Y[[i]][, grepl("unspecified", colnames(Y[[i]]))],
#     Y_bal = Y[[i]][, grepl("balancing", colnames(Y[[i]]))]
#   )
#   
#   Y[[i]][, grepl("food", colnames(Y[[i]]))] <- Y[[i]][, grepl("food", colnames(Y[[i]]))] + corrections$food
#   Y[[i]][, grepl("other", colnames(Y[[i]]))] <- Y[[i]][, grepl("other", colnames(Y[[i]]))] + corrections$other
#   Y[[i]][, grepl("unspecified", colnames(Y[[i]]))] <- Y[[i]][, grepl("unspecified", colnames(Y[[i]]))] + corrections$unspecified
#   
#   # Remove balancing column
#   Y[[i]] <- Y[[i]][, !grepl("balancing", colnames(Y[[i]]))]
#   
#   after <- sum(Y[[i]])
#   if (!all.equal(before, after, tolerance = 1e-6)) {
#     stop(sprintf("Mass inconsistency at i = %d: before = %.0f, after = %.0f", i, before, after))
#   }
# }





# Derive total output X ---------------------------------------------

X <- mapply(function(x, y) {
  rowSums(x) + rowSums(y)
}, x = Z_m, y = Y)




# PROBLEM: There are some products with only zeros in the rows, except of the main diagonal
# i.e. the value on the main diagonal equals total output
# this is mainly due to reporting issues in FAOSTAT, where some countries report seed = production
# SOLUTION: We move 80% of the value to final demand, equally spreading over all fd-categories

fd_labels <- fread(file.path(output_dir,"fd_labels.csv"))
io_labels <- read_csv(file.path(output_dir,"io_labels.csv"))

# year <- 2019
for(year in years){
  
  print(year)
  
  Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi <- Y[[as.character(year)]]
  Xi <- X[,as.character(year)]
  
  # Assign column names
  colnames(Yi) <- fd_labels$fd
  
  # Precompute global totals
  Y_global <- t(agg(t(agg(Yi))))
  
  # Pre-identify relevant indices where update is needed
  diag_Zmi <- Matrix::diag(Zmi)
  valid <- (Xi != 0) & (diag_Zmi >= Xi)
  
  # Get area match matrix (cache once)
  area_match <- fd_labels$area_code
  
  for (i in which(valid)) {
    
    area_col <- which(area_match == io_labels$area_code[i])
    temp <- Yi[i, area_col]
    
    if (sum(temp) == 0) {
      temp <- Y_global[rownames(Y_global) == io_labels$comm_code[i], ]
    }
    
    if (sum(temp) > 0) {
      # Compute new Yi row for area
      bal <- mean(Zmi[i,i], Zvi[i,i]) * 0.8
      share <- temp / sum(temp)
      Yi[i, area_col] <- temp + bal * share
      
      # Update Z matrices
      Zmi[i, i] <- Zvi[i, i] <- mean(Zmi[i,i], Zvi[i,i]) * 0.2
      
      # Update X
      Xi[i] <- sum(Zmi[i, ]) + sum(Yi[i, ])
    }
  }
  
  # Save back results
  Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]] <- Yi
  X[,as.character(year)] <- Xi
}


# Store X, Y, Z variables
saveRDS(Z_m, file.path(output_dir,"Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir,"Z_value.rds"))
saveRDS(Y, file.path(output_dir,"Y.rds"))
saveRDS(X, file.path(output_dir,"X.rds"))




# create version of fabio with losses endogenized (on the main diagonal of Z) ---
# i.e. a version where losses are considered an own use of each sector instead of being a final demand category

for(year in years){
  
  print(year)
  
  # remove losses from Y
  Yi <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi <- Yi[, !grepl("losses", colnames(Yi))]
  
  Y[[as.character(year)]] <- Yi
  
  # reshape losses + balancing for adding them later to the main diagonals of each submatrix of Z
  ## Get the number of rows and columns in the data matrix
  num_rows <- nrow(losses)
  num_cols <- nrow(losses) / ncol(losses)

  ## Define a function for reshaping
  reshape_column <- function(v) {
    m <- matrix(0, ncol = num_cols, nrow = num_rows)
    indices <- ((seq_len(length(v)) - 1) %% num_cols) + 1
    m[cbind(seq_len(length(v)), indices)] <- v
    return(m)
  }

  ## Apply the reshape_column function to each column using lapply
  matrix_list <- lapply(1:ncol(losses), function(i) {
    v <- losses[, i]
    reshape_column(v)
  })

  ## Combine the matrices in the list using cbind()
  combined_matrix <- do.call(cbind, matrix_list)
  combined_matrix <- as(combined_matrix, "dgCMatrix")

  # add losses to the main diagonals of each submatrix of Z_m
  Zi <- Z_m[[as.character(year)]]
  Zi <- Zi + combined_matrix
  Z_m[[as.character(year)]] <- Zi

  # add losses to the main diagonals of each submatrix of Z_v
  Zi <- Z_v[[as.character(year)]]
  Zi <- Zi + combined_matrix
  Z_v[[as.character(year)]] <- Zi
  
}


# PROBLEM: There are some products with only zeros in the rows, except of the main diagonal
# i.e. the value on the main diagonal equals total output
# this is mainly due to reporting issues in FAOSTAT, where some countries report seed = production
# SOLUTION: We move 80% of the value to final demand, equally spreading over all fd-categories

fd_labels <- fread(file.path(output_dir,"losses/fd_labels.csv"))

# year <- 2019
for(year in years){
  
  print(year)
  
  Zmi <- Z_m[[as.character(year)]]
  Zvi <- Z_v[[as.character(year)]]
  Yi <- Y[[as.character(year)]]
  Xi <- X[,as.character(year)]
  
  # Assign column names
  colnames(Yi) <- fd_labels$fd
  
  # Precompute global totals
  Y_global <- t(agg(t(agg(Yi))))
  
  # Pre-identify relevant indices where update is needed
  diag_Zmi <- Matrix::diag(Zmi)
  valid <- (Xi != 0) & (diag_Zmi >= Xi)
  
  # Get area match matrix (cache once)
  area_match <- fd_labels$area_code
  
  for (i in which(valid)) {
    
    area_col <- which(area_match == io_labels$area_code[i])
    temp <- Yi[i, area_col]
    
    if (sum(temp) == 0) {
      temp <- Y_global[rownames(Y_global) == io_labels$comm_code[i], ]
    }
    
    if (sum(temp) > 0) {
      # Compute new Yi row for area
      bal <- mean(Zmi[i,i], Zvi[i,i]) * 0.8
      share <- temp / sum(temp)
      Yi[i, area_col] <- temp + bal * share
      
      # Update Z matrices
      Zmi[i, i] <- Zvi[i, i] <- mean(Zmi[i,i], Zvi[i,i]) * 0.2
      
      # Update X
      Xi[i] <- sum(Zmi[i, ]) + sum(Yi[i, ])
    }
  }
  
  # Save back results
  Z_m[[as.character(year)]] <- Zmi
  Z_v[[as.character(year)]] <- Zvi
  Y[[as.character(year)]] <- Yi
  X[,as.character(year)] <- Xi
  
}


saveRDS(X, file.path(output_dir,"losses/X.rds"))
saveRDS(Y, file.path(output_dir,"losses/Y.rds"))
saveRDS(Z_m, file.path(output_dir,"losses/Z_mass.rds"))
saveRDS(Z_v, file.path(output_dir,"losses/Z_value.rds"))




# allocate ghg emissions to products --------------------------------------------------------------
ghg <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_ghg.rds")
gwp <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_gwp.rds")
luh <- readRDS("/mnt/nfs_fineprint/tmp/fabio/ghg/E_luh2.rds")

ghg_names <- ghg[[1]][,1]
gwp_names <- gwp[[1]][,1]
luh_names <- luh[[1]][,1]

write_csv(data.frame(ghg_names), file.path(output_dir,"ghg_names.csv"))
write_csv(data.frame(gwp_names), file.path(output_dir,"gwp_names.csv"))
write_csv(data.frame(luh_names), file.path(output_dir,"luh_names.csv"))

# remove years not included in this version of FABIO
ghg <- ghg[as.character(years[years %in% as.integer(names(ghg))])]
gwp <- gwp[as.character(years[years %in% as.integer(names(gwp))])]
luh <- luh[as.character(years[years %in% as.integer(names(luh))])]

# remove countries not included in this version of FABIO
columns_to_keep <- substr(colnames(ghg[["2010"]]),1,3) %in% regions[current==TRUE, iso3c]
ghg <- lapply(ghg, function(x) x[, columns_to_keep])
gwp <- lapply(gwp, function(x) x[, columns_to_keep])
luh <- lapply(luh, function(x) x[, columns_to_keep])

nrreg <- length(unique(io_labels$area_code))
range <- rep(c(1:97,99:116,118:121), nrreg) + rep(((0:(nrreg-1))*121), each=119)

ghg_m <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = ghg, y = trans_m[seq_along(years[years %in% as.numeric(names(ghg))])])
gwp_m <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = gwp, y = trans_m[seq_along(years[years %in% as.numeric(names(ghg))])])
luh_m <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = luh, y = trans_m[seq_along(years[years %in% as.numeric(names(ghg))])])
ghg_v <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = ghg, y = trans_v[seq_along(years[years %in% as.numeric(names(ghg))])])
gwp_v <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = gwp, y = trans_v[seq_along(years[years %in% as.numeric(names(ghg))])])
luh_v <- mapply(function(x, y) { as.matrix(x[,range]) %*% y }, x = luh, y = trans_v[seq_along(years[years %in% as.numeric(names(ghg))])])

saveRDS(ghg_m, file.path(output_dir,"E_ghg_mass.rds"))
saveRDS(gwp_m, file.path(output_dir,"E_gwp_mass.rds"))
saveRDS(luh_m, file.path(output_dir,"E_luh_mass.rds"))

saveRDS(ghg_v, file.path(output_dir,"E_ghg_value.rds"))
saveRDS(gwp_v, file.path(output_dir,"E_gwp_value.rds"))
saveRDS(luh_v, file.path(output_dir,"E_luh_value.rds"))

