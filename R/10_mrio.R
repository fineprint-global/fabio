
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
items <- fread("inst/items_full.csv")
nrcom <- nrow(items)
Y <- readRDS(file.path(output_dir,"mr_use_fd.rds"))

# Rebalance row sums for each year
for(i in seq_along(Z_m)){

  X <- rowSums(Z_m[[i]]) + rowSums(Y[[i]])

  for(j in which(X < 0)){
    reg <- j %/% nrcom + 1
    # print(paste0(regions[reg, name], " / ", X[j]))
    Y[[i]][j, paste0(regions[reg, code], "_balancing")] <-
      Y[[i]][j, paste0(regions[reg, code], "_balancing")] - X[j]
  }

}

# Integrate balancing and residuals, food and processing
for(i in seq_along(Y)){
  
  Y[[i]][, grepl("balancing", colnames(Y[[i]]))] <-
    Y[[i]][, grepl("balancing", colnames(Y[[i]]))] +
    Y[[i]][, grepl("residuals", colnames(Y[[i]]))]
  
  Y[[i]][, grepl("food", colnames(Y[[i]]))] <-
    Y[[i]][, grepl("food", colnames(Y[[i]]))] +
    Y[[i]][, grepl("processing", colnames(Y[[i]]))]
  
  Y[[i]] <- Y[[i]][, !grepl("residuals", colnames(Y[[i]]))]
  Y[[i]] <- Y[[i]][, !grepl("processing", colnames(Y[[i]]))]
  
}


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
  
  colnames(Yi) <- fd_labels$fd
  Y_global <- t(agg(t(agg(Yi))))
  
  for(i in 1:nrow(io_labels)){
    if(Xi[i]!=0 & Zmi[i,i] >= Xi[i]) { 
      # print(paste0(io_labels[i,]))
      # print(as.numeric(mean(Zmi[i,i], Zvi[i,i])))
      temp <- Yi[i, fd_labels$area_code==io_labels$area_code[i]]
      if(sum(temp)==0){ temp <- Y_global[rownames(Y_global)==io_labels$comm_code[i],] }
      Yi[i, fd_labels$area_code==io_labels$area_code[i]] <- temp + mean(Zmi[i,i], Zvi[i,i]) * 0.8 / sum(temp) * temp
      Zmi[i,i] <- Zvi[i,i] <- mean(Zmi[i,i], Zvi[i,i]) * 0.2
      Xi[i] <- sum(Zmi[i,]) + sum(Yi[i,])
    }
  }
  
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




# # redistribute balancing over all uses proportionally ---------------------------------------------
# regions <- fread("inst/regions_full.csv")[current==TRUE]
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
# saveRDS(Z_m, file.path(output_dir,"Z_mass_b.rds"))
# saveRDS(Z_v, file.path(output_dir,"Z_value_b.rds"))
# saveRDS(Y, file.path(output_dir,"Y_b.rds"))




# create the losses version of fabio ---

for(year in years){
  
  print(year)
  
  # remove losses from Y
  Yi <- Y[[as.character(year)]]
  losses <- as.matrix(Yi[, grepl("losses", colnames(Yi))])
  Yi <- Yi[, !grepl("losses", colnames(Yi))]
  
  # # remove balancing from Y
  # balancing <- as.matrix(Yi[, grepl("balancing", colnames(Yi))])
  # Yi <- Yi[, !grepl("balancing", colnames(Yi))]
  
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
    v <- losses[, i] # + balancing[, i]
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
  
  colnames(Yi) <- fd_labels$fd
  Y_global <- t(agg(t(agg(Yi))))
  
  for(i in 1:nrow(io_labels)){
    if(Xi[i]!=0 & Zmi[i,i] >= Xi[i]) { 
      # print(paste0(io_labels[i,]))
      # print(as.numeric(mean(Zmi[i,i], Zvi[i,i])))
      temp <- Yi[i, fd_labels$area_code==io_labels$area_code[i]]
      if(sum(temp)==0){ temp <- Y_global[rownames(Y_global)==io_labels$comm_code[i],] }
      Yi[i, fd_labels$area_code==io_labels$area_code[i]] <- temp + mean(Zmi[i,i], Zvi[i,i]) * 0.8 / sum(temp) * temp
      Zmi[i,i] <- Zvi[i,i] <- mean(Zmi[i,i], Zvi[i,i]) * 0.2
      Xi[i] <- sum(Zmi[i,]) + sum(Yi[i,])
    }
  }
  
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
# Note: this part needs to be updated in order to fit the new country classification
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

