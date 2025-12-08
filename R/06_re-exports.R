
library("data.table")
library("Matrix")
library("tidyverse")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")


# Read data ---------------------------------------------------------------------

btd <- readRDS("data/btd_bal.rds")
cbs <- readRDS("data/cbs_full.rds")

areas <- fread("inst/regions_full.csv")[current==TRUE, code]
items <- fread("inst/items_full_123.csv")[,item_code]
n <- length(areas)


# Prepare reallocation of re-exports --------------------------------------
# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  item_code = rep(items, each = length(areas) ^ 2))

# Precompute area index
area_index <- setNames(seq_along(areas), areas)

# Fill this structure per year btd values
# Then do re-export reallocation via the Leontief inverse for each item
btd_final <- vector("list", length(years))
sup_shares <- vector("list", length(years))
names(btd_final) <- names(sup_shares) <- years

for(i in seq_along(years)) {
  y <- years[i]
  cat("Calculating year ", y, ".\n", sep = "")
  
  # Add BTD values to the template
  mapping <- merge(mapping_templ,
                   btd[year == y, c("from_code", "to_code", "item_code", "value")],
                   by = c("from_code", "to_code", "item_code"), all.x = TRUE)
  
  # Eliminate NA values
  mapping[is.na(value), value := 0]
  
  # Restructure in a list with matrices per item
  mapping_reex <- lapply(
    split(mapping, by = "item_code", keep.by = FALSE),
    function(x) {
      out <- data.table::dcast(x, from_code ~ to_code,
                               fun.aggregate = sum, value.var = "value")[, -"from_code"]
      as(out, "Matrix")})
  
  mapping_shares <- copy(mapping_reex)
  
  # Slice cbs once for the year
  cbs_slice <- cbs[year == y, .(area_code, item_code, domestic_supply, domestic_use, supply)]
  setkey(cbs_slice, item_code, area_code)
  
  # Run re-export reallocation per item
  for(j in as.character(items)) {
    data_item <- cbs_slice[.(as.integer(j), areas)]
    
    # domestic supply vector (same order as 'areas' because `data` was merged by area_code)
    DS <- pmax(0, data_item$domestic_supply)
    DS[is.na(DS)] <- 0
    
    # total supply vector (same order as 'areas' because `data` was merged by area_code)
    TS <- pmax(0, data_item$supply)
    TS[is.na(TS)] <- 0
    
    # domestic use vector (same order as 'areas' because `data` was merged by area_code)
    DU <- pmax(0, data_item$domestic_use)
    DU[is.na(DU)] <- 0
    
    # bilateral trade matrix
    mat <- mapping_reex[[j]]
    
    # convert into CsparseMatrix
    T <- as(mat, "CsparseMatrix")

    # n <- length(areas)
    
    if (na_sum(mat) == 0) { final_result <- T + Diagonal(x = DU)
    } else {
      
      A <- sweep(T, 1, TS, FUN = "/")
      A[is.na(A)] <- 0

      # Solve linear system: X = (I - A)^(-1) %*% DS
      I <- Diagonal(n)
      # Try regular solve, fallback to generalized inverse if singular
      L <- tryCatch(solve(I - A),
                    error = function(e) MASS::ginv(as.matrix(I - A)))

      # Allocate exports proportionally to total domestic use
      F <- L * DS
      F[F < 0] <- 0
      
      # Final bilateral flows: re-scale X to domestic supply
      col_sums <- colSums(F)
      S <- t(t(F) / col_sums)
      final_result <- t(t(S) * DU)
      
      # final numeric matrix (rounded), keep sparse
      final_result <- round(as.matrix(final_result))
      final_result <- Matrix::Matrix(final_result, sparse = TRUE)
      S <- Matrix::Matrix(S, sparse = TRUE)
    }
    
    mapping_reex[[j]] <- final_result
    mapping_shares[[j]] <- S
    
  }
  
  btd_final[[i]] <- mapping_reex
  sup_shares[[i]] <- mapping_shares
  
}

saveRDS(sup_shares, "data/sup_shares_list.rds")


# melt all btd matrices into one data.table
btd_final <- lapply(names(btd_final), function(y) {
  lst <- btd_final[[y]]
  lapply(names(lst), function(name) {
    out <- lst[[name]]
    colnames(out) <- areas
    out <- data.table(from_code = areas, as.matrix(out))
    out <- melt(out, id.vars = "from_code",
                variable.name = "to_code", variable.factor = FALSE)
    out[, year := as.integer(y)]           # use the outer list name
    out[, item_code := as.integer(name)]   # add item_code
    out[, to_code := as.integer(to_code)]
    out
  }) |> rbindlist()
}) |> rbindlist()


# melt all supply share matrices into one data.table
sup_shares <- lapply(names(sup_shares), function(y) {
  lst <- sup_shares[[y]]
  lapply(names(lst), function(name) {
    out <- lst[[name]]
    colnames(out) <- areas
    out <- data.table(from_code = areas, as.matrix(out))
    out <- melt(out, id.vars = "from_code",
                variable.name = "to_code", variable.factor = FALSE)
    out[, year := as.integer(y)]           # use the outer list name
    out[, item_code := as.integer(name)]   # add item_code
    out[, to_code := as.integer(to_code)]
    out
  }) |> rbindlist()
}) |> rbindlist()


# Remove negative values
btd_final[, value := pmax(0, value)]
sup_shares[, value := pmax(0, value)]
# Add commodity codes
items <- fread("inst/items_full_123.csv")
btd_final[, comm_code := items$comm_code[match(btd_final$item_code, items$item_code)]]
sup_shares[, comm_code := items$comm_code[match(sup_shares$item_code, items$item_code)]]


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_final, "data/btd_final.rds")
saveRDS(sup_shares, "data/sup_shares.rds")

