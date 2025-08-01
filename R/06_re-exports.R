
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
# Split stock changes into
# - positive values (stock additions)  --> part of domestic use
# - negative values (stock withdrawals) --> part of domestic supply
cbs[, `:=`(stock_positive = ifelse(stock_addition > 0, stock_addition, 0),
           stock_negative = ifelse(stock_addition < 0, -stock_addition, 0))]
# negative stock additions previously decreased use
cbs[, `:=`(supply = na_sum(production, imports, stock_negative),
           use = na_sum(production, imports, stock_negative, -exports))]

# Create a structure to map importers to exporters per item (+ targets)
mapping_templ <- data.table(
  from_code = rep(areas, each = length(areas), times = length(items)),
  to_code = rep(areas, times = length(areas) * length(items)),
  item_code = rep(items, each = length(areas) ^ 2))

# Fill this structure per year btd values
# Then do re-export reallocation via the Leontief inverse for each item
# Note that we loop this over years, so memory requirements can easily be
# reduced if necessary.
btd_final <- vector("list", length(years))
names(btd_final) <- years

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

  # Run re-export reallocation per item
  for(j in as.character(items)) {
    data <- merge(data.table(area_code = areas),
                  cbs[year==y & item_code==as.integer(j),
                      .(area_code, production, supply, use)],
                  by = "area_code", all.x = TRUE)
    data[is.na(data)] <- 0

    mat <- mapping_reex[[j]]
    A <- sweep(mat, 2, data$supply, FUN = "/")
    A[is.na(A)] <- 0
    I <- diag(n)
    
    # catch a problem with item 2593 in 2018-2020 and item 1157 in 2022 (matrix seems to be close to singular)
    if((y %in% 2018:2020 & j=="2593")|(y %in% 2022 & j=="1157")) {
      L <- MASS::ginv(as.matrix(I - A))
      L[L < 0] <- 0
    } else L <- solve(I - A)
    
    x <- L %*% data$use
    
    # Share of country k's production used to satisfy final demand in country l
    result <- matrix(0, n, n)
    for (k in 1:n) {
      for (l in 1:n) {
        result[k, l] <- ifelse(x[k] > 0, L[k, l] * data$production[k] / x[k], 0)
      }
    }
    
    # Multiply by final demand vector to get final flows
    final_result <- round(result * matrix(rep(data$use, each = n), nrow = n))

    mapping_reex[[j]] <- final_result
  }

  btd_final[[i]] <- lapply(names(mapping_reex), function(name) {
    out <- mapping_reex[[name]]
    colnames(out) <- areas
    out <- data.table(from_code = areas, as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
    out[, .(year = y, item_code = as.integer(name),
            from_code = as.integer(from_code), to_code = as.integer(to_code), value)]
  })
}

# One datatable per year
btd_final <- lapply(btd_final, rbindlist)
# One datatable
btd_final <- rbindlist(btd_final)
# Add commodity codes
items <- fread("inst/items_full_123.csv")
btd_final[, comm_code := items$comm_code[match(btd_final$item_code, items$item_code)]]


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_final, "data/btd_final.rds")
