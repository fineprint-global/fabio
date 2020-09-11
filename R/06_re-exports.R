
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")


years <- 1986:2013


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/btd_bal.rds")
cbs <- readRDS("data/cbs_full.rds")

areas <- sort(unique(cbs$area_code))
items <- unique(cbs$item_code)



# Prepare reallocation of re-exports --------------------------------------

cbs[, dom_use := na_sum(feed, food, losses, other, processing, seed, stock_addition, balancing, unspecified)]
cbs[, total_use := na_sum(dom_use, exports)]

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

  # Run iterative proportional fitting per item
  for(j in as.character(items)) {
    data <- merge(data.table(area_code = areas),
                  cbs[year==y & item_code==as.integer(j),
                      .(area_code, production, dom_use, total_use, dom_share = production / total_use)],
                  by = "area_code", all = TRUE)
    data[is.na(dom_use), dom_use := 0]
    data[is.na(total_use), total_use := 0]
    data[is.na(dom_share), dom_share := 0]

    denom <- data$total_use
    denom[denom == 0] <- 1
    mat <- mapping_reex[[j]]
    # catch problem with one data point in year 2002
    # trade with 1107 (Asses) from 250 (Dem. rep. Congo) to 251 (Zambia)
    # reduce value from 30 to 20
    if(y==2002 & j=="1107") mat[185,184] <- mat[185,184]/3*2
    mat <- t(t(mat) / denom)
    mat <- diag(nrow(mat)) - mat
    mat <- solve(mat)
    mat <- mat * data$dom_share
    mat <- t(t(mat) * data$dom_use)

    mapping_reex[[j]] <- mat
  }

  btd_final[[i]] <- lapply(names(mapping_reex), function(name) {
    out <- mapping_reex[[name]]
    colnames(out) <- areas
    out <- data.table(from_code = areas, as.matrix(out))
    out <- melt(out, id.vars = c("from_code"), variable.name = "to_code", variable.factor = FALSE)
    out[, .(year = y, item_code = as.integer(name),
            from_code = as.integer(from_code), to_code = as.integer(to_code), value)]
  })

  cat("Calculated year ", y, ".\n", sep = "")
}

# One datatable per year
btd_final <- lapply(btd_final, rbindlist)
# One datatable
btd_final <- rbindlist(btd_final)
# Add commodity codes
items <- fread("inst/items_full.csv")
btd_final[, comm_code := items$comm_code[match(btd_final$item_code, items$item_code)]]


# Store the balanced sheets -----------------------------------------------
saveRDS(btd_final, "data/btd_final.rds")
