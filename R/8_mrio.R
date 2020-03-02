
library("data.table")
library("Matrix")
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

sup <- readRDS("data/sup_final.rds")

cbs <- readRDS("data/cbs_final.rds")
btd <- readRDS("data/btd_full.rds")

use <- readRDS("data/use_final.rds")
use_fd <- readRDS("data/use_fd_final.rds")

years <- seq(1986, 2013)
areas <- unique(cbs$area_code)
processes <- unique(use$proc_code)
commodities <- unique(use$comm_code)


# Supply ---

template <- data.table(expand.grid(
  proc_code = processes, comm_code = commodities, stringsAsFactors = FALSE))
setkey(template, proc_code, comm_code)

mr_sup_m <- lapply(years, function(x) {
  matrices <- lapply(areas, function(y, sup_y) {
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, production)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, production = 0)]
    } else {template[sup_x]}
    out <- tryCatch(dcast(out, proc_code ~ comm_code,
      value.var = "production", fun.aggregate = na_sum, fill = 0),
      error = function(e) {stop("Issue at ", x, "-", y, ": ", e)})
    Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
      dimnames = list(out$proc_code, colnames(out)[-1]))
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, production)])
  bdiag(matrices)
})

mr_sup_p <- lapply(years, function(x) {
  matrices <- lapply(areas, function(y, sup_y) {
    sup_x <- sup_y[area_code == y, .(proc_code, comm_code, price)]
    out <- if(nrow(sup_x) == 0) {
      template[, .(proc_code, comm_code, price = 0)]
    } else {template[sup_x]}
    out <- tryCatch(dcast(out, proc_code ~ comm_code,
      value.var = "price", fun.aggregate = na_sum, fill = 0),
      error = function(e) {stop("Issue at ", x, "-", y, ": ", e)})
    Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
      dimnames = list(out$proc_code, colnames(out)[-1]))
  }, sup_y = sup[year == x, .(area_code, proc_code, comm_code, price)])
  bdiag(matrices)
})

names(mr_sup_m) <- names(mr_sup_p) <- years

saveRDS(mr_sup_m, "data/mr_sup_m.rds")
saveRDS(mr_sup_p, "data/mr_sup_p.rds")


# CBS and BTD ---

template <- data.table(expand.grid(
  from_code = areas, to_code = areas,
  comm_code = commodities, stringsAsFactors = FALSE))
setkey(template, from_code, comm_code, to_code)


btd[, comm_code := items$comm_code[match(btd$item_code, items$item_code)]]

btd_cast <- lapply(years, function(x, btd_x) {
  out <- dcast(merge(template,
    btd_x[year == x, .(from_code, to_code, comm_code, value)],
    by = c("from_code", "to_code", "comm_code"), all.x = TRUE),
    from_code + comm_code ~ to_code,
    value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  Matrix(data.matrix(out[, c(-1, -2)]), sparse = TRUE,
    dimnames = list(paste0(out$from_code, "-", out$comm_code),
      colnames(out)[c(-1, -2)]))
}, btd_x = btd[, .(year, from_code, to_code, comm_code, value)])

cbs[, comm_code := items$comm_code[match(cbs$item_code, items$item_code)]]

cbs_cast <- lapply(years, function(x, cbs_x) {
  out <- dcast(merge(template[, .(area_code = from_code, comm_code)],
    cbs_x[year == x, .(area_code, comm_code, production)],
    by = c("area_code", "comm_code"), all.x = TRUE),
    area_code + comm_code ~ area_code,
    value.var = "production", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  Matrix(data.matrix(out[, c(-1, -2)]), sparse = TRUE,
    dimnames = list(paste0(out$area_code, "-", out$comm_code),
      colnames(out)[c(-1, -2)]))
}, cbs_x = cbs[, .(year, area_code, comm_code, production)])

total <- mapply(`+`, cbs_cast, btd_cast)
names(cbs_cast) <- names(btd_cast) <- names(total) <- years

comms <- gsub("(^[0-9]+)-(c[0-9]+)", "\\2", rownames(total[[1]]))
is <- as.numeric(vapply(unique(comms), function(x) {which(comms == x)},
  numeric(length(unique(areas)))))
js <- rep(seq_along(areas), each = length(unique(comms)))
agg <- Matrix::sparseMatrix(i = is, j = js)

total_shares <- lapply(total, function(x, agg, js) {
  x_agg <- crossprod(x, agg)
  # x_svd <- svd(x_agg)
  # x_svd$d <- pmax(x_svd$d, 1e-12)
  # x_pseudoinv <- x_svd$u %*% diag(1 / x_svd$d) %*% t(x_svd$v)
  # x %*% x_pseudoinv
  out <- as.matrix(x / x_agg[js, ])
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
}, agg = agg, js = js)


# Use ---

template <- data.table(expand.grid(
  area_code = areas, proc_code = processes, comm_code = commodities,
  stringsAsFactors = FALSE))
setkey(template, area_code, proc_code, comm_code)


use_cast <- lapply(years, function(x, use_x) {
  out <- dcast(merge(template[, .(area_code, proc_code, comm_code)],
    use_x[year == x, .(area_code, proc_code, comm_code, use)],
    by = c("area_code", "proc_code", "comm_code"), all.x = TRUE),
    comm_code ~ area_code + proc_code,
    value.var = "use", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  Matrix(data.matrix(out[, c(-1)]), sparse = TRUE,
    dimnames = list(out$comm_code, colnames(out)[-1]))
}, use_x = use[, .(year, area_code, proc_code, comm_code, use)])

mr_use <- mapply(function(x, y) {
  mr_x <- x[rep(seq_along(commodities), length(areas)), ]
  n_proc <- length(processes)
  for(j in seq_along(areas)) { # Could do this vectorised
    mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] <-
      mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] * y[, j]
  }
  return(mr_x)
}, use_cast, total_shares)

saveRDS(mr_use, "data/mr_use.rds")


# Use FD ---

template <- data.table(expand.grid(
  area_code = areas, comm_code = commodities,
  variable = c("food", "other", "stock_addition", "balancing"),
  stringsAsFactors = FALSE))
setkey(template, area_code, comm_code, variable)

use_fd[, comm_code := items$comm_code[match(use_fd$item_code, items$item_code)]]
use_fd <- melt(use_fd[, .(year, area_code, comm_code,
  food, other, stock_addition, balancing)],
  id.vars = c("year", "area_code", "comm_code"))

mr_use_fd <- lapply(years, function(x, use_fd_x) {
  out <- dcast(merge(template[, .(area_code, comm_code, variable)],
    use_fd_x[year == x, .(area_code, comm_code, variable, value)],
    by = c("area_code", "comm_code", "variable"), all.x = TRUE),
    comm_code ~ area_code + variable,
    value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0)
  Matrix(data.matrix(out[, -1]), sparse = TRUE,
    dimnames = list(out$comm_code, colnames(out)[-1]))
}, use_fd[, .(year, area_code, comm_code, variable, value)])

mr_use_fd <- mapply(function(x, y) {
  mr_x <- x[rep(seq_along(commodities), length(areas)), ]
  n_proc <- 4L
  for(j in seq_along(areas)) { # Could do this vectorised
    mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] <-
      mr_x[, seq(1 + (j - 1) * n_proc, j * n_proc)] * y[, j]
  }
  return(mr_x)
}, mr_use_fd, total_shares)

saveRDS(mr_use_fd, "data/mr_use_fd.rds")


# MRIO Table ---

# Mass
trans_m <- lapply(mr_sup_m, function(x) {
  out <- as.matrix(x / rowSums(x))
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
})

Z_m <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_m)

# Price
trans_p <- lapply(mr_sup_m, function(x) {
  out <- as.matrix(x / rowSums(x))
  out[!is.finite(out)] <- 0 # See Issue #75
  return(as(out, "Matrix"))
})

Z_p <- mapply(function(x, y) {
  x %*% y
}, x = mr_use, y = trans_p)
