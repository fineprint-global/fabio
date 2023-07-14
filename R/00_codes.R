
library(data.table)
library(tidyverse)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
processes <- fread("inst/items_use.csv")
processes <- unique(processes[,1:2])
nrreg <- nrow(regions[cbs==TRUE])
nrcom <- nrow(items)
nrproc <- nrow(processes)
fd <- c("balancing", "food", "losses", "other", "processing", "residuals", "stock_addition", "tourist", "unspecified")
nrfd <- length(fd)


io_codes <- data.table(area_code = rep(regions[cbs==TRUE, code], each = nrcom),
  area = rep(regions[cbs==TRUE, name], each = nrcom),
  item_code = rep(items$item_code, nrreg),
  item = rep(items$item, nrreg),
  comm_code = rep(items$comm_code, nrreg),
  comm_group = rep(items$comm_group, nrreg),
  group = rep(items$group, nrreg))

su_codes <- data.table(area_code = rep(regions[cbs==TRUE, code], each = nrproc),
  area = rep(regions[cbs==TRUE, name], each = nrproc),
  proc_code = rep(processes$proc_code, nrreg),
  proc = rep(processes$proc, nrreg))

fd_codes <- data.table(area_code = rep(regions[cbs==TRUE, code], each = nrfd),
  area = rep(regions[cbs==TRUE, name], each = nrfd),
  fd = rep(fd, nrreg))

fwrite(io_codes, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")
fwrite(su_codes, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/su_codes.csv")
fwrite(fd_codes, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/fd_codes.csv")
fwrite(items[, .(comm_code, item_code, item, comm_group, unit, moisture)],
       file="/mnt/nfs_fineprint/tmp/fabio/v1.2/items.csv")
fwrite(regions[cbs==TRUE, .(iso3c, area_code = code, area = name, region, EU27)],
       file="/mnt/nfs_fineprint/tmp/fabio/v1.2/regions.csv")
