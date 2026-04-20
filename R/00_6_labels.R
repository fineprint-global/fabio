
library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

# Read regions
regions_full <- fread("inst/regions_full.csv")
regions <- regions_full[current==TRUE, .(iso3c, area_code = code, area = name,
                                         continent, region, EU27)]
fwrite(regions, "inst/regions_tidy.csv")
nrreg <- nrow(regions)


# set vector for creating cbs and sua level labels
loop <- c("cbs", "sua")

for(i in loop){
  
  if(i == "cbs"){
    items <- fread("inst/items_full_123.csv")
    processes <- fread("inst/items_use.csv")
  } else{
    items <- fread("inst/sua/items_sua.csv")
    processes <- fread("inst/sua/proc_sua.csv")
  }
  processes <- unique(processes[,1:2])
  setorder(processes, proc_code)
  nrcom <- nrow(items)
  nrproc <- nrow(processes)
  fd <- c("food", "losses", "other", "stock_addition", "tourist")
  nrfd <- length(fd)

  io_labels <- data.table(
    iso3c = rep(regions$iso3c, each = nrcom),
    area_code = rep(regions$area_code, each = nrcom),
    area = rep(regions$area, each = nrcom),
    continent = rep(regions$continent, each = nrcom),
    comm_code = rep(items$comm_code, nrreg),
    item_code = rep(items$item_code, nrreg),
    item = rep(items$item, nrreg),
    unit = rep(items$unit, nrreg),
    comm_group = rep(items$comm_group, nrreg),
    group = rep(items$group, nrreg))
  
  su_labels <- data.table(
    iso3c = rep(regions$iso3c, each = nrproc),
    area_code = rep(regions$area_code, each = nrproc),
    area = rep(regions$area, each = nrproc),
    continent = rep(regions$continent, each = nrproc),
    proc_code = rep(processes$proc_code, nrreg),
    proc = rep(processes$proc, nrreg))
  
  fd_labels <- data.table(
    iso3c = rep(regions$iso3c, each = nrfd),
    area_code = rep(regions$area_code, each = nrfd),
    area = rep(regions$area, each = nrfd),
    continent = rep(regions$continent, each = nrfd),
    fd = rep(fd, nrreg))

  if(i == "cbs"){
    fwrite(io_labels, file=file.path(output_dir,"io_labels.csv"))
    fwrite(su_labels, file=file.path(output_dir,"su_labels.csv"))
    fwrite(fd_labels, file=file.path(output_dir,"fd_labels.csv"))
    fwrite(fd_labels[!fd %in% c("losses")], file=file.path(output_dir,"losses/fd_labels.csv"))
    fwrite(items[, .(comm_code, item_code, item, unit, group, comm_group)],
           file=file.path(output_dir,"items.csv"))
    fwrite(regions, file=file.path(output_dir,"regions.csv"))
  } else {
    fwrite(io_labels, file=file.path(output_dir_v525,"io_labels.csv"))
    fwrite(su_labels, file=file.path(output_dir_v525,"su_labels.csv"))
    fwrite(fd_labels, file=file.path(output_dir_v525,"fd_labels.csv"))
    fwrite(fd_labels[!fd %in% c("losses")], 
           file=file.path(output_dir_v525,"losses/fd_labels.csv"))
    fwrite(items[, .(comm_code, item_code, item, unit, group, comm_group)],
           file=file.path(output_dir_v525,"items.csv"))
    fwrite(regions, file=file.path(output_dir_v525,"regions.csv"))
  }
}




