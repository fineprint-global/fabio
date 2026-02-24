
library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

# Extensions for FABIO v.2 ---------------------------
files_cbs <- list.files("data/extensions/cbs", pattern = "\\.rds$", full.names = TRUE)
data_cbs <- lapply(files_cbs, readRDS)

# Combine
E_cbs <- lapply(years, function(yr) {
  do.call(rbind, lapply(data_cbs, function(x) x[[as.character(yr)]]))
})

# set list names
names(E_cbs) <- years

# set rownames
for (yr in as.character(years)) {
  rownames(E_sua[[yr]]) <- row_names
}

# save
saveRDS(E_cbs, paste(output_dir, "E_new.rds"))

# Extensions for FABIO v.2_525 ---------------------------
files_sua <- list.files("data/extensions/sua", pattern = "\\.rds$", full.names = TRUE)
data_sua <- lapply(files_sua, readRDS)

# Combine
E_sua <- lapply(years, function(yr) {
  do.call(rbind, lapply(data_sua, function(x) x[[as.character(yr)]]))
})

# set list names
names(E_sua) <- years
row_names <- tools::file_path_sans_ext(basename(files_sua))

# set row names
for (yr in as.character(years)) {
  rownames(E_sua[[yr]]) <- row_names
}

#save
saveRDS(E_sua, "/mnt/nfs_fineprint/tmp/fabio/v2_525/E.rds")


# Check if label filenames are the same as in cbs and sua folders -> if not, print message

# if yes, compile to one .csv and save to output_dir




