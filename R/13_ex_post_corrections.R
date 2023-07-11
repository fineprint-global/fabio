
library(data.table)
library(tidyverse)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
io <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")
su <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/su_codes.csv")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
Zm <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_mass.rds")
Zv <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Z_value.rds")

