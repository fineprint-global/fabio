
library(data.table)
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")
regions <- regions[regions$cbs]
nrreg <- nrow(regions)
nrcom <- nrow(items)

crop <- readRDS("./data/tidy/crop_tidy.rds")
crop[!area_code %in% regions$code, `:=`(area_code = 999, area = "ROW")]
crop <- crop[, list(value = na_sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]

template <- data.table(area_code = rep(regions$code, each = nrcom),
                       area = rep(regions$name, each = nrcom),
                       item_code = rep(items$item_code, nrreg),
                       item = rep(items$item, nrreg),
                       comm_code = rep(items$comm_code, nrreg),
                       comm_group = rep(items$comm_group, nrreg),
                       group = rep(items$group, nrreg))

years <- 1986:2013

E <- lapply(years, function(x, y) {

  y <- y[year==x & item_code %in% items$item_code]
  conc <- match(paste(template$area_code,template$item_code),paste(y$area_code,y$item_code))
  template[, landuse := y[element=="Area harvested", value][conc]]
  template[, biomass := y[element=="Production", value][conc]]

}, crop[, .(year, element, area_code, item_code, value)])

names(E) <- years

saveRDS(E, file="/mnt/nfs_fineprint/tmp/fabio/neu/E.rds")
saveRDS(E, file="~/wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/FABIO data/neu/E.rds")
