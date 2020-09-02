
library(data.table)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
regions <- regions[cbs==TRUE]
nrreg <- nrow(regions)
nrcom <- nrow(items)

crop <- readRDS("./data/tidy/crop_tidy.rds")
crop[!area_code %in% regions$code, `:=`(area_code = 999, area = "ROW")]
crop <- crop[, list(value = na_sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]
grassland_yields <- read.csv("input/grazing/grazing.csv")
sup <- readRDS("data/sup_final.rds")

template <- data.table(area_code = rep(regions$code, each = nrcom),
                       area = rep(regions$name, each = nrcom),
                       item_code = rep(items$item_code, nrreg),
                       item = rep(items$item, nrreg),
                       comm_code = rep(items$comm_code, nrreg),
                       comm_group = rep(items$comm_group, nrreg),
                       group = rep(items$group, nrreg))

years <- 1986:2013

E <- lapply(years, function(x, y) {

  y_land <- y[element=="Area harvested" & year==x & item_code %in% items$item_code]
  y_biomass <- y[element=="Production" & year==x & item_code %in% items$item_code]
  conc_land <- match(paste(template$area_code,template$item_code),paste(y_land$area_code,y_land$item_code))
  conc_biomass <- match(paste(template$area_code,template$item_code),paste(y_biomass$area_code,y_biomass$item_code))
  template[, landuse := y_land[, value][conc_land]]
  template[, biomass := y[, value][conc_biomass]]
  grass <- sup[year==x & item_code==2001]
  template[, grazing := grass$production[match(template$area_code, grass$area_code)]]
  template[item_code==2001, biomass := grazing]
  template[, grazing := grassland_yields$t_per_ha[match(template$area_code,grassland_yields$area_code)]]
  template[item_code==2001, landuse := biomass / grazing]
  template[, grazing := NULL]

}, crop[, .(year, element, area_code, item_code, value)])

names(E) <- years

saveRDS(E, file="/mnt/nfs_fineprint/tmp/fabio/neu/E.rds")
