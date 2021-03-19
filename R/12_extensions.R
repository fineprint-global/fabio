
library(data.table)
library(tidyverse)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
nrreg <- nrow(regions[cbs==TRUE])
nrcom <- nrow(items)

X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/X.rds")
grassland_yields <- fread("input/grazing/grazing.csv")
water_crop <- fread("input/water/water_crop.csv")
water_fodder <- water_crop[water_item == "Fodder crops/Managed grass"]
water_fodder <- merge(regions[cbs==TRUE, .(area_code = code, area = name, water_code, water_area)],
  water_fodder[, .(water_code, water_area, water_item, value, water_type)],
  by = c("water_code", "water_area"), all.x = TRUE, allow.cartesian = TRUE)
water_fodder <- dcast(water_fodder, area_code + area ~ water_type, fun=sum)
water_lvst <- fread("input/water/water_lvst.csv")
water_pasture <- fread("input/water/water_pasture.csv")

# calculate crop water footprint
water_crop <- merge(regions[, .(area_code = code, area = name, water_code, water_area)],
  water_crop[, .(water_code, water_area, water_item, value, water_type)],
  by = c("water_code", "water_area"), all = TRUE, allow.cartesian = TRUE)

conc_water <- fread("inst/conc_water.csv")
conc <- match(water_crop$water_item, conc_water$water_item)
water_crop <- water_crop[, `:=`(fao_code = conc_water$fao_code[conc],
                                item_code = conc_water$item_code[conc],
                                item = conc_water$item[conc])]
crop <- readRDS("./data/tidy/crop_full.rds")
water_crop <- merge(crop[unit == "tonnes" & value > 0 & item_code %in% unique(water_crop$fao_code),
  .(area_code, fao_code = item_code, year, production = value)],
  water_crop[!is.na(fao_code),
  .(area_code, fao_code, item_code, item, water_type, intensity = value)],
  by = c("area_code", "fao_code"),
  all.x = TRUE, allow.cartesian = TRUE)
water_crop <- water_crop[, `:=`(value = production * intensity)]
water_crop[!area_code %in% regions[cbs==TRUE, code], `:=`(area_code = 999)]
water_crop <- water_crop[, list(value = na_sum(value)),
  by = .(area_code, item_code, item, year, water_type)]

# Calculate water footprint of meat processing
live <- readRDS("./data/tidy/live_tidy.rds")
meat <- live[element == "Production" & unit == "tonnes",
  .(area_code, area, year, item_code, item, value)]

src_item <- c(867, 947, 977, 1017, 1035, 1097, 1108, 1111, 1127, 1141, 1151, 1158, 1808)
tgt_item <- c(2731, 2731, 2732, 2732, 2733, 2735, 2735, 2735, 2735, 2735, 2735, 2735, 2734)
tgt_name <- c("Bovine Meat", "Bovine Meat", "Mutton & Goat", "Mutton & Goat",
              "Pigmeat", "Meat, Other", "Meat, Other", "Meat, Other", "Meat, Other",
              "Meat, Other", "Meat, Other", "Meat, Other", "Poultry Meat")
conc <- match(meat$item_code, src_item)
meat[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
meat <- meat[!is.na(item_code), ]
meat <- meat[, list(value = na_sum(value)),
  by = .(area_code, area, item_code, item, year)]
meat$blue <- water_lvst$blue[match(meat$item_code, water_lvst$item_code)]
meat[, `:=`(blue = blue * value, value = NULL)]

# Calculate water footprint of livestock
stocks <- live[element == "Stocks",
  .(area_code, area, year, item_code, item, value)]
stocks$blue <- water_lvst$blue[match(stocks$item_code, water_lvst$item_code)]
stocks[, `:=`(blue = blue * value, value = NULL)]

water_lvst <- rbind(meat, stocks)
rm(live, meat, stocks, src_item, tgt_item, tgt_name)

# read production data
sup <- readRDS("data/sup_final.rds")
crop <- readRDS("./data/tidy/crop_tidy.rds")
crop[!area_code %in% regions[cbs==TRUE, code], `:=`(area_code = 999, area = "ROW")]
crop <- crop[, list(value = na_sum(value)),
  by = .(area_code, area, element, year, unit, item_code, item)]

template <- data.table(area_code = rep(regions[cbs==TRUE, code], each = nrcom),
  area = rep(regions[cbs==TRUE, name], each = nrcom),
  item_code = rep(items$item_code, nrreg),
  item = rep(items$item, nrreg),
  comm_code = rep(items$comm_code, nrreg),
  comm_group = rep(items$comm_group, nrreg),
  group = rep(items$group, nrreg))


years <- 1986:2013

E <- lapply(years, function(x, y) {

  y_land <- y[element=="Area harvested" & year==x & item_code %in% items$item_code]
  y_biomass <- y[element=="Production" & year==x & item_code %in% items$item_code[items$group == "Primary crops"]]
  conc_land <- match(paste(template$area_code,template$item_code),paste(y_land$area_code,y_land$item_code))
  conc_biomass <- match(paste(template$area_code,template$item_code),paste(y_biomass$area_code,y_biomass$item_code))
  template[, landuse := y_land[, value][conc_land]]
  template[, biomass := y_biomass[, value][conc_biomass]]
  grass <- sup[year==x & item_code==2001]
  grass[is.na(production), production := 0]
  template[, grazing := grass$production[match(template$area_code, grass$area_code)]]
  template[item_code==2001, biomass := grazing]
  template[, grazing := grassland_yields$t_per_ha[match(template$area_code,grassland_yields$area_code)]]
  template[item_code==2001, landuse := biomass / grazing]
  template[, grazing := NULL]

  # add water footprints
  water <- water_lvst[water_lvst$year == x]
  template[, blue := water$blue[match(paste0(template$area_code, template$item_code),
    paste0(water$area_code, water$item_code))]]
  template[, green := as.numeric(water_pasture$value[match(template$area_code, water_pasture$area_code)]) * biomass]
  template[item_code != 2001, green := 0]
  template[, `:=`(fodder_blue = water_fodder$blue[match(template$area_code, water_fodder$area_code)],
                  fodder_green = water_fodder$green[match(template$area_code, water_fodder$area_code)])]
  template[item_code == 2000, `:=`(blue = fodder_blue * biomass, green = fodder_green * biomass)]
  template[, `:=`(fodder_blue = NULL, fodder_green = NULL)]
  water_blue <- water_crop[water_type == "blue" & year == x]
  water_green <- water_crop[water_type == "green" & year == x]
  conc_water <- match(paste(template$area_code, template$item_code),
    paste(water_blue$area_code, water_blue$item_code))
  template[, `:=`(crops_blue = water_blue$value[conc_water], crops_green = water_green$value[conc_water])]
  template[is.na(blue) | blue == 0, blue := crops_blue]
  template[is.na(green) | green == 0, green := crops_green]
  template[, `:=`(crops_blue = NULL, crops_green = NULL)]
  template[is.na(landuse), landuse := 0]
  template[is.na(biomass), biomass := 0]
  template[is.na(blue), blue := 0]
  template[is.na(green), green := 0]
  template[, `:=`(landuse = round(landuse), biomass = round(biomass),
    blue = round(blue), green = round(green))]

  # fill gaps in land use with global average yields
  yields <- template[, .(comm_code, landuse, biomass)] %>%
    group_by(comm_code) %>%
    summarize(yield = na_sum(biomass) / na_sum(landuse))
  template[, yield := yields$yield[match(template$comm_code, yields$comm_code)]]
  template[landuse == 0 & biomass > 0 & is.finite(yield), landuse := round(biomass / yield)]
  template[, yield := NULL]
  template[, output := X[,as.character(x)]]
  template[landuse>0 & output>0 & biomass==0, biomass := output]
  template[, output := NULL]

}, crop[, .(year, element, area_code, item_code, value)])

names(E) <- years

saveRDS(E, file="/mnt/nfs_fineprint/tmp/fabio/v2/E.rds")
