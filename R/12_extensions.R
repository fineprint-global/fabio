
library(data.table)
library(tidyverse)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
nrreg <- nrow(regions[cbs==TRUE])
nrcom <- nrow(items)

X <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/X.rds")
grassland_yields <- fread("input/grazing/grazing.csv")
water_crop <- fread("input/water/water_crop.csv")
water_fodder <- water_crop[water_item == "Fodder crops/Managed grass"]
water_fodder <- merge(regions[cbs==TRUE, .(area_code = code, area = name, water_code, water_area)],
  water_fodder[, .(water_code, water_area, water_item, value, water_type)],
  by = c("water_code", "water_area"), all.x = TRUE, allow.cartesian = TRUE)
water_fodder <- dcast(water_fodder, area_code + area ~ water_type, fun=sum)
water_lvst <- fread("input/water/water_lvst.csv")
water_pasture <- grassland_yields %>% select(area_code, area, iso3c, continent, m3_per_ha)

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

# prepare N extension
N <- read_csv("./input/extensions/N_kg_per_ha.csv")
N$region <- regions$region[match(N$iso3c, regions$iso3c)]
N <- merge(regions[cbs==TRUE,.(iso3c,area_code = code)], N, by = "iso3c", all = TRUE)
N <- gather(N, key = "com", value = "value", -region, -iso3c, -area_code)
avg_N <- N %>%
  group_by(region, com) %>%
  summarise(avg = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(region)) %>%
  group_by(com) %>%
  bind_rows(summarise(., avg = mean(avg, na.rm = TRUE), region = NA))
  # bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(., na.rm = TRUE) else "Global"))
N <- merge(N, avg_N, by = c("region", "com"), all.x = TRUE)
N$value[is.na(N$value)] <- ifelse(is.na(N$avg[is.na(N$value)]), NA, N$avg[is.na(N$value)])
N <- N[, c("area_code", "iso3c", "com", "value")]
N$area_code[N$area_code==62] <- 238  # Ethiopia
N$area_code[N$area_code==206] <- 276  # Sudan
N <- N %>% arrange(across(c(area_code, com)))
items_conc <- read_csv("./inst/items_conc.csv")
N$com <- items_conc$com_1.2[match(N$com, items_conc$com_1.1)]
N <- N[!is.na(N$com) & !is.na(N$area_code),]

# prepare P extension
P <- read_csv("./input/extensions/P_kg_per_ha.csv")
P$region <- regions$region[match(P$iso3c, regions$iso3c)]
P <- merge(regions[cbs==TRUE,.(iso3c,area_code = code)], P, by = "iso3c", all = TRUE)
P <- gather(P, key = "com", value = "value", -region, -iso3c, -area_code)
avg_P <- P %>%
  group_by(region, com) %>%
  summarise(avg = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(region)) %>%
  group_by(com) %>%
  bind_rows(summarise(., avg = mean(avg, na.rm = TRUE), region = NA))
# bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(., na.rm = TRUE) else "Global"))
P <- merge(P, avg_P, by = c("region", "com"), all.x = TRUE)
P$value[is.na(P$value)] <- ifelse(is.na(P$avg[is.na(P$value)]), NA, P$avg[is.na(P$value)])
P <- P[, c("area_code", "iso3c", "com", "value")]
P$area_code[P$area_code==62] <- 238  # Ethiopia
P$area_code[P$area_code==206] <- 276  # Sudan
P <- P %>% arrange(across(c(area_code, com)))
P$com <- items_conc$com_1.2[match(P$com, items_conc$com_1.1)]
P <- P[!is.na(P$com) & !is.na(P$area_code),]


years <- 1986:2019

E <- lapply(years, function(x, y) {

  data <- data.table(
    area_code = rep(regions[cbs==TRUE, code], each = nrcom),
    area = rep(regions[cbs==TRUE, name], each = nrcom),
    item_code = rep(items$item_code, nrreg),
    item = rep(items$item, nrreg),
    comm_code = rep(items$comm_code, nrreg),
    comm_group = rep(items$comm_group, nrreg),
    group = rep(items$group, nrreg))

  y_land <- y[element=="Area harvested" & year==x & item_code %in% items$item_code]
  y_biomass <- y[element=="Production" & year==x & item_code %in% items$item_code[items$group == "Primary crops"]]
  conc_land <- match(paste(data$area_code,data$item_code),paste(y_land$area_code,y_land$item_code))
  conc_biomass <- match(paste(data$area_code,data$item_code),paste(y_biomass$area_code,y_biomass$item_code))
  data[, landuse := y_land[, value][conc_land]]
  data[, biomass := y_biomass[, value][conc_biomass]]
  grass <- sup[year==x & item_code==2001]
  grass[is.na(production), production := 0]
  data[, grazing := grass$production[match(data$area_code, grass$area_code)]]
  data[item_code==2001, biomass := grazing]
  data[, grazing := grassland_yields$t_per_ha[match(data$area_code,grassland_yields$area_code)]]
  data[item_code==2001, landuse := round((biomass * 0.2) / grazing)]
  data[, grazing := NULL]

  # cap grazing landuse at 80% of a country's land area
  data[, landarea := grassland_yields$land_1000ha[match(data$area_code,grassland_yields$area_code)]]
  data[item == "Grazing", landuse := ifelse((landuse / 1000) > (landarea * 0.8), (landarea * 1000 * 0.8), landuse)]
  data[, landarea := NULL]

  # add water footprints
  water <- water_lvst[water_lvst$year == x]
  data[, blue := water$blue[match(paste(data$area_code, data$item_code),
    paste(water$area_code, water$item_code))]]
  data[, green := as.numeric(water_pasture$m3_per_ha[match(data$area_code, water_pasture$area_code)]) * landuse]
  data[item_code != 2001, green := 0]
  data[, `:=`(fodder_blue = water_fodder$blue[match(data$area_code, water_fodder$area_code)],
                  fodder_green = water_fodder$green[match(data$area_code, water_fodder$area_code)])]
  data[item_code == 2000, `:=`(blue = fodder_blue * biomass, green = fodder_green * biomass)]
  data[, `:=`(fodder_blue = NULL, fodder_green = NULL)]
  water_blue <- water_crop[water_type == "blue" & year == x]
  water_green <- water_crop[water_type == "green" & year == x]
  conc_water <- match(paste(data$area_code, data$item_code),
    paste(water_blue$area_code, water_blue$item_code))
  data[, `:=`(crops_blue = water_blue$value[conc_water], crops_green = water_green$value[conc_water])]
  data[is.na(blue) | blue == 0, blue := crops_blue]
  data[is.na(green) | green == 0, green := crops_green]
  data[, `:=`(crops_blue = NULL, crops_green = NULL)]
  data[is.na(landuse), landuse := 0]
  data[is.na(biomass), biomass := 0]
  data[is.na(blue), blue := 0]
  data[is.na(green), green := 0]
  data[, `:=`(landuse = round(landuse), biomass = round(biomass),
    blue = round(blue), green = round(green))]

  # fill gaps in land use with global average yields
  yields <- data[, .(comm_code, landuse, biomass)] %>%
    group_by(comm_code) %>%
    summarize(yield = na_sum(biomass) / na_sum(landuse))
  data[, yield := yields$yield[match(data$comm_code, yields$comm_code)]]
  data[landuse == 0 & biomass > 0 & is.finite(yield), landuse := round(biomass / yield)]
  data[, yield := NULL]
  data[, output := X[,as.character(x)]]
  data[landuse>0 & output>0 & biomass==0, biomass := output]
  data[, output := NULL]

  # add N and P application (kg per ha)
  data[, ':='(p_application = ifelse(is.na(P$value), 0, round(P$value * landuse, 3)),
              n_application = ifelse(is.na(N$value), 0, round(N$value * landuse)))]


}, y = crop[, .(year, element, area_code, item_code, value)])

names(E) <- years

saveRDS(E, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/E.rds")


# build biodiversity extensions (potential species loss from land use [10^-6 species])
biodiv <- read_csv("./input/extensions/biodiversity.csv")
biodiv_data <- t(biodiv[, -(1:3)])
biodiv_codes <- biodiv[, 1:3]

E_biodiv <- lapply(E, function(x) {
  data <- merge(x[,1:8], aggregate(x$landuse, by=list(area_code=x$area_code), FUN=sum),
                    by = "area_code", all.x = TRUE)
  data[item == "Grazing", x := landuse]
  data2 <- biodiv_data[rep(1:192, each = 123),]
  data2 <- data2 / data$x * data$landuse
  data2[!is.finite(data2)] <- 0
  colnames(data2) <- paste0(biodiv_codes$species,"_",biodiv_codes$land)
  data[, `:=`(x = NULL, landuse = NULL)]
  data <- cbind(data, data2)
})

names(E_biodiv) <- years
saveRDS(E_biodiv, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/E_biodiv.rds")
write.csv(biodiv_codes, file="/mnt/nfs_fineprint/tmp/fabio/v1.2/biodiv_codes.csv")


# extrapolate emissions data
library(Matrix)

# read ghg emissions data
ghg <- list()
ghg[[1]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_mass.rds")
ghg[[2]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_mass.rds")
ghg[[3]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_mass.rds")
ghg[[4]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_value.rds")
ghg[[5]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_value.rds")
ghg[[6]] <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_value.rds")

# extrapolate emissions data
for(i in 2014:2019){
  for(j in 1:length(ghg)){
    data <- t(t(ghg[[j]][["2013"]]) / E[["2013"]]$biomass * E[["2019"]]$biomass)
    data[!is.finite(data)] <- 0
    ghg[[j]][[as.character(i)]] <- data
  }
}

saveRDS(ghg[[1]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_mass.rds")
saveRDS(ghg[[2]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_mass.rds")
saveRDS(ghg[[3]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_mass.rds")
saveRDS(ghg[[4]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_ghg_value.rds")
saveRDS(ghg[[5]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_gwp_value.rds")
saveRDS(ghg[[6]], "/mnt/nfs_fineprint/tmp/fabio/v1.2/E_luh_value.rds")

