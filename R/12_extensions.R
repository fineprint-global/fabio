
library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")[current==TRUE]
nrreg <- nrow(regions)
nrcom <- nrow(items)

X <- readRDS(file.path(output_dir,"X.rds"))
grassland_yields <- fread("input/grazing/grazing.csv")
water_crop <- fread("input/water/water_crop.csv")
water_fodder <- water_crop[water_item == "Fodder crops/Managed grass"]
water_fodder <- merge(regions[, .(area_code = code, area = name, water_code, water_area)],
  water_fodder[, .(water_code, water_area, water_item, value, water_type)],
  by = c("water_code", "water_area"), all.x = TRUE, allow.cartesian = TRUE)
water_fodder <- dcast(water_fodder, area_code + area ~ water_type, fun=sum)
water_lvst <- fread("input/water/water_lvst.csv")
water_pasture <- grassland_yields %>% select(area_code, area, iso3c, continent, m3_per_ha)

# calculate crop water footprint -----------------------------------------------
water_crop <- merge(regions[, .(area_code = code, area = name, water_code, water_area)],
  water_crop[, .(water_code, water_area, water_item, value, water_type)],
  by = c("water_code", "water_area"), all = TRUE, allow.cartesian = TRUE)

conc_water <- fread("inst/conc_water.csv")
conc <- match(water_crop$water_item, conc_water$water_item)
water_crop <- water_crop[, `:=`(fao_code = conc_water$fao_code[conc],
                                item_code = conc_water$item_code[conc],
                                item = conc_water$item[conc])]
crop <- readRDS("./data/tidy/crop_full.rds")
water_crop <- merge(crop[unit == "tonnes" & value > 0 & item_code %in% unique(water_crop$fao_code) & element == "Production",
  .(area_code, fao_code = item_code, year, production = value)],
  water_crop[!is.na(fao_code),
  .(area_code, fao_code, item_code, item, water_type, intensity = value)],
  by = c("area_code", "fao_code"),
  all.x = TRUE, allow.cartesian = TRUE)
water_crop <- water_crop[, `:=`(value = production * intensity)]
water_crop[!area_code %in% regions[, code], `:=`(area_code = 999)]
water_crop <- water_crop[, list(value = na_sum(value)),
  by = .(area_code, item_code, item, year, water_type)]

# Calculate water footprint of meat processing ---------------------------------
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

# Calculate water footprint of livestock ---------------------------------------
stocks <- live[element == "Stocks",
  .(area_code, area, year, item_code, item, value)]
stocks$blue <- water_lvst$blue[match(stocks$item_code, water_lvst$item_code)]
stocks[, `:=`(blue = blue * value, value = NULL)]

water_lvst <- rbind(meat, stocks)
rm(live, meat, stocks, src_item, tgt_item, tgt_name)

# read production data ---------------------------------------------------------
sup <- readRDS("data/sup_final.rds")
crop <- readRDS("./data/tidy/crop_tidy.rds")
crop[!area_code %in% regions[, code], `:=`(area_code = 999, area = "ROW")]
crop <- crop[, list(value = na_sum(value)),
  by = .(area_code, area, element, year, unit, item_code, item)]

# prepare N extension ---------------------------------------------------------
N <- read_csv("./input/extensions/N_kg_per_ha.csv")
N <- merge(regions[, .(iso3c, area_code = code, region)], N, by = "iso3c", all = TRUE)
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


# prepare P extension ---------------------------------------------------------
P <- read_csv("./input/extensions/P_kg_per_ha.csv")
P <- merge(regions[, .(iso3c, area_code = code, region)], P, by = "iso3c", all = TRUE)
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



# build extensions ---------------------------------------------------------
E <- lapply(years, function(x, y) {

  data <- data.table(
    area_code = rep(regions[, code], each = nrcom),
    area = rep(regions[, name], each = nrcom),
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

saveRDS(E, file=file.path(output_dir,"E.rds"))


# build biodiversity extensions ---------------------------------------------------------
# (potential species loss from land use per hectare)
biodiv <- read_csv("./input/extensions/biodiversity.csv")
biodiv_data <- t(biodiv[, -(1:3)])
biodiv_data <- biodiv_data[rownames(biodiv_data) %in% regions[, iso3c],]
biodiv_labels <- biodiv[, 1:3]
biodiv_data <- biodiv_data[regions[, iso3c],]

E_biodiv <- lapply(E, function(x) {
  # data <- merge(x[,1:8], aggregate(x$landuse, by=list(area_code=x$area_code), FUN=sum),
  #                   by = "area_code", all.x = TRUE)
  # data[item == "Grazing", x := landuse]
  data2 <- biodiv_data[rep(seq_along(regions$code), each = 123),]
  colnames(data2) <- paste0(biodiv_labels$species,"_",biodiv_labels$land)
  data2[x$item != "Grazing", grepl("pasture", colnames(data2))] <- 0
  data2[x$item == "Grazing", grepl("cropland", colnames(data2))] <- 0
  data2 <- data2 * x$landuse
  data2[!is.finite(data2)] <- 0
  data <- cbind(x[,1:7], data2)
})

names(E_biodiv) <- years
saveRDS(E_biodiv, file=file.path(output_dir,"E_biodiv.rds"))
biodiv_labels <- biodiv_labels[biodiv_labels$land %in% c("cropland", "pasture"),]
write.csv(biodiv_labels, file=file.path(output_dir,"biodiv_labels.csv"))


# extrapolate emissions data ---------------------------------------------------------
library(Matrix)

# read ghg emissions data
ghg <- list()
names <- c("ghg_mass", "gwp_mass", "luh_mass", "ghg_value", "gwp_value", "luh_value")
for(i in seq_along(names)){
  ghg[[i]] <- readRDS(paste0(output_dir,"/E_",names[i],".rds"))
}

# extrapolate emissions data
for(i in (max(as.integer(names(ghg[[1]])))+1):max(years)){
  for(j in 1:length(ghg)){
    data <- t(t(ghg[[j]][["2013"]]) / X[,"2013"] * X[,as.character(i)])
    data[!is.finite(data)] <- 0
    ghg[[j]][[as.character(i)]] <- data
  }
}

for(i in seq_along(names)){
  saveRDS(ghg[[i]], paste0(output_dir,"/E_",names[i],".rds"))
}
