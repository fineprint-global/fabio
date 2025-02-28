
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
# N <- read_csv("./input/extensions/N_kg_per_ha.csv")
# N <- merge(regions[, .(iso3c, area_code = code, region)], N, by = "iso3c", all = TRUE)
# N <- gather(N, key = "com", value = "value", -region, -iso3c, -area_code)
# avg_N <- N %>%
#   group_by(region, com) %>%
#   summarise(avg = mean(value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(!is.na(region)) %>%
#   group_by(com) %>%
#   bind_rows(summarise(., avg = mean(avg, na.rm = TRUE), region = NA))
#   # bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(., na.rm = TRUE) else "Global"))
# N <- merge(N, avg_N, by = c("region", "com"), all.x = TRUE)
# N$value[is.na(N$value)] <- ifelse(is.na(N$avg[is.na(N$value)]), NA, N$avg[is.na(N$value)])
# N <- N[, c("area_code", "iso3c", "com", "value")]
# N$area_code[N$area_code==62] <- 238  # Ethiopia
# N$area_code[N$area_code==206] <- 276  # Sudan
# N <- N %>% arrange(across(c(area_code, com)))
# items_conc <- read_csv("./inst/items_conc.csv")
# N$com <- items_conc$com_1.2[match(N$com, items_conc$com_1.1)]
# N <- N[!is.na(N$com) & !is.na(N$area_code),]

HFUBC <- fread("./input/extensions/NPK_application_by_crop.csv") #HFUBC data
#Are we always talking about the same crop area? 
HFUBC <- HFUBC[,.(Country, ISO3_code, Year, Crop, Crop_area_k_ha, N_k_t, P2O5_k_t, K2O_k_t )]
HFUBC <- HFUBC[Year %in% c("1990/91","1991/92","1992/93", "1989/90", "1999/2000", "1998/99", "1997-98"), 
           Year := substr(Year, 1, 4)]                         #convert fertilizer years to calendar years where necessary
HFUBC[, `:=` (P_tons = P2O5_k_t* 0.436, K_tons = K2O_k_t * 0.83, N_tons = N_k_t)] #convert to elemental P and K
HFUBC[, `:=` (P2O5_k_t = NULL, N_k_t = NULL, K2O_k_t = NULL)]
HFUBC[, ':=' (N_rate = N_tons/Crop_area_k_ha, P_rate = P_tons/Crop_area_k_ha, 
            K_rate = K_tons/Crop_area_k_ha, year = as.integer(Year))][, `:=` (N_tons =NULL, P_tons = NULL, 
                                                     K_tons = NULL, Country = NULL,
                                                     Year = NULL)]
HFUBC_full <- CJ(name = regions$name,
               item = items[group == "Primary crops", item], 
               year = years)
HFUBC_full[, `:=` (region = regions$region[match(name, regions$name)], 
                 iso3c = regions$iso3c[match(name, regions$name)])]


conc <- fread("./input/extensions/item_conc_fabio_hfubc.csv")
HFUBC_full <- merge(HFUBC_full, conc, by.x = "item", by.y = "fabio", allow.cartesian =TRUE)
HFUBC_full <- merge(HFUBC_full, HFUBC, by.x = c("iso3c", "year", "hfubc"), 
                  by.y = c("ISO3_code", "year", "Crop") , all.x =TRUE)
HFUBC_aggregated <- HFUBC_full[, .(
  Crop_area_k_ha = sum(Crop_area_k_ha, na.rm = TRUE), # Sum of crop area
  N_rate = mean(N_rate, na.rm = TRUE), # Average N rate
  P_rate = mean(P_rate, na.rm = TRUE), # Average P rate
  K_rate = mean(K_rate, na.rm = TRUE)  # Average K rate
), by = .(item, iso3c, year)]

HFUBC_avail <- HFUBC_aggregated[!is.na(N_rate) & !is.nan(N_rate)][, region := regions$region[match(iso3c, regions$iso3c)]]



#### NPKGrids data
library(terra)
library(sf)
library(data.table)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)

# Define directories for fertilizer and area datasets
nc_fert_dir <- "./input/extensions/NPKGrids"
nc_area_dir <- "./input/extensions/Cropgrids"
nc_fert_files <- list.files(nc_fert_dir, pattern = "\\.nc$", full.names = TRUE)
nc_area_files <- list.files(nc_area_dir, pattern = "\\.nc$", full.names = TRUE)

# # Load global country boundaries (WGS84) with ISO3 codes
# countries <- ne_countries(scale = "medium", returnclass = "sf")[, c("iso_a3", "geometry")]

# Initialize an empty data.table to store summary statistics
crop_summary <- data.table()
harv_summary <- data.table()


countries_in_order <- readRDS("./input/extensions/countries_in_order.RDS")

# Process each pair of NetCDF files (one for fertilizer and one for crop area)
for (i in seq_along(nc_fert_files)) {
  nc_fert <- nc_fert_files[171]  
  nc_area <- nc_area_files[171]
  
  # Read the fertilizer and area NetCDF files
  r_fert <- rast(nc_fert)
  r_area <- rast(nc_area)
  
  # Specify bands for fertilizer and area (crop_area and crop_harvest)
  bands_fert <- c(1, 4, 7)  # Fertilizer bands
  bands_area <- c(1, 2)     # crop_area and crop_harvest
  
  # Helper function to process a band (for both fertilizer and area data)
  process_band <- function(r, band) {
    var <- r[[band]]
    band_name <- names(r)[band]
    var_dt <- as.data.table(as.data.frame(var, xy = TRUE, na.rm = TRUE))
    setnames(var_dt, c("lon", "lat", band_name))  # Use band name directly
    return(var_dt)
  }
  
  # Process fertilizer bands and crop area bands
  fert_rate_list <- lapply(bands_fert, function(band) process_band(r_fert, band))
  area_rate_list <- lapply(bands_area, function(band) process_band(r_area, band))
  
  # Convert lists to one data.table
  fert_rate_dt <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE), fert_rate_list)
  area_rate_dt <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat"), all = TRUE), area_rate_list)
  
  # Combine fertilizer data, crop area data
  data_crop <- cbind(fert_rate_dt, area_rate_dt[, !"lon", with = FALSE][, !"lat", with = FALSE][,!"harvarea", with = FALSE])
  data_harv <- cbind(fert_rate_dt, area_rate_dt[, !"lon", with = FALSE][, !"lat", with = FALSE][,!"croparea", with = FALSE])
  
  # # The following is only done once to get a dt with countries in the right order
  
  # # Convert the combined data.table to an sf object
  # data <- st_as_sf(combined_data, coords = c("lon", "lat"), crs = 4326)
  # Assign ISO3 country codes
  # data <- st_join(data, countries, join = st_intersects) # take countries from here to make a dataset in the right order and then cbind with dataset
  # setDT(data)
  # data[, .(iso_a3)]
  # saveRDS(data, "./input/countries_in_order.RDS" )   

  data_crop <- cbind(countries_in_order, data_crop)
  data_harv <- cbind(countries_in_order, data_harv)
  
  # Remove rows with missing country codes or non-positive areas
  data_crop <- data_crop[!is.na(iso_a3) & croparea > 0 & Nrate != -1]
  data_harv <- data_harv[!is.na(iso_a3) & harvarea > 0 & Nrate != -1]
  
  
  # Calculate weighted averages
  summary_data_crop <- data_crop[, .(
    min_N = min(Nrate, na.rm = TRUE),
    max_N = max(Nrate, na.rm = TRUE),
    weighted_N = sum(Nrate * croparea / sum(croparea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  summary_data_crop[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  
  
  summary_data_harv <- data_harv[, .(
    min_N = min(Nrate, na.rm = TRUE),
    max_N = max(Nrate, na.rm = TRUE),
    weighted_N = sum(Nrate * harvarea / sum(harvarea, na.rm = TRUE), na.rm = TRUE)
  ), by = c("iso_a3")]
  # Add crop name from file name
  summary_data_harv[, crop := sub("^[^_]*_(.*)...$", "\\1", basename(nc_fert))]
  
  # Append to the overall summary table
  crop_summary <- rbind(crop_summary, summary_data_crop, fill = TRUE)
  harv_summary <- rbind(harv_summary, summary_data_harv, fill = TRUE)
}






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

biodiv_new <- fread("input/extensions/biodiversity_new.csv", dec=",")
#convert to hectares
CF_cols <- grep("_", names(biodiv_new), value = TRUE)
biodiv_new[, (CF_cols) := lapply(.SD, function(x) x * 10000), .SDcols = CF_cols]
biodiv_new[, country := iconv(country, from = "", to = "UTF-8", sub = "")]
biodiv_new[, country := ifelse(toupper(country) == country & grepl("[A-Z]", country), 
                               tools::toTitleCase(tolower(country)), 
                               country)]

#find missing countries and fill gaps
countries_missing_in_bio <- as.data.table(setdiff(regions$name, biodiv_new$country))
china_row <- biodiv_new[country == "China, mainland"] # duplicate China values for Hong Kong
china_row[["country"]] <- "China, Hong Kong SAR"
sudan_row <- biodiv_new[country == "Sudan"] #duplicate Sudan's values for South Sudan
sudan_row[["country"]] <- "South Sudan"

biodiv_new <- rbind(biodiv_new, china_row, sudan_row)

#add RoW by averaging CFs of countries available in bio but not in FABIO
RoW_countries <- setdiff(biodiv_new$country, regions$name)# find countries not in fabio
RoW_CF <- biodiv_new[country %in% RoW_countries,]
col_means <- RoW_CF[, lapply(.SD, mean, na.rm = TRUE), .SDcols = !c("country")]
col_means[, country := "RoW"]
#Add Timor-Leste, assuming RoW CFs
TLS_row <- copy(col_means) 
TLS_row[,country := "Timor-Leste" ]
biodiv_new <- rbind(biodiv_new, col_means, TLS_row)

#delete RoW countries, add iso3c and order by area code for easier handling in extensions
biodiv_new <- biodiv_new[!country %in% RoW_countries,]
biodiv_new[, `:=`(iso3c = regions$iso3c[match(biodiv_new$country, regions$name)],
                  country_code = regions$code[match(biodiv_new$country, regions$name)])] 
biodiv_new <- biodiv_new[order(country_code)]
biodiv_new[,`:=`(country = NULL, country_code = NULL)]
setcolorder(biodiv_new, c("iso3c", "glo_annual_crops" , "glo_permanent_crops" , "glo_pasture" , 
                          "reg_annual_crops" , "reg_permanent_crops", "reg_pasture"))

#clean up
rm(col_means,china_row, sudan_row, TLS_row, RoW_CF, RoW_countries, countries_missing_in_bio, CF_cols)


#biodiv with from Chaudhary & Brooks, 2018
# biodiv <- read_csv("./input/extensions/biodiversity.csv")
# biodiv_data <- t(biodiv[, -(1:3)])
# biodiv_data <- biodiv_data[rownames(biodiv_data) %in% regions[, iso3c],]
# biodiv_labels <- biodiv[, 1:3]
# biodiv_data <- biodiv_data[regions[, iso3c],]



E_biodiv <- lapply(E, function(x) {
  # data <- merge(x[,1:8], aggregate(x$landuse, by=list(area_code=x$area_code), FUN=sum),
  #                   by = "area_code", all.x = TRUE)
  # data[item == "Grazing", x := landuse]
  data2 <- biodiv_new[rep(seq_along(regions$code), each = 123),]
  annual_crops <- c(
    "Rice and products", "Wheat and products", "Barley and products", "Maize and products", 
    "Rye and products", "Oats", "Millet and products", "Sorghum and products", "Cereals, Other", 
    "Potatoes and products", "Cassava and products", "Sweet potatoes", "Roots, Other", "Yams", 
    "Sugar beet", "Beans", "Peas", "Pulses, Other and products", "Soyabeans", "Groundnuts", 
    "Sunflower seed", "Rape and Mustardseed", "Seed cotton", "Sesame seed", "Tomatoes and products", 
    "Onions", "Vegetables, Other", "Jute", "Jute-Like Fibres", "Soft-Fibres, Other", "Sisal", 
    "Abaca", "Hard Fibres, Other", "Tobacco", "Fodder crops", "Cottonseed", "Sugar cane", "Oilcrops, Other"
  )
  permanent_crops <- c(
    "Coconuts - Incl Copra", "Oil, palm fruit", "Olives (including preserved)", 
    "Oranges, Mandarines", "Lemons, Limes and products", "Grapefruit and products", 
    "Citrus, Other", "Bananas", "Plantains", "Apples and products", "Pineapples and products", 
    "Dates", "Grapes and products (excl wine)", "Fruits, Other", "Nuts and products", 
    "Coffee and products", "Cocoa Beans and products", "Tea (including mate)", "Pepper", 
    "Pimento", "Cloves", "Spices, Other", "Rubber", "Hops", "Sweeteners, Other"
  )
  data2[x$item != "Grazing", which(grepl("pasture", colnames(data2)))] <- 0
  data2[!x$item %in% annual_crops, which(grepl("annual", colnames(data2)))] <- 0
  data2[!x$item %in% permanent_crops, which(grepl("permanent", colnames(data2)))] <- 0
  data2[!x$item %in% annual_crops & !x$item %in% permanent_crops & x$item != "Grazing",
        which(grepl("_",colnames(data2)))] <- 0
  data2[, (2:7) := lapply(.SD, function(y) y * x$landuse), .SDcols = 2:7]
  data <- cbind(x[,1:7], data2)
})

names(E_biodiv) <- years
saveRDS(E_biodiv, file=file.path(output_dir,"E_biodiv.rds"))


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
