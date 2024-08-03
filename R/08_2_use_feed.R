# Feed use ----------------------------------------------------------------

# Use animal stocks
live <- readRDS("data/tidy/live_tidy.rds")

# Feed supply
feed_sup <- cbs[feed > 0 | item_code %in% c(2000),
                c("area_code", "area", "item_code", "item", "year", "feed")]
# Convert to dry matter
feed_sup <- merge(feed_sup, items[, c("item_code", "moisture", "feedtype")],
                  by = c("item_code"), all.x = TRUE)
feed_sup[, dry := feed * (1 - moisture)]




# Feed requirements -------------------------------------------------------



# Estimates from Krausmann et al. (2008) ----------------------------------
# Own assumption for other camelids such as llamas and alpacas.
conv_k <- fread("inst/conv_krausmann.csv")

feed_req_k <- merge(conv_k,
                    live[element == "Stocks", c("area_code", "area", "item_code", "year", "value")],
                    by = c("item_code"), all.x = TRUE)
feed_req_k[, `:=`(
  total = value * conversion, value = NULL, conversion = NULL, item = NULL,
  original_value = NULL,
  crops = 0, residues = 0, grass = 0, fodder = 0, scavenging = 0, animals = 0, cakes = 0)]




# Estimates from Bouwman et al. (2013) ------------------------------------
conv_b <- fread("inst/conv_bouwman.csv")
conc_b <- fread("inst/conc_bouwman.csv")
regions <- fread("inst/regions_full.csv")

# Add process-information
conv_b <- merge(conv_b, conc_b,
                by = "item", all.x = TRUE, allow.cartesian = TRUE)
# Add country-information
conv_b <- merge(conv_b,
                regions[, .(area_code = code, area = name, region_code)],
                by = c("region_code"), all.x = TRUE, allow.cartesian = TRUE)

# Extend estimates to full timeline (weighted)
interpolate <- function(y, conv_b) {
  years_avail <- unique(conv_b$year)
  if(y %in% years_avail) { # No need to interpolate
    return(conv_b[year == y,
                  .(area_code, area, item, year, proc_code, feedtype, type, conversion)])}
  # Get closest years and inter-/extrapolate their feed conversion rates
  years_min <- ifelse(y < min(years_avail), min(years_avail),
                      ifelse(y > max(years_avail), years_avail[2],
                             max(years_avail[(years_avail - y) < 0])))
  year_max <- ifelse(y > max(years_avail), max(years_avail),
                     ifelse(y < min(years_avail), years_avail[length(years_avail)-1],
                            min(years_avail[(years_avail - y) > 0])))
  difference <- (conv_b$conversion[conv_b$year == year_max] -
                   conv_b$conversion[conv_b$year == years_min]) / (year_max - years_min) * (y - years_min)
  data <- conv_b[year == years_min]
  data[, .(area_code, area, item, year = y, proc_code, feedtype, type, conversion = conversion + difference)]
}
conv_b <- rbindlist(lapply(sort(unique(use$year)), interpolate, conv_b = conv_b))

cat("Calculating feed demand from supply for the following items:\n\t",
    paste0(collapse = "; ", unique(sup[item_code %in%
                                         c(2848, 2731, 2732, 2733, 2734, 2735), item])),
    ".\n", sep = "")

# Get indigenous livestock production in tonnes
live_b <- live[element == "Production" & unit == "tonnes", ]
live_b[, `:=`(element = NULL, unit = NULL)]

# src_code <- c(944, 972, 1012, 1032, 1055, 1775, 882, 951, 982, 1020, 1130)
src_code <- c(867, 947, 977, 1017, 1035, 1808, 882, 951, 982, 1020, 1130) # this regular meat and not indigenous!
tgt_code <- c(866, 946, 976, 1016, 1034, 2029, 2848, 2848, 2848, 2848, 2848)
tgt_proc <- c("p085", "p086", "p087", "p088", "p089", "p090", "p099", "p100",
              "p101", "p102", "p103")
tgt_item <- c("Beef cattle", "Beef cattle", "Sheep and goats",
              "Sheep and goats", "Pigs", "Poultry", "Dairy cattle", "Dairy cattle",
              "Dairy cattle", "Dairy cattle", "Dairy cattle")
conc <- match(live_b$item_code, src_code)
live_b[, `:=`(item_code = tgt_code[conc],
              proc_code = tgt_proc[conc], item = tgt_item[conc])]
live_b <- live_b[!is.na(item_code), ]

feed_req_b <- live_b[year %in% years, .(area_code, area, year, item_code, item, proc_code,
                         production = value)]
rm(conc, live_b, tgt_code, tgt_proc, tgt_item, src_code)

feed_req_b <- merge(all.x = TRUE, allow.cartesian = TRUE,
                    feed_req_b,
                    conv_b[, c("area_code", "year", "proc_code", "item",
                               "feedtype", "conversion")],
                    by = c("area_code", "year", "proc_code", "item"))

# Calculate feed requirements
feed_req_b[, converted := round(production * conversion, 3)]

# Aggregate over processes, areas, items and years
feed_req_b <- data.table::dcast(feed_req_b, value.var = "converted", fun.aggregate = na_sum,
                                area_code + area + year + item + proc_code ~ feedtype)

# Define share of fodder crops in residues
feed_req_b[, `:=`(
  fodder = ifelse(item == "Pigs", residues * 0.025, # 0.025 for pigs, 0 for poultry
                  ifelse(item == "Poultry", 0, residues * 0.5)))] # 0.5 for ruminants
# Define share of oilcakes in residues
feed_req_b[, cakes := 0]
feed_req_b[, `:=`(cakes = ifelse(item %in% c("Pigs", "Poultry", "Dairy cattle"), residues * 0.5,
                                 ifelse(item == "Beef cattle", residues * 0.1, residues * 0.05)),
                  residues = na_sum(residues, -cakes))] # 0.05 for sheep and goats

# Add missing variables
processes <- unique(sup[, .(proc_code, proc)])
feed_req_b[, proc := processes$proc[match(feed_req_b$proc_code, processes$proc_code)]]

# Create total
feed_req_b[, `:=`(total = na_sum(animals, crops, grass, fodder,
                                 residues, scavenging, cakes), item_code = 0, item = NULL)]








# Estimates from GLEAM ---------------------------------------------------
feed_req_g <- fread("input/GLEAM/GLEAM3_intakes_dashboard.csv")

# convert kg in tonnes
feed_req_g[, `:=`(dm_intake = dm_intake / 1000, unit = NULL)]
# remove grass intake of swiss chickens
feed_req_g[animal=="Chicken" & feed_category=="Grass and leaves", dm_intake := 0]
# rename countries
feed_req_g[, `:=`(area = regions$name[match(feed_req_g$iso3c, regions$iso3c)],
                  area_code = regions$code[match(feed_req_g$iso3c, regions$iso3c)])]
# correct area name and code for Ethiopia and Sudan
feed_req_g[iso3c=="ETH", `:=`(area = "Ethiopia", area_code = 238)]
feed_req_g[iso3c=="SDN", `:=`(area = "Sudan", area_code = 276)]

# Merging feed intake for Abyei (to Sudan and South Sudan) and Isle of Man (to United Kingdom)
feed_req_g <- merge(feed_req_g[area != "Abyei",], 
                    feed_req_g[area == "Abyei", .(animal,feed_category,Abyei = dm_intake)],
                    by = c("animal","feed_category"),
                    all.x = TRUE)
feed_req_g <- merge(feed_req_g[area != "Isle of Man"], 
                    feed_req_g[area == "Isle of Man", .(animal,feed_category,IsleofMan = dm_intake)],
                    by = c("animal","feed_category"),
                    all.x = TRUE)
feed_req_g[iso3c %in% c("SSD","SDN"), dm_intake := na_sum(dm_intake, Abyei / 2)]
feed_req_g[iso3c == "GBR", dm_intake := na_sum(dm_intake, IsleofMan)]
feed_req_g[, `:=`(Abyei = NULL, IsleofMan = NULL)]

# For countries where GLEAM data is available but not FABIO, we aggregate them into ROW
feed_req_g[!iso3c %in% regions[current==TRUE, iso3c], `:=`(iso3c="ROW", area="RoW")]
feed_req_g <- feed_req_g[, list(dm_intake = na_sum(dm_intake)), 
                         by = c("iso3c", "area_code", "area", "animal", "feed_category")]


# Adding process codes 
# -> this creates a table where the values are not true due to double counting
conc_gleam <- fread("inst/conc_gleam.csv", header = TRUE)
feed_req_g <- merge.data.table(feed_req_g, conc_gleam, by="animal", all=TRUE, allow.cartesian = TRUE)
setcolorder(feed_req_g, c("iso3c", "area", "animal", "proc", "proc_code"))

# 2.2 Split dm intake for dairy and meat animals using bouwman
# We use the Bouwman ratios between meat and dairy herds to determine dry matter intake 
# per feedtype, country and process. We determine the Bouwman ratios for the columns: 
# a) crops, b) grass, c) residue and d) fodder 
bouwman <- feed_req_b[proc_code %in% c("p085", "p086", "p087", "p088", "p099", "p100", "p101", "p102") & year == 2015]
bouwman[, animal_category := ifelse(grepl("cattle", proc, ignore.case = TRUE), "Cattle",
                                    ifelse(grepl("buffaloes", proc, ignore.case = TRUE), "Buffalo",
                                           ifelse(grepl("sheep", proc, ignore.case = TRUE), "Sheep", "Goats")))]
bouwman[, dairy := ifelse(grepl("dairy", proc, ignore.case = TRUE), "dairy", "meat")]
bouwman <- bouwman %>% 
  select(-proc_code, -proc, -item_code, -total, -animals, -scavenging) %>% 
  gather("feed_category", "value", -area_code, -area, -year, -animal_category, -dairy) %>% 
  spread(dairy, value) %>% 
  mutate(dairy_share = dairy / na_sum(dairy, meat)) %>% 
  mutate(dairy_share = ifelse(is.na(dairy_share), 0, dairy_share)) %>% 
  as.data.table()

# We match the dairy shares to the gleam feedtypes a) grains + other edible b) grass and leaves,
# c) oil seed cakes and d) fodder crops
feed_category_b <- c("crops", "crops", "fodder", "grass", "residues")
feed_category_g <- c("Grains", "Other edible", "Fodder crop", "Grass and leaves", "Oil seed cakes")
feed_category_lookup <- setNames(feed_category_g, feed_category_b)
bouwman$feed_category <- feed_category_lookup[bouwman$feed_category]
temp <- bouwman[feed_category=="Grains"]
temp$feed_category <- "Other edible"
bouwman <- rbind(bouwman, temp)
feed_req_g <- merge(feed_req_g, bouwman[, .(area, animal = animal_category, feed_category, dairy_share)],
                    by = c("area", "animal", "feed_category"), all.x=TRUE)

# fill gaps with global average dairy shares
# this wont be needed after estimating missing meat and milk production values!!!!!!!!!!!!!!!!!!!!!!
avg_dairy <- feed_req_g %>% 
  group_by(animal, feed_category) %>% 
  summarize(dairy_share = mean(dairy_share, na.rm=T)) %>% 
  as.data.table()
feed_req_g <- merge(feed_req_g, avg_dairy[, .(animal, feed_category, avg_dairy_share = dairy_share)],
                    by = c("animal", "feed_category"), all.x=TRUE)
feed_req_g[, dairy_share := ifelse(is.na(dairy_share), avg_dairy_share, dairy_share)]
feed_req_g[proc_code %in% c("p085", "p086", "p087", "p088", "p099", "p100", "p101", "p102"), 
           dm_intake := ifelse(grepl("Dairy", proc), dm_intake * dairy_share, dm_intake * (1-dairy_share))]
feed_req_g[, `:=`(dairy_share = NULL, avg_dairy_share = NULL)]

# formatting to wide 
feed_req_g <- dcast(feed_req_g, iso3c + area_code + area + animal + proc_code + proc ~ feed_category,
                    value.var = "dm_intake", fill = 0) 
# derive total dm intake per process
feed_req_g[, total := na_sum(`By-products`, `Crop residues`, `Fodder crop`, Grains, `Grass and leaves`, 
             `Oil seed cakes`, `Other edible`, `Other non-edible`)]

# scale up chicken's feed intake to meat and egg production of poultry birds
# include the following items
# c(1058, 1062, 1069, 1073, 1080, 1089, 1091)
# c("Meat, chicken","Eggs, hen, in shell","Meat, duck","Meat, goose and guinea fowl","Meat, turkey","Meat, bird nes","Eggs, other bird, in shell")
poultry <- live[year==2015 & unit=="tonnes" & element == "Production" &
                  item_code %in% c(1058, 1062, 1069, 1073, 1080, 1089, 1091)]
# convert into dry matter
poultry[, moisture := ifelse(item_code %in% c(1062,1091), 0.744, 0.6)]
poultry[, value := value * (1-moisture)]
# derive share of chicken in poultry
poultry[, type := ifelse(item_code %in% c(1058, 1062),"chicken", "other")]
poultry <- poultry %>% 
  group_by(area_code, area, type) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(type, value) %>% 
  as.data.table()
poultry[, chicken_share := chicken / na_sum(chicken, other)]
feed_req_g <- merge(feed_req_g, poultry[,.(area_code, chicken_share)], 
                    by = "area_code", all.x = TRUE)
feed_req_g[is.na(chicken_share), chicken_share := 1]
feed_req_g[animal=="Chicken", `:=`(Grains = Grains / chicken_share,
                                   `Oil seed cakes` = `Oil seed cakes` / chicken_share,
                                   `Other edible` = `Other edible` / chicken_share,
                                   total = total / chicken_share,
                                   animal = "Poultry")]
feed_req_g[, `:=`(year = 2015,
                  item_code = 0,
                  animals = 0, 
                  crops = Grains + `Other edible`,
                  grass = `Grass and leaves`,
                  cakes = `Oil seed cakes`,
                  residues = 0,
                  scavenging = 0,
                  fodder = `Fodder crop`)]
feed_req_g[, `:=`(Grains = NULL, `Grass and leaves` = NULL, `Fodder crop` = NULL, 
                  `Oil seed cakes` = NULL, `Other edible` = NULL, iso3c = NULL,
                  `By-products` = NULL, `Crop residues` = NULL, `Other non-edible` = NULL,
                  chicken_share = NULL, animal = NULL)]


# estimate feed intake for other years --------------------------
# assume same change rates as for bouwman
# problem: in gleam some countries do not use fodder crops for cattle, non for pigs
area_id <- 11
process <- "p085"
for(area in regions$code){
  for(process in unique(feed_req_g$proc_code)){
    data_b <- feed_req_b[area_code==area_id & proc_code==process]
    data_g <- feed_req_g[area_code==area_id & proc_code==process]
    
  }
}








# Integrate GLEAM, Bouwman and Krausmann feed requirements
feed_req <- bind_rows(feed_req_b[!paste(proc_code,area) %in% paste(feed_req_g$proc_code,feed_req_g$area) & total != 0],
                      feed_req_g[total > 0], 
                      feed_req_k[!is.na(total) & total > 0])
rm(feed_req_k, feed_req_b, feed_req_g)







# OLD: Integrate Bouwman and Krausmann feed requirements
feed_req_b[, residues := cakes]
feed_req_b[, cakes := NULL]
feed_req <- bind_rows(feed_req_b[total > 0],
                      feed_req_k[!is.na(total) & total > 0])

# Allocate total feed demand from Krausmann to the Bouwman split -----
feed_alloc <- feed_req[item_code == 0,
                       lapply(list(animals, crops, grass, fodder, residues, scavenging, total),
                              na_sum),
                       by = list(area_code, year)]
feed_alloc <- feed_alloc[V7 > 0, list(area_code, year,
                                      animals_f = V1 / V7, crops_f = V2 / V7, grass_f = V3 / V7,
                                      fodder_f  = V4 / V7, residues_f = V5 / V7, scavenging_f = V6 / V7)]
feed_alloc[is.na(feed_alloc)] <- 0

feed_req <- merge(feed_req, feed_alloc,
                  by = c("area_code", "year"), all.x = TRUE)
rm(feed_alloc)
# Use global average split if no info is available
feed_req[, `:=`(
  animals_f = ifelse(is.na(animals_f), mean(animals_f, na.rm = TRUE), animals_f),
  crops_f = ifelse(is.na(crops_f), mean(crops_f, na.rm = TRUE), crops_f),
  grass_f = ifelse(is.na(grass_f), mean(grass_f, na.rm = TRUE), grass_f),
  fodder_f = ifelse(is.na(fodder_f), mean(fodder_f, na.rm = TRUE), fodder_f),
  residues_f = ifelse(is.na(residues_f), mean(residues_f, na.rm = TRUE), residues_f),
  scavenging_f = ifelse(is.na(scavenging_f), mean(scavenging_f, na.rm = TRUE), scavenging_f))]
# This simple procedure assumes equal feed composition for
# Horses, Asses, Mules, Camels, Camelids, other
feed_req[item_code != 0 & !proc %in% c("Rabbits husbandry", "Rodents husbandry, other"),
         `:=`(animals = total * animals_f, crops = total * crops_f,
              grass = total * grass_f, fodder = total * fodder_f, residues = total * residues_f,
              scavenging = total * scavenging_f)]
# Rabbits and hares, Rodents, other
feed_req[item_code != 0 & proc %in% c("Rabbits husbandry", "Rodents husbandry, other"),
         `:=`(animals = total * animals_f, crops = total * (crops_f + grass_f),
              grass = 0, fodder = total * fodder_f, residues = total * residues_f,
              scavenging = total * scavenging_f)]

# Kick factors again
feed_req[, `:=`(animals_f = NULL, crops_f = NULL, grass_f = NULL,
                fodder_f = NULL, residues_f = NULL, scavenging_f = NULL)]

# Aggregate RoW countries in feed_req
feed_req <- replace_RoW(feed_req, codes = regions[current == TRUE, code])
feed_req <- feed_req[, lapply(.SD, na_sum),
                     by = c("area_code", "area", "proc_code", "proc", "year")]


# Adapt feed-demand to available feed-supply -----
feed_sup[, total_dry := na_sum(dry), by=c("area_code","year","feedtype")]
feed_req <- data.table::melt(feed_req, id=c("area_code","area","year","proc_code","proc"),
                             measure=c("crops","animals","residues","fodder","grass"),
                             variable.name="feedtype", value.name="req")
feed_req[, total_req := na_sum(req), by = c("area_code", "year", "feedtype")]

feed <- merge(feed_sup[, .(area_code, area, year, item_code, item, feedtype, moisture,
                           sup_dry = dry, total_sup_dry = total_dry, sup_fresh = feed)],
              feed_req, by=c("area_code", "area", "year", "feedtype"), all = TRUE, allow.cartesian = TRUE)

feed[feedtype != "grass", req := req / total_req * total_sup_dry]

# Allocate feed-use from feed types to individual crops -----
feed[feedtype != "grass", req := req / total_sup_dry * sup_dry]
feed[feedtype == "grass", `:=`(item_code = 2001, item = "Grazing", moisture = 0.8)]

# Convert to fresh weight
feed[, feed_use := round(req / (1 - moisture),0)]

# Add feed use back to the use table
feed[, comm_code := items$comm_code[match(feed$item_code, items$item_code)]]
use <- merge(use,
             feed[!is.na(feed_use), .(area_code, area, year, proc_code, proc, item_code, item, feed_use)],
             by = c("area_code", "area", "year", "item_code", "item", "proc_code", "proc"), all = TRUE)
conc <- match(use$item_code, items$item_code)
use[, comm_code := items$comm_code[conc]]
use[!is.na(feed_use), `:=`(use = feed_use)]
use[, `:=`(feed_use = NULL)]

# Add grazing (feed_use of item_code 2001 == grass_r) to supply and balances
grazing <- feed[item_code == 2001, list(grazing = na_sum(feed_use)),
                by = c("area_code", "year", "item_code")]

sup <- merge(sup, grazing,
             by = c("area_code", "year", "item_code"), all.x = TRUE)
sup[item_code == 2001, production := grazing]
sup[, grazing := NULL]

cbs <- merge(cbs, grazing,
             by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[item_code == 2001, `:=`(
  production = grazing, total_supply = na_sum(grazing, imports))]
cbs[, grazing := NULL]

# Clean up
rm(feed, grazing, feed_req, feed_sup, live)

