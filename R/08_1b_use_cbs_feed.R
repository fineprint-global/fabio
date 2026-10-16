# Feed use ----------------------------------------------------------------

# Use animal stocks from FAOSTAT
live <- readRDS("data/tidy/live_tidy.rds")

# Feed supply
feed_sup <- cbs[feed > 0 & !item_code %in% c(2000),
                c("area_code", "area", "item_code", "item", "year", "feed")]

# Convert crops to dry matter
feed_sup <- merge(feed_sup, items[, c("item_code", "moisture", "feedtype")],
                  by = c("item_code"), all.x = TRUE)
feed_sup[, dry := feed * (1 - moisture)]


# add fodder crop supply (full detail for deriving dm content)
fodder_sup <- readRDS("data/tidy/fodder_crop_non_agg_tidy.rds")[, item := NULL]
fodder_sup <- fodder_sup[ unit == "tonnes"]
fodder_dm <- fread("inst/gleam_dry_matter.csv")
fodder_sup[, dm := (fodder_dm$feed_dry_matter[match(item_code, fodder_dm$item_code)])/100]

#calculate production-weighted dry matter average per country (to convert back to fresh weight later)
fodder_sup[, dm_weighted := sum((value/sum(value, na.rm = TRUE)) * dm, na.rm = TRUE), 
           by = .(year, area_code)]

# convert fodder crops to dry matter and aggregate
fodder_sup[, dry := value * dm]
fodder_sup <- fodder_sup[, .(dry = sum(dry ,na.rm = TRUE), feed = sum(value, na.rm = TRUE),
                             area = unique(area), dm = unique(dm_weighted)), 
                             by = .(year, area_code)]

fodder_sup[, `:=` (item_code = 2000, item = "Fodder crops", feedtype = "fodder",
                   moisture = 1-dm)][, dm := NULL]

feed_sup <- rbind(fodder_sup, feed_sup, use.names = TRUE)

# Checking difference to previous moisture approach
# feed_sup[, feed_new:= fodder_sup$feed_new[match(paste(area_code,year, item_code),paste(fodder_sup$area_code,
#                                                                             fodder_sup$year, fodder_sup$item_code))]]
# 
# feed_sup[, moisture_new := (1- fodder_sup$dm)[match(paste(area_code,year, item_code),paste(fodder_sup$area_code,
#                                                                                           fodder_sup$year, fodder_sup$item_code))]]
# 
# feed_sup[,moisture_diff := abs(moisture_new-moisture)]

# result: up to 26% difference in fodder crop moisture content





# Feed requirements -------------------------------------------------------



# Estimates from Krausmann et al. (2008) ----------------------------------
# Own assumption for other camelids such as llamas and alpacas.
conv_k <- fread("inst/conv_krausmann.csv")

feed_req_k <- merge(conv_k,
                    live[year %in% years & element == "Stocks", c("area_code", "area", "item_code", "year", "value")],
                    by = c("item_code"), all.x = TRUE)
feed_req_k[, `:=`(total = value * conversion)]
feed_req_k[, `:=`(value = NULL, conversion = NULL, item = NULL, original_value = NULL, 
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

cat("Calculating Bouwman feed demand from supply for the following items:\n\t",
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


# add stocks from GLW -----------
# Goal: get per capita values per feedtype, multiply with faostat stocks (they differ from GLW)
## Download and aggregate GLW data -----
# Download settings 
base            <- "https://dataverse.harvard.edu"
dataverse_alias <- "glw_4"
out_root        <- "input/GLW"
overwrite       <- FALSE   # TRUE to re-download files that already exist

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Helper: GET + parse JSON 
get_json <- function(url, query = NULL) {
  r <- GET(url, query = query, timeout(120))
  stop_for_status(r)
  fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$data
}

# Layer 1: list the datasets inside the dataverse 
message("Listing datasets in dataverse '", dataverse_alias, "' ...")
datasets <- get_json(paste0(base, "/api/dataverses/", dataverse_alias, "/contents"))
message("Found ", length(datasets), " datasets.\n")

# Helper: derive a clean folder name from a dataset title 
species_from_title <- function(title) {
  sp <- sub("^Global\\s+(.*?)\\s+distribution.*$", "\\1",
            title, ignore.case = TRUE)
  if (identical(sp, title)) sp <- title            # fallback if no match
  sp <- tolower(trimws(sp))
  gsub("[^a-z0-9]+", "_", sp)                      # filesystem-safe
}

# Layer 2: for each dataset, list and download its files 
for (ds in datasets) {
  pid <- paste0(ds$protocol, ":", ds$authority, "/", ds$identifier)
  
  meta   <- get_json(paste0(base, "/api/datasets/:persistentId/"),
                     query = list(persistentId = pid))
  ver    <- meta$latestVersion
  fields <- ver$metadataBlocks$citation$fields
  title  <- Find(function(f) identical(f$typeName, "title"), fields)$value
  
  species <- species_from_title(title)
  dest_dir <- file.path(out_root, species)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  files <- ver$files
  message(sprintf("[%s]  %s  (%d files)", species, title, length(files)))
  
  for (f in files) {
    fname <- if (!is.null(f$dataFile$filename)) f$dataFile$filename else f$label
    fid   <- f$dataFile$id
    dest  <- file.path(dest_dir, fname)
    
    if (file.exists(dest) && !overwrite) {
      message("   skip (exists): ", fname)
      next
    }
    
    message("   downloading: ", fname)
    resp <- GET(paste0(base, "/api/access/datafile/", fid),
                query = list(format = "original"),
                write_disk(dest, overwrite = TRUE),
                timeout(600))
    if (http_error(resp)) {
      warning("   FAILED (", status_code(resp), "): ", fname)
      if (file.exists(dest)) file.remove(dest)
    }
  }
}

message("\nDone. Files saved under: ", normalizePath(out_root))


## Aggregate GLW by country ---------------
# Settings 
in_root  <- "input/GLW"          # where the per-species folders live
method   <- "Da"                 # "Da" (dasymetric, standard) or "Aw" (areal-weighted) -> no difference at national level
regions_current <- fread("inst/regions_full.csv")[current == TRUE]

# Country polygons from terra
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries <- countries[, c("iso_a3_eh", "admin")]
names(countries)[1:2] <- c("iso_a3", "country")

#  Loop over species folders 
species_dirs <- list.dirs(in_root, recursive = FALSE)
stopifnot(length(species_dirs) > 0)

results <- list()

for (sp_dir in species_dirs) {
  species <- basename(sp_dir)
  
  tifs <- list.files(sp_dir, pattern = "\\.tif$",
                     full.names = TRUE, ignore.case = TRUE)
  if (length(tifs) == 0) {
    message("No .tif in ", sp_dir, " - skipping.")
    next
  }
  
  # Prefer the requested method (Da/Aw); fall back to the first tif found.
  pick <- grep(paste0("_", method, "\\.tif$"), tifs,
               ignore.case = TRUE, value = TRUE)
  tif  <- if (length(pick) > 0) pick[1] else tifs[1]
  
  message(sprintf("[%s] %s", species, basename(tif)))
  
  r <- rast(tif)
  r <- ifel(r < 0, NA, r)                       # drop NoData (-9999) if present
  polys <- st_transform(countries, crs(r))
  
  # exact_extract fractionally weights border pixels by coverage, so animals
  # in a boundary pixel are split between countries instead of double-counted.
  polys[[species]] <- exact_extract(r, polys, "sum", progress = FALSE)
  
  results[[species]] <- st_drop_geometry(polys[, c("iso_a3", "country", species)])
}

#  Combine into one wide table (country x species) 
country_totals_glw <- Reduce(function(a, b) full_join(a, b, by = c("iso_a3", "country")),
                             results)

country_totals_glw <- melt(as.data.table(country_totals_glw),
                           id.vars = c("iso_a3", "country"),
                           variable.name = "species",
                           value.name = "stocks")

# round to whole animals and order by cattle if present
country_totals_glw[, stocks := round(stocks)]

# Aggregate RoW
country_totals_glw[, iso3c := regions_current$iso3c[match(iso_a3, regions_current$iso3c)]]
country_totals_glw[is.na(iso3c), `:=` (iso3c = "ROW", country = "RoW")]
country_totals_glw[iso3c == "AUS", `:=` (country = "Australia")]
country_totals_glw <- country_totals_glw[ , .(stocks = sum(stocks, na.rm = TRUE)),
                                          by = .(iso3c, country, species)]


# tidy
rm( countries, datasets, ds, f, fields, files, meta, polys,
    r,  results, ver)


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
feed_req_g[!iso3c %in% regions[current==TRUE, iso3c], `:=`(iso3c="ROW", area="RoW", area_code=999)]
feed_req_g <- feed_req_g[, list(dm_intake = sum(dm_intake, na.rm = TRUE)), 
                         by = c("iso3c", "area_code", "area", "animal", "feed_category")]

# Convert to per capita values using GLW stocks
conc_glw_gleam <- data.table(
  species = c("buffaloes", "cattle", "chickens", "ducks", "goats", "horses", "pigs", "sheep"),
  animal  = c("Buffalo",   "Cattle", "Chicken",  NA,      "Goats", NA,       "Pigs", "Sheep")
)

feed_req_g[, species_glw := conc_glw_gleam$species[match(animal, conc_glw_gleam$animal)]]
feed_req_g[, stocks_glw := country_totals_glw$stocks[match(paste(iso3c, species_glw), 
                                                       paste(country_totals_glw$iso3c,
                                                             country_totals_glw$species))]]

feed_req_g[, pc_dm_intake := dm_intake/stocks_glw]

# exclude non-finite values (there are only 8 and they are marginal. E.g., 0.44 tons of dry
# matter of non-edibles for KIR chickens but 0 chickens reported)
feed_req_g[is.nan(pc_dm_intake) | is.infinite(pc_dm_intake), pc_dm_intake := 0]

# add FAO stocks to find new totals
## add chickens back to live for this exercise from fao input data (will be scaled to poultry birds later)
chicken_stocks <- fread("input/fao/Production_Crops_Livestock_E_All_Data_(Normalized).csv")
chicken_stocks <- chicken_stocks[Year %in% years & `Item Code` == 1057 & `Area Code` %in% regions$code &
                                   Element == "Stocks",
                                 .(area_code = `Area Code`, area = Area, element = Element,
                                   year = Year, unit = Unit, item_code = `Item Code`, 
                                   item = Item, value = Value)]
chicken_stocks[!area_code %in% regions_current$code, `:=` (area_code = 999, area = "RoW")]
chicken_stocks <- chicken_stocks[, .(value = sum(value, na.rm = TRUE)),
                                 by = .(year, area, area_code, item, item_code, unit,
                                        element)]

live_g <- rbind(live, chicken_stocks, use.names = TRUE)

# tidy live
live_g <- live_g[element == "Stocks" & year %in% years ]
live_g <- live_g[!area_code %in% regions_current$code | area_code == 999, `:=` (area_code = 999, area = "RoW")]
live_g <- live_g[, .(value = sum(value, na.rm = TRUE)), by = .(area_code, area, element,
                                                        year, unit, item_code, item)]
live_g[ unit == "1000 An", `:=` (value = value * 1000 , unit = "An") ]


# Create conc for adding FAO stocks to GLEAM
conc_live_gleam <- data.table(
  item = c("Cattle", "Buffaloes", "Sheep", "Goats", "Pigs", "Horses", "Asses", "Mules",
           "Camels", "Rabbits and hares", "Rodents, other", "Camelids, other", "Chickens"),
  item_code = c(866L, 946L, 976L, 1016L, 1034L, 1096L, 1107L, 1110L,
                1126L, 1140L, 1150L, 1157L, 1057L),
  animal = c("Cattle", "Buffalo", "Sheep", "Goats", "Pigs", NA, NA, NA,
             NA, NA, NA, NA, "Chicken")
)


# exclude animals that are not in GLEAM from live stocks
live_g <- live_g[item_code %in% conc_live_gleam[!is.na(animal), item_code]]
feed_req_g[, item_code := conc_live_gleam$item_code[match(animal, conc_live_gleam$animal)]]


# create full timeline (except Maldives where no animal stocks are reported)
## find unique year/item/area_code combinations where FAO reports stocks 
## Idea: we do not want to include per capita feed where there are no stocks 

live_g[, variable := paste0(year, "_", item_code, "_", area_code)]


feed_req_g_full <- CJ(live_g$variable, feed_category = unique(feed_req_g$feed_category))
feed_req_g_full[, c("year", "item_code", "area_code") := tstrsplit(V1, "_", fixed = TRUE, type.convert = TRUE)][, V1 := NULL]
feed_req_g_full[, `:=` (iso3c = regions_current$iso3c[match(area_code, regions_current$code)],
                        item = items$item[match(item_code, items$item_code)])]
feed_req_g_full[item_code == 1057, item := "Chicken"]

# add stocks from FAO and per capita requirements from GLEAM/GLW
feed_req_g_full[, stocks := live_g$value[match(paste(year, item_code, area_code),
                                             paste(live_g$year, live_g$item_code, 
                                                   live_g$area_code))]]

feed_req_g_full[, pc_feed := feed_req_g$pc_dm_intake[match(paste(area_code, 
                                                                 item_code, feed_category),
                                                           paste(feed_req_g$area_code,
                                                                 feed_req_g$item_code,
                                                                 feed_req_g$feed_category))]]


# Create weighted regional averages for cases where there are stocks but no feed
## add regions for regional averages
feed_req_g_full[, region := regions$region[match(iso3c, regions$iso3c)]]

## Create weighted average using helper column to exclude rows from weighting, where pc_feed is NA
feed_req_g_full[, w := fifelse(is.na(pc_feed), NA_real_, as.numeric(stocks))]
feed_req_g_full[, region_wght_avg := sum(w * pc_feed, na.rm = TRUE) / sum(w, na.rm = TRUE),
                by = .(region, item_code, feed_category, year)]
# groups with no valid rows or zero total stocks give NaN, so set them to NA
feed_req_g_full[is.nan(region_wght_avg), region_wght_avg := NA_real_]

## Repeat the same for world averages
feed_req_g_full[, world_wght_avg := sum(w * pc_feed, na.rm = TRUE) / sum(w, na.rm = TRUE),
                by = .(item_code, feed_category, year)]

feed_req_g_full[is.nan(world_wght_avg), world_wght_avg := NA_real_]

## first use regional averages, then world averages to fill the gaps
feed_req_g_full[is.na(pc_feed) & !is.na(region_wght_avg), pc_feed := region_wght_avg ]
feed_req_g_full[is.na(pc_feed) & !is.na(world_wght_avg), pc_feed := world_wght_avg ]

feed_req_g_full[, feed_demand := stocks * pc_feed]
# feed_req_g <- dcast(feed_req_g_full,  year + item_code + iso3c + item ~ feed_category,
#                     value.var = "feed_demand")
feed_req_g <- copy(feed_req_g_full[, .(iso3c, year, item_code, item,feed_category, dm_intake = feed_demand)])
feed_req_g[, area := regions$name[match(iso3c, regions$iso3c)]]

rm(feed_req_g_full, chicken_stocks, live_g)

# Adding process codes 
# -> this creates a table with double-counting in the values
conc_gleam <- fread("inst/conc_gleam.csv", header = TRUE)[, animal := NULL]
feed_req_g <- merge.data.table(feed_req_g, conc_gleam, by="item_code", all=TRUE, allow.cartesian = TRUE)
setcolorder(feed_req_g, c("iso3c", "area", "item", "item_code", "proc", "proc_code"))

# Split dm intake for dairy and meat animals using bouwman
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
  as.data.table() %>% 
  setDT()

# We match the dairy shares to the gleam feedtypes a) grains + other edible b) grass and leaves,
# c) oil seed cakes and d) fodder crops
feed_category_b <- c("crops", "crops", "crops", "fodder", "grass", "cakes", "residues", "residues")
feed_category_g <- c("Grains", "Other edible", "By-products", "Fodder crop", "Grass and leaves", "Oil seed cakes", "Crop residues","Other non-edible" )
lookup <- data.table(feed_category_b = feed_category_b,
                     feed_category_g = feed_category_g)


bouwman <- merge(bouwman, lookup,
                 by.x = "feed_category", by.y = "feed_category_b",
                 all.x = TRUE, allow.cartesian = TRUE)

bouwman[, feed_category := feed_category_g][, feed_category_g := NULL]
conc_gleam <- fread("inst/conc_gleam.csv")
bouwman[, item_code := conc_gleam$item_code[match(animal_category, conc_gleam$animal)]]

feed_req_g <- merge(feed_req_g, bouwman[, .(area, item_code, feed_category, dairy_share)],
                    by = c("area", "item_code", "feed_category"), all.x=TRUE)

# fill gaps with global average dairy shares
# this wont be needed after estimating missing meat and milk production values!!!!!!!!!!!!!!!!!!!!!!
avg_dairy <- feed_req_g %>% 
  group_by(item_code, feed_category) %>% 
  summarize(dairy_share = mean(dairy_share, na.rm=T)) %>% 
  as.data.table()
feed_req_g <- merge(feed_req_g, avg_dairy[, .(item_code, feed_category, avg_dairy_share = dairy_share)],
                    by = c("item_code", "feed_category"), all.x=TRUE)
feed_req_g[, dairy_share := ifelse(is.na(dairy_share), avg_dairy_share, dairy_share)]
feed_req_g[proc_code %in% c("p085", "p086", "p087", "p088", "p099", "p100", "p101", "p102"), 
           dm_intake := ifelse(grepl("Dairy", proc), dm_intake * dairy_share, dm_intake * (1-dairy_share))]
feed_req_g[, `:=`(dairy_share = NULL, avg_dairy_share = NULL)]
feed_req_g[, area_code := regions$code[match(iso3c, regions$iso3c)]]

# formatting to wide 
feed_req_g <- dcast(feed_req_g, iso3c + area_code + area + item_code + proc_code + proc + year ~ feed_category,
                    value.var = "dm_intake", fill = 0) 
# derive total dm intake per process
feed_req_g[, total := na_sum(`By-products`, `Crop residues`, `Fodder crop`, Grains, `Grass and leaves`, 
             `Oil seed cakes`, `Other edible`, `Other non-edible`)]

# scale up chicken's feed intake to meat and egg production of poultry birds
# include the following items
# c(1058, 1062, 1069, 1073, 1080, 1089, 1091)
# c("Meat, chicken","Eggs, hen, in shell","Meat, duck","Meat, goose and guinea fowl","Meat, turkey","Meat, bird nes","Eggs, other bird, in shell")
poultry <- live[unit=="tonnes" & element == "Production" &
                  item_code %in% c(1058, 1062, 1069, 1073, 1080, 1089, 1091)]
# convert into dry matter (74.4% for eggs, 60% for meat)
poultry[, moisture := ifelse(item_code %in% c(1062,1091), 0.744, 0.6)]
poultry[, value := value * (1-moisture)]
# derive share of chicken in poultry
poultry[, type := ifelse(item_code %in% c(1058, 1062),"chicken", "other")]
poultry <- poultry %>% 
  group_by(area_code, area, type, year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  spread(type, value) %>% 
  as.data.table()
poultry[, chicken_share := chicken / na_sum(chicken, other)]
feed_req_g <- merge(feed_req_g, poultry[,.(area_code, year, chicken_share)], 
                    by = c("area_code", "year"), all.x = TRUE)
feed_req_g[is.na(chicken_share), chicken_share := 1]
feed_req_g[item_code == 1057, `:=`(Grains = Grains / chicken_share,
                                   `Oil seed cakes` = `Oil seed cakes` / chicken_share,
                                   `Other edible` = `Other edible` / chicken_share,
                                   `Other non-edible` = `Other non-edible`/ chicken_share,
                                   `Fodder crop` = `Fodder crop` /chicken_share,
                                   `Grass and leaves` =  `Grass and leaves` / chicken_share,
                                   `Crop residues` =  `Crop residues` / chicken_share,
                                   `By-products` =  `By-products` / chicken_share,
                                   total = total / chicken_share,
                                   item_code = 2029
                                  )]
feed_req_g[, `:=`(crops = Grains + `Other edible`,
                  grass = `Grass and leaves`,
                  cakes = `Oil seed cakes`,
                  fodder = `Fodder crop`,
                  residues = `Crop residues`)]
feed_req_g[, `:=`(Grains = NULL, `Grass and leaves` = NULL, `Fodder crop` = NULL, 
                  `Oil seed cakes` = NULL, `Other edible` = NULL, iso3c = NULL,
                  `By-products` = NULL, `Crop residues` = NULL, `Other non-edible` = NULL,
                  chicken_share = NULL)]


# estimate fodder crop use for cattle where grass is used but no fodder crops --------------------------
# we assign the values of grass use to fodder crops and later up/down-scale them to fodder crop supply
feed_req_g[fodder==0 & proc_code %in% c("p085", "p099"), fodder := grass / 100]

# get feed types "animals", and "scavenging" from bouwman
feed_req_g <- merge.data.table(feed_req_g, feed_req_b[, .(area_code,proc_code,year,animals,scavenging)],
                               by = c("area_code","proc_code", "year"), all.x = TRUE)

# use dairy camel feed from bouwman
feed_req_g <- rbind(feed_req_g, feed_req_b[year==2015 & proc_code=="p103", 
                                           c("area_code","year", "proc_code", "area", "proc", "total", "item_code", 
                                             "crops", "grass", "cakes", "fodder", "animals", "residues", "scavenging")], 
                    use.names = TRUE)

# set all item_codes to 0 (these are no longer needed and the 0 will be used later
# for plitting up Krausmann feed requirements)
feed_req_g[, item_code := 0]


# # estimate feed intake for other years (old) --------------------------
# # assume same change rates as for bouwman
# 
# # Ensure feed_req_b is sorted by area_code, proc_code, and year
# setkey(feed_req_b, area_code, proc_code, year)
# 
# # Create a new data.table for change rates relative to 2015
# change_rates <- feed_req_b[year == 2015, 
#   .(area_code, proc_code, crops_2015 = crops, grass_2015 = grass, cakes_2015 = cakes, 
#     fodder_2015 = fodder, residues_2015 = residues, scavenging_2015 = scavenging, animals_2015 = animals)]
# 
# # Now merge the 2015 base values with all years in feed_req_b
# change_rates <- merge(feed_req_b, change_rates, by = c("area_code", "proc_code"), all.x = TRUE)
# 
# # Calculate change rates for all years relative to 2015
# change_rates[, `:=`(
#   crops_change = crops / crops_2015,
#   grass_change = grass / grass_2015,
#   cakes_change = cakes / cakes_2015,
#   fodder_change = fodder / fodder_2015,
#   residues_change = residues / residues_2015,
#   scavenging_change = scavenging / scavenging_2015,
#   animals_change = animals / animals_2015
# )]
# 
# #is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
# num_cols <- setdiff(colnames(change_rates), c("area_code", "proc_code", "area", "year", "item_code"))
# change_rates[, (num_cols) := lapply(.SD, function(x) ifelse(is.na(x),0, x)), .SDcols = num_cols]
# 
# # Duplicate values for whole time series by creating a Cartesian product of feed_req_g and the years
# # Create a Cartesian product (cross join) of feed_req_g and all years (including 2015)
# feed_req_g_all_years <- CJ(
#   area_code = unique(feed_req_g$area_code),
#   proc_code = unique(feed_req_g$proc_code),
#   year = unique(feed_req_b$year)
# )
# 
# # Merge the Cartesian product back with feed_req_g, keeping 2015 values where they exist
# feed_req_g_all_years <- merge(feed_req_g_all_years, feed_req_g, by = c("area_code", "proc_code"), all.x = TRUE)
# feed_req_g <- feed_req_g_all_years[!is.na(proc)]
# 
# # Merge change rates into feed_req_g_all_years
# feed_req_g <- merge(feed_req_g, 
#   change_rates[, .(area_code, proc_code, year, crops_change, grass_change, cakes_change, 
#                    fodder_change, residues_change, scavenging_change, animals_change)], 
#   by = c("area_code", "proc_code", "year"), all.x = TRUE)
# 
# # Estimating feed use for the next year based on change rates
# feed_req_g[, `:=`(
#   crops = crops * crops_change,
#   grass = grass * grass_change,
#   cakes = cakes * cakes_change,
#   fodder = fodder * fodder_change,
#   residues = residues * residues_change,
#   scavenging = scavenging * scavenging_change,
#   animals = animals * animals_change
# )]
# 
# # Calculate total dm intake per process
# feed_req_g[, total := na_sum(animals, crops, fodder, scavenging, grass, if_else(residues>cakes, residues, cakes))]
# feed_req_g <- feed_req_g[total > 0]

# Aggregate RoW in feed_req_b and feed_req_k
num_cols <- setdiff(colnames(feed_req_b), c("area_code","area", "year", "proc_code", "proc", "item_code"))
feed_req_b[!area_code %in% regions_current$code, `:=` (area_code = 999, area = "RoW") ]
feed_req_b <- feed_req_b[, lapply(.SD, sum, na.rm = TRUE),
                        by = .(area_code, area, year, proc_code, proc, item_code),
                        .SDcols = num_cols
]

feed_req_k[!area_code %in% regions_current$code, `:=` (area_code = 999, area = "RoW") ]
feed_req_k <- feed_req_k[, lapply(.SD, sum, na.rm = TRUE),
                         by = .(area_code, area, year, proc_code, proc, item_code),
                         .SDcols = num_cols
]

# Integrate GLEAM, Bouwman and Krausmann feed requirements
feed_req <- bind_rows(feed_req_b[!paste(proc_code,area) %in% paste(feed_req_g$proc_code,feed_req_g$area) & total != 0],
                      feed_req_g[, .(area_code, proc_code, year, area, proc, total, item_code,
                                     crops, grass, cakes, fodder, animals, residues, scavenging)], 
                      feed_req_k[!is.na(total) & total > 0])

feed_req[is.na(feed_req)] <- 0




# Allocate total feed demand from Krausmann to the GLEAM/Bouwman split -----
feed_alloc <- feed_req[item_code == 0,
                       lapply(list(animals, crops, grass, fodder, cakes, scavenging, total),
                              na_sum),
                       by = list(area_code, year)]
feed_alloc <- feed_alloc[V7 > 0, list(area_code, year,
                                      animals_f = V1 / V7, crops_f = V2 / V7, grass_f = V3 / V7,
                                      fodder_f  = V4 / V7, cakes_f = V5 / V7, scavenging_f = V6 / V7)]
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
  cakes_f = ifelse(is.na(cakes_f), mean(cakes_f, na.rm = TRUE), cakes_f),
  scavenging_f = ifelse(is.na(scavenging_f), mean(scavenging_f, na.rm = TRUE), scavenging_f))]
# This simple procedure assumes equal feed composition for
# Horses, Asses, Mules, Camels, Camelids, other, "Rabbits husbandry", "Rodents husbandry, other"
feed_req[item_code != 0,
         `:=`(animals = total * animals_f, crops = total * crops_f,
              grass = total * grass_f, fodder = total * fodder_f, cakes = total * cakes_f,
              scavenging = total * scavenging_f)]

# Kick factors again
feed_req[, `:=`(animals_f = NULL, crops_f = NULL, grass_f = NULL,
                fodder_f = NULL, cakes_f = NULL, scavenging_f = NULL)]

# Aggregate RoW countries in feed_req
feed_req <- replace_RoW(feed_req, codes = regions[current == TRUE, code])
feed_req <- feed_req[, lapply(.SD, na_sum),
                     by = c("area_code", "area", "proc_code", "proc", "year")]


# Adapt feed-demand to available feed-supply -----
feed_sup[, total_sup_dry := na_sum(dry), by=c("area_code","year","feedtype")]
feed_req <- data.table::melt(feed_req, id=c("area_code","area","year","proc_code","proc"),
                             measure=c("crops","animals","cakes","fodder","grass"),
                             variable.name="feedtype", value.name="req")
feed_req[, total_req := na_sum(req), by = c("area_code", "year", "feedtype")]

feed <- merge(feed_sup[, .(area_code, area, year, item_code, item, feedtype, moisture,
                           sup_dry = dry, total_sup_dry, sup_fresh = feed)],
              feed_req, by=c("area_code", "area", "year", "feedtype"), all = TRUE, allow.cartesian = TRUE)

feed[feedtype=="grass", sup_dry := req]
feed[feedtype=="grass", total_sup_dry := na_sum(sup_dry), by=c("area_code","year")]

feed[, total_req_all := na_sum(req), by = c("area_code", "year")]
feed[, total_sup_dry_all := na_sum(sup_dry), by=c("area_code","year")]
feed[, ratio := total_sup_dry_all / total_req_all]
feed[, ratio_feedtype := total_sup_dry / total_req]
feed <- feed[year %in% years,]

# # check feed
# check_feed <- unique(feed[, .(year, feedtype, total_sup_dry, total_req)]) %>%  
#   group_by(feedtype, year) %>% 
#   summarize(sup = round(na_sum(total_sup_dry)/1000000), 
#             req = round(na_sum(total_req)/1000000)) %>% 
#   mutate(ratio = round(sup/req,1))
# 
# check_feed <- unique(feed[year==2023, 
#                           .(area_code, area, feedtype, total_sup_dry, total_req)]) %>%  
#   group_by(feedtype, area) %>% 
#   summarize(sup = round(na_sum(total_sup_dry)/1000), 
#             req = round(na_sum(total_req)/1000)) %>% 
#   mutate(ratio = round(sup/req,1))
# 
# check_feed <- unique(feed[year==2023, 
#                           .(area_code, area, 
#                             sup = round(total_sup_dry_all/1000), 
#                             req = round(total_req_all/1000))]) %>%  
#   mutate(ratio = round(sup/req,1))
# 
# unique(feed[area=="Canada" & year==2023, .(feedtype, total_sup_dry, total_req)]) %>% 
#   group_by(feedtype) %>% 
#   summarize(sup = round(na_sum(total_sup_dry)/1000), 
#             req = round(na_sum(total_req)/1000)) %>% 
#   mutate(ratio = round(sup/req,1))
# 
# unique(feed[area=="Canada" & year==2023, .(feedtype, proc, req = round(req/1000))]) %>% 
#   spread(feedtype, req)


# Rescale feed so that total requirements meet total supply
feed[feedtype != "grass" & area != "Kiribati", req := req / total_req * total_sup_dry]
# Note: there is a problem in the data with Kiribati for which GLEAM reports around 100 to 500 times more feed requirements than FAOSTAT

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
sup[item_code == 2001, `:=`(
  production = grazing, supply = grazing)]
sup[, grazing := NULL]

cbs <- merge(cbs, grazing,
             by = c("area_code", "year", "item_code"), all.x = TRUE)
cbs[item_code == 2001, `:=`(
  production = grazing, total_supply = grazing, feed = grazing)]
cbs[, grazing := NULL]

# Clean up
rm(avg_dairy, bouwman,  feed,  feed_category_b, feed_category_g,
   grazing, feed_req, feed_sup, live, feed_req_b, feed_req_g, feed_req_k,
    conc_b, conc_gleam, conv_b, conv_k,  poultry, conc_glw_gleam, conc_live_gleam,
   country_totals_glw, lookup, regions_current, fodder_dm, fodder_sup)

