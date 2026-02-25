
library("data.table")
library("Matrix")
library("tidyverse")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full_123.csv")


# CBS ---------------------------------------------------------------------

cat("\nBuilding full CBS.\n")

cbs <- readRDS("data/tidy/cbs_tidy.rds")

cat("Removing items from CBS that are not used in FABIO:\n\t",
  paste0(unique(cbs[!item_code %in% items$item_code, item]),
    sep = "", collapse = "; "), ".\n", sep = "") # Eggs and Milk are duplicated with different codes (2948, 2949)
# Particularly fish and aggregates
cbs <- dt_filter(cbs, item_code %in% items$item_code & year %in% years)



# BTD ---------------------------------------------------------------------

btd <- readRDS("data/tidy/btd_full_tidy.rds")

# Imports
imps <- btd[!unit %in% c("usd"), list(value = round(na_sum(value))),
            by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
                          value.var = "value")
imps[, `:=`(value = ifelse(!is.na(An), An, ifelse(!is.na(`1000 An`), `1000 An`, tonnes)),
            An = NULL, `1000 An` = NULL, tonnes = NULL)]

# Exports
exps <- btd[!unit %in% c("usd"), list(value = round(na_sum(value))),
            by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
                          value.var = "value")
exps[, `:=`(value = ifelse(!is.na(An), An, ifelse(!is.na(`1000 An`), `1000 An`, tonnes)),
            An = NULL, `1000 An` = NULL, tonnes = NULL)]


# # Forestry ----------------------------------------------------------------
#
# cat("\nAdding forestry production data.\n")
#
# fore <- readRDS("data/tidy/fore_prod_tidy.rds")
#
# fore[, `:=`(supply = na_sum(production, imports),
#   other = na_sum(production, imports, -exports),
#   stock_withdrawal = 0, stock_addition = 0,
#   feed = 0, food = 0, losses = 0, processing = 0,
#   seed = 0, balancing = 0)]
# fore[other < 0, `:=`(balancing = other, other = 0)]
#
# cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
# rm(fore)


# Add items that are now contained in SUA ---------------------
sua <- readRDS("data/tidy/sua_cbs_tidy.rds")

# add data entries not contained in cbs
codes_cbs <- paste(cbs$area_code,cbs$area,cbs$item_code,cbs$item,cbs$year)
codes_sua <- paste(sua$area_code,sua$area,sua$item_code,sua$item,sua$year)
cbs <- rbind(cbs, sua[!codes_sua %in% codes_cbs & item_code %in% items$item_code])

# we use some data from sua to replace cbs data due to mistakes in the fbs/cbs database 
# e.g., palm kernels is mistaken for oil palm fruit in fbs, beer is wrong for later years, palmkernel oil has gaps for some countries
sua_items <- c("Palm kernels", "Palmkernel Oil", "Palm Oil", "Oil, palm fruit", "Beer")
cbs <- rbind(cbs[!item %in% sua_items,], sua[item %in% sua_items,])


# Replace crop production data with the data from the prod dataset -------------------------------------
# Note: There are many gaps and even wrong values in the food balances and suas. We therefore replace all production values.
cat("\nAdding crop production data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]
setkey(crop_prod, year, area_code, item_code)

# Replace production data in CBS with data from FAOSTAT's Production domain ---
cbs <- merge(cbs, crop_prod,
             by = c("area_code", "area", "item_code", "item", "year"), all = TRUE)
cbs[!is.na(value) & !item %like% "Other" & item != "Beer", production := value]
cbs[!is.na(value) & is.na(production), production := value]
cbs[, value := NULL]


# Estimate cbs for items/countries not included -------------------------------------

# Filter countries and items that are not yet in CBS
addcbs <- dt_filter(crop_prod[item_code %in% items$item_code & year %in% years],
                    ! paste(area_code,item_code,year) %in% paste(cbs$area_code,cbs$item_code,cbs$year))

# Technical conversion factors to impute processing ---
tcf_crop <- fread("inst/tcf_crop.csv")

C <- data.table::dcast(tcf_crop, item_code ~ source_code, fill = 0, value.var = "tcf")
tcf_codes <- list(C[, item_code], as.integer(colnames(C[, -1])))
C <- as(C[, -1], "Matrix")
dimnames(C) <- tcf_codes

tcf_data <- addcbs[item_code %in% unlist(tcf_codes),
  .(year, area_code, item_code, production = value)]
setkey(tcf_data, year, area_code, item_code) # Quick merge & ensure item-order
yrs <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_data$area_code))

# Base processing on production + imports - exports
tcf_data[imps[, .(year, area_code = to_code, item_code, imports = value)],
  on = c("area_code", "item_code", "year"), imports := imports]
tcf_data[exps[, .(year, area_code = from_code, item_code, exports = value)],
  on = c("area_code", "item_code", "year"), exports := exports]

# Production of items
output <- tcf_data[data.table(expand.grid(year = yrs,
  area_code = areas, item_code = unique(tcf_codes[[1]])))]
output[, `:=`(value = production,
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_data[data.table(expand.grid(year = yrs,
  area_code = areas, item_code = unique(tcf_codes[[2]])))]
input[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(input, function(x) {`<`(x, 0)}, value = 0, cols = "value")
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_data[data.table(expand.grid(year = yrs,
  area_code = areas, item_code = tcf_codes[[2]]))]
setkey(results, year, area_code, item_code)
results[, `:=`(value = NA_real_,
  production = NULL, imports = NULL, exports = NULL)]

# Fill during a loop over years and areas
for(x in yrs) {
  output_x <- output[year == x, ]
  input_x <- input[year == x, ]
  for(y in areas) {
    output_y <- output_x[area_code == y, value]
    input_y <- input_x[area_code == y, value]
    # Skip if no data is available
    if(all(output_y == 0) || all(input_y == 0)) {next}
    results[year == x & area_code == y,
      value := fill_tcf(y = output_y, z = input_y, C = C, cap = FALSE)]
  }
}
results <- results[!is.na(value), .(year, area_code, item_code, processing = value)]

# Allocate uses
addcbs[, `:=`(production = value, value = NULL)]
addcbs[, exports := exps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(exps$year, exps$from_code, exps$item_code))]]
addcbs[, imports := imps$value[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
  paste(imps$year, imps$to_code, imps$item_code))]]
addcbs[, supply := na_sum(production,imports)]

# rebalance supply and use

# allocate processing inputs to 'processing'
addcbs[, processing := results$processing[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
                                                paste(results$year, results$area_code, results$item_code))]]
# Allocate 'Seed cotton', 'Oil, palm fruit', 'Hops', 'Sugar cane' and 'Palm kernels' supply to processing
addcbs[item_code %in% c(254, 328, 677, 2536, 2562), processing := na_sum(supply, -exports)]

# add 'Cotton lint', 'Jute', 'Jute-Like Fibres', 'Soft-Fibres', 'Other Sisal', 'Abaca',
# 'Hard Fibres, Other', 'Tobacco', and 'Rubber' to other uses
addcbs[item_code %in% c(2661,2662,2663,2664,2665,2666,2667,2671,2672), other := na_sum(supply, -exports)]

# Allocate 'Fodder crops' supply to feed
addcbs[item_code %in% c(2000), feed := na_sum(supply, -exports)]

# allocate rest to 'food'
addcbs[, food := na_sum(supply,-processing, -other, -feed, -exports)]
addcbs[, balancing := 0]
addcbs[food < 0, `:=`(balancing = food, food = 0)]

# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkip filling cbs seed.",
  "Apparently data on seed is not reported in newer faostat versions.\n")

# Add to CBS ---
cat("\nAdding ", nrow(addcbs), " missing cbs accounts.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, addcbs)

rm(crop_prod, addcbs, tcf_crop, tcf_codes, tcf_data, input, output, results, yrs, areas, C)


# This is not necessary anymore, since oil cakes are now reported again
# # Oilcrop cakes data is not reported anymore by FAOSTAT
# # - 1986-2013: use data from old FAOSTAT release
# # - 2014-current: impute production from primary crop processing use / oil production
# 
# cbs_old <- readRDS("input/fao/cbs_nonfood_old_with_oil_cakes.rds")
# 
# crp_item <- c(2555,2552,2557,2558,2559,2562,2560,2561,2570)
# crp_name <- c("Soyabeans", "Groundnuts", "Sunflower seed", "Rape and Mustardseed", "Cottonseed", "Palm kernels", "Coconuts - Incl Copra", "Sesame seed", "Oilcrops, Other")
# cak_item <- c(2590,2591,2592,2593,2594,2595,2596,2597,2598)
# cak_name <- c("Soyabean Cake", "Groundnut Cake", "Sunflowerseed Cake", "Rape and Mustard Cake", "Cottonseed Cake", "Palmkernel Cake", "Copra Cake", "Sesameseed Cake", "Oilseed Cakes, Other")
# oil_item <- c(2571,2572,2573,2574,2575,2576,2578,2579,2586)
# oil_name <- c("Soyabean Oil", "Groundnut Oil", "Sunflowerseed Oil", "Rape and Mustard Oil", "Cottonseed Oil", "Palmkernel Oil", "Coconut Oil", "Sesameseed Oil", "Oilcrops Oil, Other")
# 
# # extract oilcrop prcessing and cake + oil production
# cbs_crp_proc <- cbs[item_code %in% crp_item, .(area_code, area, item_code, item, year, processing)]
# cbs_cak_prod <- cbs_old[`Item Code` %in% cak_item & Element == "Production" & `Area Code` %in% regions$code[regions$cbs==TRUE], 
#                         .(area_code = `Area Code`, area = Area, item_code = `Item Code`, 
#                           item = Item, year = Year, production = Value)]
# cbs_oil_prod <- cbs[item_code %in% oil_item, .(area_code, area, item_code, item, year, production)]
# 
# # match cake production with crop processing and oil output
# conc <- match(cbs_cak_prod$item_code, cak_item)
# cbs_cak_prod[, `:=`(source_item = crp_item[conc], source_name = crp_name[conc],
#                oil_item = oil_item[conc], oil_name = oil_name[conc])]
# cbs_cak_prod <- merge(cbs_cak_prod, cbs_crp_proc,
#                  by.x = c("area_code", "area", "source_item", "source_name", "year"),
#                  by.y = c("area_code", "area", "item_code", "item", "year"),
#                  all.x = TRUE)
# cbs_cak_prod <- merge(cbs_cak_prod, cbs_oil_prod[,.(area_code, area, item_code, item, year, oil_production = production)],
#                  by.x = c("area_code", "area", "oil_item", "oil_name", "year"),
#                  by.y = c("area_code", "area", "item_code", "item", "year"),
#                  all.x = TRUE)
# cbs_cak_prod <- cbs_cak_prod[production > 0,]
# #cbs_cak_prod <- cbs_cak_prod[production == 0, production := NA]
# 
# # compute annual conversion factors / cake-to-oil factors
# cbs_cak_prod[, `:=`(tcf = ifelse(is.finite(production / processing), production / processing, NA),
#                cof = ifelse(is.finite(production / oil_production), production / oil_production, NA))]
# # compute averages across all available years (using only post-2000 values to account for technical changes)
# cak_conv <- cbs_cak_prod[year %in% 2000:2013, .(tcf = mean(tcf, na.rm = TRUE), cof = mean(cof, na.rm = TRUE)), by = .(area_code, area, item_code, item, source_item, source_name, oil_item, oil_name)]
# # fill NAs with global averages
# cak_conv_glob <- cak_conv[, .(tcf_global = mean(tcf, na.rm = TRUE),
#                               cof_global = mean(cof, na.rm = TRUE)),
#                           by = .(item_code)]
# #cak_conv[, `:=` (tcf_global = mean(tcf, na.rm = TRUE), cof_global = mean(cof, na.rm = TRUE)), by = .(item_code, item, source_item, source_name, oil_item, oil_name)]
# cak_conv <- merge(cak_conv, cak_conv_glob, by = c("item_code"))
# cak_conv[, `:=` (tcf = ifelse(is.na(tcf), tcf_global, tcf),
#                  cof = ifelse(is.na(cof), cof_global, cof))]
# cak_conv[,`:=` ( tcf_global = NULL, cof_global = NULL)]
# # NOTE: if we want to derive cake production also for countries that never had cake production between 2000 and 2013, 
# # we need an additional step to fill tcf/cof with global averages for these cases!
# 
# # apply factors to cbs in following order: if processing > 0, use tcf, and if processing = 0 but oil production > 0, use cof
# cake <- cbind(cak_conv[rep(1:nrow(cak_conv), each = length(years))], year = years[rep(1:length(years), nrow(cak_conv))])
# cake <- merge(cake, cbs_crp_proc[processing > 0,],
#                  by.x = c("area_code", "area", "source_item", "source_name", "year"),
#                  by.y = c("area_code", "area", "item_code", "item", "year"),
#                  all.x  = TRUE) # all.y = TRUE
# cake <- merge(cake, cbs_oil_prod[production > 0,][,.(area_code, area, item_code, item, year, oil_production = production)],
#                  by.x = c("area_code", "area", "oil_item", "oil_name", "year"),
#                  by.y = c("area_code", "area", "item_code", "item", "year"),
#                  all.x = TRUE) #  all.y = TRUE
# cake[, `:=` (processing = ifelse(is.na(processing), 0, processing),
#                 oil_production = ifelse(is.na(oil_production), 0, oil_production)) ]
# # filter relevant years for extrapolation
# cake <- cake[year >= 2010,]
# # estimate processing use if no or too little is reported (for all oil crops except oil crops, other)
# cake[item_code != 2598 & processing == 0, processing := oil_production / (1-tcf-0.03)]
# # apply factors
# cake[, production := processing * tcf]
# 
# # # TODO: in case oil + cake production exceed source processing input, correct via cof
# # # discuss if we should adjust cbs for this!
# # cake[(production + oil_production) > processing, oil_production := production / cof]
# # cake[(production + oil_production) > processing, processing := production + oil_production]
# 
# # adapt area names to match naming conventions in new fao data
# cake[, area := regions$name[match(cake$area_code,regions$code)]]
# 
# 
# # add trade
# cak_imp <- crop[element == "Import quantity" & unit == "tonnes" & item_code %in% cak_item & year %in% years, ]
# cak_exp <- crop[element == "Export quantity" & unit == "tonnes" & item_code %in% cak_item & year %in% years, ]
# cak_imp[, `:=`(element = NULL, unit = NULL)]
# cak_exp[, `:=`(element = NULL, unit = NULL)]
# 
# cake <- merge(cake[, .(area_code, area, item_code, item, year, production)],
#               cak_imp[, .(area_code, area, item_code, item, year, imports = value)],
#               by = c("area_code", "area", "item_code", "item", "year"),
#               all = TRUE)
# cake <- merge(cake,
#               cak_exp[, .(area_code, area, item_code, item, year, exports = value)],
#               by = c("area_code", "area", "item_code", "item", "year"),
#               all = TRUE)
# 
# # add trade values from btd in case they are missing in live_trad
# cake <- merge(
#   cake,
#   imps[item_code %in% cak_item & value > 0 & year %in% years,  # these are in heads as well
#        c("to_code", "to", "item_code", "item", "year", "value")],
#   by.x = c("area_code", "area", "item_code", "item", "year"),
#   by.y = c("to_code", "to", "item_code", "item", "year"),
#   all = TRUE)
# cake[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
# cake <- merge(
#   cake,
#   exps[item_code %in% cak_item & value > 0 & year %in% years,
#        c("from_code", "from", "item_code", "item", "year", "value")],
#   by.x = c("area_code", "area", "item_code", "item", "year"),
#   by.y = c("from_code", "from", "item_code", "item", "year"),
#   all = TRUE)
# cake[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
# 
# # compute total supply
# cake[, supply := na_sum(production, imports)]
# # reduce exports where they exceed total supply (per definition not the case any more!)
# cake[, exports := ifelse(exports > supply, supply, exports)]
# # all uses are assumed to go to feed
# # TODO: check if appropriate! in a few cases, cakes are also used for "other"
# cake[, feed := na_sum(production, imports, -exports)]
# cake <- cake[supply > 0, ]
# 
# 
# # Add to CBS ---
# cat("\nAdding ", nrow(cake), " missing oilseed cake accounts.\n", sep = "")
# cbs <- dplyr::bind_rows(cbs, cake)
# 
# rm(crp_item, crp_name, cak_item, cak_name, oil_item, oil_name,
#    cbs_cak_prod, cbs_crp_proc, cbs_oil_prod, cbs_old,
#    cak_conv, cak_conv_glob,
#    cak_imp, cak_exp, cake)


# Fill livestock -----------------------------

# NOTE: this creates cbs for live animals not contained in cbs

cat("\nFilling missing livestock data.\n")

# # Map "Meat indigenous, ..." items to CBS items
# --> no longer present in production data! Replace by computing production + imp - exp
#src_item <- c(1137, 944, 1032, 1012, 1775, 1055, 1120, 1144,
#   972, 1161, 1154, 1122, 1124)

# meat and dairy animals
tgt_item <- c(866, 1016, 976, 2029, 1034, 1140, 946, 1150)
tgt_name <- c("Cattle", "Goats", "Sheep", "Poultry Birds", "Pigs",
              "Rabbits and hares", "Buffaloes", "Rodents, other")
# Map "Meat, ..." items to CBS items
src_item <- c(867, 1017, 977, 1808, 1035, 1141, 947, 1151)

live <- readRDS("data/tidy/live_tidy.rds")[year %in% years & element == "Producing Animals/Slaughtered"]
live[, `:=`(element = NULL, unit = NULL)]

conc <- match(live$item_code, src_item)
live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live <- live[!is.na(item_code), ]

# draft and sport animals
live_draft <- readRDS("data/tidy/live_tidy.rds")[year %in% years & element == "Stocks"]
live_draft[, `:=`(element = NULL, unit = NULL)]
tgt_item <- c(1126, 1096, 1157, 1107, 1110)
tgt_name <- c("Camels", "Horses", "Camelids, other", "Asses", "Mules")
live_draft <- live_draft[item_code %in% tgt_item]

# combine meat and draft animals
live <- merge(live, live_draft,
  by = c("area_code", "area", "item_code", "item", "year"), all = TRUE)
live[ , `:=`(value = na_sum(value.x, value.y), value.x = NULL, value.y = NULL)]

# Add trade data
live_trad <- readRDS("data/tidy/live_tidy.rds")[year %in% years]
live_trad[item %in% items[unit=="1000 animals", item] & unit == "An", 
          `:=`(unit = "1000 An", value = value / 1000)]
live_imp <- live_trad[element == "Import quantity" & unit %in% c("An", "1000 An") & 
                        item_code %in% items$item_code, ]
live_exp <- live_trad[element == "Export quantity" & unit %in% c("An", "1000 An") & 
                        item_code %in% items$item_code, ]

live <- merge(live[, .(area_code, area, item_code, item, year, production = value)],
              live_imp[, .(area_code, area, item_code, item, year, imports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)
live <- merge(live,
              live_exp[, .(area_code, area, item_code, item, year, exports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)

# add trade values from btd in case they are missing in live_trad
live <- merge(
  live,
  imps[item_code %in% unique(live[, item_code]) & value > 0,  # these are in heads as well
       c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
live <- merge(
  live,
  exps[item_code %in% live[, item_code] & value > 0,
       c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]

# estimate livestock raised in a country (slaughtered + exported - imported)
live[, production := na_sum(production, exports, -imports)]
live[production < 0, production := 0]

# compute total supply
live[, supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
live[, exports := ifelse(exports > supply, supply, exports)]

# Allocate uses
# all uses of meat animals are assumed to go to 'processing'
live[!item %in% tgt_name, processing := na_sum(production, imports, -exports)]
# all uses of draft and sport animals are allocated to 'other'
live[item %in% tgt_name, other := na_sum(production, imports, -exports)]

cbs <- dplyr::bind_rows(cbs, live)

rm(src_item, tgt_item, tgt_name, conc, live, live_trad)


# Estimate cbs for meat and other livestock-based items/countries not included -------------------------------------

live <- readRDS("data/tidy/live_tidy.rds")

live <- live[element == "Production" & unit == "tonnes" & year %in% years, ]
live[, `:=`(element = NULL, unit = NULL)]

# Map "Meat, ..." items AND non-food livestock items ("Hides and skins", "Wool", "Silk") to CBS items
src_item <- c(1035,1089,1097,1108,1111,1127,
              1141,1151,1158,1163,1164,1166,1172,1176,
              1806,1807,1808,
              919, 957, 995, 999, 1025,
              987, 1185, 1186)
tgt_item <- c(2733,2735,2735,2735,2735,2735,2735,
              2735,2735,2735,2735,2735,2735,2735,
              2731,2732,2734,
              2748, 2748, 2748, 2748, 2748,
              2746, 2747, 2747)
tgt_name <- c("Pigmeat","Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other",
              "Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other",
              "Bovine Meat","Mutton & Goat Meat","Poultry Meat",
              "Hides and skins", "Hides and skins", "Hides and skins", "Hides and skins", "Hides and skins",
              "Wool (Clean Eq.)", "Silk", "Silk")
conc <- match(live$item_code, src_item)
live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live <- live[!is.na(item_code), ]
live[, `:=`(production = value, value = NULL)]
live <- live[, .(production = sum(production, na.rm = TRUE)), by = setdiff(names(live), "production")]


# Add trade data
live_trad <- readRDS("data/tidy/live_tidy.rds")
live_trad <- live_trad[element %in% c("Import quantity", "Export quantity") & unit == "tonnes" & year %in% years, ]
conc_trad <- match(live_trad$item_code, src_item)
live_trad[, `:=`(item_code = tgt_item[conc_trad], item = tgt_name[conc_trad])]
live_trad <- live_trad[!is.na(item_code), ]
live_trad <- live_trad[, .(value = sum(value, na.rm = TRUE)), by = setdiff(names(live_trad), "value")]

live_imp <- live_trad[element == "Import quantity", ]
live_exp <- live_trad[element == "Export quantity", ]

live <- merge(live[, .(area_code, area, item_code, item, year, production)],
              live_imp[, .(area_code, area, item_code, item, year, imports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)
live <- merge(live,
              live_exp[, .(area_code, area, item_code, item, year, exports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)


# add trade values from btd in case they are missing in live_trad
live <- merge(
  live,
  imps[item_code %in% unique(live[, item_code]) & value > 0,
       .(to_code, to, item_code, item, year, value)],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]

live <- merge(
  live,
  exps[item_code %in% unique(live[, item_code]) & value > 0,
       .(from_code, from, item_code, item, year, value)],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all = TRUE)
live[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]


# Filter countries and items that are not yet in CBS
live <- live[!paste(area_code, item_code, year) %in% paste(cbs$area_code, cbs$item_code, cbs$year) & year %in% years]# & !is.na(production)]

live[, supply := na_sum(production, imports)]
# reduce exports where they exceed total supply
live[, exports := ifelse(exports > supply, supply, exports)]
# add use to other for hides and skins, silk and wool, and to unspecified for all others
live[, other := ifelse(item_code %in% c(2748,2747,2746), na_sum(production, imports, -exports), 0)]
live[, food := ifelse(item_code %in% c(2748,2747,2746), 0, na_sum(production, imports, -exports))]
live[, balancing := 0]

live <- live[round(supply) > 0, ]

# Add to CBS ---
cat("\nAdding ", nrow(live), " missing cbs accounts for meat and non-food livestock items.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, live)



# Fill Ethanol -----------------------------

cat("\nAdding ethanol production data to CBS.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")[year %in% years]

# Keep one unit and recode for merging
eth <- eth[, `:=`(unit = NULL,
  item = "Alcohol, Non-Food", item_code = 2659)]

eth_cbs <- merge(cbs[item == "Alcohol, Non-Food", ], eth, all = TRUE,
  by = c("area_code", "area", "year", "item", "item_code"))

cat("Using EIA/IEA ethanol production values where FAO's",
  "CBS are not (or under-) reported.\n")
eth_cbs[production < value | is.na(production), production := value]
eth_cbs[, value := NULL]

# compute total supply
eth_cbs[, supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
eth_cbs[(exports + other) > (supply + stock_withdrawal), exports := supply + stock_withdrawal - other]
eth_cbs[exports < 0, exports := 0]
# all supply is assumed to go to other uses
eth_cbs[, other := na_sum(production, imports, -exports, -stock_addition)]
# rebalance
eth_cbs[, balancing := na_sum(production, imports, -exports, -stock_addition, -other)]

cbs <- rbindlist(list(cbs[item_code != 2659, ], eth_cbs[year %in% years]), use.names = TRUE)
rm(eth, eth_cbs)


# Create RoW --------------------------------------------------------------

# remove regions "Unspecified" and "Others (adjustment)"
cbs <- cbs[! area %in% c("Unspecified", "Others (adjustment)")]

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[current == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
           by = c("area_code", "area", "item_code", "item", "year")]


# Add BTD data ------------------------------------------------------------

cat("\nAdding missing export and import data from BTD.\n")

cat("\nGiving preference to units in the following order:\n",
    "\t 'An' > '1000 An' > 'tonnes'\n", "Dropping 'usd'.\n", sep = "")

# Imports
imps <- btd[!unit %in% c("usd"), list(value = round(na_sum(value))),
            by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
                          value.var = "value")
imps[, `:=`(value = ifelse(!is.na(An), An, ifelse(!is.na(`1000 An`), `1000 An`, tonnes)),
            An = NULL, `1000 An` = NULL, tonnes = NULL)]

# Exports
exps <- btd[!unit %in% c("usd"), list(value = round(na_sum(value))),
            by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
                          value.var = "value")
exps[, `:=`(value = ifelse(!is.na(An), An, ifelse(!is.na(`1000 An`), `1000 An`, tonnes)),
            An = NULL, `1000 An` = NULL, tonnes = NULL)]

cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(
  imports = ifelse(is.na(imports) | area_code == 999 | (!is.na(value) & imports == 0),
                   value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports) | area_code == 999 | (!is.na(value) & exports == 0),
                          value, exports), value = NULL)]
rm(imps, exps)



# Rebalance and tidy cbs ----------------------------------------------
# Round to 0 digits
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "balancing",
         "residuals", "tourist", "stock_addition", "stock_withdrawal")
cbs[, (cols) := lapply(.SD, function(x) round(x, 0)), .SDcols = cols]

# Replace negative values with '0'
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("imports", "exports", "feed", "food", "losses", "stock_addition",
                           "other", "processing", "production", "seed", "tourist", "stock_withdrawal"))

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -tourist, -residuals, -stock_addition)]

# calculate supply and use
cbs[, `:=`(domestic_supply = na_sum(production, stock_withdrawal))]
cbs[, `:=`(supply = na_sum(domestic_supply, imports))]
cbs[, `:=`(domestic_use = na_sum(food, feed, other, tourist, seed, losses, processing, stock_addition))]
cbs[, `:=`(use = na_sum(domestic_use, exports))]


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_intermediate.rds")
