
library("data.table")
library("Matrix")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

# CBS ---------------------------------------------------------------------

cat("\nBuilding full CBS.\n")

cbs <- readRDS("data/tidy/cbs_tidy.rds")

cat("Removing items from CBS that are not used in the FAO-MRIO:\n\t",
  paste0(unique(cbs[!item_code %in% items$item_code, item]),
    sep = "", collapse = "; "), ".\n", sep = "") # Eggs and Milk are duplicated with different codes (2948, 2949)
# Particularly fish and aggregates
cbs <- dt_filter(cbs, item_code %in% items$item_code)


# Prepare BTD data --------------------------------------------------------

cat("\nAdding information from BTD.\n")

btd <- readRDS("data/tidy/btd_full_tidy.rds")

cat("\nGiving preference to units in the following order:\n",
  "\t 'head' > 'tonnes'\n", "Dropping 'usd'.\n", sep = "")

# Imports
imps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(to_code, to, item_code, item, year, unit)]
imps <- data.table::dcast(imps, to_code + to + item_code + item + year ~ unit,
  value.var = "value")
imps[, `:=`(value = ifelse(!is.na(head), head, tonnes),
  head = NULL, tonnes = NULL)]

# Exports
exps <- btd[!unit %in% c("usd"), list(value = na_sum(value)),
  by = list(from_code, from, item_code, item, year, unit)]
exps <- data.table::dcast(exps, from_code + from + item_code + item + year ~ unit,
  value.var = "value")
exps[, `:=`(value = ifelse(!is.na(head), head, tonnes),
  head = NULL, tonnes = NULL)]


# # Forestry ----------------------------------------------------------------
#
# cat("\nAdding forestry production data.\n")
#
# fore <- readRDS("data/tidy/fore_prod_tidy.rds")
#
# fore[, `:=`(total_supply = na_sum(production, imports),
#   other = na_sum(production, imports, -exports),
#   stock_withdrawal = 0, stock_addition = 0,
#   feed = 0, food = 0, losses = 0, processing = 0,
#   seed = 0, balancing = 0)]
# fore[other < 0, `:=`(balancing = other, other = 0)]
#
# cbs <- rbindlist(list(cbs, fore), use.names = TRUE)
# rm(fore)


# Add items that are now contained in SUA ---------------------

# so far, we only use palm fruit and palm kernels (palm kernels is included as a category in fbs but mistaken for fruit in fbs)
sua <- readRDS("data/tidy/sua_tidy.rds")
sua_items <- c("Oil palm fruit", "Palm kernels")
sua <- sua[item %in% sua_items,]

conc_sua_fabio <- data.frame(item_sua = sua_items, item = c("Oil, palm fruit", "Palm kernels"), item_code_sua = c(254, 256), item_code = c(254, 2562))

# assign FABIO codes
sua[, `:=`(item_code = conc_sua_fabio$item_code[match(item_code_fcl, conc_sua_fabio$item_code_sua)],
           item = conc_sua_fabio$item[match(item_code_fcl, conc_sua_fabio$item_code_sua)],
           item_code_fcl = NULL)]
# we take only years after 2013
sua <- sua[year > 2013, ]

# remove palm kernels in cbs after 2013 and bind sua data to cbs
cbs <- cbs[!(item == "Palm kernels" & year > 2013),]
cbs <- rbind(cbs, sua, use.names = TRUE)

# Fill crop production where missing -------------------------------------

cat("\nAdding crop data.\n")

crop <- readRDS("data/tidy/crop_tidy.rds")

crop_prod <- crop[element == "Production" & unit == "tonnes", ]
crop_prod[, `:=`(element = NULL, unit = NULL)]
setkey(crop_prod, year, area_code, item_code)

# Add production to CBS where is.na or is zero ---
cbs <- merge(cbs, crop_prod,
             by = c("area_code", "area", "item_code", "item", "year"), all.x = TRUE)
cat("\nFilling missing cbs production with crop production data. Items:\n",
    paste0(unique(cbs[(is.na(production) | production == 0) & !is.na(value), item]), collapse = "; "),
    ".\n", sep = "")
cbs[!is.na(value) & (is.na(production) | production == 0), production := value]
cbs[, value := NULL]


# Estimate cbs for items/countries not included -------------------------------------

# Filter countries and items that are not yet in CBS
addcbs <- dt_filter(crop_prod[item_code %in% items$item_code],
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
years <- sort(unique(tcf_data$year))
areas <- sort(unique(tcf_data$area_code))

# Base processing on production + imports - exports
tcf_data[imps[, .(year, area_code = to_code, item_code, imports = value)],
  on = c("area_code", "item_code", "year"), imports := imports]
tcf_data[exps[, .(year, area_code = from_code, item_code, exports = value)],
  on = c("area_code", "item_code", "year"), exports := exports]

# Production of items
output <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[1]])))]
output[, `:=`(value = production,
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(output, is.na, 0, cols = "value")
# Production of source items
input <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = unique(tcf_codes[[2]])))]
input[, `:=`(value = na_sum(production, imports, -exports),
  production = NULL, imports = NULL, exports = NULL)]
dt_replace(input, function(x) {`<`(x, 0)}, value = 0, cols = "value")
dt_replace(input, is.na, 0, cols = "value")
# Processing of source items - to fill
results <- tcf_data[data.table(expand.grid(year = years,
  area_code = areas, item_code = tcf_codes[[2]]))]
setkey(results, year, area_code, item_code)
results[, `:=`(value = NA_real_,
  production = NULL, imports = NULL, exports = NULL)]

# Fill during a loop over years and areas
for(x in years) {
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
addcbs[, total_supply := na_sum(production,imports)]
# allocate processing inputs to 'processing'
addcbs[, processing := results$processing[match(paste(addcbs$year, addcbs$area_code, addcbs$item_code),
                                                paste(results$year, results$area_code, results$item_code))]]
# Allocate 'Seed cotton', 'Oil, palm fruit', 'Hops', 'Sugar cane' and 'Palm kernels' supply to processing
addcbs[item_code %in% c(254, 328, 677, 2536, 2562), processing := na_sum(total_supply, -exports)]

# add 'Cotton lint', 'Jute', 'Jute-Like Fibres', 'Soft-Fibres', 'Other Sisal', 'Abaca',
# 'Hard Fibres, Other', 'Tobacco', and 'Rubber' to other uses
addcbs[item_code %in% c(2662,2663,2664,2665,2666,2667,2671,2672), other := na_sum(total_supply, -exports)]

# Allocate 'Fodder crops' supply to feed
addcbs[item_code %in% c(2000), feed := na_sum(total_supply, -exports)]

# allocate rest to 'unspecified'
addcbs[, unspecified := na_sum(total_supply,-processing, -other, -feed, -exports)]
addcbs[, balancing := 0]
addcbs[unspecified < 0, `:=`(balancing = unspecified, unspecified = 0)]

# we only use data until 2019
addcbs <- addcbs[year <= 2019,]

# cat("\nFilling missing cbs seed with crop seed data.\n")
# crop_seed <- crop[element == "Seed", ]
cat("\nSkip filling cbs seed.",
  "Apparently data on seed is not reported in newer faostat versions.\n")

# Add to CBS ---
cat("\nAdding ", nrow(addcbs), " missing cbs accounts.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, addcbs)

rm(crop_prod, addcbs,
  tcf_crop, tcf_codes, tcf_data, input, output, results, years, areas,
  C, input_x, output_x, input_y, output_y)


# Oilcrop cakes after 2013: impute production from primary crop processing use / oil production ------------

crp_item <- c(2555,2552,2557,2558,2559,2562,2560,2561,2570)
crp_name <- c("Soyabeans", "Groundnuts", "Sunflower seed", "Rape and Mustardseed", "Cottonseed", "Palm kernels", "Coconuts - Incl Copra", "Sesame seed", "Oilcrops, Other")
cak_item <- c(2590,2591,2592,2593,2594,2595,2596,2597,2598)
cak_name <- c("Soyabean Cake", "Groundnut Cake", "Sunflowerseed Cake", "Rape and Mustard Cake", "Cottonseed Cake", "Palmkernel Cake", "Copra Cake", "Sesameseed Cake", "Oilseed Cakes, Other")
oil_item <- c(2571,2572,2573,2574,2575,2576,2578,2579,2586)
oil_name <- c("Soyabean Oil", "Groundnut Oil", "Sunflowerseed Oil", "Rape and Mustard Oil", "Cottonseed Oil", "Palmkernel Oil", "Coconut Oil", "Sesameseed Oil", "Oilcrops Oil, Other")

# extract oilcrop prcessing and cake + oil production
cbs_crp_proc <- cbs[item_code %in% crp_item, .(area_code, area, item_code, item, year, processing)]
cbs_cak_prod <- cbs[item_code %in% cak_item, .(area_code, area, item_code, item, year, production)]
cbs_oil_prod <- cbs[item_code %in% oil_item, .(area_code, area, item_code, item, year, production)]

# match cake production with crop processing and oil output
conc <- match(cbs_cak_prod$item_code, cak_item)
cbs_cak_prod[, `:=`(source_item = crp_item[conc], source_name = crp_name[conc],
               oil_item = oil_item[conc], oil_name = oil_name[conc])]
cbs_cak_prod <- merge(cbs_cak_prod, cbs_crp_proc,
                 by.x = c("area_code", "area", "source_item", "source_name", "year"),
                 by.y = c("area_code", "area", "item_code", "item", "year"),
                 all.x = TRUE)
cbs_cak_prod <- merge(cbs_cak_prod, cbs_oil_prod[,.(area_code, area, item_code, item, year, oil_production = production)],
                 by.x = c("area_code", "area", "oil_item", "oil_name", "year"),
                 by.y = c("area_code", "area", "item_code", "item", "year"),
                 all.x = TRUE)
cbs_cak_prod <- cbs_cak_prod[production > 0,]
#cbs_cak_prod <- cbs_cak_prod[production == 0, production := NA]

# compute annual conversion factors / cake-to-oil factors
cbs_cak_prod[, `:=`(tcf = ifelse(is.finite(production / processing), production / processing, NA),
               cof = ifelse(is.finite(production / oil_production), production / oil_production, NA))]
# compute averages across all available years (using only post-2000 values to account for technical changes)
cak_conv <- cbs_cak_prod[year %in% 2000:2013, .(tcf = mean(tcf, na.rm = TRUE), cof = mean(cof, na.rm = TRUE)), by = .(area_code, area, item_code, item, source_item, source_name, oil_item, oil_name)]
# fill NAs with global averages
cak_conv_glob <- cak_conv[, .(tcf_global = mean(tcf, na.rm = TRUE),
                              cof_global = mean(cof, na.rm = TRUE)),
                          by = .(item_code)]
#cak_conv[, `:=` (tcf_global = mean(tcf, na.rm = TRUE), cof_global = mean(cof, na.rm = TRUE)), by = .(item_code, item, source_item, source_name, oil_item, oil_name)]
cak_conv <- merge(cak_conv, cak_conv_glob, by = c("item_code"))
cak_conv[, `:=` (tcf = ifelse(is.na(tcf), tcf_global, tcf),
                 cof = ifelse(is.na(cof), cof_global, cof))]
cak_conv[,`:=` ( tcf_global = NULL, cof_global = NULL)]
# NOTE: if we want derive cake production also for countries who never had cake production before 2013, we need an addition step tp fill tcf/cof witj global averages for these cases!

# apply factors to cbs in following order: if processing > 0, use tcf, and if processing = 0 but oil production > 0, use cof
yrs <- 2014:2019
#cak_base <- cbs_crp_proc[, ]
#cbs_cak <- merge(cak_conv, data.table(year = 2014:2019), by = NULL)
cake <- cbind(cak_conv[rep(1:nrow(cak_conv), each = length(yrs))], year = yrs[rep(1:length(yrs), nrow(cak_conv))])
cake <- merge(cake, cbs_crp_proc[processing > 0,],
                 by.x = c("area_code", "area", "source_item", "source_name", "year"),
                 by.y = c("area_code", "area", "item_code", "item", "year"),
                 all.x  = TRUE) # all.y = TRUE
cake <- merge(cake, cbs_oil_prod[production > 0,][,.(area_code, area, item_code, item, year, oil_production = production)],
                 by.x = c("area_code", "area", "oil_item", "oil_name", "year"),
                 by.y = c("area_code", "area", "item_code", "item", "year"),
                 all.x = TRUE) #  all.y = TRUE
cake[, `:=` (processing = ifelse(is.na(processing), 0, processing),
                oil_production = ifelse(is.na(oil_production), 0, oil_production)) ]
# filter relevant years for extrapolation
cake <- cake[year > 2013,]
# estimate processing use if no or too little is reported (for all oil crops except oil crops, other)
cake[item_code != 2598 & processing == 0, processing := oil_production / (1-tcf-0.03)]
# apply factors
cake[, production := processing * tcf]

# # TODO: in case oil + cake production exceed source processing input, correct via cof
# # discuss if we should adjust cbs for this!
# cake[(production + oil_production) > processing, oil_production := production / cof]
# cake[(production + oil_production) > processing, processing := production + oil_production]


# add trade
cak_imp <- crop[element == "Import Quantity" & unit == "tonnes" & item_code %in% cak_item & year %in% yrs, ]
cak_exp <- crop[element == "Export Quantity" & unit == "tonnes" & item_code %in% cak_item & year %in% yrs, ]
cak_imp[, `:=`(element = NULL, unit = NULL)]
cak_exp[, `:=`(element = NULL, unit = NULL)]

cake <- merge(cake[, .(area_code, area, item_code, item, year, production)],
              cak_imp[, .(area_code, area, item_code, item, year, imports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)
cake <- merge(cake,
              cak_exp[, .(area_code, area, item_code, item, year, exports = value)],
              by = c("area_code", "area", "item_code", "item", "year"),
              all = TRUE)

# add trade values from btd in case they are missing in live_trad
cake <- merge(
  cake,
  imps[item_code %in% cak_item & value > 0 & year %in% yrs,  # these are in heads as well
       c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all = TRUE)
cake[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cake <- merge(
  cake,
  exps[item_code %in% cak_item & value > 0 & year %in% yrs,
       c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all = TRUE)
cake[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]

# compute total supply
cake[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
cake[, exports := ifelse(exports > total_supply, total_supply, exports)]
# all uses are assumed to go to feed
# TODO: check if appropriate! in a few cases, cakes are also used for "other"
cake[, feed := na_sum(production, imports, -exports)]
cake <- cake[total_supply >= 0, ]

# Add to CBS ---
cat("\nAdding ", nrow(cake), " missing oilseed cake accounts.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, cake)

rm(crp_item, crp_name, cak_item, cak_name, oil_item, oil_name,
   cbs_cak_prod, cbs_crp_proc, cbs_oil_prod,
   cak_conv, cak_conv_glob, yrs,
   cak_imp, cak_exp, cake)


# Fill livestock -----------------------------

# NOTE: this creates cbs for live animals not contained in cbs

cat("\nFilling missing livestock data.\n")

# live <- readRDS("data/tidy/live_tidy.rds")
#
# live <- live[element == "Production" & unit == "head", ] # this is empty!!
# live[, `:=`(element = NULL, unit = NULL)]


# # Map "Meat indigenous, ..." items to CBS items
# --> no longer present in production data! Replace by computing production + imp - exp
#src_item <- c(1137, 944, 1032, 1012, 1775, 1055, 1120, 1144,
#   972, 1161, 1154, 1122, 1124)
tgt_item <- c(1126, 866, 1016, 976, 2029, 1034, 1096, 1140,
   946, 1157, 1150, 1107, 1110)
tgt_name <- c("Camels", "Cattle", "Goats", "Sheep", "Poultry Birds", "Pigs",
   "Horses", "Rabbits and hares", "Buffaloes", "Camelids, other",
   "Rodents, other", "Asses", "Mules")
#conc <- match(live$item_code, src_item)
#live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
#live <- live[!is.na(item_code), ]

# Map "Meat, ..." items to CBS items
src_item <- c(1127, 867, 1017, 977, 1808, 1035, 1097, 1141,
  947, 1158, 1151, 1108, 1111)

live <- readRDS("data/tidy/live_tidy.rds")
live <- live[element == "Producing Animals/Slaughtered" &
  unit == "head", ]
live[, `:=`(element = NULL, unit = NULL)]

conc <- match(live$item_code, src_item)
live[, `:=`(item_code = tgt_item[conc], item = tgt_name[conc])]
live <- live[!is.na(item_code), ]

#live <- merge(live, live_alt,
#  by = c("area_code", "area", "year", "item_code", "item"), all = TRUE)
#live[ , `:=`(value =
#  ifelse(is.na(value.x), value.y, ifelse(value.x == 0, value.y, value.x)),
#  value.x = NULL, value.y = NULL)]

# Add trade data
live_trad <- readRDS("data/tidy/live_tidy.rds")

live_imp <- live_trad[element == "Import Quantity" & unit == "head" & item_code %in% items$item_code, ]
live_exp <- live_trad[element == "Export Quantity" & unit == "head" & item_code %in% items$item_code, ]

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

# additional necessary step in v1.2: adjust production as production + exp - imp
live[, production := na_sum(production, exports, -imports)]
live[production < 0, production := 0]

# compute total supply
live[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
live[, exports := ifelse(exports > total_supply, total_supply, exports)]
# all uses are assumed to go to processing
live[, processing := na_sum(production, imports, -exports)]
#live <- live[total_supply > 0, ]

cbs <- dplyr::bind_rows(cbs, live)

rm(src_item, tgt_item, tgt_name, conc, live, live_trad)


# Estimate cbs for meat and other livestock-based items/countries not included -------------------------------------

live <- readRDS("data/tidy/live_tidy.rds")

live <- live[element == "Production" & unit == "tonnes", ]
live[, `:=`(element = NULL, unit = NULL)]

# Map "Meat, ..." items AND non-food livestock items ("Hides and skins", "Wool", "Silk") to CBS items
src_item <- c(1035,1097,1108,1111,1127,
              1141,1151,1158,1163,1166,
              1806,1807,1808,
              919, 957, 995, 999, 1025,
              987, 1185, 1186)
tgt_item <- c(2733,2735,2735,2735,2735,
              2735,2735,2735,2735,2735,
              2731,2732,2734,
              2748, 2748, 2748, 2748, 2748,
              2746, 2747, 2747)
tgt_name <- c("Pigmeat","Meat, Other","Meat, Other","Meat, Other","Meat, Other",
              "Meat, Other","Meat, Other","Meat, Other","Meat, Other","Meat, Other",
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
live_trad <- live_trad[element %in% c("Import Quantity", "Export Quantity") & unit == "tonnes", ]
conc_trad <- match(live_trad$item_code, src_item)
live_trad[, `:=`(item_code = tgt_item[conc_trad], item = tgt_name[conc_trad])]
live_trad <- live_trad[!is.na(item_code), ]
live_trad <- live_trad[, .(value = sum(value, na.rm = TRUE)), by = setdiff(names(live_trad), "value")]

live_imp <- live_trad[element == "Import Quantity", ]
live_exp <- live_trad[element == "Export Quantity", ]

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
live <- live[!paste(area_code, item_code, year) %in% paste(cbs$area_code, cbs$item_code, cbs$year) & year <= 2019]# & !is.na(production)]

live[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply
live[, exports := ifelse(exports > total_supply, total_supply, exports)]
# add use to other for hides and skins, silk and wool, and to unspecified for all others
live[, other := ifelse(item_code %in% c(2748,2747,2746), na_sum(production, imports, -exports), 0)]
live[, unspecified := ifelse(item_code %in% c(2748,2747,2746), 0, na_sum(production, imports, -exports))]
live[, balancing := 0]

live <- live[total_supply > 0, ]

# Add to CBS ---
cat("\nAdding ", nrow(live), " missing cbs accounts for meat and non-food livestock items.\n", sep = "")
cbs <- dplyr::bind_rows(cbs, live)



# Fill Ethanol -----------------------------

cat("\nAdding ethanol production data to CBS.\n")

eth <- readRDS("data/tidy/eth_tidy.rds")

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
eth_cbs[, total_supply := na_sum(production, imports)]
# reduce exports where they exceed total supply (per definition not the case any more!)
eth_cbs[(exports + other) > (total_supply + stock_withdrawal), exports := total_supply + stock_withdrawal - other]
eth_cbs[exports < 0, exports := 0]
# all supply is assumed to go to other uses
eth_cbs[, other := na_sum(production, imports, -exports, -stock_addition)]
# rebalance
eth_cbs[, balancing := na_sum(production, imports, -exports, -stock_addition, -other)]

cbs <- rbindlist(list(cbs[item_code != 2659, ], eth_cbs), use.names = TRUE)
rm(eth, eth_cbs)



# Add BTD data ------------------------------------------------------------

cat("\nAdding missing export and import data to CBS from BTD.\n")
cbs <- merge(
  cbs, imps[, c("to_code", "to", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("to_code", "to", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(imports = ifelse(is.na(imports), value, imports), value = NULL)]
cbs <- merge(
  cbs, exps[, c("from_code", "from", "item_code", "item", "year", "value")],
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[, `:=`(exports = ifelse(is.na(exports), value, exports), value = NULL)]
rm(imps, exps)


# Create RoW --------------------------------------------------------------

# remove regions "Unspecified" and "Others (adjustment)"
cbs <- cbs[! area %in% c("Unspecified", "Others (adjustment)")]

# Aggregate RoW countries in CBS
cbs <- replace_RoW(cbs, codes = regions[cbs == TRUE, code])
cbs <- cbs[, lapply(.SD, na_sum),
  by = c("area_code", "area", "item_code", "item", "year")]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
  codes = c(regions[cbs == TRUE, code], 252, 254))
btd <- btd[, lapply(.SD, na_sum), by = c("from_code", "from",
  "to_code", "to", "comm_code", "item_code", "item", "unit", "year")]

# Remove ROW-internal trade from CBS
intra <- btd[from_code==to_code & unit!="usd", sum(value), by=c("from_code","from","item_code","item","year")]
cbs <- merge(cbs, intra,
  by.x = c("area_code", "area", "item_code", "item", "year"),
  by.y = c("from_code", "from", "item_code", "item", "year"),
  all.x = TRUE)
cbs[!is.na(V1), `:=`(exports = na_sum(exports,-V1),
  imports = na_sum(imports,-V1))]
cbs[, V1 := NULL]
rm(intra)

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)


# Rebalance columns -------------------------------------------------------

# TODO: this could be improved! Decide how to use residuals here.

cat("\nRebalance CBS.\n")

# Round to whole numbers
cols = c("imports", "exports", "feed", "food", "losses", "other",
         "processing", "production", "seed", "balancing", "unspecified",
         "residuals", "tourist")
cbs[, (cols) := lapply(.SD, round), .SDcols = cols]

# Replace negative values with '0'
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
  cols = c("imports", "exports", "feed", "food", "losses",
    "other", "processing", "production", "seed", "tourist"))
# TODO: should tourist negatives be eleiminated or not? (just a few minor cases)

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
        -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified, -tourist, -residuals)]

# TODO: For now, I move negative residuals to balancing and then neutralize negative balancing via other elements
# in the future, we might consider to keep them as completely separate categories, or merge them completely
# in any case, the sum of balancing and residuals should never be <0 in the end
# as long as we do not introduce an "unknown source" region, we need to adapt the other cbs items accordingly

# move negative residuals to balancing
cbs[residuals < 0, `:=`(balancing = na_sum(residuals, balancing), residuals = 0)]

# neutralize negative balancing via other items
cat("\nAdjust 'residuals' for ", cbs[balancing < 0 &
                                     !is.na(residuals) & residuals >= -balancing*0.99, .N],
    " observations, where `balancing < 0` and `residuals >= -balancing` to ",
    "`residuals = residuals - balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(residuals) & residuals >= -balancing*0.99,
    `:=`(residuals = na_sum(residuals, balancing),
         balancing = 0)]


cat("\nAdjust 'exports' for ", cbs[balancing < 0 &
    !is.na(exports) & exports >= -balancing*0.99, .N],
    " observations, where `balancing < 0` and `exports >= -balancing` to ",
    "`exports = exports - balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(exports) & exports >= -balancing*0.99,
    `:=`(exports = na_sum(exports, balancing),
         balancing = 0)]
cbs[exports < 0, `:=`(balancing = exports,
                      exports = 0)]

cat("\nAdjust 'processing' for ", cbs[balancing < 0 &
    !is.na(processing) & processing >= -balancing, .N],
    " observations, where `balancing < 0` and `processing >= -balancing` to ",
    "`processing = processing + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(processing) & processing >= -balancing,
    `:=`(processing = na_sum(processing, balancing),
         balancing = 0)]

cat("\nAdjust 'other' for ", cbs[balancing < 0 &
    !is.na(other) & other >= -balancing, .N],
    " observations, where `balancing < 0` and `other >= -balancing` to ",
    "`other = other + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(other) & other >= -balancing,
    `:=`(other = na_sum(other, balancing),
         balancing = 0)]

cat("\nAdjust 'tourist' for ", cbs[balancing < 0 &
    !is.na(tourist) & tourist >= -balancing, .N],
    " observations, where `balancing < 0` and `tourist >= -balancing` to ",
    "`tourist = tourist + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(other) & other >= -balancing,
    `:=`(other = na_sum(other, balancing),
         balancing = 0)]

cat("\nAdjust 'unspecified' for ", cbs[balancing < 0 &
    !is.na(unspecified) & unspecified >= -balancing, .N],
    " observations, where `balancing < 0` and `unspecified >= -balancing` to ",
    "`unspecified = unspecified + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(unspecified) & unspecified >= -balancing,
    `:=`(unspecified = na_sum(unspecified, balancing),
         balancing = 0)]

cat("\nAdjust 'stock_addition' for ", cbs[balancing < 0 &
    !is.na(stock_addition) & stock_addition >= -balancing, .N],
    " observations, where `balancing < 0` and `stock_addition >= -balancing` to ",
    "`stock_addition = stock_addition + balancing`.\n", sep = "")
cbs[balancing < 0 & !is.na(stock_addition) & stock_addition >= -balancing,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

cat("\nAdjust uses proportionally for ", cbs[balancing < 0, .N],
    " observations, where `balancing < 0`", sep = "")
# TODO: include residuals here?
cbs[, divisor := na_sum(exports, other, processing, seed, food, feed, stock_addition, tourist)]
cbs[balancing < 0 & divisor >= -balancing,
    `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
         processing = round(na_sum(processing, (balancing / divisor * processing))),
         exports = round(na_sum(exports, (balancing / divisor * exports))),
         other = round(na_sum(other, (balancing / divisor * other))),
         seed = round(na_sum(seed, (balancing / divisor * seed))),
         food = round(na_sum(food, (balancing / divisor * food))),
         feed = round(na_sum(feed, (balancing / divisor * feed))),
         tourist = round(na_sum(tourist, (balancing / divisor * tourist))),
         balancing = 0)]
cbs[, `:=`(stock_withdrawal = -stock_addition,
           divisor = NULL)]

# Rebalance table
cbs[, balancing := na_sum(production, imports, stock_withdrawal,
                          -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified, -tourist, -residuals)]

# Attribute rest (resulting from rounding differences) to stock changes
cbs[balancing < 0,
    `:=`(stock_addition = na_sum(stock_addition, balancing),
         stock_withdrawal = na_sum(stock_withdrawal, -balancing),
         balancing = 0)]

# Adjust 'total_supply' to new production values
cbs[, total_supply := na_sum(production, imports)]

cat("\nSkip capping 'exports', 'seed' and 'processing' at",
  "'total_supply + stock_withdrawal'.\n")


# # Balance CBS imports and exports -------------------------------------------------------
# # --> This balancing step was moved to script 05_balance.R
#
# # Adjust CBS to have equal export and import numbers per item per year
# # This is very helpful for the iterative proportional fitting of bilateral trade data
# cbs_bal <- cbs[, list(exp_t = sum(exports, na.rm = TRUE), imp_t = sum(imports, na.rm = TRUE)),
#                by = c("year", "item_code", "item")]
# cbs_bal[, `:=`(diff = na_sum(exp_t, -imp_t), exp_t = NULL, imp_t = NULL,
#                area_code = 999, area = "RoW")]
# # Absorb the discrepancies in "RoW"
# cbs <- merge(cbs, cbs_bal,
#              by = c("year", "item_code", "item", "area_code", "area"), all = TRUE)
# cbs[area_code == 999, `:=`(
#   exports = ifelse(diff < 0, na_sum(exports, -diff), exports),
#   imports = ifelse(diff > 0, na_sum(imports, diff), imports))]
# cbs[, diff := NULL]
#
# rm(cbs_bal); gc()
#
# # Rebalance RoW
# cbs[, balancing := na_sum(production, imports, stock_withdrawal,
#                           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified)]
#
# cbs[, divisor := na_sum(other, processing, seed, food, feed, stock_addition, unspecified)]
# cbs[balancing < 0 & divisor >= -balancing,
#     `:=`(stock_addition = round(na_sum(stock_addition, (balancing / divisor * stock_addition))),
#          unspecified = round(na_sum(unspecified, (balancing / divisor * unspecified))),
#          processing = round(na_sum(processing, (balancing / divisor * processing))),
#          other = round(na_sum(other, (balancing / divisor * other))),
#          seed = round(na_sum(seed, (balancing / divisor * seed))),
#          food = round(na_sum(food, (balancing / divisor * food))),
#          feed = round(na_sum(feed, (balancing / divisor * feed))),
#          balancing = 0)]
# cbs[, `:=`(stock_withdrawal = -stock_addition, divisor = NULL,
#            balancing = na_sum(production, imports, stock_withdrawal,
#           -exports, -food, -feed, -seed, -losses, -processing, -other, -unspecified))]
# cbs[balancing < 0,
#     `:=`(production = na_sum(production, -balancing),
#          balancing = 0)]
# cbs[, total_supply := na_sum(production, imports)]



cat("\nAllocate remaining supply from 'unspecified' and 'balancing' to uses.\n")

cat("\nHops, oil palm fruit, palm kernels, sugar crops and live animals to 'processing'.\n")
cbs[item_code %in% c(254, 328, 677, 866, 946, 976, 1016, 1034, 2029, 1096, 1107, 1110,
  1126, 1157, 1140, 1150, 1171, 2536, 2537, 2562) & na_sum(unspecified, balancing) > 0,
  `:=`(processing = na_sum(processing, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nNon-food crops to 'other'.\n")
cbs[item_code %in% c(2662, 2663, 2664, 2665, 2666, 2667, 2671, 2672, 2659,
  1864, 1866, 1867, 2661, 2746, 2748, 2747) & na_sum(unspecified, balancing) > 0,
  `:=`(other = na_sum(other, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nFeed crops to 'feed'.\n")
cbs[item_code %in% c(2000, 2001, 2555, 2559, 2590, 2591, 2592, 2593, 2594,
  2595, 2596, 2597, 2598, 2749) & na_sum(unspecified, balancing) > 0,
  `:=`(feed = na_sum(feed, unspecified, balancing),
       unspecified = 0, balancing = 0)]

cat("\nRest (mostly 'food', 'feed' and 'processing') remains in 'unspecified' and 'balancing'.\n")


# Save --------------------------------------------------------------------

saveRDS(cbs, "data/cbs_full.rds")
saveRDS(btd, "data/btd_full.rds")
