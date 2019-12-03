
library(data.table)
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


# Colnames ----------------------------------------------------------------

rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value",
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  # After casting
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses" = "other",
  "Processing" = "processing",
  # Units
  # "1000 US$" = "k_usd",
  # "1000 Head" = "k_capita",
  "Head" = "head",
  "tonnes" = "tonnes",
  "Export" = "exports",
  "Import" = "imports",
  # Fish
  "COUNTRY" = "country",
  # "AREA" = "water_area",
  "SOURCE" = "source_code",
  "SPECIES" = "species",
  "YEAR" = "year",
  "UNIT" = "unit",
  "QUANTITY" = "value"
)

# CBS ---------------------------------------------------------------------

cat("\nTidying CBS.\n")

cbs <- rbind(readRDS("input/fao/cbs_crop.rds"),
             readRDS("input/fao/cbs_live.rds"))

cbs <- dt_rename(cbs, rename, drop = TRUE)

# Country / Area adjustments
cbs <- area_kick(cbs, code = 351, pattern = "China", groups = TRUE)
cbs <- area_merge(cbs, orig = 62, dest = 238, pattern = "Ethiopia")
cbs <- area_fix(cbs, regions)

# Widen by element
cbs <- dcast(cbs,
             area_code + area + item_code + item + year ~ element,
             value.var = "value")
cbs <- dt_rename(cbs, rename, drop = FALSE)

# Replace NA values with 0
cbs <- dt_replace(cbs, is.na, value = 0)
# Make sure values are not negative
cbs <- dt_replace(cbs, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("total_supply", "imports", "exports", "feed",
                           "food", "losses", "other", "processing",
                           "production", "seed"))

cat("Recoding 'total_supply' from",
    "'production + imports - exports + stock_withdrawal'", "to",
    "'production + imports'.\n")
cbs[, total_supply := production + imports]

# Add more intuitive 'stock_addition' and fix discrepancies with 'total_supply'
cbs[, stock_addition := -stock_withdrawal]
cat("Found ", cbs[stock_addition > total_supply, .N],
    " occurences of 'stock_addition' exceeding 'total_supply'.\n",
    "Keeping values as is.\n", sep = "")
# cbs[stock_addition > total_supply, stock_addition := total_supply]

# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
cat("\nAdd 'balancing' column for supply and use discrepancies.\n")
cbs[, balancing := total_supply - stock_addition -
        (exports + food + feed + seed + losses + processing + other)]

# Store
saveRDS(cbs, "data/tidy/cbs_tidy.rds")
rm(cbs)


# BTD ---------------------------------------------------------------------

cat("\nTidying BTD.\n")

btd <- readRDS("input/fao/btd_prod.rds")

btd <- dt_rename(btd, rename, drop = TRUE)

# Country / Area adjustments
for(col in c("reporter_code", "partner_code")) {
  btd <- area_kick(btd, code = 351, pattern = "China", groups = TRUE, col = col)
  btd <- area_merge(btd, orig = 62, dest = 238, pattern = "Ethiopia", col = col)
  btd <- area_fix(btd, regions, col = col)
}

# Cut down on the size
btd <- dt_filter(btd, !item_code %in%
                   c("Waters,ice etc" = 631, "Cotton waste" = 769,
                     "Vitamins" = 853, "Hair, goat, coarse" = 1031,
                     "Beehives" = 1181, "Beeswax" = 1183, "Hair, fine" = 1218,
                     "Crude materials" = 1293, "Waxes vegetable" = 1296))
btd <- dt_filter(btd, value > 0)

btd[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]
t
# Apply TCF to observations with 'unit' == "tonnes"
btd <- merge(btd, fread("inst/tcf_btd.csv"),
             by.x = "item_code", by.y = "item_code", all.x = TRUE)
cat("Applying TCF to trade data, where `unit == 'tonnes'` applies.\n")
btd[unit != "tonnes", tcf := 1]
btd <- tcf_apply(btd, na.rm = FALSE, filler = 1, fun = `/`)

# Aggregate to CBS items
btd_conc <- fread("inst/conc_btd-cbs.csv")

cat("Aggregating BTD items to the level of CBS.\n")
item_match <- match(btd[["item_code"]], btd_conc[["btd_item_code"]])
btd[, `:=`(item_code = btd_conc$cbs_item_code[item_match],
           item = btd_conc$cbs_item[item_match])]
btd <- btd[, list(value = sum(value)),
           by = .(reporter_code, reporter, partner_code, partner,
                  item_code, item, year, imex, unit)]
cat("Aggregation from", length(item_match), "to", nrow(btd), "observations.\n")

# Recode "1000 Head" to "head"
btd[unit == "1000 Head", `:=`(value = value * 1000, unit = "Head")]
btd[unit == "Head", `:=`(unit = "head")]
# Recode "1000 US$" to "usd"
btd[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

# 2019-06-07: Keep unit variable
# Widen by unit
# btd <- dcast(btd,
#              reporter_code + reporter + partner_code + partner +
#                item_code + item + year + imex ~ unit,
#              value.var = "value")
# btd <- dt_rename(btd, rename, drop = FALSE)
# btd <- dt_replace(btd, is.na, value = 0)

# Store
saveRDS(btd, "data/tidy/btd_tidy.rds")
rm(btd, btd_conc, item_match)


# Forestry ----------------------------------------------------------------

cat("\nTidying forestry.\n")

#
# Production
fore_prod <- readRDS("input/fao/fore_prod.rds")

fore_prod <- dt_rename(fore_prod, rename, drop = TRUE)

# Country / Area adjustments
fore_prod <- area_kick(fore_prod, code = 351, pattern = "China", groups = TRUE)
fore_prod <- area_merge(fore_prod, orig = 62, dest = 238, pattern = "Ethiopia")
fore_prod <- area_fix(fore_prod, regions)

# Cut down to certain products
fore_prod <- dt_filter(fore_prod, item_code %in%
                         c("Wood fuel" = 1864,
                           "Industrial roundwood, coniferous" = 1866,
                           "Industrial roundwood, non-coniferous" = 1867))
fore_prod <- dt_filter(fore_prod, value > 0)
# fore_prod <- dt_filter(fore_prod, unit != "1000 US$")
# Recode "1000 US$" to "usd"
fore_prod[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

fore_prod[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

# Get this in the format of CBS
fore_prod <- dt_filter(fore_prod, unit == "m3")
fore_prod[, unit := NULL]
fore_prod <- dcast(fore_prod,
                   area_code + area + item_code + item + year ~ imex,
                   value.var = "value")
fore_prod <- dt_rename(fore_prod, rename, drop = FALSE)

# Store
saveRDS(fore_prod, "data/tidy/fore_prod_tidy.rds")
rm(fore_prod)

#
# Trade
fore_trad <- readRDS("input/fao/fore_trad.rds")

fore_trad <- dt_rename(fore_trad, rename)

# Country / Area adjustments
for(col in c("reporter_code", "partner_code")) {
  fore_trad <- area_kick(fore_trad, groups = TRUE, col = col)
  fore_trad <- area_merge(fore_trad, orig = 62, dest = 238,
                          pattern = "Ethiopia", col = col)
  fore_trad <- area_fix(fore_trad, regions, col = col)
}

# Cut down to certain products
fore_trad <- dt_filter(fore_trad, item_code %in%
                         c("Industrial roundwood, coniferous" = 1651,
                           "Industrial roundwood, non-coniferous tropical" = 1657,
                           "Industrial roundwood, non-coniferous non-tropical" = 1670))
# fore_trad <- dt_filter(fore_trad, unit != "m3")
# Recode "1000 US$" to "usd"
fore_trad[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]

fore_trad[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

# Aggregate to forestry production items
fore_conc <- fread("inst/conc_forestry.csv")

cat("Aggregating forestry trade items to the level of forestry production.\n")
item_match <- match(fore_trad[["item_code"]], fore_conc[["trad_item_code"]])
fore_trad[, `:=`(item_code = fore_conc$prod_item_code[item_match],
                 item = fore_conc$prod_item[item_match])]
fore_trad <- fore_trad[, list(value = sum(value)),
                       by = .(reporter_code, reporter, partner_code, partner,
                              item_code, item, year, imex, unit)]
cat("Aggregation from", length(item_match), "to",
    nrow(fore_trad), "observations.\n")

# 2019-06-07: Keep unit variable
# Widen by unit
# fore_trad <- dcast(fore_trad,
#                    reporter_code + reporter + partner_code + partner +
#                      item_code + item + year + imex ~ unit,
#                    value.var = "value")
# fore_trad <- dt_rename(fore_trad, rename, drop = FALSE)
# fore_trad <- dt_replace(fore_trad, is.na, value = 0)

# Store
saveRDS(fore_trad, "data/tidy/fore_trad_tidy.rds")
rm(fore_trad, fore_conc, item_match)


# Crops -------------------------------------------------------------------

cat("\nTidying crops.\n")

crop_conc <- fread("inst/conc_crop-cbs.csv")

#
# Production
crop <- rbind(readRDS("input/fao/crop_prod.rds"),
              readRDS("input/fao/crop_proc.rds"))

crop <- dt_rename(crop, rename, drop = TRUE)

# Country / Area adjustments
crop <- area_kick(crop, code = 351, pattern = "China", groups = TRUE)
crop <- area_merge(crop, orig = 62, dest = 238, pattern = "Ethiopia")
crop <- area_fix(crop, regions)

crop <- merge(crop, crop_conc,
              by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
crop <- tcf_apply(crop, fun = `*`, na.rm = TRUE)

# Aggregate
crop <- crop[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit,
                    cbs_item_code, cbs_item)]
crop <- dt_rename(crop, drop = FALSE,
                  rename = c("cbs_item_code" = "item_code",
                             "cbs_item" = "item"))
crop <- dt_filter(crop, value > 0)

#
# Primary
crop_prim <- readRDS("input/fao/crop_prim.rds")

crop_prim <- dt_rename(crop_prim, rename, drop = TRUE)

# Country / Area adjustments
crop_prim <- area_kick(crop_prim, code = 351, pattern = "China", groups = TRUE)
crop_prim <- area_merge(crop_prim, orig = 62, dest = 238, pattern = "Ethiopia")
crop_prim <- area_fix(crop_prim, regions)

crop_prim <- dt_filter(crop_prim, element != "Yield")

# Only keep fodder crops
crop_prim <- merge(crop_prim, crop_conc,
                   by.x = "item_code", by.y = "crop_item_code", all.x = TRUE)
crop_prim <- dt_filter(crop_prim, cbs_item_code == 2000)

# Aggregate
crop_prim <- crop_prim[, list(value = sum(value)),
                       by = .(area_code, area, element, year, unit,
                              cbs_item_code, cbs_item)]
crop_prim <- dt_rename(crop_prim, drop = FALSE,
                       rename = c("cbs_item_code" = "item_code",
                                  "cbs_item" = "item"))
crop_prim <- dt_filter(crop_prim, value > 0)

#
# Bind all parts & store
saveRDS(rbind(crop, crop_prim), "data/tidy/crop_tidy.rds")
rm(crop, crop_prim, crop_conc)


# Livestock ---------------------------------------------------------------

cat("\nTidying livestocks.\n")

live_conc <- fread("inst/conc_live-cbs.csv")

live <- rbind(readRDS("input/fao/live_prod.rds"),
              readRDS("input/fao/live_proc.rds"),
              readRDS("input/fao/live_prim.rds"))

live <- dt_rename(live, rename)

# Country / Area adjustments
live <- area_kick(live, code = 351, pattern = "China", groups = TRUE)
live <- area_merge(live, orig = 62, dest = 238, pattern = "Ethiopia")
live <- area_fix(live, regions)

live <- merge(live, live_conc,
              by.x = "item_code", by.y = "live_item_code", all.x = TRUE)
live <- dt_filter(live, !is.na(cbs_item_code))

# Aggregate
live <- live[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit,
                    cbs_item_code, cbs_item)]
live <- dt_rename(live, drop = FALSE, rename = c("cbs_item_code" = "item_code",
                                                 "cbs_item" = "item"))
live <- dt_filter(live, value > 0)

# Recode "1000 Head" to "head"
live[unit == "1000 Head", `:=`(value = value * 1000, unit = "Head")]
live[unit == "Head", `:=`(unit = "head")]
# Recode "1000 US$" to "usd"
live[unit == "1000 US$", `:=`(value = value * 1000, unit = "usd")]


# Store
saveRDS(live, "data/tidy/live_tidy.rds")
rm(live, live_conc)


# Fish --------------------------------------------------------------------

cat("\nTidying fish.\n")

fish <- readRDS("input/fao/fish_prod.rds")

fish <- dt_rename(fish, rename, drop = TRUE)

fish[, source := ifelse(source_code == 4, "Capture", "Aquaculture")]

# Country / Area adjustments
country_match <- match(fish[["country"]], regions[["fish"]])
fish[, `:=`(area = regions$name[country_match],
            area_code = regions$code[country_match],
            country = NULL)]

fish <- dt_filter(fish, !is.na(area))

fish <- area_kick(fish, code = 351, pattern = "China", groups = TRUE)
fish <- area_merge(fish, orig = 62, dest = 238, pattern = "Ethiopia")
fish <- area_fix(fish, regions)

# Store
saveRDS(fish, "data/tidy/fish_tidy.rds")
rm(fish, country_match)
