
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


# Colnames ----------------------------------------------------------------

rename_comtrade <- c(
  "year" = "year",
  "trade_flow" = "element",
  "reporter_iso" = "reporter",
  "partner_iso" = "partner",
  "commodity_code" = "item_code",
  "commodity" = "item",
  "qty_unit" = "unit",
  "qty" = "value",
  # "netweight_kg" = "net_weight",
  "trade_value_usd" = "usd"
)

rename_baci <- c(
  "t" = "year",
  "k" = "category",
  "i" = "exporter",
  "j" = "importer",
  "v" = "1000 US$",
  "q" = "tonnes"
)


# Comtrade ----------------------------------------------------------------

cat("\nTidying Comtrade.\n")

comtrade <- readRDS("input/trade/comtrade.rds")

comtrade <- dt_rename(comtrade, rename_comtrade, drop = TRUE)

# Country concordance
reporter_match <- match(comtrade[["reporter"]], regions[["iso3c"]])
partner_match <- match(comtrade[["partner"]], regions[["iso3c"]])
comtrade[, `:=`(reporter = regions$name[reporter_match],
  reporter_code = regions$code[reporter_match],
  partner = regions$name[partner_match],
  partner_code = regions$code[partner_match])]
for(col in c("reporter_code", "partner_code")) {
  comtrade <- area_merge(comtrade, orig = 62, dest = 238,
    col = col, pattern = "Ethiopia")
  comtrade <- area_merge(comtrade, orig = 206, dest = 276,
    col = col, pattern = "Sudan")
}
comtrade <- dt_filter(comtrade, !is.na(reporter) & !is.na(partner))

# add Re-Exports to Exports
comtrade[grep("Re-Export", element), element := "Export"]

comtrade[, imex := factor(element)]
comtrade[, value := as.double(value)]
comtrade[, item_code := as.integer(item_code)]

cat("Converting Ethanol from litres to kilograms", "(1l == 0.7893kg).\n")
comtrade[unit == "Volume in litres" & item_code == 2207,
  `:=`(value = value * 0.7893, unit = "Weight in kilograms")]

comtrade <- dcast(comtrade,
  reporter_code + reporter + partner_code + partner + year + imex +
  item_code + item + usd ~ unit, value.var = "value", fun.aggregate = sum)
comtrade[, `:=`(`No Quantity` = NULL, `Number of items` = NULL)]
comtrade <- dt_rename(comtrade, drop = FALSE, c("Weight in kilograms" = "kg"))
# Convert from kg to tonnes
comtrade[, `:=`(tonnes = kg / 1000, kg = NULL)]

# Estimate missing quantities using global average price per item and year

# Aggregate
comtrade_agg <- comtrade[!is.na(tonnes) & usd > 0,
  list(price_per_tonne = na_sum(usd) / na_sum(tonnes)),
  by = c("year", "item_code")]
comtrade <- merge(comtrade, comtrade_agg,
  by = c("year", "item_code"), all.x = TRUE)
comtrade[is.na(tonnes) | tonnes == 0,
  tonnes := round(usd / price_per_tonne, 3)]
comtrade[, price_per_tonne := NULL]

# Use a unit variable
comtrade <- melt(comtrade, measure.vars = c("usd", "tonnes"),
  variable.name = "unit", variable.factor = FALSE)

# Belgium-Luxembourg before 2000 together
comtrade[reporter_code==255 & reporter=="Belgium" & year<2000,
     `:=`(reporter_code=15, reporter="Belgium-Luxembourg")]
comtrade[partner_code==255 & partner=="Belgium" & year<2000,
     `:=`(partner_code=15, partner="Belgium-Luxembourg")]

# Aggregate
comtrade <- comtrade[, list(value = na_sum(value)),
  by = c("year", "item_code", "item", "reporter_code", "reporter",
    "partner_code", "partner", "unit", "imex")]

# Store
saveRDS(comtrade, "data/tidy/comtrade_tidy.rds")
rm(comtrade, partner_match, reporter_match)


# BACI --------------------------------------------------------------------

cat("\nTidying BACI.\n")

baci <- readRDS("input/trade/baci_sel.rds")

baci <- dt_rename(baci, rename_baci, drop = TRUE)

# Country concordance
importer_match <- match(baci[["importer"]], regions[["baci"]])
exporter_match <- match(baci[["exporter"]], regions[["baci"]])
baci[, `:=`(importer = regions$name[importer_match],
  importer_code = regions$code[importer_match],
  exporter = regions$name[exporter_match],
  exporter_code = regions$code[exporter_match])]

for(col in c("importer_code", "exporter_code")) {
  baci <- area_merge(baci, orig = 62, dest = 238,
    col = col, pattern = "Ethiopia")
  baci <- area_merge(baci, orig = 206, dest = 276,
    col = col, pattern = "Sudan")
}
baci <- dt_filter(baci, !is.na(importer) & !is.na(exporter))

# Belgium-Luxembourg before 2000 together
baci[exporter_code==255 & exporter=="Belgium" & year<2000,
     `:=`(exporter_code=15, exporter="Belgium-Luxembourg")]
baci[importer_code==255 & importer=="Belgium" & year<2000,
     `:=`(importer_code=15, importer="Belgium-Luxembourg")]

baci[, item_code := as.integer(category)]

# Convert from 1000 US$ to usd
baci[, `:=`(usd = `1000 US$` * 1000, `1000 US$` = NULL)]

# Estimate missing quantities using global average price per item and year

# Aggregate
baci_agg <- baci[!is.na(tonnes) & usd > 0,
  list(price_per_tonne = na_sum(usd) / na_sum(tonnes)),
  by = c("year", "item_code")]
baci <- merge(baci, baci_agg, by = c("year", "item_code"), all.x = TRUE)
baci[is.na(tonnes) | tonnes == 0, tonnes := round(usd / price_per_tonne, 3)]
baci[, price_per_tonne := NULL]

# Introduce unit variable
baci <- melt(baci, measure.vars = c("usd", "tonnes"),
  variable.name = "unit", variable.factor = FALSE)

# Store
saveRDS(baci, "data/tidy/baci_tidy.rds")
rm(baci, importer_match, exporter_match)
