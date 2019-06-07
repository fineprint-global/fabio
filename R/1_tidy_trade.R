
library(data.table)
source("R/1_tidy_functions.R")

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
  "hs6" = "category",
  "i" = "exporter",
  "j" = "importer",
  "v" = "1000 US$",
  "q" = "tons"
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
}
comtrade <- dt_filter(comtrade, !is.na(reporter) & !is.na(partner))

comtrade <- dt_filter(comtrade, !unit %in% c("No Quantity", "Number of items"))

# Add Re-Exports to Exports
comtrade[grep("Re-Export", element), element := "Export"]

comtrade[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]
comtrade[, value := as.double(value)]

comtrade <- dcast(comtrade,
                  reporter_code + reporter + partner_code + partner +
                    year + imex + item_code + item +
                    usd ~ unit,
                  value.var = "value", fun.aggregate = sum)
comtrade <- dt_rename(comtrade, drop = FALSE,
                      c("Weight in kilograms" = "kg",
                        "Volume in litres" = "litres"))
# 2019-06-07: Use a unit variable
comtrade <- melt(comtrade, measure.vars = c("usd", "litres", "kg"),
                 variable.name = "unit", variable.factor = FALSE)

## cat("Converting Ethanol from kilograms to litres and vice versa",
#     "(1l == 0.7893kg).\n")
# comtrade[, `:=`(litres = as.double(litres),
#                 kg = as.double(kg))]
# comtrade[is.na(litres) & item_code == 2207, litres := kg / 0.7893]
# comtrade[is.na(kg) & item_code == 2207, kg := litres * 0.7893]

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
}
baci <- dt_filter(baci, !is.na(importer) & !is.na(exporter))

# 2019-06-07: Introduce unit variable
baci <- melt(baci, measure.vars = c("1000 US$", "tons"),
             variable.name = "unit", variable.factor = FALSE)

# Store
saveRDS(baci, "data/tidy/baci_tidy.rds")
rm(baci, importer_match, exporter_match)
