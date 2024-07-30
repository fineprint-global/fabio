
library("data.table")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")

regions <- fread("inst/regions_full.csv")


# Colnames ----------------------------------------------------------------

rename_baci <- c(
  "t" = "year",
  "k" = "category",
  "i" = "exporter",
  "j" = "importer",
  "v" = "1000 US$",
  "q" = "tonnes"
)


# BACI --------------------------------------------------------------------

cat("\nTidying BACI.\n")

### adapt BACI regions code: Belgium now code 56 and Belgium_Luxembourg separately contained with 58

baci <- readRDS("input/trade/baci_sel.rds")
#baci_v2 <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v2/input/trade/baci_sel.rds")

baci <- dt_rename(baci, rename_baci, drop = TRUE)
#baci_v2 <- dt_rename(baci_v2, rename_baci, drop = TRUE)

baci <- baci[year %in% years,]

# Country concordance
importer_match <- match(baci[["importer"]], regions[["baci"]])
exporter_match <- match(baci[["exporter"]], regions[["baci"]])
baci[, `:=`(importer = regions$name[importer_match],
  importer_code = regions$code[importer_match],
  exporter = regions$name[exporter_match],
  exporter_code = regions$code[exporter_match])]

#importer_match <- match(baci_v2[["importer"]], regions[["baci"]])
#exporter_match <- match(baci_v2[["exporter"]], regions[["baci"]])
#baci_v2[, `:=`(importer = regions$name[importer_match],
#            importer_code = regions$code[importer_match],
#            exporter = regions$name[exporter_match],
#            exporter_code = regions$code[exporter_match])]

for(col in c("importer_code", "exporter_code")) {
  baci <- area_merge(baci, orig = 62, dest = 238,
    col = col, pattern = "Ethiopia")
  baci <- area_merge(baci, orig = 206, dest = 276,
    col = col, pattern = "Sudan")
}
baci <- dt_filter(baci, !is.na(importer) & !is.na(exporter))

# Add item_code
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
