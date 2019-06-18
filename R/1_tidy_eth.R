
library(data.table)
source("R/1_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


# EIA Bio-Ethanol ---------------------------------------------------------

cat("\nTidying EIA ethanol.\n")

eth_eia <- readRDS("input/ethanol/eth_eia.rds")

# Country concordance
country_match <- match(eth_eia[["country"]], regions[["eia"]])
eth_eia[, `:=`(area = regions$name[country_match],
               area_code = regions$code[country_match],
               country = NULL)]

eth_eia <- dt_filter(eth_eia, !is.na(area))

eth_eia <- melt(eth_eia, id.vars = c("area", "area_code"),
                variable.name = "year", value.name = "value_eia",
                variable.factor = FALSE)

cat("Converting from ktoe to tonnes",
    "(1 tonne == 1 ktoe / 0.64 toe/tonne * 1000 tonnes/kt).\n")
eth_eia <- dt_filter(eth_eia, !is.na(value_eia))
eth_eia[, `:=`(value_eia = round(value_eia / 0.64 * 1000, 4),
               unit = "tonnes")]

rm(country_match)


# IEA Bio-Ethanol ---------------------------------------------------------

cat("\nTidying IEA ethanol.\n")

eth_iea <- readRDS("input/ethanol/eth_iea.rds")

# Country concordance
country_match <- match(eth_iea[["country"]], regions[["iso3c"]])
eth_iea[, `:=`(area = regions$name[country_match],
               area_code = regions$code[country_match],
               country = NULL)]

eth_iea <- dt_filter(eth_iea, !is.na(area))

eth_iea <- melt(eth_iea, id.vars = c("area", "area_code"),
                variable.name = "year", value.name = "value_iea",
                variable.factor = FALSE)

cat("Converting from k_barrels/day to tonnes/year",
    "(ton/y == 1000 bbl/d * 365.25 d/y * 158.9873 l/bbl * 0.7893 kg/l).\n")
eth_iea <- dt_filter(eth_iea, !is.na(value_iea))
eth_iea[, `:=`(value_iea = round(value_iea * 365.25 * 158.9873 * 0.7893, 4),
               unit = "tonnes")]

rm(country_match)


# Bio-Ethanol -------------------------------------------------------------

eth <- merge(eth_eia, eth_iea, all = TRUE)

cat("Merging EIA and IEA ethanol - values chosen via `max(eia, iea)`.\n")
eth[, `:=`(value = pmax(value_eia, value_iea, na.rm = TRUE),
           value_eia = NULL, value_iea = NULL, year = as.integer(year))]
cat("Adding Sudan breakup.")
eth[year > 2011 & area == "Sudan (former)", area := "Sudan"]

# Store
saveRDS(eth, "data/tidy/eth_tidy.rds")
rm(eth, eth_eia, eth_iea)
