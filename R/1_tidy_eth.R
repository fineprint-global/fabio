
library(data.table)
source("R/1_tidy_fun.R")

regions <- fread("inst/regions_full.csv")


# EIA Bio-Ethanol ---------------------------------------------------------

cat("Processing EIA ethanol.\n")

eth_eia <- readRDS("input/ethanol/eth_eia.rds")

# Country concordance
country_match_eia <- match(eth_eia[["country"]], regions[["eia"]])
eth_eia[, `:=`(country = regions$name[country_match_eia],
               country_code = regions$code[country_match_eia])]

eth_eia <- dt_filter(eth_eia, !is.na(country))

eth_eia <- melt(eth_eia, id.vars = c("country", "country_code"),
                variable.name = "year", value.name = "value_eia",
                variable.factor = FALSE)

# Convert from ktoe to tonnes
eth_eia <- dt_filter(eth_eia, !is.na(value_eia))
eth_eia[, value_eia := round(value_eia / 0.64 * 1000, 4)]


# IEA Bio-Ethanol ---------------------------------------------------------

cat("Processing IEA ethanol.\n")

eth_iea <- readRDS("input/ethanol/eth_iea.rds")

# Country concordance
country_match_iea <- match(eth_iea[["country"]], regions[["iso3c"]])
eth_iea[, `:=`(country = regions$name[country_match_iea],
               country_code = regions$code[country_match_iea])]

eth_iea <- dt_filter(eth_iea, !is.na(country))

eth_iea <- melt(eth_iea, id.vars = c("country", "country_code"),
                variable.name = "year", value.name = "value_iea",
                variable.factor = FALSE)

# Convert from k_barrels/day to tonnes/year
eth_iea <- dt_filter(eth_iea, !is.na(value_iea))
eth_iea[, value_iea := round(value_iea * 365.25 * 158.9873 * 0.7893, 4)]


# Bio-Ethanol -------------------------------------------------------------

eth <- merge(eth_eia, eth_iea, all = TRUE)

cat("Merging EIA and IEA ethanol - values chosen via `max(eia, iea)`.\n")
eth[, `:=`(value = pmax(value_eia, value_iea, na.rm = TRUE),
           value_eia = NULL, value_iea = NULL, year = as.integer(year))]

# Store
saveRDS(eth, "data/tidy/eth_tidy.rds")

