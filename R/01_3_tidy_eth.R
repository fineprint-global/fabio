
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")


# EIA Bio-Ethanol ---------------------------------------------------------

cat("\nTidying EIA biofuels.\n")

eth_eia <- readRDS("input/biofuels/biofuels_eia.rds")

# Country concordance
country_match <- match(eth_eia[["iso3c"]], regions[["iso3c"]])
eth_eia[, `:=`(area = regions$name[country_match],
  area_code = regions$code[country_match], area_name = NULL)]

eth_eia <- dt_filter(eth_eia, !is.na(area))

eth_eia <- melt(eth_eia, id.vars = c("iso3c", "area", "area_code", "item"),
  variable.name = "year", value.name = "value_eia", variable.factor = FALSE)

eth_eia[, value_eia := as.numeric(value_eia)]
eth_eia <- dt_filter(eth_eia, !is.na(value_eia))

eth_eia <- area_merge(eth_eia, orig = 62, dest = 238, pattern = "Ethiopia")

# cat("Converting from 1000 barrels/day to tonnes/year",
#   "(1000 bbl/d == 365.25 d/y * 158987.3 l/1000 bbl * 0.0007893 tonnes/l).\n")
# eth_eia[, `:=`(value_eia = round(value_eia * 365.25 * 158.9873 * 0.7893, 3),
#   unit = "tonnes")]

# Convert to tonnes
eth_eia[, `:=`(value_eia = round(value_eia * 1000), unit = "tonnes")]

rm(country_match)


# IEA Bio-Ethanol ---------------------------------------------------------

cat("\nTidying IEA ethanol.\n")

eth_iea <- readRDS("input/biofuels/biofuels_iea.rds")

# Country concordance
country_match <- match(eth_iea[["Country"]], regions[["iea"]])
eth_iea[, `:=`(area = regions$name[country_match],
  area_code = regions$code[country_match], Country = NULL)]

eth_iea <- dt_filter(eth_iea, !is.na(area))

eth_iea <- melt(eth_iea, id.vars = c("area", "area_code", "Time"),
  variable.name = "product", value.name = "value_iea", variable.factor = FALSE)
eth_iea <- rename(eth_iea, year = "Time")
eth_iea[, value_iea := as.numeric(value_iea)]

cat("Converting from ktoe to tonnes",
    "(1 ktoe ==  950.10 tonnes).\n")
eth_iea <- dt_filter(eth_iea, !is.na(value_iea))
eth_iea[, `:=`(value_iea = round(value_iea * 950.10, 0), unit = "tonnes")]

rm(country_match)


# Bio-Ethanol -------------------------------------------------------------

eth <- merge(eth_eia[item=="ethanol production (Mmt)", .(area,area_code,year,value_eia,unit)], 
             eth_iea[product=="Biogasoline", .(area,area_code,year,value_iea,unit)], 
             by=c("area_code", "area", "year", "unit") , all = TRUE)

eth[, year := as.integer(year)]

cat("Merging EIA and IEA ethanol - values chosen via `max(eia, iea)`.\n")
eth[, `:=`(value = pmax(value_eia, value_iea, na.rm = TRUE),
  value_eia = NULL, value_iea = NULL)]

eth <- area_merge(eth, orig = 62, dest = 238, pattern = "Ethiopia")
eth <- area_merge(eth, orig = 206, dest = 276, pattern = "Sudan")
eth <- area_fix(eth, regions)


# Store
saveRDS(eth, "data/tidy/eth_tidy.rds")
rm(eth, eth_eia, eth_iea)
