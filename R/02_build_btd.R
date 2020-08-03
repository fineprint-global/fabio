
library("data.table")
source("R/01_tidy_functions.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

years <- 1986:2017

# Comtrade and BACI are used for ethanol and fishery trade
comtrade <- readRDS("data/tidy/comtrade_tidy.rds")
baci <- readRDS("data/tidy/baci_tidy.rds")


# BTD ---------------------------------------------------------------------

cat("\nBuilding full BTD.\n")

btd <- readRDS("data/tidy/btd_tidy.rds")

# Change from reporting & partner country to receiving & supplying country
btd[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  reporter = NULL, reporter_code = NULL,
  partner = NULL, partner_code = NULL)]

# Give preference to import flows over export flows
btd <- flow_pref(btd, pref = "Import")
btd[, imex := NULL]

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)


# Forestry ----------------------------------------------------------------

cat("\nAdding forestry trade data.\n")

fore <- readRDS("data/tidy/fore_trad_tidy.rds")

# Change from reporting & partner country to receiving & supplying country
fore[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  reporter = NULL, reporter_code = NULL,
  partner = NULL, partner_code = NULL)]

# Give preference to import flows over export flows
fore <- flow_pref(fore, pref = "Import")
fore[, imex := NULL]

# Exclude intra-regional trade flows
fore <- dt_filter(fore, from_code != to_code)

# # Fill < 1997 with 1997
# fore_fill <- lapply(seq(years[1], 1996), function(x, data, obs) {
#   dt <- data[obs, ]
#   dt$year <- x
#   return(dt)
# }, data = fore, obs = which(fore[, year] == 1997))
#
# fore <- rbind(rbindlist(fore_fill), fore)
# rm(fore_fill)


# Ethanol -----------------------------------------------------------------

cat("\nAdding ethanol trade data.\n")

# Comtrade is used for `year < 1995`
eth_com <- comtrade[grep("2207", item_code), ]

# Change from reporting & partner country to receiving & supplying country
eth_com[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  item = "Alcohol, Non-Food", item_code = 2659,
  reporter = NULL, reporter_code = NULL, partner = NULL, partner_code = NULL)]

# Give preference to import flows over export flows
eth_com <- flow_pref(eth_com, pref = "Import")
eth_com[, imex := NULL]

# Exclude intra-regional trade flows
eth_com <- dt_filter(eth_com, from_code != to_code)

# Fill < 1988 with 1988
eth_com_fill <- lapply(seq(years[1], 1987), function(x, data, obs) {
  dt <- data[obs, ]
  dt$year <- x
  return(dt)
}, data = eth_com, obs = which(eth_com[, year] == 1988))

eth_com <- rbind(rbindlist(eth_com_fill), eth_com)


# BACI is used for `year >= 1995`
eth_baci <- baci[grep("^2207[0-9]*$", category), ]
eth_baci[, `:=`(item = "Alcohol, Non-Food", item_code = 2659, category = NULL)]

eth_baci <- dt_rename(eth_baci, drop = FALSE,
  rename = c("exporter" = "from", "exporter_code" = "from_code",
    "importer" = "to", "importer_code" = "to_code"))

# Bind Comtrade & BACI
eth <- rbind(eth_com, eth_baci)

rm(eth_com, eth_com_fill, eth_baci)


# Fish --------------------------------------------------------------------

cat("\nAdding fishery trade data.\n")

# Comtrade is used for `year < 1995`
fish_com <- comtrade[grep("^.*30[1-4]$", item_code), ]

# Change from reporting & partner country to receiving & supplying country
fish_com[, `:=`(from = ifelse(imex == "Import", partner, reporter),
  from_code = ifelse(imex == "Import", partner_code, reporter_code),
  to = ifelse(imex == "Import", reporter, partner),
  to_code = ifelse(imex == "Import", reporter_code, partner_code),
  item = "Fish, Seafood", item_code = 2960,
  reporter = NULL, reporter_code = NULL, partner = NULL, partner_code = NULL)]

# Give preference to import flows over export flows
fish_com <- flow_pref(fish_com, pref = "Import")
fish_com[, imex := NULL]

# Exclude intra-regional trade flows
fish_com <- dt_filter(fish_com, from_code != to_code)

# Fill < 1988 with 1988
fish_com_fill <- lapply(seq(years[1], 1987), function(x, data, obs) {
  dt <- data[obs, ]
  dt$year <- x
  return(dt)
}, data = fish_com, obs = which(fish_com[, year] == 1988))

fish_com <- rbind(rbindlist(fish_com_fill), fish_com)

# BACI is used for `year >= 1995`
fish_baci <- baci[grep("^30[1-4]", category), ]
fish_baci[, `:=`(item = "Fish, Seafood", item_code = 2960, category = NULL)]

fish_baci <- dt_rename(fish_baci, drop = FALSE,
  rename = c("exporter" = "from", "exporter_code" = "from_code",
    "importer" = "to", "importer_code" = "to_code"))

# Bind Comtrade & BACI
fish <- rbind(fish_com, fish_baci)

rm(fish_com, fish_com_fill, fish_baci)



# Merge and save ----------------------------------------------------------

btd <- rbindlist(list(btd, fore, eth, fish), use.names = TRUE)

# Replace negatives with 0
btd <- dt_replace(btd, function(x) {`<`(x, 0)}, value = 0, cols = "value")

# Filter to needed items, i.e. exclude "Brans", "Infant food", "Miscellaneous",
# "Miscellaneous" is particularly relevant for Norway (11% of total imports in 2013),
# but also not irrelevant for many other countries
btd <- btd[item_code %in% items$item_code, ]

# Remove regions "unspecified" and "adjustment" from the BTD
btd <- btd[!from_code %in% c(252, 254) & !to_code %in% c(252, 254), ]

# Aggregate values
btd <- btd[, list(value = na_sum(value)), by = c("item_code", "item",
  "from", "from_code", "to", "to_code", "year", "unit")]

# Add commodity codes
btd[, comm_code := items$comm_code[match(btd$item_code, items$item_code)]]

# Subset to only keep head for live animals
btd <- btd[!(comm_code %in% c("c097", "c098", "c099", "c100", "c101", "c102", "c103",
                             "c104", "c105", "c106", "c107", "c108", "c109", "c110") & unit == "tonnes")]


# Store -------------------------------------------------------------------

saveRDS(btd, "data/tidy/btd_full_tidy.rds")
