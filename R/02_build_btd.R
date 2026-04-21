
library("data.table")
library("tidyverse")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")

# BACI is used for ethanol and fishery trade
baci <- readRDS("data/tidy/baci_tidy.rds")


# BTD ---------------------------------------------------------------------

cat("\nBuilding full BTD.\n")

btd <- readRDS("data/tidy/btd_tidy.rds")



# # Forestry ----------------------------------------------------------------
#
# cat("\nAdding forestry trade data.\n")
#
# fore <- readRDS("data/tidy/fore_trad_tidy.rds")
#
# # Change from reporting & partner country to receiving & supplying country
# fore[, `:=`(from = ifelse(imex == "Import", partner, reporter),
#   from_code = ifelse(imex == "Import", partner_code, reporter_code),
#   to = ifelse(imex == "Import", reporter, partner),
#   to_code = ifelse(imex == "Import", reporter_code, partner_code),
#   reporter = NULL, reporter_code = NULL,
#   partner = NULL, partner_code = NULL)]
#
# # Give preference to import flows over export flows
# fore <- flow_pref(fore, pref = "Import")
# fore[, imex := NULL]
#
# # Exclude intra-regional trade flows
# fore <- dt_filter(fore, from_code != to_code)
#
# # # Fill < 1997 with 1997
# # fore_fill <- lapply(seq(years[1], 1996), function(x, data, obs) {
# #   dt <- data[obs, ]
# #   dt$year <- x
# #   return(dt)
# # }, data = fore, obs = which(fore[, year] == 1997))
# #
# # fore <- rbind(rbindlist(fore_fill), fore)
# # rm(fore_fill)


# Ethanol -----------------------------------------------------------------

cat("\nAdding ethanol trade data.\n")

eth <- baci[grep("^2207[0-9]*$", item_code), ]
eth[, `:=`(item = "Alcohol, Non-Food", item_code = 2659)]

eth <- dt_rename(eth, drop = FALSE,
  rename = c("exporter" = "from", "exporter_code" = "from_code",
    "importer" = "to", "importer_code" = "to_code"))



# # Fish --------------------------------------------------------------------
# 
# cat("\nAdding fishery trade data.\n")
# 
# fish <- baci[grep("^30[1-9]", item_code), ] %>%
#   filter(unit=="tonnes")
# 
# # match with tcfs and convert into fresh fish equivalents
# tcf <- read.csv("inst/tcf_fish.csv")
# fish <- merge(fish, tcf, by.x="item_code", by.y="hs_code", all.x = TRUE)
# fish[, value := value * tcf]
# fish[, `:=`(item = category, category = NULL, tcf = NULL, name = NULL)]
# fish[, `:=`(item_code = code, code = NULL)]
# 
# fish <- dt_rename(fish, drop = FALSE,
#   rename = c("exporter" = "from", "exporter_code" = "from_code",
#     "importer" = "to", "importer_code" = "to_code"))



# Merge --------------------------------------------------------------------

# btd <- rbindlist(list(btd, eth, fish), use.names = TRUE)
btd <- rbindlist(list(btd, eth), use.names = TRUE)

# Replace negatives with 0 (except for regions "Unspecified" and "Others (adjustments)")
# (Not needed, because there are only negatives for these two regions.)
# btd[value < 0 & !from_code %in% c(252,254) & !to_code %in% c(252,254),
#     value := 0]

# Filter items and years, i.e. exclude "Brans", "Infant food", "Miscellaneous",
# ("Miscellaneous" is particularly relevant for Norway (11% of total imports in 2013),
#   but somehow relevant also for many other countries)
btd <- btd[item_code %in% items$item_code & year %in% years, ]

# Aggregate RoW countries in BTD
btd <- replace_RoW(btd, cols = c("from_code", "to_code"),
                   codes = c(regions[current == TRUE, code], 252, 254))

# Remove ROW-internal trade from BTD
btd <- dt_filter(btd, from_code != to_code)

# Aggregate values
btd <- btd[, .(value = na_sum(value)), 
           by = setdiff(names(eth), "value")]

# Add commodity codes
btd[, comm_code := items$comm_code[match(btd$item_code, items$item_code)]]

# Subset to only keep head and usd for live animals
btd_live_tonnes <- btd[(comm_code %in% items[comm_group == "Live animals", comm_code] & unit == "tonnes")]
btd <- btd[!(comm_code %in% items[comm_group == "Live animals", comm_code] & unit == "tonnes")]

setorder(btd, by = year)



# Store -------------------------------------------------------------------

saveRDS(btd, "data/tidy/btd_full_tidy.rds")
