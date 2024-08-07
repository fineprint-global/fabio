
library("data.table")
source("R/01_tidy_functions.R")
source("R/00_system_variables.R")

items <- fread("inst/items_full.csv")

# BACI is used for ethanol and fishery trade
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

# Give preference to reported export flows over import flows
btd <- flow_pref(btd, pref = "Export")
btd[, imex := NULL]

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)


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

eth <- baci[grep("^2207[0-9]*$", category), ]
eth[, `:=`(item = "Alcohol, Non-Food", item_code = 2659, category = NULL)]

eth <- dt_rename(eth, drop = FALSE,
  rename = c("exporter" = "from", "exporter_code" = "from_code",
    "importer" = "to", "importer_code" = "to_code"))



# Fish --------------------------------------------------------------------

cat("\nAdding fishery trade data.\n")

fish <- baci[grep("^30[1-5]", category), ]
fish[, `:=`(item = "Fish, Seafood", item_code = 2960, category = NULL)]

fish <- dt_rename(fish, drop = FALSE,
  rename = c("exporter" = "from", "exporter_code" = "from_code",
    "importer" = "to", "importer_code" = "to_code"))



# Merge --------------------------------------------------------------------

btd <- rbindlist(list(btd, eth, fish), use.names = TRUE) #fore

# Replace negatives with 0 (except for regions "Unspecified" and "Others (adjustments)")
# (Not needed, because there are only negatives for these two regions.)
# btd[value < 0 & !from_code %in% c(252,254) & !to_code %in% c(252,254),
#     value := 0]

# Filter items and years, i.e. exclude "Brans", "Infant food", "Miscellaneous",
# ("Miscellaneous" is particularly relevant for Norway (11% of total imports in 2013),
#   but somehow relevant also for many other countries)
btd <- btd[item_code %in% items$item_code & year %in% years, ]

# Aggregate values
btd <- btd[, list(value = na_sum(value)), by = c("item_code", "item",
  "from", "from_code", "to", "to_code", "year", "unit")]

# Add commodity codes
btd[, comm_code := items$comm_code[match(btd$item_code, items$item_code)]]

# Subset to only keep head and usd for live animals
btd_live_tonnes <- btd[(comm_code %in% items[comm_group == "Live animals", comm_code] & unit == "tonnes")]
btd <- btd[!(comm_code %in% items[comm_group == "Live animals", comm_code] & unit == "tonnes")]

setorder(btd, by = year)



# Handle outliers ----------------------------------------------------------
# some example outliers where exports in btd are a lot higher than total_supply
# area_code	|	area	|	year	|	item_code	|	item	|	production	|	exports	|	imports	|	total_supply	|	diff
# 144	|	Mozambique	|	2019	|	2671	|	Tobacco	|	142041	|	2093003	|	34756	|	176797	|	-1916206
# 144	|	Mozambique	|	2020	|	2671	|	Tobacco	|	67000	|	1074105	|	47018	|	114018	|	-960086
# 231	|	United States of America	|	2020	|	2661	|	Cotton lint	|	3180410	|	3847577	|	12126	|	3192536	|	-655041
# 177	|	Puerto Rico	|	1999	|	2514	|	Maize and products	|	700	|	641483	|	4	|	704	|	-640779
# 10	|	Australia	|	2021	|	2661	|	Cotton lint	|	114751	|	717061	|	156	|	114907	|	-602154
# 177	|	Puerto Rico	|	1996	|	2514	|	Maize and products	|	750	|	596125	|	233	|	983	|	-595141
# 177	|	Puerto Rico	|	1997	|	2514	|	Maize and products	|	820	|	554312	|	1	|	821	|	-553491
# 100	|	India	|	2021	|	2667	|	Hard Fibres, Other	|	591440.97	|	1123744	|	3812	|	595253	|	-528490


# data <- btd %>% 
#   group_by(comm_code, item_code, item, from_code, from, to_code, to, unit) %>% 
#   mutate(q1 = quantile())
# data <- btd[comm_code == "c002" & from_code == 231 & to_code == 41 & unit=="tonnes", value]

# for(u in unique(btd$unit)){ # u <- "tonnes"
#   for(i in unique(btd$from_code)){  # i <- 231
#     for(c in unique(btd$comm_code)){  #  c <- "c060"
#       check <- btd[unit==u & from_code==i & comm_code==c, list(value = na_sum(value)), 
#                    by = c("item_code", "item", "from", "from_code", "year", "unit")]
#       print(paste0(c, ": ", length(tsoutliers(check$value)$index)))
#       if(length(tsoutliers(check$value)$index) != 0 & sum(check$value) > 100000){
#         for(j in unique(btd$to_code)){  #  j <- 35
#           if(length(tsoutliers(check$value)$index) != 0 & sum(check$value) > 10000){
#             check <- btd[unit==u & from_code==i & to_code==j & comm_code==c]
#             print(paste0(length(tsoutliers(check$value)$index), ": ", j))
#           }
#         }
#       }
#     }
#   }
# }

# clean <- tsclean(data, lambda = NULL)
# compare <- round(cbind(data, clean))



# Store -------------------------------------------------------------------

saveRDS(btd, "data/tidy/btd_full_tidy.rds")
