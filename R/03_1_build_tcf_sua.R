library("tidyverse")
library("data.table")
source("R/00_system_variables.R")

regions_full <- fread("inst/regions_full.csv")
regions <- fread("inst/regions.csv")

tcf <- readRDS("data/tidy/tcf_sua_tidy.rds") 
tcf_avg <- fread("inst/sua/tcf_sua_world_averages.csv")
tcf_expert <- fread("inst/sua/tcf_sua_expert.csv")
#cbs <- readRDS("data/tidy/cbs_tidy.rds")  
#prod_trad <- readRDS("data/tidy/prod_trad_full.rds")
  
proc_sua <- fread("inst/sua/proc_sua.csv")
sua <- readRDS("data/sua_full.rds")
use_structure <- fread("inst/sua/use_structure_sua.csv")

items_sua <- fread("inst/sua/items_sua.csv")

# Gap-filling ----------------------------------------------------------------
cat("\nGap-filling TCF for countries that have produced the processed item in at least one year\n")

cat("\n1. Copy values for similar countries and countries with new borders\n")

fill_country_gaps <- function(dt, source_countries, target_countries) {
  stopifnot(length(source_countries) == length(target_countries))
  
  # Create a mapping data.table for faster joins
  mapping <- data.table(source = source_countries, target = target_countries)
  
  source_data <- dt[country_sua %in% source_countries & !is.na(value),
                    .(country_sua, item_sua, variable, value)]
  
  # Join mapping to source_data to know where to copy to
  setnames(source_data, "country_sua", "source")
  source_data <- merge(source_data, mapping, by = "source", allow.cartesian = TRUE)
  
  # Prepare keys for fast join
  setkey(dt, country_sua, item_sua, variable)
  setkey(source_data, target, item_sua, variable)
  
  # Fill missing values
  dt[source_data, on = .(country_sua = target, item_sua, variable),
     value := ifelse(is.na(value), i.value, value)]
}


source_vec <- c("Montenegro", "Belgium", "Sudan", "Indonesia", "Micronesia", 
                "China, mainland", "China, mainland")
target_vec <- c("Serbia", "Luxembourg", "South Sudan", "Timor-Leste", "Marshall Islands", 
                "China, Taiwan Province of", "China, Macao SAR")
fill_country_gaps(tcf, source_vec, target_vec)


# Add aggregate supply data from sua_tidy, cbs and prod_trad 
# -> only gap-fill where production is positive
prod_agg <- sua[, .(total_prod = sum(production, na.rm = TRUE)),
                     by = .(area, item)]

# cbs <- cbs[year %in% years & item %in% items_sua$item & !item %in% sua$item, .(area, item, production)]
# cbs <- cbs[,.(total_prod = sum(production, na.rm = TRUE)),
#            by = .(area, item)]
# 
# prod <- prod_trad[year %in% years & element == "Production" & !item %in% cbs$item &
#                     !item %in% sua$item & item %in% items_sua$item, .(area, item, production = value) ]
# prod <- prod[,.(total_prod = sum(production, na.rm = TRUE)),
#             by = .(area, item)]
# 
# prod_agg <- rbind(prod_agg, cbs, prod)


# add aggregated production to tcf
tcf <- merge(tcf, prod_agg, by.x = c("country_sua", "item_sua"),
                  by.y = c( "area", "item"),
                  all.x = TRUE)
tcf <- tcf[total_prod > 0 & !is.na(total_prod)]
tcf[is.nan(value), value := NA_real_]


cat("\n2. Impute regional averages, weighted by production\n")
tcf[, region := regions_full$region[match(country_sua, regions_full$name)]]

tcf[, reg_avg := if (all(is.na(value))) NA_real_ else
  sum((value * total_prod) / sum(total_prod[!is.na(value)], na.rm = TRUE), na.rm = TRUE),
  by = .(region, item_sua, variable)]

tcf[is.na(value), value := reg_avg]
tcf[is.nan(value), value := NA_real_]

cat("\n3. Calculate global averages, weighted by production\n")
# these are not integrated yet -> the global averages from the trees will be used
# first
tcf[, avg := if (all(is.na(value))) NA_real_ else
  sum((value * total_prod) / sum(total_prod[!is.na(value)], na.rm = TRUE), na.rm = TRUE),
  by = .(item_sua, variable)]

# clean up
tcf <- tcf[variable == "extraction rates", .(country_sua, item_sua, value = 
                                              round(value/100,2), avg =
                                               round(avg/100, 2))]


# Use structure ---------------------------------------------------------
# parent_item -> process -> child_item
use_items_sua <- merge(use_structure, proc_sua, by.x = c("child"), by.y = c("item"), all = TRUE)
use_items_sua[, parent_code := items_sua$comm_code[match(parent, items_sua$item)]]
use_items_sua <- use_items_sua[, .(parent, parent_code, proc, proc_code, child, child_code = comm_code)]
setorder(use_items_sua, parent_code, child_code, proc_code)

# problem from allocating processes only to the child item: 
# by-products that come from different parents/processes
exclude <- fread("inst/sua/exclude.csv")
use_items_sua <- fsetdiff(use_items_sua , exclude[, .(parent, parent_code, proc,
                                                      proc_code, child, child_code)])
saveRDS(use_items_sua, "data/sua/use_items_sua.rds")


# Add use and process information --------------------------------------------
# create full table for gap filling with use_items_sua repeated along countries

tcf_full <- CJ(area = unique(regions$area), row_id = seq_len(nrow(use_items_sua)))[
  , cbind(.SD, use_items_sua[row_id]), .SDcols = "area"
]

# add production for filtering rows
# -> this way we only have tcf for countries where production has taken place in at least one year
tcf_full <- merge(tcf_full, prod_agg[,.(area, item, production_child = total_prod)],
                  by.x = c("area", "child"), by.y = c("area", "item"),
                  all.x = TRUE)
tcf_full <- merge(tcf_full, prod_agg[,.(area, item, production_parent = total_prod)],
                  by.x = c("area", "parent"), by.y = c("area", "item"),
                  all.x = TRUE)


# Excluding rows where either parent or child are missing 
# -> this excludes all primary products automatically
tcf_full <- tcf_full[!is.na(production_child) & !is.na(production_parent) &
                       production_child > 0 & production_parent > 0]


# add data from tcf_official on country level
tcf_full <- merge(tcf_full, tcf[,.(country_sua, item_sua, 
                                                extraction_rate = value, avg)],
                           by.x =c("area", "child"), by.y = c("country_sua","item_sua"),
                           all.x = TRUE)

# add averages from commodity trees 
tcf_full <- merge(tcf_full, tcf_avg, by.x = c("proc", "proc_code","child", "child_code"),
                  by.y = c("proc", "proc_code", "item", "comm_code"), all.x = TRUE)

# adapt min and max to real values
tcf_full[, min_extr := min(extraction_rate, na.rm = TRUE), by = .(parent, proc, child)]
tcf_full[min_extr < min | is.na(min), min := min_extr][,min_extr := NULL]

tcf_full[, max_extr := max(extraction_rate, na.rm = TRUE), by = .(parent, proc, child)]
tcf_full[is.na(max) | max_extr > max, max := max_extr][, max_extr := NULL]

# fill gaps with world averages from tcf trees
tcf_full[is.na(extraction_rate) & !is.na(avg_trees), extraction_rate := avg_trees]

# fill remaining gaps with world averages calculated from country tables earlier
tcf_full[is.na(extraction_rate) & !is.na(avg), extraction_rate := avg]

# fill remaining gaps with the average of min and max
tcf_full[is.na(extraction_rate) & !is.na(min) & !is.na(max), 
         extraction_rate := (min + max)/2 ]

# clean up
tcf_full[, `:=` (production_child = NULL, production_parent = NULL,
                 avg = NULL, avg_trees = NULL)]


# Problem cases --------------------------------------------------------------

# In some it is unclear what parent process the extraction rate refers to
# - the country level extraction rates are only provided for the child item, 
# not parent/child combinations

# find problem cases (child items that have more than one parent)
problems <- use_items_sua[(child %in% use_items_sua[ , .N, by = child][N > 1, child]) &
                            child %in% tcf_full[!is.na(extraction_rate), child]]
problem_items <- problems[, .(unique(child))]
problems_full <- tcf_full[child %in% problems$child]

# deal with problem cases (decisions on how to deal with which item are based on "expert_tcf")

cat("\nCase 1: extraction rate is the same for different parents\n")

problems_full[child %in% c("Malt, whether or not roasted", "Starch of potatoes",
                          "Grape juice", "Rice-fermented beverages"),
              `:=`(corrected_rate = extraction_rate, corrected = TRUE)]

cat("\nCase 2: extraction rate only belongs to one parent and it can be identified which
# parent it belongs to\n")
problems_full[(child == "Bran of rice" & proc == "Milled rice production" &
                 parent == "Rice")|
                (child == "Starch of maize" & parent == "Flour of maize")|
                (child == "Maize gluten" & parent == "Flour of maize") |
                (child == "Tapioca of potatoes" & parent == "Potatoes") |
                (child == "Wine" & parent == "Must of grape")|
                (child == "Cheese from whole cow milk" & parent == "Raw milk of cattle")|
                (child == "Chocolate products nes" & parent == "Cocoa powder and cake"),
              `:=`(corrected_rate = extraction_rate, corrected = TRUE)]

cat("\nCase 3: it is not possible to determine which parent is implied in the official rate\n")

# -> all extraction rates will be taken from expert table
problems_full[is.na(corrected), `:=` (corrected_rate = 100000000)]

# add corrected rates back to tcf_full
tcf_full <- merge(tcf_full, problems_full[,.(proc, child, area, parent, corrected_rate)], 
                  by = c("proc", "child", "area", "parent"), all.x = TRUE)
tcf_full[is.na(extraction_rate) & !is.na(corrected_rate), extraction_rate := corrected_rate]
tcf_full[extraction_rate == 100000000, extraction_rate := NA][, corrected_rate := NULL]

# #Finding missing values to create expert table
# #find officially available rates -> used in "building_expert_tcf"
# tcf_avg_avail <- unique(tcf_full[!is.na(extraction_rate), .(parent, proc, child)])
# fwrite(tcf_avg_avail, "input/tcf_sua/tcf_avg_avail.csv")
# 
# tcf_min_avail <- unique(tcf_full[!is.na(min), .(parent, proc, child)])
# fwrite(tcf_min_avail, "input/tcf_sua/tcf_min_avail.csv")
# 
# tcf_max_avail <- unique(tcf_full[!is.na(max), .(parent, proc, child)])
# fwrite(tcf_max_avail, "input/tcf_sua/tcf_max_avail.csv")


# Merging TCF and TCF_expert ---------------------------------------------------
tcf_full <- merge(tcf_full, tcf_expert[,.(parent, child, proc,expert_rate = extraction_rate, 
                                          expert_min = min, expert_max = max)], 
                  by = c("parent", "child", "proc"), all.x = TRUE)
tcf_full[is.na(extraction_rate), extraction_rate := expert_rate]
tcf_full[is.na(min), min := expert_min]
tcf_full[is.na(max), max := expert_max]

# add area codes
regions <- fread("inst/regions_full.csv")
tcf_full[, area_code := regions$code[match(area, regions$name)]]

# clean up
tcf_full[, `:=` (expert_rate = NULL, expert_min = NULL, expert_max = NULL)]

setcolorder(tcf_full, c("area", "area_code", "parent", "parent_code", "proc", "proc_code", 
                        "child", "child_code", "extraction_rate", "min", "max"))
setorder(tcf_full, area_code , parent_code, proc_code, child_code)
saveRDS(tcf_full, "data/sua/tcf_sua_final.rds")

rm(problem_items, problems, problems_full ,prod_agg, tcf, tcf_avg, tcf_expert)



