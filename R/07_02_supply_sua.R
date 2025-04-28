library("tidyverse")
library("data.table")

years <- c(2010:2022)

items_sua <- fread("inst/items_sua.csv")
proc_sua <- fread("inst/proc_sua.csv")
ex_rates <- fread("inst/extraction_rate_averages.csv")

sua <- readRDS("data/tidy/sua_tidy.rds")
tcf <- readRDS("data/tidy/tcf_tidy.rds")

# model after this (not needed in the end)
supply_cbs <- readRDS("data/sup_final.rds")

# build supply table ------
# create sua supply table -> here some of the values are double counted (e.g., skim milk)
supply_sua <- sua[, .(area, item_code_fcl, item, year, production)]
supply_sua <- merge(supply_sua, proc_sua, by = c("item_code_fcl", "item"), 
                    allow.cartesian = TRUE)

# find items that come from several coupled processes (e.g., fat of poultry comes from duck, turkey and chicken production)
by_products <- proc_sua[, .N, by = item][N>1]
by_products <- proc_sua[by_products, on = "item"]

# get processes that supply several items and their items 
double_proc <- supply_sua[proc_code %in% by_products$proc_code]

# get extraction rates for rice bran from milled rice production (for bran from milled (husked) 
# production we only have a world average)
double_proc <-  merge(double_proc, tcf[variable == "extraction rates",
                                       .(item_sua, country_sua, year, extraction_rate = value/100)],
                      by.x =c("item","area", "year" ),
                      by.y = c("item_sua","country_sua", "year" ),
                      all.x = TRUE)

# set rice (milled and husked) extraction rate to NA
double_proc[item == "Bran of rice" & proc == "Rice (milled and husked) production",
            extraction_rate := NA]

# get world averages and ranges for other processes
double_proc <- merge(double_proc, ex_rates[, .(item, proc_code, avg, min, max)], 
                     by = c("item", "proc_code"), all.x = TRUE )                                               
double_proc[is.na(extraction_rate), extraction_rate := avg]
double_proc[, avg := NULL]

# separate into main products and by-products to estimate how much of each by-product comes from which process
double_proc[, by_product := ifelse(item %in% by_products$item ,TRUE, FALSE)]

by_products <- double_proc[by_product == TRUE][, `:=`(by_product = NULL, item_code_fcl = NULL, comm_code = NULL)]
setnames(by_products, c("item", "production", "extraction_rate", "min", "max"), 
         c("by_item", "total_by_production","by_extraction", "by_min", "by_max"))

main_products <- double_proc[by_product == FALSE][, `:=`(by_product = NULL, item_code_fcl = NULL, comm_code = NULL)]
setnames(main_products, c("item", "production", "extraction_rate", "min", "max"), 
         c("main_item", "main_production", "main_extraction", "main_min", "main_max"))

# merge main and by products back into one table 
double_proc <- merge(main_products, by_products, 
                     by = c("proc_code", "proc", "area", "year"))

#find the ratios that the main and by products should have on average
double_proc[, by_main_ratio := by_extraction/main_extraction]

# define the range -> assumption that they vary together or is it possible for them to vary in opposing directions?
double_proc[, by_main_max := by_max/main_min]
double_proc[, by_main_min := by_min/main_max]
double_proc[, `:=` (by_extraction = NULL, by_max = NULL, by_min = NULL, 
                    main_extraction = NULL, main_max = NULL, main_min = NULL)]
double_proc <-  double_proc[total_by_production != 0]


# estimate by-production for each process from main production
double_proc[, absolute_by_production := main_production * by_main_ratio]
double_proc[is.na(absolute_by_production), absolute_by_production := 
              main_production * (by_main_max - by_main_min)]

# estimate absolute minimum
double_proc[, absolute_by_min := 
              main_production * by_main_min]

#estimate absolute maximum
double_proc[, absolute_by_max := 
              main_production * by_main_max]

# finding average relative deviation from estimation to min and max for processed products to gap fill ranges
avg_deviation_min <- ex_rates[!is.na(avg) & !is.na(min) & avg != 1,
                                 mean((avg - min) / avg, 
                                      na.rm = TRUE)]
avg_deviation_max <- ex_rates[!is.na(avg) & !is.na(max) & avg != 1,
                              mean((max- avg) / avg, 
                                   na.rm = TRUE)]

# fill missing absolute minimum/maximum values with average relative deviation from absolute estimated value to obtain ranges
double_proc[is.na(absolute_by_min), 
            absolute_by_min := absolute_by_production - (absolute_by_production*avg_deviation_min)]
double_proc[is.na(absolute_by_max), 
            absolute_by_max := absolute_by_production + (absolute_by_production*avg_deviation_max)]

# adjust estimates for buttermilk and skim milk according to their production ratios
# find production ratios from buttermilk to skim milk
bm_total <- double_proc[by_item == "Buttermilk, curdled and acidified milk",
                        .(area, year, buttermilk_total = total_by_production)]
skim_total <- double_proc[by_item == "Skim milk of cows",
                          .(area, year, skim_milk_total = total_by_production)]
double_proc[bm_total, on = .(area, year), 
            buttermilk_total := i.buttermilk_total, 
            by = .EACHI]
double_proc[skim_total, on = .(area, year), 
            skim_milk_total := i.skim_milk_total, 
            by = .EACHI]

double_proc[, bms_ratio := buttermilk_total / skim_milk_total][
  ,`:=` (buttermilk_total = NULL, skim_milk_total = NULL)]

# adjust estimates
double_proc[by_item == "Buttermilk, curdled and acidified milk",
            (names(.SD)) := lapply(.SD, function(x) x * bms_ratio), 
            .SDcols = patterns("^absolute")]
double_proc[by_item == "Skim milk of cows",
            (names(.SD)) := lapply(.SD, function(x) x * (1-bms_ratio)), 
            .SDcols = patterns("^absolute")][, bsm:ratio = NULL]

rm(bm_total, skim_total, by_products, main_products)


# for now: exclude NAs 
#-> need to gapfill the extraction rates in earlier steps to deal with this!!!!!!!!!!!!!!!!!!
double_proc <- double_proc[!is.na(absolute_by_production)]


# clean table
double_proc[, `:=`(main_production = NULL,by_main_ratio = NULL, 
                   by_main_max = NULL, by_main_min = NULL)]
setnames(double_proc, c("absolute_by_production", "absolute_by_min","absolute_by_max"),
         c("estimated_by_production", "estimated_by_min","estimated_by_max"))

# sum up estimated production to rescale to total reported production
double_proc[, total_estimated_production := 
              sum(estimated_by_production, na.rm = TRUE),
            by = .(area, year, by_item)]

# find ratio of reported to estimated total values
double_proc[, ratio := total_by_production/total_estimated_production]

# create scaled production by item
double_proc[, scaled_by_production := estimated_by_production * ratio]

# find cases that are out of range 
outliers <- double_proc[scaled_by_production < estimated_by_min | 
                          scaled_by_production > estimated_by_max  ]

# how much do they deviate from their ratio?










# for joining the tables back together
single_proc <- supply_sua[!proc_code %in% by_products$proc_code]
bind_rows(double_proc, single_proc)


