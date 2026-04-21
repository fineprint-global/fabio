library("tidyverse")
library("data.table")

years <- c(2010:2022)
regions <- fread("inst/regions.csv")

items_sua <- fread("inst/sua/items_sua.csv")
proc_sua <- fread("inst/sua/proc_sua.csv")

sua <- readRDS("data/sua_full.rds")
sua[, comm_code := items_sua$comm_code[match(item_code, items_sua$item_code)]] # for now
tcf <- readRDS("data/sua/tcf_sua_final.rds")

# model after this (not needed in the end)
supply_cbs <- readRDS("data/sup_final.rds")

# build supply table ------
# create sua supply table -> here some of the values are double counted (e.g., skim milk)
supply_sua <- sua[, .(area, area_code, item_code, item, year, production)]
supply_sua <- merge(supply_sua, proc_sua[, .(proc, proc_code, comm_code, item_code)], 
                    by = c("item_code"), 
                    allow.cartesian = TRUE)


# find items that come from coupled processes (e.g., skim milk comes from butter and cream production)
by_proc <- proc_sua[, .N, by = item][N>1]
by_proc <- proc_sua[by_proc, on = "item"]

# get processes that supply several items and their items 
double_proc <- supply_sua[proc_code %in% by_proc$proc_code]


# add extraction rates from tcf 
double_proc <-  merge(double_proc, tcf[, .(child_code, area_code, proc_code, extraction_rate, min, max)],
                      by.x =c("comm_code","area_code", "proc_code" ),
                      by.y = c("child_code","area_code", "proc_code"),
                      all.x = TRUE)

# problem: some by products come from processes that have several parents and extraction rates
# here the extraction rates are averaged based on the parent production in that country and year
extraction_avg <- tcf[proc_code %in% c("p256", "p396", "p398")]
extraction_avg <- CJ(year = years, row_id = 1:nrow(extraction_avg))[
  extraction_avg[,row_id := .I], on = "row_id"][, row_id := NULL]

extraction_avg[, prod := sua$production[
  match(
    paste(area_code, parent_code, year),
    paste(sua$area_code, sua$comm_code, sua$year))]]
extraction_avg[, total_prod := sum(prod, na.rm = TRUE), by = .(area_code, year, proc_code,child_code)]
extraction_avg[, parent_share := prod/total_prod]

extraction_avg[, c("extraction_rate", "min", "max") := 
                 lapply(.SD, function(x) round(x * parent_share, 2)), 
               .SDcols = c("extraction_rate", "min", "max")]

# aggregate
extraction_avg[,  c("extraction_rate", "min", "max") :=  
                 lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
               .SDcols = c("extraction_rate", "min", "max"),
               by = .(area_code, year, proc_code, child_code)]
extraction_avg <- extraction_avg[, .(area_code, area, year, proc,
                                            proc_code, item = child,
                                            comm_code = child_code,
                                            extraction_rate, min, max) ]
extraction_avg[, item_code := items_sua$item_code[match(comm_code, items_sua$comm_code)]]
extraction_avg <- unique(extraction_avg)
extraction_avg[, production := sua$production[match(paste(area_code, year, comm_code),
                                              paste(sua$area_code, sua$year, sua$comm_code))]]

# merge back together (now with unique extraction rates)
double_proc <- double_proc[!paste(area_code, year, proc_code, comm_code) %in%
                             paste(extraction_avg$area_code, extraction_avg$year, 
                                   extraction_avg$proc_code, extraction_avg$comm_code)]
double_proc <- rbind(extraction_avg, double_proc)

# separate into main products and by-products to estimate how much of each by-product comes from which process
double_proc[, by_product := ifelse(item_code %in% by_proc$item_code ,TRUE, FALSE)]

by_products <- double_proc[by_product == TRUE][, `:=`(by_product = NULL, item_code = NULL)]
setnames(by_products, c("item", "comm_code", "production", "extraction_rate", "min", "max"), 
         c("by_item", "by_code", "total_by_production","by_extraction", "by_min", "by_max"))

main_products <- double_proc[by_product == FALSE][, `:=`(by_product = NULL, item_code = NULL)]
setnames(main_products, c("item","comm_code", "production", "extraction_rate", "min", "max"), 
         c("main_item", "main_code", "main_production", "main_extraction", "main_min", "main_max"))

# merge main and by products back into one table 
double_proc <- merge(by_products, main_products,
                     by = c("proc_code", "proc", "area", "area_code", "year"), all.x = TRUE)

#find the ratios of the by product to the main product
double_proc[, by_main_ratio := by_extraction/main_extraction]

# define the range 
double_proc[, by_main_max := by_max/main_min]
double_proc[, by_main_min := by_min/main_max]
double_proc_relative <- copy(double_proc)
double_proc[, `:=` (by_extraction = NULL, by_max = NULL, by_min = NULL, 
                    main_extraction = NULL, main_max = NULL, main_min = NULL)]
# double_proc <-  double_proc[total_by_production != 0 & main_production != 0]


# estimate by-production for each process from main production
double_proc[, absolute_by_production := main_production * by_main_ratio]
double_proc[is.na(absolute_by_production), absolute_by_production := 
              main_production * ((by_main_max + by_main_min)/2)]

# estimate absolute minimum
double_proc[, absolute_by_min := 
              main_production * by_main_min]

#estimate absolute maximum
double_proc[, absolute_by_max := 
              main_production * by_main_max]


# adjust estimates for buttermilk and skim milk according to their production ratios
# find production ratios from buttermilk to skim milk
bm_total <- double_proc[by_code == "c446",
                        .(area_code, year, buttermilk_total = total_by_production)]
skim_total <- double_proc[by_code == "c441",
                          .(area_code, year, skim_milk_total = total_by_production)]
double_proc[bm_total, on = .(area_code, year), 
            buttermilk_total := i.buttermilk_total, 
            by = .EACHI]
double_proc[skim_total, on = .(area_code, year), 
            skim_milk_total := i.skim_milk_total, 
            by = .EACHI]

# add buttermilk and skim milk to find their ratio
double_proc[, special_milk_total := buttermilk_total + skim_milk_total]
double_proc[, buttermilk_share := buttermilk_total/special_milk_total]
double_proc[, skim_milk_share := skim_milk_total/special_milk_total]


# adjust estimates
double_proc[by_code == "c446",
            (names(.SD)) := lapply(.SD, function(x) x * buttermilk_share), 
            .SDcols = patterns("^absolute")]
double_proc[by_code == "c441",
            (names(.SD)) := lapply(.SD, function(x) x * (skim_milk_share)), 
            .SDcols = patterns("^absolute")]

# clean up
rm(bm_total, skim_total, main_products)

double_proc[, `:=`(by_main_ratio = NULL, 
                   by_main_max = NULL, by_main_min = NULL, buttermilk_total = NULL, 
                   skim_milk_total = NULL, special_milk_total = NULL,
                   skim_milk_share = NULL, buttermilk_share = NULL)]
setnames(double_proc, c("absolute_by_production", "absolute_by_min","absolute_by_max"),
         c("estimated_by_production", "estimated_by_min","estimated_by_max"))

# sum up estimated production
double_proc[, total_estimated_production := 
              sum(estimated_by_production, na.rm = TRUE),
            by = .(area, year, by_item)]

# find ratio of reported to estimated total values for re-scaling
double_proc[, ratio := total_by_production/total_estimated_production]

# create scaled production by item
double_proc[, scaled_by_production := estimated_by_production * ratio]


# NOTE: In the use table in the end: tcfs will be held variable
# # find cases that are out of range 
outliers <- double_proc[scaled_by_production < estimated_by_min | 
                        scaled_by_production > estimated_by_max  ]


# find deviation from max/min
outliers[ratio > 1, overshoot := scaled_by_production - estimated_by_max]
outliers[ratio < 1, undershoot := estimated_by_min - scaled_by_production]

# relative deviation
outliers[, c("relative_overshoot", "relative_undershoot") := lapply(.SD, function(x){
  x/estimated_by_production
}), .SDcols =  c("overshoot", "undershoot") ]

# tidy for better overview
outliers[, `:=` (total_by_production = NULL, estimated_by_max = NULL, estimated_by_min =NULL,
                 total_estimated_production = NULL, ratio = NULL)]

# round
outliers[, names(outliers) := lapply(.SD, function(x){
  if (is.numeric(x)) round(x,2)  else x
})]

# allow for 20% over/undershoot
bad_outliers <- outliers[(!is.na(relative_undershoot) & relative_undershoot > 0.2) |
                           (!is.na(relative_overshoot) & relative_overshoot >0.2) , ]

#clean up
double_proc[, `:=` (estimated_by_production = NULL, estimated_by_max = NULL,
                    estimated_by_min = NULL, total_estimated_production = NULL,
                    ratio = NULL, total_by_production = NULL, 
                    main_item = NULL, main_production = NULL, 
                    by_code = NULL, main_code = NULL)]
setnames(double_proc, c("by_item", "scaled_by_production"), c("item", "production"))
double_proc[, `:=` (item_code = items_sua$item_code[match(item, items_sua$item)],
                    comm_code = items_sua$comm_code[match(item, items_sua$item)])]

# exclude rows that are in double_proc from supply_sua
supply_sua <- supply_sua[!paste(area_code, year, proc_code, item_code) %in%
                                paste(double_proc$area_code, double_proc$year,
                                      double_proc$proc_code, double_proc$item_code)]

supply_sua <- bind_rows(double_proc,supply_sua)
saveRDS(supply_sua, "data/sua/supply_sua_final.rds")


