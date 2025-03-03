##############################################################################################
##  FABIO Feed Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)
library(wbstats)
library(ggsankey)
library(ggplot2)
library(sf)
library(rnaturalearth)


is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Read labels --------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
regions <- fread(file=paste0(input_path,"regions.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
io <- fread(paste0(input_path,"io_labels.csv"))
su <- fread(paste0(input_path,"su_labels.csv"))

use <- readRDS("data/use_final.rds")
mr_use <-  readRDS(paste0(input_path,"mr_use.rds"))

areas <- regions$iso3c
processes <- unique(su[, .(proc_code,proc)])
commodities <- items$comm_code

yr <- 2021

# Creating feed demand matrix ----------------------------------------------

mr_use_yr <- mr_use[[as.character(yr)]]
rownames(mr_use_yr) <- paste0(rep(areas, each=length(commodities)), "_", commodities)
colnames(mr_use_yr) <- su$proc_code
livestock_proc <- processes[proc %like% "farming" | proc %like% "husbandry", proc_code]
feed_labels <- su[proc_code %in% livestock_proc, ]

mr_feed_use_yr <- mr_use_yr[, colnames(mr_use_yr) %in% livestock_proc]
colnames(mr_feed_use_yr) <- paste0(feed_labels$iso3c, "_", feed_labels$proc_code)
#saveRDS(mr_feed_use_yr, "input/temp_input/Y_feed_21.rds")

# Leontief calculations ----------------------------------------------------

# Read data 
allocation <- "value"
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
L <- readRDS(file=paste0(input_path,"losses/",yr,"_L_",allocation,".rds"))
Y <- mr_feed_use_yr
E <- readRDS(file="input/temp_input/E_biodiv.rds")   #to be changed to input path when E saved correctly 
E <- E[[as.character(yr)]]

################################# Calculating results ####################################
# Calculate mass footprints ----------------------------------------------------
FP <-  L %*% Y
colnames(FP) <- colnames(Y)
rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results_mass <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value =FP@x)
results_mass[,`:=`(year = yr,
              unit = "tonnes",
              iso_origin = substr(origin,1,3),
              item_origin = substr(origin,5,100),
              iso_target = substr(target,1,3),
              proc_target = substr(target,5,100))]

results_mass[,`:=`(comm_code = items$comm_code[match(results_mass$item_origin,items$item)],
              group_origin = items$comm_group[match(results_mass$item_origin,items$item)],
              continent_origin = regions$continent[match(results_mass$iso_origin, regions$iso3c)],
              continent_target = regions$continent[match(results_mass$iso_target, regions$iso3c)],
              origin = NULL, target = NULL)]
results_mass[item_origin %in% items[group!="Primary crops", item], value := 0]
results_mass <- results_mass %>% relocate(value, .after = last_col())
results_mass_no_gf <- copy(results_mass)
results_mass_no_gf[item_origin %in% c("Grazing", "Fodder crops"), value := 0]



# Calculate global BD footprints based on pdf per ton --------------------------
#(through land use) (risk of global extinction)
Xi <- X[, as.character(yr)]
ext <- rowSums(E[, 9:11]) / as.vector(Xi)
ext[!is.finite(ext) | is.na(ext)] <- 0
MP <- ext * L
FP <- MP %*% Y
colnames(FP) <- colnames(Y)
rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results_g <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value =FP@x)
results_g[,`:=`(year = yr,
                   unit = "pdf",
                   iso_origin = substr(origin,1,3),
                   item_origin = substr(origin,5,100),
                   iso_target = substr(target,1,3),
                   proc_target = substr(target,5,100))]
results_g[,`:=`(comm_code = items$comm_code[match(results_g$item_origin,items$item)],
                   group_origin = items$comm_group[match(results_g$item_origin,items$item)],
                   continent_origin = regions$continent[match(results_g$iso_origin, regions$iso3c)],
                   continent_target = regions$continent[match(results_g$iso_target, regions$iso3c)],
                   origin = NULL, target = NULL)]
results_g <- results_g %>% relocate(value, .after = last_col())
results_g[item_origin %in% items[group!="Primary crops", item], value := 0]

# #Find BF distribution within LS groups
# BF_dist <- copy(results_g)
# BF_dist[, ls_group:=livestock_groups$livestock_group[match(proc_target, livestock_groups$proc_code)]]
# BF_dist <- BF_dist[ls_group == "Ruminants", .(fp = sum(value)), by = proc_target]
# BF_dist[, proc_name := processes$proc[match(proc_target, processes$proc_code)]]
# BF_dist[, fp_share := fp/sum(fp)]
# write.csv(BF_dist, "output/tables/BF_distribution_ls_groups_global.csv")



# Calculate regional BD footprints based on mass per hectare -------------------
#(risk of regional extinction) -> only used in Appendix
ext <- rowSums(E[, 12:14]) / as.vector(Xi)
ext[!is.finite(ext) | is.na(ext)] <- 0
MP <- ext * L
FP <- MP %*% Y
colnames(FP) <- colnames(Y)
rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results_r <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value =FP@x)
results_r[,`:=`(year = yr,
                   unit = "pdf",
                   iso_origin = substr(origin,1,3),
                   item_origin = substr(origin,5,100),
                   iso_target = substr(target,1,3),
                   proc_target = substr(target,5,100))]
results_r[,`:=`(comm_code = items$comm_code[match(results_r$item_origin,items$item)],
                   group_origin = items$comm_group[match(results_r$item_origin,items$item)],
                   continent_origin = regions$continent[match(results_r$iso_origin, regions$iso3c)],
                   continent_target = regions$continent[match(results_r$iso_target, regions$iso3c)],
                   origin = NULL, target = NULL)]
results_r %>% relocate(value, .after = last_col())
results_r[item_origin %in% items[group!="Primary crops", item], value := 0]


# Calculate per capita global BD footprints ---------------------------------
pop_data <- as.data.table(wbstats::wb_data(indicator = "SP.POP.TOTL", start_date = yr, end_date = yr))
pop_data <- pop_data[,.(iso3c, pop = SP.POP.TOTL)]
attributes(pop_data$pop)$label <- NULL
pop_data <- merge(pop_data, regions, by = "iso3c", all = TRUE)
pop_data[is.na(area_code), iso3c := "ROW"]
pop_data <- pop_data[ , .(pop = sum(pop, na.rm = TRUE)), by= iso3c]
pop_data <- pop_data[pop == 0, pop := NA]
pop_data[,mil_pop := pop/1000000][,pop := NULL]

results_g_pop <- merge(results_g, pop_data, by.x = "iso_target", by.y = "iso3c")
results_g_pop <- results_g_pop[, .(sum_value = sum(value, na.rm = TRUE), mil_pop = unique(mil_pop)), by = iso_target]
results_g_pop[, per_mil_cap := sum_value/mil_pop][, `:=`(sum_value = NULL, mil_pop = NULL)]
results_g_pop[, country := regions$area[match(iso_target, regions$iso3c)]]

write.csv(results_g_pop, "output/tables/per_capita_BFs.csv")

# Calculate footprints per thousand tonnes of animal protein produced -----

#get protein shares from cbs (not used in original fabio -> needs to be cleaned up)
source("R/01_tidy_functions.R")  # for some reason there is a difference in the names of "regions"- > have to change in tidy_functions to work here
cbs_food <- readRDS("input/fao/cbs_food_new.rds")
cbs_food <- cbs_food[Year == as.character(yr),][,`:=`(Flag = NULL, Note = NULL)]
cbs_food[Unit == "1000 tonnes", `:=`(Value = ifelse(is.na(Value), 0, Value*1000) , Unit = "tonnes")]
cbs_nonfood <- readRDS("input/fao/cbs_nonfood_new.rds")
cbs_nonfood <- cbs_nonfood[Year == as.character(yr) , 1:(ncol(cbs_nonfood)-2)]
cbs_nonfood[Element == "Food supply quantity (tonnes)", Element := "Food"]
cbs_nonfood <- merge(cbs_nonfood, cbs_food[, .SD, .SDcols = c("Area Code", "Item Code", "Element", "Year Code", "Value")],
                     all.x = TRUE,
                     by = c("Area Code", "Item Code", "Element", "Year Code"),
                     suffixes = c("", ".food"))
cbs_nonfood <- cbs_nonfood[is.na(Value.food) ,]
cbs_nonfood <- cbs_nonfood[,`:=` (Value.food = NULL)]

# bind
cbs <- rbind(cbs_food, cbs_nonfood, fill=TRUE)
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Element" = "element",
  "Year" = "year",
  "Unit" = "unit",
  "Value" = "value")
cbs <- dt_rename(cbs, rename, drop = TRUE)


# Country / Area adjustments
cbs <- area_kick(cbs, code = 351, pattern = "China", groups = TRUE)
cbs <- area_merge(cbs, orig = 62, dest = 238, pattern = "Ethiopia")
cbs <- area_merge(cbs, orig = 206, dest = 276, pattern = "Sudan")
cbs <- area_fix(cbs, regions)

# filter needed elements
cbs <- cbs[ element %in% c("Food supply quantity (kg/capita/yr)", "Protein supply quantity (g/capita/day)")]
cbs <- data.table::dcast(cbs, area_code + area + item_code + item  ~ element,
                         value.var = "value") # fun.aggregate = sum, na.rm = TRUE sum is used her because remaining duplicates only contain NAs in food balance
cbs <- dt_filter(cbs, item_code %in% items$item_code)
cbs[, group := items$group[match(item_code, items$item_code)]]


ls_protein <- cbs[group == "Livestock products",][, group := NULL]
ls_protein[, `:=`(`Food supply kg/capita/day` = `Food supply quantity (kg/capita/yr)`/ 365,
                  `Protein supply kg/capita/day` = `Protein supply quantity (g/capita/day)`/1000)][
                    ,`:=`(`Food supply quantity (kg/capita/yr)` = NULL,  `Protein supply quantity (g/capita/day)`= NULL)
                  ]


# get production by ls group to make argument that ruminants cause disproportionate harm
# protein_by_group <- copy(ls_protein)
# protein_by_group <- protein_by_group[item %in% c("Bovine Meat","Mutton & Goat Meat",  "Butter, Ghee",                            
#                       "Milk - Excluding Butter"),group := "Ruminants"]
# protein_by_group <- protein_by_group[item %in% c("Pigmeat"), group := "Pigs" ]
# protein_by_group <- protein_by_group[item %in% c("Poultry Meat","Eggs"), group := "Poultry Birds" ]
# protein_by_group <- protein_by_group[item %in% c("Meat, Other"), group := "Equids and Camelids" ]
# protein_by_group <- protein_by_group[!is.na(group), .(total_supply_pc = sum(`Protein supply kg/capita/day`)), 
#                                      by = .(group, area)] 
# protein_by_group <- protein_by_group[!is.na(group), .(mean_protein = mean(total_supply_pc)), 
#                                      by = group ]
# protein_by_group[, total_prot := sum(mean_protein)]
# protein_by_group[, group_share := mean_protein/total_prot][, `:=`(mean_protein = NULL, total_prot = NULL)]
# write.csv(protein_by_group, "output/tables/global_average_protein_supply_by_ls_group.csv")

#continue with protein share from before
ls_protein[, protein_share := `Protein supply kg/capita/day`/ `Food supply kg/capita/day`]
ls_protein[protein_share > 0.4, protein_share := 0.4]   #deal with unrealistic numbers (probably rounding mistakes)
ls_protein <- ls_protein[is.na(protein_share) | is.nan(protein_share), protein_share := mean(protein_share, na.rm = TRUE), by = item]

#get production in thousand tonnes from cbs_full -> shorter than cleaning up cbs fully here 
#(this will be multiplied with protein share per item to determine total protein production
#in a country)
cbs <- readRDS("data/cbs_full.rds")
ls_production <- cbs[year == as.character(yr),.(area_code, area, item, production)]
ls_production[, group := items$group[match(item, items$item)]]
ls_production <- ls_production[group == "Livestock products", ][,group := NULL]
ls_production[, prod_1000_t := production/1000][, production := NULL]


protein_prod <- merge(ls_production, ls_protein[,.(protein_share, item, area_code)], by = c("area_code", "item"), all = TRUE)
protein_prod[,iso3c:=regions$iso3c[match(area_code, regions$area_code)]]
protein_prod[, prot_prod := prod_1000_t * protein_share][, `:=` (prod_1000_t = NULL, protein_share =NULL)]
protein_prod <- protein_prod[,.(total_protein = sum(prot_prod, na.rm = TRUE)), by = iso3c]
protein_prod[, country := regions$area[match(iso3c, regions$iso3c)]]

#per capita protein production t/cap
pc_protein_prod <- protein_prod[, `:=` (protein_tons = total_protein *1000, pop = pop_data$mil_pop[match(iso3c, pop_data$iso3c)] * 1000000)]
pc_protein_prod[, pc_prot_tons := protein_tons/pop]
pc_protein_prod[, country := regions$area[match(iso3c, regions$iso3c)]]


results_g_prot <- results_g[, .(sum_value = sum(value, na.rm = TRUE)), by = iso_target]
results_g_prot <- merge(results_g_prot, protein_prod[, .(iso3c, total_protein)], by.x = "iso_target" , by.y = "iso3c")
results_g_prot[, prot_fp := sum_value/total_protein][, `:=`(sum_value = NULL, total_protein = NULL)]
results_g_prot[!is.finite(prot_fp), prot_fp :=NA]
results_g_prot[,country := regions$area[match(iso_target, regions$iso3c)]]

#write.csv(results_g_prot, "output/tables/BFs_per_1000t_ls_protein.csv")
#write.csv(protein_prod, "output/tables/protein_production.csv")
rm(cbs, cbs_food, cbs_nonfood, FP, protein_prod, MP, ext, L, Y, X, Xi, E)


# Calculate BD impact per ton of feed consumption ----------------------------
feed_consumption <- results_mass[,.(total_feed_consumption = sum(value)), by= .(iso_target)]
feed_impact <- results_g[,.(bd_value = sum(value)), by= iso_target]
average_impact <- merge(feed_impact, feed_consumption, by = "iso_target")
average_impact[, average_impact := bd_value / total_feed_consumption][, `:=` (bd_value = NULL, total_feed_consumption = NULL)]
average_impact[, country := regions$area[match(iso_target, regions$iso3c)]]
#write.csv(average_impact, "output/tables/CF_impact_per_1000_t_feed_cons.csv")


# Calculate feed production in tons per capita ---------------------------------
feed_production[, pop := pop_data$mil_pop[match(iso_origin, pop_data$iso3c)]*1000000]
feed_production[, per_cap := value/pop]
feed_production[, country := regions$area[match(iso_origin, regions$iso3c)]]
#write.csv(feed_production, "output/tables/feed_production_ton.csv")


# Prep for visualizations ------------------------------------

livestock_groups <- processes[proc %like% "farming" | proc %like% "husbandry", .(proc_code,proc)][
  proc %like% "attle"|proc %like% "uffaloes"|proc %like% "heep"|proc %like% "oats", livestock_group:="Ruminants"
][proc %like% "Pigs", livestock_group:="Pigs"
][proc %like% "amel" | proc %like% "Horses" | proc %like% "Mules" | proc %like% "Asses", livestock_group := "Equids and Camelids"
][proc %like% "Rabbits" | proc %like% "Rodents", livestock_group:="Rodents"
][proc %like% "Poultry", livestock_group:="Poultry Birds"]






################################ Visualizations ##################################

# Most of the following code can be run by itself if script is run until here
#There are some exceptions with the import/export visualizations which depend on each other.
#This is commented when it is the case

############# Maps ####################################

# Livestock protein production per capita --------------------------------
world_map <- ne_countries(scale = "medium", returnclass = "sf")
fp <- copy(pc_protein_prod)
pc_protein_prod <- protein_prod[pc_prot_tons >0,]
world_fp <- left_join(world_map, pc_protein_prod, by = c("iso_a3_eh" = "iso3c"))


#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),
  inverse = function(x) 10^x
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(pc_protein_prod$pc_prot_tons, na.rm = TRUE)), log10(max(pc_protein_prod$pc_prot_tons, na.rm = TRUE)),  length.out=6)
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["pc_prot_tons"]]), size = 0.05) +
  labs(
    fill = "tons per capita per year", 
    title = "Per Capita Livestock Protein Production"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("3.8e-4", "1.31e-3", "4.46e-3", "1.52e-2", "5.16e-2", "1.76e-1"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25,  # Increased bar width for better visibility
      barheight = 1,  # Keep height small for a sleek look
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_PC-Protein_prod.png", plot = plot_CF_impact, width = 14, height = 7, dpi = 300)
write.csv(pc_protein_prod, "output/tables/pc_protein_production.csv")



# CF per 1000 tonnes of feed consumption ----------------------------
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_fp <- left_join(world_map, average_impact, by = c("iso_a3_eh" = "iso_target"))

#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),
  inverse = function(x) 10^x
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(average_impact$average_impact, na.rm = TRUE)), log10(max(average_impact$average_impact, na.rm = TRUE)),  length.out=6)
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
plot_CF_impact <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["average_impact"]]), size = 0.05) +
  labs(
    fill = "pdf per ton", 
    title = "Impact per Ton"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("1.69e-15", "1.61e-14", "1.54e-13", "1.47e-12", "1.41e-11", "1.35e-10"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25,  # Increased bar width for better visibility
      barheight = 1,  # Keep height small for a sleek look
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_CF_impact_per_ton_CB.png", plot = plot_CF_impact, width = 14, height = 7, dpi = 300)




# Per capita feed production -----------------------------------------------

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_fp <- left_join(world_map, feed_production, by = c("iso_a3_eh" = "iso_origin"))

#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x + 0.01),
  inverse = function(x) 10^x
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(feed_production$per_cap + 0.01 , na.rm = TRUE)), log10(max(feed_production$per_cap, na.rm = TRUE)),  length.out=6)
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
plot_feed_prod <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["per_cap"]]), size = 0.05) +
  labs(
    fill = "tons per capita", 
    title = "Per Captia Feed Production"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("0.20", "0.60", "1.82", "5.50", "16.61", "50.13"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25,  # Increased bar width for better visibility
      barheight = 1,  # Keep height small for a sleek look
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_Feed_production.png", plot = plot_feed_prod, width = 14, height = 7, dpi = 300)




# BD footprint per country in global pdf, absolute -----------------------------
world_map <- ne_countries(scale = "medium", returnclass = "sf")
fp_results_g <- copy(results_g)
fp <- fp_results_g[, .(sum_value = sum(value)), by = iso_target]
# fp[, country := regions$area[match(iso_target, regions$iso3c)]]
# write_csv(fp, "output/tables/BD_footprints_global_pdf.csv")
world_fp <- left_join(world_map, fp, by = c("iso_a3_eh" = "iso_target"))

#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),   
  inverse = function(x) 10^x     
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(fp$sum_value)), log10(max(fp$sum_value)),  length.out=6) 
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
plot_BD_fp <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["sum_value"]]), size = 0.05) +
  labs(
    fill = "pdf", 
    title = "Absolute"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("8.10e-08", "7.24e-07", "6.47e-06", "5.79e-05", "5.17e-04", "4.62e-03"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25,  # Increased bar width for better visibility
      barheight = 1,  # Keep height small for a sleek look
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_BD_footprints_global_pdf.png", plot = plot_BD_fp, width = 14, height = 7, dpi = 300)



# BD footprint per country regional --------------------------------------
#(same but with regional pdf for comparison, good for appendix)
fp_results_r <- copy(results_r)
fp <- fp_results_r[, .(sum_value = sum(value)), by = iso_target]
#write_csv(fp, "output/tables/BD_footprints_regional_pdf.csv")
world_fp <- left_join(world_map, fp, by = c("iso_a3_eh" = "iso_target"))

plot_BD_fp_reg <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["sum_value"]]), size = 0.05) +
  labs(
    fill = "potentially disappeared fraction of species", 
    title = "Regional Biodiversity Footprints of Livestock Feed"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks,          # same breaks as in global footprints for comparability
    labels = c("8.10e-08", "7.24e-07", "6.47e-06", "5.79e-05", "5.17e-04", "4.62e-03"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25, 
      barheight = 1, 
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_BD_footprints_regional_pdf.png", plot = plot_BD_fp_reg, width = 14, height = 7, dpi = 300)



# BD footprint per country in global pdf, per capita -----------------------
world_map <- ne_countries(scale = "medium", returnclass = "sf")
fp <- copy(results_g_pop)
#write_csv(fp, "output/tables/BD_footprints_global_pdf.csv")
world_fp <- left_join(world_map, fp, by = c("iso_a3_eh" = "iso_target"))

#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),   
  inverse = function(x) 10^x     
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(fp$per_mil_cap, na.rm = TRUE)), log10(max(fp$per_mil_cap, na.rm = TRUE)),  length.out=6) 
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
pc_BD_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["per_mil_cap"]]), size = 0.05) +
  labs(
    fill = "pdf", 
    title = "Per Million Capita"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("4.32e-08", "2.25e-07", "1.18e-06", "6.13e-06", "3.19e-05", "1.67e-04"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25,  # Increased bar width for better visibility
      barheight = 1,  # Keep height small for a sleek look
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )

print(pc_BD_plot)
ggsave("output/plots/Map_BD_per_capita_footprints.png", plot = pc_BD_plot, width = 14, height = 7, dpi = 300)


# BD footprints per 1000t of protein ------------------------------------------
world_map <- ne_countries(scale = "medium", returnclass = "sf")
fp <- copy(results_g_prot)
#write_csv(fp, "output/tables/BD_footprints_global_pdf.csv")
world_fp <- left_join(world_map, fp, by = c("iso_a3_eh" = "iso_target"))

#create logarithmic color scale to highlight differences in small values
transform_custom <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),   
  inverse = function(x) 10^x     
)
#create sequence based on color scale to display legend values evenly
log_seq <- seq(log10(min(fp$prot_fp, na.rm = TRUE)), log10(max(fp$prot_fp, na.rm = TRUE)),  length.out=6) 
x_seq <- as.data.table(lapply(log_seq, function(x) 10^x))
x_seq <- pivot_longer(x_seq, V1:V6)
x_seq <- x_seq %>% arrange(value)
my_breaks <- as.vector(x_seq$value)

#create map
prot_BD_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["prot_fp"]]), size = 0.05) +
  labs(
    fill = "pdf", 
    title = "Per 1000t of Livestock Protein Production"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks, 
    labels = c("9.46e-09", "5.24e-08", "2.90e-07", "1.60e-06", "8.87e-06", "4.91e-05"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank(),                               
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.title = element_text(size = 20, hjust = 0.5),  
    legend.text = element_text(size = 15),               
    plot.title = element_text(hjust = 0.5, size = 30),  
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 25, 
      barheight = 1,  
      title.position = "top",      
      title.hjust = 0.5,
      direction = "horizontal"  
    )
  )


ggsave("output/plots/Map_BD_footprints_per_1000t_protein.png", plot = prot_BD_plot, width = 14, height = 7, dpi = 300)




# BD import maps by continent-------------------------------------------------
#prep data imports
fp_imp <- copy(results_g)
fp_imp <- fp_imp %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  mutate(import = if_else(continent_origin==continent_target, "domestic", "imported")) %>% 
  filter(import != "domestic")%>% 
  group_by(iso_origin, continent_target) %>% 
  summarise(total_value = sum(value, na.rm = TRUE))%>%
  filter(continent_target != "ROW") %>%
  mutate(total_value = if_else(total_value < 9.802700e-11, 0, total_value)) %>% 
  setDT


#### Plot

#create logarithmic color scale to highlight differences in small values
transform_custom_trade <- scales::trans_new(
  name = "custom_log",
  transform = function(x) log10(x),   
  inverse = function(x) 10^x, 
)
#create sequence based on color scale to display legend values evenly
log_seq_trade <- seq(log10(1.057951e-10), log10(4.690246e-04),  length.out=6) #range of imp/exp datapoints
x_seq_trade <- as.data.table(lapply(log_seq_trade, function(x) 10^x))
x_seq_trade <- pivot_longer(x_seq_trade, V1:V6)
x_seq_trade <- x_seq_trade %>% arrange(value)
my_breaks_trade <- c(0, x_seq_trade$value)  # Combine 0 with the sequence
my_labels_trade <- c("0", sprintf("%.2e", x_seq_trade$value))


#Import maps (only as example, continent needs to be changed accordingly, legend will be 
# added in the end with all continents, otherwise displayed incorrectly)
#continents <- c("AFR" , "EUR" , "LAM" , "NAM" , "OCE" , "ASI") 

world_map <- ne_countries(scale = "medium", returnclass = "sf")
fp_imp_ASI <- fp_imp[continent_target == "ASI", ]
world_fp <- left_join(world_map, fp_imp_ASI, by = c("iso_a3_eh" = "iso_origin"))

ASI_imp_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["total_value"]]), size = 0.05) +
  labs(
    fill = "global pdf (species per year)", 
    title = "ASIA"
  ) +
  scale_fill_gradientn(
    colors = c("yellow", viridis::viridis(length(my_breaks_trade) - 1, direction = -1)), # Include yellow for 0
    values = scales::rescale(my_breaks_trade),  # Rescale breaks
    breaks = my_breaks_trade, 
    labels = my_labels_trade, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    plot.title = element_text(hjust = 0.5, size = 14),           
    panel.grid = element_blank(),                                
    axis.text = element_blank(),
    axis.title = element_blank(),
    #legend.position = "none"
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.8, 
      barheight = 8,                 
      title.position = "top",      
      title.hjust = 0.5              
    )
  )

print(ASI_imp_plot)
ggsave("output/plots/Import_ASI_BD.png", plot = ASI_imp_plot, width = 10, height = 7, dpi = 300)

#Plot for displaying legend correctly -> need to have another look at scaling
world_map <- ne_countries(scale = "medium", returnclass = "sf")
#fp_imp_ASI <- fp_imp[continent_target == "ASI", ]
world_fp <- left_join(world_map, fp_imp, by = c("iso_a3_eh" = "iso_origin"))

imp_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["total_value"]]), size = 0.05) +
  labs(
    fill = "pdf", 
    title = "ASIA"
  ) +
  scale_fill_viridis_c(
    breaks = my_breaks_trade, 
    labels = c("1.06e-10", "2.26e-9", "4.82e-8", "1.03e-6", "2.20e-5", "4.69e-4"),
    trans = transform_custom,
    direction = -1, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),  
    plot.title = element_text(hjust = 0.5, size = 14),           
    panel.grid = element_blank(),                                
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",  # Move legend to bottom
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "cm") # Adjust key size if needed
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 30,   # Make legend wide
      barheight = 0.8,  # Reduce height for a more horizontal look
      title.position = "top",      
      title.hjust = 0.5              
    )
  )

print(imp_plot)
ggsave("output/plots/Legend_imp_exp.png", plot = imp_plot, width = 10, height = 7, dpi = 300)


# BD export maps by continent ----------------------------------------------

#prep data exports
fp_exp <- copy(results_g)
fp_exp <- fp_exp %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  mutate(export = if_else(continent_origin==continent_target, "domestic", "exported"))%>%
  filter(export != "domestic")%>% 
  group_by(continent_origin, iso_target) %>% 
  summarise(total_value = sum(value, na.rm = TRUE))%>%
  filter(continent_origin != "ROW") %>% 
  mutate(total_value = if_else(total_value < 9.802700e-11, 0, total_value)) %>% 
  setDT

#Plot export map
fp_exp_OCE <- fp_exp[continent_origin == "OCE", ]
world_fp <- left_join(world_map, fp_exp_OCE, by = c("iso_a3_eh" = "iso_target"))

OCE_exp_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["total_value"]]), size = 0.05) +
  labs(
    fill = "global pdf (species per year)", 
    title = "OCEANIA"
  ) +
  scale_fill_gradientn(
    colors = c("yellow", viridis::viridis(length(my_breaks_trade) - 1, direction = -1)), # Include yellow for 0
    values = scales::rescale(my_breaks_trade),  # Rescale breaks
    breaks = my_breaks_trade, 
    labels = my_labels_trade, 
    na.value = "lightgrey"
  ) +
  coord_sf(crs = "+proj=robin") + # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),  
    plot.title = element_text(hjust = 0.5, size = 14),          
    panel.grid = element_blank(),                                
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
    # legend.background = element_blank(),
    # legend.key = element_blank(),
    # legend.title = element_text(size = 9, hjust = 0.5),  
    # legend.text = element_text(size = 9),               
    # legend.key.size = unit(0.8, "cm")                   
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 0.8, 
      barheight = 8,                 
      title.position = "top",      
      title.hjust = 0.5              
    )
  )


ggsave("output/plots/Export_OCE_BD.png", plot = OCE_exp_plot, width = 10, height = 7, dpi = 300)



# Imports and Exports at country and value chain scale ----------------------
# (Here, the code builds upon itself -> do not run plot by plot)  
fp_imp_iso <- copy(results_g[,.(item_origin,iso_origin,iso_target, value)])
fp_imp_iso <- fp_imp_iso %>% 
  filter(value != 0) %>% 
  mutate(import = if_else(iso_origin == iso_target, "domestic", "imported")) %>%
  group_by(iso_target, import) %>% 
  summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(id_cols = iso_target, names_from = import, values_from = total_value) %>% 
  setDT

fp_exp_iso <- copy(results_g[,.(item_origin,iso_origin,iso_target, value)])  
fp_exp_iso <- fp_exp_iso %>% 
  filter(value != 0) %>% 
  mutate(export = if_else(iso_origin == iso_target, "domestic", "exported")) %>%
  group_by(iso_origin, export) %>% 
  summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop') %>% 
  pivot_wider(id_cols = iso_origin, names_from = export, values_from = total_value) %>% 
  subset(select = -domestic) %>% 
  setDT

# #sum up imports and exports by continent for numbers within text
# fp_trade_agg <- merge(fp_imp_iso, fp_exp_iso, by.x="iso_target", by.y="iso_origin")
# fp_trade_agg[,domestic := NULL][, continent := regions$continent[match(iso_target, regions$iso3c)]]
# fp_trade_agg <- fp_trade_agg[,.(import = sum(imported, na.rm = TRUE), export =sum (exported, na.rm = TRUE)), by = continent]

#Find largest absolute importers and exporters
fp_trade_abs <- merge(fp_imp_iso, fp_exp_iso, by.x="iso_target", by.y="iso_origin")
fp_trade_abs[,domestic := NULL]
setnames(fp_trade_abs, "iso_target", "iso3c")
fp_trade_abs[, top10_import := imported %in% sort(imported, decreasing = TRUE)[1:10]]
fp_trade_abs[, top10_export := exported %in% sort(exported, decreasing = TRUE)[1:10]]

imports <- fp_trade_abs[top10_import == TRUE][, ggcolumn := "import"]
exports <- fp_trade_abs[top10_export == TRUE][, ggcolumn := "export"]
fp_trade_abs <- rbind(imports, exports)
fp_trade_abs[, ggcolumn := factor(ggcolumn, levels = c("import", "export"))]
fp_trade_abs[, country := regions$area[match(iso3c, regions$iso3c)]]
fp_trade_abs[country == "China, mainland", country := "China"][
  country == "United States of America", country := "USA" ][
    country == "Republic of Korea", country := "South Korea"
  ]

top10_importers <- fp_trade_abs[ggcolumn == "import", iso3c]
top10_exporters <- fp_trade_abs[ggcolumn == "export", iso3c]

#find top value chains for largest importers and exporters (for the appendix table everything was changed to 100)
value_chain_diversity <- copy(results_g)
value_chain_diversity <- value_chain_diversity[iso_origin!=iso_target,]
value_chain_diversity <- value_chain_diversity[, .(value = sum(value)), by = .(iso_origin, iso_target, item_origin) ]
top_export_chains <- value_chain_diversity[iso_origin %in% top10_exporters, .SD[which.max(value)], by = iso_origin]
top_export_chains[,total_export := fp_trade_abs$exported[match(iso_origin, fp_trade_abs$iso3c)]]
top_export_chains[, share := value/total_export]
top_export_chains[, country_origin := regions$area[match(iso_origin, regions$iso3c)]]
top_export_chains[,country_target := regions$area[match(iso_target, regions$iso3c)]]

top_import_chains <- value_chain_diversity[iso_target %in% top10_importers, .SD[which.max(value)], by = iso_target]
top_import_chains [, total_import := fp_trade_abs$imported[match(iso_target, fp_trade_abs$iso3c)]]
top_import_chains[, share := value/total_import]
top_import_chains[, country_origin := regions$area[match(iso_origin, regions$iso3c)]]
top_import_chains[,country_target := regions$area[match(iso_target, regions$iso3c)]]

write.csv(top_export_chains, "output/tables/top_exporter_value_chains.csv")
write.csv(top_import_chains, "output/tables/top_importer_value_chains.csv")

# Bar chart top 10 importers and exporters
trade_plot <- ggplot(fp_trade_abs, aes(
  x = reorder(country, ifelse(ggcolumn == "import", -imported, exported)), 
  y = ifelse(ggcolumn == "import", -imported, exported), 
  fill = ggcolumn
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) + 
  scale_fill_manual(values = c("import" = "#98d9e4", "export" = "#ef8a0c")) + 
  labs(
    title = "Top 10 Importers and Exporters",
    x = NULL,  # X-axis label
    y = "pdf",  # Y-axis label
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),  
    axis.text.y = element_text(size = 20, color = "black"),  
    axis.text.x = element_text(size = 20, color = "black"), 
    axis.title.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 20, color = "black", hjust = 0.47),
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),  
    plot.background = element_rect(fill = "white", color = NA),  
    panel.grid = element_blank()  
  )

print(trade_plot)
ggsave("output/plots/top10_abs_importers_exporters.png", plot = trade_plot, width = 10, height = 7, dpi = 300)




# Find largest per capita importers and exporters
fp_imp_exp <- merge(fp_imp_iso, fp_exp_iso, by.x="iso_target", by.y="iso_origin")
fp_imp_exp[,domestic := NULL]
fp_imp_exp[,pop := pop_data$mil_pop[match(iso_target, pop_data$iso3c)]]
fp_imp_exp[, `:=` (imported = imported/pop, exported = exported/pop)][, pop := NULL]
# fp_imp_exp[, country := regions$area[match(iso_target, regions$iso3c)]]
# write.csv(fp_imp_exp, "output/tables/import_export.csv") #for Appendix
fp_imp_exp[, top10_import := imported %in% sort(imported, decreasing = TRUE)[1:10]]
fp_imp_exp[, top10_export := exported %in% sort(exported, decreasing = TRUE)[1:10]]

imports <- fp_imp_exp[top10_import == TRUE][, ggcolumn := "import"]
exports <- fp_imp_exp[top10_export == TRUE][, ggcolumn := "export"]
fp_imp_exp <- rbind(imports, exports)
fp_imp_exp[, ggcolumn := factor(ggcolumn, levels = c("import", "export"))]
fp_imp_exp[, country := regions$area[match(iso_target, regions$iso3c)]]

# Create bar chart
imp_exp_plot <- ggplot(fp_imp_exp, aes(
  x = reorder(country, ifelse(ggcolumn == "import", -imported, exported)),
  y = ifelse(ggcolumn == "import", -imported, exported),
  fill = ggcolumn
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c("import" = "#98d9e4", "export" = "#ef8a0c")) +
  labs(
    title = "Top 10 Importers and Exporters",
    x = NULL,  # X-axis label
    y = "pdf per million inhabitants",  # Y-axis label
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank()
  )

print(imp_exp_plot)
ggsave("output/plots/top5_importers_exporters.png", plot = imp_exp_plot, width = 10, height = 7, dpi = 300)



# Net importers and exporters
fp_net_total <- merge(fp_imp_iso, fp_exp_iso, by.x="iso_target", by.y="iso_origin")
fp_net_total[, net_import := imported-exported][,total:= imported + domestic] # export doesn't need to be subtracted, because it was never added
fp_net_total[, country := regions$area[match(fp_net_total$iso_target, regions$iso3c)]]
fp_net_total <- fp_net_total[!is.na(imported) & !is.na(exported),]
quantiles <- fp_net_total[,quantile(net_import, probs=c(0.1, 0.9))]
fp_net_total[,class := fifelse(net_import <= quantiles[1], "net exporter (top 10%)",
                               fifelse(net_import > quantiles[1] & net_import <=quantiles[2], 
                                       "middle", "net importer (top 10%)"))]

#write_csv(fp_net_total, "output/tables/BD_net_importers_net_exporters.csv")


#### Plot net importers/exporters
world_fp <- left_join(world_map, fp_net_total, by = c("iso_a3_eh" = "iso_target"))
world_fp$class <- factor(world_fp$class, levels = c("net exporter (top 10%)", "middle", "net importer (top 10%)")) #for legend order

net_plot <- ggplot(data = world_fp) +
  geom_sf(aes(fill = .data[["class"]]), size = 0.05) +
  labs(
    title = "Net Importers and Exporters of Biodiversity Footprints",
    fill = NULL 
  ) +
  coord_sf(crs = "+proj=robin") +  # Robinson projection
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 15),  # Increase legend text size
    plot.title = element_text(hjust = 0.5, size = 25),  # Increase title size
    legend.key.size = unit(0.8, "cm")
  ) +
  scale_fill_viridis_d(option = "D",
                       na.value = "lightgrey") +  
  guides(
    fill = guide_legend(override.aes = list(size = 4))  
  )



ggsave("output/plots/Map_BD_net_importers_exporters.png", plot = net_plot, width = 10, height = 7, dpi = 300)


# Value chains  --------------------------------------------------------
#(colors by continent, lines by country)
results_chains <- copy(results_g)
results_chains <- results_chains[iso_origin!=iso_target,]
top_5_chains <- results_chains[order(-value)][, .SD[1], by = .(iso_origin, iso_target)][1:5]

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map$continent <- as.factor(world_map$continent)
world_map_robin <- st_transform(world_map, crs = "+proj=robin")

# Calculate centroids in Robinson projection
centroids_robin <- st_centroid(world_map_robin)
centroids_df <- data.frame(
  iso = world_map_robin$iso_a3,
  continent = world_map_robin$continent,
  lon = st_coordinates(centroids_robin)[,1],
  lat = st_coordinates(centroids_robin)[,2]
)

# Add coordinates and continent information to the top_5_chains data.table
top_5_chains <- top_5_chains[centroids_df, on = c("iso_origin" = "iso"), 
                             `:=`(lon_from = i.lon, lat_from = i.lat, continent_origin = i.continent)]
top_5_chains <- top_5_chains[centroids_df, on = c("iso_target" = "iso"), 
                             `:=`(lon_to = i.lon, lat_to = i.lat, continent_to = i.continent)]

#plot

world_map_robin <- world_map_robin %>%
  mutate(fill_color = case_when(
    iso_a3 %in% top_5_chains$iso_origin ~ iso_a3,
    iso_a3 %in% top_5_chains$iso_target ~ "target",
    TRUE ~ "other"
  ))

origin_colors <- viridis(6, option = "D", begin = 0.1, end = 0.9)
unique_iso_codes <- unique(c(top_5_chains$iso_origin, top_5_chains$iso_target))
fill_colors <- c(setNames(origin_colors[1:length(unique_iso_codes)], unique_iso_codes),
                 "other" = "grey", "target" = "grey40")

ggplot(data = world_map_robin) +
  geom_sf(aes(fill = fill_color), color = "white", size = 0.1) +
  scale_fill_manual(
    values = fill_colors, 
    breaks = c(unique(top_5_chains$iso_origin), "target"),
    labels = c(unique(top_5_chains$iso_origin), "Target Countries"),
    name = "Countries"
  ) +
  geom_curve(
    data = top_5_chains,
    aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to, 
        size = value, color = iso_origin),
    curvature = 0.2,
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  scale_color_manual(
    values = setNames(origin_colors[1:length(unique(top_5_chains$iso_origin))], 
                      unique(top_5_chains$iso_origin)),
    name = "Origin Countries"
  ) +
  scale_size_continuous(range = c(1, 2), name = "Value Chain Importance") +
  coord_sf(crs = "+proj=robin") +
  labs(title = "Top 5 Value Chains") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",  # Hide default legend
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Create custom legend (most of the heavy lifting done in PPT)
legend_data <- data.frame(
  lon_from = rep(1, 5),   # All arrows start from the same vertical line
  lat_from = seq(5, 1, by = -1),  # Position arrows vertically
  lon_to = rep(1.5, 5),   # Shorter arrow length
  lat_to = seq(5, 1, by = -1),  # Keep same vertical alignment
  value = top_5_chains$value[1:5],  # Values for line thickness
  iso_origin = top_5_chains$iso_origin[1:5],  # Origin country
  iso_target = top_5_chains$iso_target[1:5]   # Target country
)

library(scales)

# Create the custom legend with straight and shorter arrows, and larger fonts
legend_data <- data.frame(
  lon_from = rep(1, 5),   # All arrows start from the same vertical line
  lat_from = seq(5, 1, by = -1),  # Position arrows vertically
  lon_to = rep(1.5, 5),   # Shorter arrow length
  lat_to = seq(5, 1, by = -1),  # Keep same vertical alignment
  value = top_5_chains$value[1:5],  # Values for line thickness
  iso_origin = top_5_chains$iso_origin[1:5],  # Origin country
  iso_target = top_5_chains$iso_target[1:5]   # Target country
)

legend_plot <- ggplot(data = legend_data) +
  geom_segment(
    aes(
      x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,
      color = iso_origin, size = value
    ),
    arrow = arrow(length = unit(0.2, "cm"))
  ) +
  # Adjust Origin text position
  geom_text(
    aes(x = lon_from -0.5, y = lat_from, label = iso_origin),  # Move further left
    hjust = -5, size = 5   # Increase font size
  ) +  # Origin labels on the left
  # Adjust Target text position
  geom_text(
    aes(x = lon_to + 0.5, y = lat_to -0.15, label = iso_target),  # Move further right
    hjust = 5, size = 5   # Increase font size
  ) +  # Target labels on the right
  # Adjust Value labels position
  geom_text(
    aes(x = (lon_from + lon_to) / 2, y = lat_from + 0.15,
        label = scientific(value)),  # Move value labels closer to arrows
    size = 5, vjust = 4   # Display value labels closer to the arrows
  ) +  # Value labels above the arrows
  scale_color_manual(
    values = setNames(origin_colors[1:length(unique(top_5_chains$iso_origin))],
                      unique(top_5_chains$iso_origin))
  ) +
  scale_size_continuous(range = c(1, 2)) +
  theme_void() +
  theme(
    legend.position = "none",  # No default legend
    plot.margin = margin(2, 2, 2, 2)  # Add more margin to compress content inward
  )  # Further compress the content by increasing the margin

ggsave("output/plots/top_5_chains_legend.png", plot= legend_plot, width = 5, height = 5, dpi = 300)



########################### BAR charts##############################

#### Mass Trade Visualizations (all excl. fodder/grazing) ---------------------

# Mass European feed imports -----------------------------------------------
# -> not included in paper, created for LinkedIn post
EUR_imp <- results_mass_no_gf %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  filter(continent_target == "EUR") %>% 
  mutate(import = if_else(continent_origin==continent_target, "domestic", "imported")) %>% 
  group_by(import, continent_target, item_origin) %>% 
  summarise(value = round(sum(value)/1000000)) %>% 
  spread(import, value, fill = 0) %>% 
  mutate(total = domestic + imported) %>% 
  mutate(import_share = round(imported / total * 100, 1)) %>% 
  arrange(desc(total), ) %>% 
  filter(total > 5) %>% 
  mutate(item_origin = str_remove(item_origin, " and products")) %>%
  mutate(item_origin = factor(item_origin, levels = item_origin))

data.table::fwrite(EUR_imp, file=paste0("./output_feed/EUR_imports_",yr,".csv"))

#create stacked bar chart EUR imports
plot_EUR <- ggplot(EUR_imp, aes(x = item_origin)) +
  geom_bar(aes(y = total, fill = "Domestic"), stat = "identity") +
  geom_bar(aes(y = imported, fill = "Imported"), stat = "identity", position = "stack") +
  scale_fill_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  labs(x = "", y = "Mio tonnes", fill = "Source") +
  theme_classic(base_size = 12) +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, max(data_world$total) * 1.1))

ggsave(filename = "feed_trade_2021.png", plot_EUR, width = 12, height = 6.3)


# Mass global feed imports ------------------------------------------------
GLO_imp <- results_mass_no_gf %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  filter(continent_target == "EUR") %>% 
  mutate(import = if_else(continent_origin==continent_target, "domestic", "imported")) %>% 
  group_by(import, continent_target) %>% 
  summarise(value = round(sum(value)/1000000)) %>% 
  spread(import, value, fill = 0) %>% 
  mutate(total = domestic + imported) %>% 
  mutate(import_share = round(imported / total * 100, 1)) %>% 
  arrange(desc(total), ) %>% 
  filter(total > 5) %>% 


data.table::fwrite(EUR_imp, file=paste0("./output_feed/EUR_imports_",yr,".csv"))

# # in-text calculations
# # calculate total biodiversity embodied in international trade 
# trade <- results_g%>% 
#   filter(value != 0) %>% 
#   mutate(continent_origin = if_else(continent_origin == "EU", "EUR", continent_origin),
#          continent_target = if_else(continent_target == "EU", "EUR", continent_target)) %>% 
#   mutate(import = if_else(iso_origin == iso_target, "domestic", "imported")) %>%
#   group_by(import) %>%
#   summarise(value = sum(value), .groups = "drop") %>% 
#   pivot_wider(names_from = import, values_from = value, values_fill = 0) %>% 
#   mutate(total = imported + domestic)
# 
# 
# # calculate exports from one country
# USA_exports <- results_g %>% 
#   filter(iso_origin == "USA" & iso_target != "USA") %>%
#   group_by(iso_target) %>% 
#   summarise(value = sum(value))



# Reorder continents by total import
GLO_imp <- GLO_imp[imported > 5,]
GLO_imp[, continent_target := fct_reorder(continent_target, imported, sum, .desc = TRUE)]
GLO_imp[, item_origin := factor(
  item_origin,
  levels = GLO_imp[, .(total_import = sum(imported)), by = item_origin][
    order(total_import), item_origin]
)]
# Create stacked bar chart 
plot_imp <- ggplot(GLO_imp, aes(x = continent_target, y = imported, fill = item_origin)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Feed Imports by Continent and Product, 2021",
    y = "Mio tonnes",
    x = "",
    fill = "Feed product"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA),   # White plot background
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 16)  # Center and enlarge title
  ) +
  scale_fill_tableau(palette = "Green-Orange-Teal")  # Tableau color scale for better visibility

ggsave(filename = "output/plots/Imports_by_continent_2021.png", plot = plot_imp, width = 10, height = 7, dpi = 300)



# Mass Global feed exports -------------------------------------
#(for descriptive part)
GLO_exp <- results_mass_no_gf %>% 
  filter(value != 0) %>% 
  mutate(continent_origin = if_else(continent_origin=="EU", "EUR", continent_origin),
         continent_target = if_else(continent_target=="EU", "EUR", continent_target)) %>% 
  mutate(export = if_else(continent_origin==continent_target, "domestic", "exported")) %>% 
  group_by(export, continent_origin, item_origin) %>% 
  summarise(value = round(sum(value)/1000000)) %>% 
  spread(export, value, fill = 0) %>% 
  mutate(total = domestic + exported) %>% 
  mutate(export_share = round(exported / total * 100, 1)) %>% 
  arrange(desc(total), ) %>% 
  filter(total > 5) %>% 
  mutate(item_origin = str_remove(item_origin, " and products")) %>%
  mutate(item_origin = factor(item_origin, levels = item_origin)) %>% 
  setDT

# Reorder continents by total export
GLO_exp <- GLO_exp[exported > 5,]
GLO_exp[, continent_origin := fct_reorder(continent_origin, exported, sum, .desc = TRUE)]
GLO_exp[, item_origin := factor(
  item_origin,
  levels = GLO_exp[, .(total_export = sum(exported)), by = item_origin][
    order(total_export), item_origin]
)]
# Create stacked bar chart 
plot_exp <- ggplot(GLO_exp, aes(x = continent_origin, y = exported, fill = item_origin)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Feed Exports by Continent and Product, 2021",
    y = "Mio tonnes",
    x = "",
    fill = "Feed product"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA),   # White plot background
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 16)  # Center and enlarge title
  ) +
  scale_fill_tableau(palette = "Green-Orange-Teal")  # Tableau color scale for better visibility

ggsave(filename = "output/plots/Exports_by_continent_2021.png", plot = plot_exp, width = 10, height = 7, dpi = 300)


#### BD Footprints----------------------------------------------------------
# Net importers/net exporters --------------------------------------------
# net exporters (based on calculations done under "maps - import/export)
fp_bar_exp <- fp_net_total[class == "net exporter (top 10%)", .(country, net_import)]   #have to run importer/exporter map code first
fp_bar_exp[, net_export := net_import * (-1)][,net_import := NULL] #equals export minus import
fp_bar_exp[country == "United Republic of Tanzania", country := "Tanzania"][
  country == "United States of America", country := "USA"][
   country == "Bolivia (Plurinational State of)", country := "Bolivia"]

ggplot(fp_bar_exp, aes(x = reorder(country, -net_export), y = net_export)) +
  geom_bar(stat = "identity", fill = "#98d9e4") +  
  labs(
    title = "Biggest Biodiversity Net Exporters",
    x = NULL,  
    y = "potentially disappeared fraction of species"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.text.y = element_text(size = 12),  
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none",  
    panel.grid = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA)  
  )


ggsave("output/plots/BD_net_exporters.png", plot = plot_exp, width = 10, height = 7, dpi = 300)

##net importers

fp_bar_imp <- fp_net_total[class == "net importer (top 10%)", .(country, net_import)]
fp_bar_imp[country == "Republic of Korea", country := "Korea"][
  country == "China, mainland", country := "China"][
    country == "United Kingdom", country := "UK"][
      country == "Iran (Islamic Republic of)", country := "Iran"][
        country == "China, Taiwan Province of", country := "Taiwan"]

ggplot(fp_bar_imp, aes(x = reorder(country, -net_import), y = net_import)) +
  geom_bar(stat = "identity", fill = "#ef8a0c") +  
  labs(
    title = "Biggest Biodiversity Net Importers",
    x = NULL,  
    y = "potentially disappeared fraction of species"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12),  
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "none",  
    panel.grid = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA)  
  )

ggsave("output/plots/BD_net_importers.png", plot = plot_imp, width = 10, height = 7, dpi = 300)


# Footprint composition (animals per continent) ----------------------------
#footprint per continent 
animal_results_g <- copy(results_g)
animal_results_g[, continent_origin := fifelse(continent_origin == "EU", "EUR", continent_origin)]
animal_results_g[, continent_target := fifelse(continent_target == "EU", "EUR", continent_target)]
animal_results_g[, group := livestock_groups$livestock_group[match(animal_results_g$proc_target,livestock_groups$proc_code)]]
animal_results_g <- animal_results_g[, .(sum_value = sum(value)), by = .(continent_target, group)]

animal_results_g[,proc:=processes$proc[match(animal_results_g$proc_target,processes$proc_code)]]
animal_results_g[, proc := gsub("husbandry|farming", "", proc)]

animal_results_g <- animal_results_g[  group!="Rodents",]

# Reorder continents alphabetically
animal_results_g <- animal_results_g %>%
  filter(continent_target != "ROW") 
animal_results_g$continent_target <- factor(animal_results_g$continent_target, 
                                            levels = sort(unique(animal_results_g$continent_target)))

animal_results_g <- animal_results_g %>%
  group_by(continent_target) %>%
  mutate(proportion = sum_value / sum(sum_value)) %>%
  ungroup() %>% 
  setDT()

#write.csv(animal_results_g, "output/tables/animal_composition.csv")

# #get global average BF by group (in-text calculation)
# global_average <- animal_results_g[, prop := (sum_value/sum(sum_value))]
# global_average <-global_average[, .(group_prop = sum(prop)), by = group]

# Create the stacked bar chart
plot_animal <- ggplot(animal_results_g, aes(x = continent_target, y = proportion, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    name = NULL,  # Remove legend title
    values = c("#ef8a0c", "#fcc66d","#3ca8bc", "#98d9e4")) + 
  labs(
    title = "By Continent and Livestock Group",
    y = "Composition",
    x = NULL 
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20, colour = "black"),  
    axis.text.y = element_text(size = 26, colour = "black"),
    axis.title.y = element_text(size = 26),  
    legend.position = "bottom",  # Move legend to the bottom
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, size = 30)
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order

ggsave("output/plots/Bar_BD_footprints_livestock.png", plot = plot_animal, width = 10, height = 7, dpi = 300)


# Footprint Composition (item groups per continent) ----------------------
comm_results_g <- copy(results_g)
comm_results_g[, continent_origin := fifelse(continent_origin == "EU", "EUR", continent_origin)]
comm_results_g[, continent_target := fifelse(continent_target == "EU", "EUR", continent_target)]

# # find most relevant crops in the groups "cereals" and "oil crops" -> most relevant groups
# # (in-text calculation)
# crops <- comm_results_g[group_origin %in% c("Cereals", "Oil crops"),] [,crop_group_total := sum(value), by = group_origin]
# crops <- crops[,.(crop_group_total =unique(crop_group_total), group_origin= unique(group_origin),
#                   crop_item_total = sum(value)), by = item_origin]
# crops[, group_share := crop_item_total/crop_group_total, by = group_origin]
# write.csv(crops, "output/tables/cereal and oil crop shares.csv")
# 

comm_results_g <- comm_results_g[, .(sum_value = sum(value)), by = .(continent_target, group_origin)]
comm_results_g <- comm_results_g[sum_value > 0 & group_origin != "Coffee, tea, cocoa" & group_origin != "Sugar crops",] #negligible
comm_results_g <- comm_results_g %>%
  filter(continent_target != "ROW")
comm_results_g$continent_target <- factor(comm_results_g$continent_target,
                                            levels = sort(unique(comm_results_g$continent_target)))

#reorder commodities by their total sum value (for ordering within bars)
comm_results_g <- comm_results_g[
  , group_origin := fct_reorder(group_origin, sum_value, sum)
]

comm_results_g <- comm_results_g %>%
  group_by(continent_target) %>%
  mutate(proportion = sum_value / sum(sum_value)) %>%
  ungroup() %>% 
  setDT

write.csv(comm_results_g,"output/tables/commodity_composition.csv")

#get global average BF by group
global_average <- comm_results_g[, prop := (sum_value/sum(sum_value))]
global_average <-global_average[, .(group_prop = sum(prop)), by = group_origin]

plot_comm <- ggplot(comm_results_g, aes(x = continent_target, y = proportion, fill = group_origin)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    name = NULL,  # Remove legend title
    values = c(
      "#e7298a",  # Pink
      "#7570b3",  # Purple
      "#d95f02",  # Orange
      "#a6dba0",  # Light green
      "#66c2a5",  # Light Green
      "#1b9e77"   # Green
    )
  ) + 
  labs(
    title = "By Continent and Commodity Group",
    y = NULL,
    x = NULL 
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20, colour = "black"),  
    axis.text.y = element_blank(),  # Remove y-axis text
    legend.position = "bottom",  # Move legend to the bottom
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, size = 30)
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order




ggsave("output/plots/Bar_BD_footprints_commodities.png", plot = plot_comm, width = 10, height = 7, dpi = 300)



# Footprint composition (commodities per livestock group) ------------------
# good for Appendix, not included in thesis
comm_livestock_results<- copy(results_g)
comm_livestock_results[, continent_origin := fifelse(continent_origin == "EU", "EUR", continent_origin)]
comm_livestock_results[, continent_target := fifelse(continent_target == "EU", "EUR", continent_target)]
comm_livestock_results[, livestock_group := livestock_groups$livestock_group[match(comm_livestock_results$proc_target,livestock_groups$proc_code)]]
comm_livestock_results <- comm_livestock_results[, .(sum_value = sum(value)), by = .(group_origin, livestock_group)]
comm_livestock_results <- comm_livestock_results[sum_value >0 & livestock_group != "Rodents" & group_origin != "Coffee, tea, cocoa" & group_origin != "Sugar crops",]  #rodents pretty irrelevant

#reorder commodities by their total sum value (for ordering within bars)
comm_livestock_results <- comm_livestock_results[
  , group_origin := fct_reorder(group_origin, sum_value, sum)
]

# Modify the livestock_group text to insert a line break
comm_livestock_results <- comm_livestock_results %>%
  mutate(livestock_group = str_replace_all(livestock_group, " ", "\n"))

# Reverse the order of the livestock_group factor levels
comm_livestock_results$livestock_group <- factor(comm_livestock_results$livestock_group, levels = rev(levels(factor(comm_livestock_results$livestock_group))))

# Plot with reversed bar order
plot_comm_ls <- ggplot(comm_livestock_results, aes(x = livestock_group, y = sum_value, fill = group_origin)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#e7298a", "#7570b3", "#d95f02", "#a6dba0",  
                               "#66c2a5", "#1b9e77"), name = NULL) + 
  labs(
    title = "By Livestock and Commodity Group",
    y = NULL,
    x = NULL 
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),  
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 20, colour = "black"),  # Horizontal x-axis text
    axis.text.y = element_blank(),  # Remove y-axis text
    legend.position = "bottom",  # Move legend to the bottom
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 22),
    plot.title = element_text(hjust = 0.5, size = 30)
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order


ggsave("output/plots/Bar_BD_footprints_ls_commodities.png", plot = plot_comm_ls, width = 10, height = 7, dpi = 300)



################### Sankeys #############################################
#All sankeys excluding intracontinental trade

# Mass sankey region -> livestock feed -> region  ----

results_mass_no_dom <- results_mass_no_gf[continent_origin!=continent_target][
  ,continent_origin := ifelse(continent_origin=="EU", "EUR", continent_origin)][
    ,continent_target := ifelse(continent_target=="EU", "EUR", continent_target)][
      ,livestock_group := livestock_groups$livestock_group[match(proc_target, livestock_groups$proc_code)]][
        ,`:=` (year=NULL, unit=NULL, iso_origin=NULL, iso_target=NULL, 
               comm_code=NULL, proc_target = NULL, item_origin=NULL)][
                 value!=0,.(value=sum(value)), by=.(continent_origin, group_origin, continent_target, livestock_group)]
setcolorder(results_mass_no_dom, c("continent_origin", "group_origin", "continent_target", "livestock_group", "value" ))
sankey_long <- make_long(results_mass_no_dom, c(continent_origin, group_origin, continent_target, livestock_group), value = value)
#saveRDS(sankey_long, "sankey_data_no_dom.rds")


ggplot(sankey_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node, flow=value)) +
  geom_sankey(flow.alpha = .6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Intercontinental trade of major feed crops in tonnes")



# BD sankey continent_orig -> continent_target -> livestock_group -----------
results_sankey_g <- copy(results_g)
results_sankey_g <-results_sankey_g[continent_origin!=continent_target,][
  ,continent_origin := ifelse(continent_origin=="EU", "EUR", continent_origin)][
    ,continent_target := ifelse(continent_target=="EU", "EUR", continent_target)][
      ,livestock_group := livestock_groups$livestock_group[match(proc_target, livestock_groups$proc_code)]][
        ,`:=` (year=NULL, unit=NULL, iso_origin=NULL, iso_target=NULL, 
               comm_code=NULL, proc_target = NULL, item_origin=NULL)][
                 ,.(value=sum(value)),by=.(continent_origin, continent_target, livestock_group)]
results_sankey_g <- results_sankey_g[!(livestock_group == "Rodents" | 
                                         continent_origin == "ROW" | 
                                         continent_target == "ROW"), ] # the values here are negligible


sankey_long <- make_long(results_sankey_g, c(continent_origin, continent_target, livestock_group), value = value)
sankey_long$node <- factor(sankey_long$node, levels = c("OCE", "EUR", "AFR", "NAM", "ASI", "LAM", "Equids and Camelids", "Poultry Birds", "Pigs", "Ruminants"))


ls_sankey_plot <- ggplot(sankey_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node, value = value)) +
  geom_sankey(flow.alpha = .6, aes(fill = factor(node))) +  
  geom_sankey_label(size = 3.5, color = "black", fill = "white") +       
  scale_fill_viridis_d(drop = FALSE) +                                       
  theme_sankey(base_size = 20) + 
  labs(x = NULL) +  
  scale_x_discrete(labels = c("Exporters", "Importers", "Final Consumption\nby Livestock")) + 
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = .5, size = 30, face = "bold"),  # Title size 30
    axis.text = element_text(size = 20, color = "black"),  # Axis text size 20
    plot.margin = margin(t = 10, r = 50, b = 10, l = 0)
  ) +  
  ggtitle("Continents")

ggsave("output/plots/Sankey_BD_origin_target_ls-group.png", plot = ls_sankey_plot, width = 10, height = 7, dpi = 300)


# BD sankey continent_orig -> livestock_group-> continent_target ------------
results_sankey_g <- copy(results_g)
results_sankey_g <-results_sankey_g[continent_origin!=continent_target,][
                                     ,comm_total := sum(value), by = item_origin]
top_6_items <- results_sankey_g[item_origin != "Grazing", 
                                .(comm_total = unique(comm_total)), 
                                by = item_origin][order(-comm_total)][1:6] # find top 6 traded comms
results_sankey_g <- results_sankey_g[item_origin %in% top_6_items$item_origin,]
results_sankey_g[,continent_origin := ifelse(continent_origin=="EU", "EUR", continent_origin)][
    ,continent_target := ifelse(continent_target=="EU", "EUR", continent_target)][
        ,`:=` (year=NULL, unit=NULL, iso_origin=NULL, iso_target=NULL, group_origin =NULL,
               comm_code=NULL, proc_target = NULL)][
                 ,.(value=sum(value)),by=.(continent_origin, continent_target, item_origin)]
results_sankey_g <- results_sankey_g[!(continent_origin == "ROW" | 
                                         continent_target == "ROW"), ][item_origin %like% "and products", 
                                    item_origin :=gsub(" and products", "", item_origin)][
                                      item_origin =="Soyabeans", item_origin := "Soybeans"][
                                        item_origin == "Oil, palm fruit", item_origin := "Palm Oil"]


sankey_long <- make_long(results_sankey_g, c(continent_origin, item_origin, continent_target), value = value)
sankey_long$node <- factor(sankey_long$node, levels = c("OCE", "EUR", "AFR", "NAM", "ASI", "LAM", 
                                                        "Rice", "Rape and Mustardseed", 
                                                        "Palm Oil", "Wheat", "Maize", "Soybeans"))
comm_sankey_plot <- ggplot(sankey_long, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node, value = value)) +
  geom_sankey(flow.alpha = .6, aes(fill = factor(node))) +  
  geom_sankey_label(size = 3.5, color = "black", fill = "white") +       
  scale_fill_viridis_d(drop = FALSE) +                                       
  theme_sankey(base_size = 20) + 
  labs(x = NULL) +  
  scale_x_discrete(labels = c("Exporters", ,"Products", "Importers")) + 
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = .5, size = 30, face = "bold"),  # Title size 30
    axis.text = element_text(size = 20, color = "black"),  # Axis text size 20
    plot.margin = margin(t = 10, r = 50, b = 10, l = 0)
  ) +  
  ggtitle("Products")

ggsave("output/plots/Sankey_BD_origin_comm_target.png", plot = comm_sankey_plot, width = 10, height = 7, dpi = 300)




# BD sankey continent_origin -> item_origin -> continent_target -> livestock_group -----
results_sankey_g <- copy(results_g)
results_sankey_g <-results_sankey_g[continent_origin!=continent_target,][
  ,comm_total := sum(value), by = item_origin]
top_6_items <- results_sankey_g[item_origin != "Grazing", 
                                .(comm_total = unique(comm_total)), 
                                by = item_origin][order(-comm_total)][1:6] # find top 6 traded comms
results_sankey_g <- results_sankey_g[item_origin %in% top_6_items$item_origin,]

results_sankey_g <- results_sankey_g[continent_origin != continent_target, ][
  , continent_origin := ifelse(continent_origin == "EU", "EUR", continent_origin)][
    , continent_target := ifelse(continent_target == "EU", "EUR", continent_target)][
      , livestock_group := livestock_groups$livestock_group[match(proc_target, livestock_groups$proc_code)]]
results_sankey_g <- results_sankey_g[, `:=` (year = NULL, unit = NULL, iso_origin = NULL, iso_target = NULL, 
                                             comm_code = NULL, proc_target = NULL, group_origin = NULL)]
results_sankey_g <- results_sankey_g[
  !(item_origin %in% c("Pigs", "Poultry Birds", "Rodents")), ] #this makes problems as they are not only ls groups but also psossible feed


results_sankey_g <- results_sankey_g[
  , .(value = sum(value)), by = .(continent_origin, item_origin, continent_target, livestock_group)]
results_sankey_g <- results_sankey_g[
  !(livestock_group == "Rodents" | continent_origin == "ROW" | continent_target == "ROW"), ][
    item_origin %like% "and products", item_origin := gsub(" and products", "", item_origin)][
      item_origin == "Soyabeans", item_origin := "Soybeans"][
        item_origin == "Oil, palm fruit", item_origin := "Palm Oil"]


sankey_long <- make_long(results_sankey_g, 
                         c(continent_origin, item_origin, continent_target, livestock_group), 
                         value = value)

# Define the order of nodes in the Sankey diagram
sankey_long$node <- factor(sankey_long$node, 
                           levels = c("OCE", "EUR", "AFR", "NAM", "ASI", "LAM", 
                                      "Rice", "Rape and Mustardseed", "Palm Oil", "Wheat", 
                                      "Maize", "Soybeans", "Equids and Camelids", "Poultry Birds", "Pigs", "Ruminants"))

# Create the Sankey diagram
sankey_plot <- ggplot(sankey_long, aes(
  x = x, next_x = next_x, node = node, next_node = next_node, 
  fill = factor(node), label = node, value = value
)) +
  geom_sankey(flow.alpha = .6, aes(fill = factor(node))) +  
  geom_sankey_label(size = 3.5, color = "black", fill = "white") +       
  scale_fill_viridis_d(drop = FALSE) +                                       
  theme_sankey(base_size = 20) + 
  labs(x = NULL) +  
  scale_x_discrete(labels = c("Exporters", "Products", "Importers", "Final Consumption\nby Livestock")) + 
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = .5, size = 30, face = "bold"),  # Title size 30
    axis.text = element_text(size = 20, color = "black"),  # Axis text size 20
    plot.margin = margin(t = 10, r = 50, b = 10, l = 0)
  ) +  
  ggtitle("Intercontinental Trade in Biodiversity Footprints")


# Save the plot
ggsave("output/plots/Sankey_BD_combined.png", plot = sankey_plot, width = 18, height = 8, dpi = 300)


# Appendix tables ---------------------------------------------------------
#footprints, production and impacts

# fp1 <- results_g[, .(BD_fp_glo = sum(value)), by = iso_target]
# fp2 <- results_r[, .(BD_fp_reg = sum(value)), by = iso_target]
# fp3 <- copy(results_g_pop)
# fp4 <- copy(results_g_prot)
# fp5 <- copy(feed_production)
# 
# fp6 <- copy(average_impact)
# 
# setnames(fp5, "value", "production")
# fp3[, country := NULL]
# fp4[,country := NULL]
# fp5[,`:=`(pop =NULL, country =NULL)]
# setnames(fp5, "iso_origin", "iso_target")
# setnames(fp6, "iso_origin", "iso_target")
# 
# # List of all data tables
# fp_list <- list(fp1, fp2, fp3, fp4, fp5, fp6)
# 
# # Merge all tables on 'iso_target'
# fp_merged <- Reduce(function(x, y) merge(x, y, by = "iso_target", all = TRUE), fp_list)
# fp_merged[, continent := regions$continent[match(iso_target, regions$iso3c)]]
# 
# write.csv(fp_merged, "output/tables/combined_table.csv")


# #feed items
# feed_items <- use[type =="feed", .(unique(item)) ]
# feed_items[, group := items$comm_group[match(V1, items$item)]]
# write.csv(feed_items, "output/tables/feed_items.csv")
# 
# #livestock groups
# write.csv(livestock_groups, "output/tables/livestock_groups.csv")

