# 15 - Calculate footprints from FABIO MRIO

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
source("R/00_system_variables.R")

# Read labels ------------------------------------------------------------------
regions <- fread(file=paste0(input_path,"regions.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
io <- fread(paste0(input_path,"io_labels.csv"))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
ex <- fread(file=paste0(input_path,"ex_labels.csv"))

# Set parameters ---------------------------------------------------------------
allocation <- "value"
year <- 2021
country <- "AUT"
consumption_categories <- unique(fd$fd)
consumption <- consumption_categories[1]
extension <- ex$Stressor[99]

# Read data --------------------------------------------------------------------
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
Y <- readRDS(file=paste0(input_path,"losses/Y.rds"))
E <- readRDS(file=paste0(input_path,"E.rds"))
L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Ei <- E[[as.character(year)]]


# Prepare calculations ---------------------------------------------------------
# Prepare extension
ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
ext[!is.finite(ext)] <- 0

# Prepare final demand
if(country=="EU27"){
  Y_country <- Yi[, (fd$continent == "EU")]
  colnames(Y_country) <- fd$fd[fd$continent == "EU"]
  Y_country <- agg(Y_country)
} else {
  Y_country <- Yi[, fd$iso3c == country]
  colnames(Y_country) <- fd$fd[fd$iso3c == country]
}


# Calculate footprints ---------------------------------------------------------
MP <- ext * L
FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))


# Make results data.table ------------------------------------------------------
# Convert from sparse matrix to data.table
colnames(FP) <- rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results <- data.table(origin=rownames(FP)[FP@i + 1], 
                      target=colnames(FP)[FP@j + 1], 
                      value =FP@x)

# Add auxiliary information
results[,`:=`(
  country_consumer = country,
  year = year,
  indicator = extension,
  country_origin = substr(origin,1,3),
  item_origin = substr(origin,5,100),
  country_target = substr(target,1,3),
  item_target = substr(target,5,100)
)]

results[,`:=`(
  group_origin = items$comm_group[match(results$item_origin,items$item)],
  group_target = items$comm_group[match(results$item_target,items$item)],
  continent_origin = regions$continent[match(results$country_origin, regions$iso3c)]
)]

# results$continent_origin[results$country_origin==country] <- country
# results$continent_origin[results$country_origin!=country] <- "REST"

# Aggregate results ------------------------------------------------------------
# by continent
data_continent <- results %>%
  mutate(group = case_when(
    group_origin == "Grazing" ~ "Grazing",
    grepl("Livestock", group_origin) ~ "Livestock",
    TRUE ~ "Crops"
  )) %>%
  mutate(group = paste(group, continent_origin, sep = "_")) %>%
  group_by(item_target, group) %>%
  filter(value != 0) %>%
  summarise(value = round(sum(value)), .groups = "drop") %>%
  spread(group, value, fill = 0)

fwrite(data_continent, 
       file.path("output", paste0("FABIO_", country, "_", year, "_", 
                                  extension, "_", consumption, "_", 
                                  allocation, "-alloc_continent.csv")))

# by domestic vs. ROW
data_domestic <- results %>%
  mutate(group = case_when(
    group_origin == "Grazing" ~ "Grazing",
    grepl("Livestock", group_origin) ~ "Livestock",
    TRUE ~ "Crops"
  )) %>%
  mutate(group = paste(group, 
                       if_else(continent_origin == country, country, "ROW"), 
                       sep = "_")) %>%
  group_by(item_target, group) %>%
  filter(value != 0) %>%
  summarise(value = round(sum(value)), .groups = "drop") %>%
  spread(group, value, fill = 0)

fwrite(data_domestic, 
       file.path("output", paste0("FABIO_", country, "_", year, "_", 
                                  extension, "_", consumption, "_", 
                                  allocation, "-alloc.csv")))



# # calculate gwp multipliers over time
# data <- io[, .(iso3c, area_code, area, continent, comm_code, item, unit)]
# for(year in years){
#   print(year)
#   L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
#   ext <- colSums(E_gwp[[as.character(year)]])
#   MP <- ext * L
#   data[, paste0("y", year) := round(colSums(MP))]
# }
# milk <- data[item %like% "Milk",]
