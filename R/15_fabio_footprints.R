##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)
source("R/00_system_variables.R")


is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Read labels
input_path <- output_dir
regions <- fread(file=paste0(input_path,"regions.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
io <- fread(paste0(input_path,"io_labels.csv"))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))

# Read data
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
Y <- readRDS(file=paste0(input_path,"losses/Y.rds"))
E <- readRDS(file=paste0(input_path,"E.rds"))
E_bio <- readRDS(file=paste0(input_path,"E_biodiv.rds"))

# Make settings
consumption_categories <- c("food","other","stock_addition","balancing")
country <- "AUT"
# country <- "FRA"
extension <- "biodiversity global"
# extension <- "biodiversity regional"
extension <- "landuse"
consumption <- "food"
spread_stocks <- FALSE
allocation <- "value"
years <- 2010:2022
year <- 2016


for(year in years){
  L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]

  # Prepare extension and final demand
  if (extension == "biodiversity global") {
    ext <- rowSums(E_bio[[as.character(year)]][, 9:11]) / as.vector(Xi)
  } else if (extension == "biodiversity regional") {
    ext <- rowSums(E[[as.character(year)]][, 12:14]) / as.vector(Xi)
  } else if (extension == "landuse") {
    ext <- (as.numeric(unlist(E[[as.character(year)]][, "cropland"])) + as.numeric(unlist(E[[as.character(year)]][, "grassland"]))) / as.vector(Xi)
  } else {
    ext <- as.numeric(unlist(E[[as.character(year)]][, ..extension])) / as.vector(Xi)
  }
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  
  if(country=="EU27"){
    Y_country <- Yi[, (fd$continent == "EU")]
    colnames(Y_country) <- fd$fd[fd$continent == "EU"]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Yi[, fd$iso3c == country]
    colnames(Y_country) <- fd$fd[fd$iso3c == country]
  }
  
  if(spread_stocks){
    stock_ratio <- Y_country[, "stock_addition"] / (rowSums(Y_country) - Y_country[, "stock_addition"])
    stock_ratio[!is.finite(stock_ratio)] <- 0
    Y_country <- as.data.table(as.matrix(Y_country))
    Y_country[, `:=`(food = food * (1 + stock_ratio),
                     other = other * (1 + stock_ratio),
                     tourist = tourist * (1 + stock_ratio),
                     unspecified = unspecified * (1 + stock_ratio),
                     stock_addition = 0)]
  }
  
  # Calculate footprints
  FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))
  colnames(FP) <- rownames(FP) <- paste0(io$iso3c, "_", io$item)
  FP <- as(FP, "TsparseMatrix")
  results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value =FP@x)
  results[,`:=`(country_consumer = country,
                year = year,
                indicator = extension,
                country_origin = substr(origin,1,3),
                item_origin = substr(origin,5,100),
                country_target = substr(target,1,3),
                item_target = substr(target,5,100))]
  
  results[,`:=`(group_origin = items$comm_group[match(results$item_origin,items$item)],
                group_target = items$comm_group[match(results$item_target,items$item)],
                continent_origin = regions$continent[match(results$country_origin, regions$iso3c)])]
  
  results$continent_origin[results$country_origin==country] <- country
  # results$continent_origin[results$country_origin!=country] <- "REST"
  
  data <- results %>%
    mutate(group = ifelse(group_origin=="Grazing", "Grazing", "Crops")) %>%
    mutate(group = ifelse(grepl("Livestock", group_origin), "Livestock", group)) %>%
    #mutate(group = ifelse(group_origin=="Fish", "Livestock", group)) %>%    # fish has no direct land or water use
    mutate(group = paste(group, continent_origin, sep = "_")) %>%
    group_by(item_target, group) %>%
    filter(value != 0) %>%
    summarise(value = round(sum(value))) %>%
    spread(group, value, fill = 0)
  data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_continent.csv"), sep=",")
  
  # data <- results %>% 
  #   group_by(item_target, continent_origin) %>% 
  #   filter(value != 0) %>% 
  #   summarise(value = sum(value)) %>% 
  #   spread(continent_origin, value, fill = 0)
  # data.table::fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,".csv"), sep=",")
  
  # data <- results %>%
  #   group_by(final_product, group_origin, country_origin) %>%
  #   summarise(value = round(sum(value))) %>%
  #   filter(value != 0) %>%
  #   spread(group_origin, value)
  # fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_detailed.csv"), sep=",")
  
  # fwrite(results, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_full.csv"), sep=",")
  
  reg <- regions$area[match(results$country_origin, regions$iso3c)]
  
  data <- results[, country_origin := reg] %>%
    filter(group_origin=="Grazing") %>%
    filter(value != 0) %>%
    group_by(item_target, continent_origin, country_origin) %>%
    summarise(value = round(sum(value))) %>% 
    filter(value != 0) %>%
    spread(item_target, value, fill = 0)
  fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_grazing.csv"), sep=",")
}



for(year in years){
  L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
  Xi <- X[, as.character(year)]
  Yi <- Y[[as.character(year)]]
  
  # Prepare extension and final demand
  if (extension == "biodiversity global") {
    ext <- rowSums(E_bio[[as.character(year)]][, 9:11]) / as.vector(Xi)
  } else if (extension == "biodiversity regional") {
    ext <- rowSums(E[[as.character(year)]][, 12:14]) / as.vector(Xi)
  } else if (extension == "landuse") {
    ext <- (as.numeric(unlist(E[[as.character(year)]][, "cropland"])) + as.numeric(unlist(E[[as.character(year)]][, "grassland"]))) / as.vector(Xi)
  } else {
    ext <- as.numeric(unlist(E[[as.character(year)]][, ..extension])) / as.vector(Xi)
  }
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  
  if(country=="EU27"){
    Y_country <- Yi[, (fd$continent == "EU")]
    colnames(Y_country) <- fd$fd[fd$continent == "EU"]
    Y_country <- agg(Y_country)
  } else {
    Y_country <- Yi[, fd$iso3c == country]
    colnames(Y_country) <- fd$fd[fd$iso3c == country]
  }
  
  Y_country[,consumption][Y_country[,consumption]<0] <- 0
  
  # Calculate footprints
  print(paste0(year,": ", round(sum(t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))))))
  
}
