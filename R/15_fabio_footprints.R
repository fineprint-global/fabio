##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Read labels
regions <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.2/current/regions.csv")
items <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.2/current/items.csv")
nrreg <- nrow(regions)
nrcom <- nrow(items)
io <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/current/io_labels.csv")
fd <- fread(file="/mnt/nfs_fineprint/tmp/fabio/v1.2/current/losses/fd_labels.csv")

# Set year and allocation method
year <- 2021
allocation <- "value"

# Read data
X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/current/losses/X.rds"))
Y <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/current/losses/Y.rds"))
L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/current/losses/",year,"_L_",allocation,".rds"))
E <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v1.2/current/E.rds"))
X <- X[, as.character(year)]
Y <- Y[[as.character(year)]]
E <- E[[as.character(year)]]

# Make settings
extensions <- colnames(E)[c(8,10:11)]
consumption_categories <- c("food","other","stock_addition","balancing")
country <- "AUT"
extension <- "landuse"
consumption <- "food"
spread_stocks <- FALSE


# Prepare extension and final demand
ext <- as.vector(as.matrix(as.numeric(unlist(E[, ..extension])) / as.vector(X)))
ext[!is.finite(ext)] <- 0
MP <- ext * L

if(country=="EU27"){
  Y_country <- Y[, (fd$continent == "EU")]
  colnames(Y_country) <- fd$fd[fd$continent == "EU"]
  Y_country <- agg(Y_country)
} else {
  Y_country <- Y[, fd$iso3c == country]
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
FP <- as(FP, "dgTMatrix")
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
results$continent_origin[results$country_origin!=country] <- "REST"

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
#   group_by(final_product, group_origin, country_origin) %>%
#   summarise(value = round(sum(value))) %>%
#   filter(value != 0) %>%
#   spread(group_origin, value)
# fwrite(data, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_detailed.csv"), sep=",")

# fwrite(results, file=paste0("./output/FABIO_",country,"_",year,"_",extension,"_",consumption,"_",allocation,"-alloc_full.csv"), sep=",")

