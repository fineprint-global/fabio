##############################################################################################
##  FABIO Footprints
##############################################################################################

library(Matrix)
library(tidyverse)
library(data.table)

source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_5_labels.R")

input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"

regions <- fread("inst/regions_full.csv")[current==TRUE]
items <- fread("inst/items_full.csv")
cbs <- readRDS("data/cbs_final.rds")
use <- readRDS("data/use_final.rds")
mr_use <-  readRDS("data/mr_use.rds")

#use <- readRDS("data/use_final.rds")
use_fd <- readRDS("data/use_fd_final.rds") # needed for reference

areas <- sort(unique(cbs$area_code))       #are these three needed? Can I condense?
processes <- sort(unique(use$proc_code))
commodities <- sort(unique(use$comm_code))

#1. filtering mr_use to only include feed use -> filter livestock processes and feed commodities, maybe need shares of feed 
#use if some commodities have several use types

#naming rows
mr_use_21 <- mr_use[["2021"]]
rownames(mr_use_21) <- paste0(rep(areas, each=length(commodities)), "_", rownames(mr_use_21))
livestock_proc <- use[proc %like% "farming" | proc %like% "husbandry",unique(proc_code)]
comm_feed <- use[type=="feed" & use > 0, unique(comm_code)]

row_subset <- substr(rownames(mr_use_21), nchar(rownames(mr_use_21)) - 3, nchar(rownames(mr_use_21))) %in% comm_feed
col_subset <- substr(colnames(mr_use_21), nchar(colnames(mr_use_21)) - 3, nchar(colnames(mr_use_21))) %in% livestock_proc
mr_feed_use_21 <- mr_use_21[row_subset, col_subset]

#take care of items that are used both as feed and seedwaste (only the case for eggs in poultry birds 
#farming in Hungary. Kazakhstan, Thailand and Ukraine)
use_21 <- use[year==2021]
use_21_wide <- dcast(use_21, area_code +area+ comm_code+ item + proc+ 
                       proc_code ~ type, value.var = "use", fill = 0)
multi_proc <- use_21_wide[!is.na(feed) & feed > 0 & (`100%` > 0 | TCF > 0 |ethanol >0 
                                                   |seedwaste > 0 |optim > 0 |slaughtering>0)]
multi_proc[, feed_share:=feed/(feed + seedwaste)]

#subsetting the values that need modifying
egg_use <- mr_feed_use_21[grepl("c111", rownames(mr_feed_use_21)),
                          colnames(mr_feed_use_21) %in% 
                          c("97_p090", "108_p090", "216_p090","203_p090", "230_p090")] 
egg_use <- sweep(egg_use, 2, multi_proc$feed_share, `*`)

mr_feed_use_21[grepl("c111", rownames(mr_feed_use_21)),
               colnames(mr_feed_use_21) %in% 
                 c("97_p090", "108_p090", "216_p090","203_p090", "230_p090")]  <- egg_use
  




#2. trade embodiment
year <- 2021

# Read data 
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
#E <- readRDS(file=paste0(input_path,"E.rds"))
#Y_FD <- readRDS(file=paste0(input_path,"Y.rds"))


Y <- Y_feed[[as.character(year)]]

#renaming rownames and renaming/reordering colnames
io_labels <- io_labels[order(io_labels$area_code, io_labels$item)]
rownames(Y) <- paste0(io_labels$area_code, "_", io_labels$item)



# get rid of grazing (c062) for calculating trade embodiment (needed again for FPs)
Y_no_g <- Y[!grepl("Grazing", rownames(Y)),]





#Total feed use by armenian asses = sum(column 1)

#total traded feed  = sum(col) - the values that are in the cells
#                     [region * commodity, feeduse * region]
#    -> do with for function




#3. Footprints
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# Set allocation method
allocation <- "value"

# Make settings
extensions <- colnames(E)[c(8,10:11)]
consumption_categories <- c("food","other","stock_addition","balancing")
country <- "AUT"
extension <- "landuse"
consumption <- "food"
spread_stocks <- FALSE


# Prepare extension and final demand
ext <- as.vector(as.matrix(as.numeric(unlist(E[, ..extension])) / as.vector(X))) #ext. per unit of output
ext[!is.finite(ext)] <- 0                                                        #sets ext to 0 where output is 0
MP <- ext * L                       #colsums are footprints by process

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

