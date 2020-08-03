#' @title FABIO extensions
#' @author Martin Bruckner, \email{martin.bruckner@@wu.ac.at}
#' 
#' @description Prepare land use and biomass extensions for FABIO model. 
#' 
#' @export

library(data.table)
library(reshape2)
library(tidyverse)

##########################################################################
# Make intitial settings
##########################################################################
# read region classification
regions <- utils::read.csv(file="./inst/fabio_input/Regions.csv", header=TRUE, sep=";")
# read commodity classification
items <- utils::read.csv(file="./inst/fabio_input/Items.csv", header=TRUE, sep=";")
# load production data with yields
load(file=paste0("/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData"))
Prod <- Prod[Prod$Element %in% c("Area harvested","Production"),]
# aggregate RoW
Prod$Country[! Prod$Country.Code %in% regions$Country.Code] <- "RoW"
Prod$Country.Code[! Prod$Country.Code %in% regions$Country.Code] <- 999

Prod <- as.data.frame(Prod %>% dplyr::group_by(Country.Code, Country, Item.Code, Item, Element, Year, Unit) %>% 
                        dplyr::summarise(Value = sum(Value)))
Prod$ID <- paste(Prod$Country.Code,Prod$Item.Code,sep="_")

year <- 2013
years <- 1986:2013
##########################################################################
# Start loop for a series of years
##########################################################################
for(year in years){
  print(year)
  
  #-------------------------------------------------------------------------
  # Read data
  #-------------------------------------------------------------------------
  X <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_X.rds"))
  Landuse <- Prod[Prod$Unit=="ha" & Prod$Year==year,]
  Biomass <- Prod[Prod$Unit=="tonnes" & Prod$Year==year,]
  nrreg <- nrow(regions)
  nrcom <- 130
  
  
  #-------------------------------------------------------------------------
  # Prepare Extension
  #-------------------------------------------------------------------------
  E <- data.frame(area_code = rep(regions$code, each = nrcom),
                  area = rep(regions$name, each = nrcom),
                  item_code = rep(items$item_code, nrreg),
                  item = rep(items$item, nrreg),
                  comm_code = rep(items$comm_code, nrreg),
                  group = rep(items$group, nrreg),
                  landuse = 0,
                  biomass = 0)
  
  E$ID <- paste(E$Country.Code,E$Item.Code,sep="_")
  E$Landuse <- Landuse$Value[match(E$ID,Landuse$ID)]
  E$Landuse[!is.finite(E$Landuse)] <- 0
  E$Landuse[E$Group != "Primary crops" & E$Item!="Sweeteners, Other"] <- 0
  E$Landuse[E$Landuse<0] <- 0
  E$Biomass <- Biomass$Value[match(E$ID,Biomass$ID)]
  E$Biomass[!is.finite(E$Biomass)] <- 0
  E$Biomass[E$Group != "Primary crops"] <- 0
  E$Biomass[E$Item=="Grazing"] <- X[E$Item=="Grazing"]
  E$Landuse[E$Landuse<0] <- 0
  
  saveRDS(E, file=paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
}






#################################################################
#
# WATER EXTENSION for FABIO
#
#################################################################

library(tidyverse)
rm(list=ls())

# read data
water <- read.csv("./inst/fabio_input/water_data.csv", stringsAsFactors = FALSE)
water_lvst <- read.csv2("./inst/fabio_input/water_lvst.csv", stringsAsFactors = FALSE)
water_grass <- read.csv("./inst/fabio_input/water_pasture.csv", stringsAsFactors = FALSE)
countries <- read.csv("./inst/fabio_input/regions_concordance_water.csv", stringsAsFactors = FALSE)
items <- read.csv("./inst/fabio_input/items_concordance_water.csv", stringsAsFactors = FALSE)
load("/mnt/nfs_fineprint/tmp/fabio/data/Prod.RData")

years <- 1986:2013

for(year in years){
  print(year)
  E <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
  blue <- water[water$Water=="blue",]
  green <- water[water$Water=="green",]
  
  prod <- Prod[Prod$Year==year & Prod$Unit=="tonnes",]
  prod$Country_code_water <- countries$Code_water[match(prod$Country.Code,countries$Area.Code)]
  prod$Country_code_fabio <- countries$Code_fabio[match(prod$Country.Code,countries$Area.Code)]
  prod$Item_water <- items$Prod_water[match(prod$Item.Code,items$Item_Code)]
  
  prod$blue <- blue$Value[match(paste(prod$Country_code_water,prod$Item_water), paste(blue$FAO.code,blue$Product))]
  prod$green <- green$Value[match(paste(prod$Country_code_water,prod$Item_water), paste(green$FAO.code,green$Product))]
  prod$blue[is.na(prod$blue)] <- 0
  prod$green[is.na(prod$green)] <- 0
  prod$Value[is.na(prod$Value)] <- 0
  
  prod$blue_water <- prod$blue * prod$Value
  prod$green_water <- prod$green * prod$Value
  
  blue_water <- prod %>% 
    group_by(Country.Code,Item) %>% 
    summarise(value = sum(blue_water))
  
  green_water <- prod %>% 
    group_by(Country.Code,Item) %>% 
    summarise(value = sum(green_water))
  
  E$ID <- NULL
  E$Blue_water <- blue_water$value[match(paste(E$Country.Code,E$Item), paste(blue_water$Country.Code,blue_water$Item))]
  E$Green_water <- green_water$value[match(paste(E$Country.Code,E$Item), paste(green_water$Country.Code,green_water$Item))]
  E$Blue_water[is.na(E$Blue_water)] <- 0
  E$Green_water[is.na(E$Green_water)] <- 0
  
  # water footprint of grazing
  E$Green_water[E$Item=="Grazing"] <- water_grass$Green_water[match(E$Country.Code,water_grass$Code)][E$Item=="Grazing"]
  E$Green_water[E$Item=="Grazing"] <- E$Green_water[E$Item=="Grazing"] * E$Biomass[E$Item=="Grazing"]
  
  
  saveRDS(E, paste0("/mnt/nfs_fineprint/tmp/fabio/",year,"_E.rds"))
}

