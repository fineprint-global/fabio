
library(data.table)
library(tidyverse)
source("R/01_tidy_functions.R")

items <- fread("inst/items_full.csv")
regions <- fread("inst/regions_full.csv")
io <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/io_codes.csv")
su <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/su_codes.csv")
fd <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/fd_codes.csv")
Y <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
fd_l <- fread("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/fd_codes.csv")
Y_l <- readRDS("/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds")

# Chinese edible oil statistics
# Sources: 
# - China National Grain & Oils Information Center, Comprehensive balance analysis of China's edible oil market, http://www.grainoil.com.cn/, accessed on 01/03/2023
# - USDA, Oilseeds and Products Update, Attaché Report (GAIN), https://fas.usda.gov/data/search?keyword=Oilseeds%20and%20Products%20Annual&reports%5B0%5D=report_commodities%3A27&reports%5B1%5D=report_commodities%3A28&reports%5B2%5D=report_regions%3A420&reports%5B3%5D=report_type%3A10251&page=0
oil <- fread("input/oils_china.csv")

years <- names(Y)
Y_new <- Y
Y_l_new <- Y_l

# correct food and other use of veg. oils for China
i = 1
for(i in seq_along(Y)){
  print(years[i])
  if(as.numeric(years[i]) < 2013){
    data <- merge(io, oil[year==2013, .(comm_code, food_share)], by = "comm_code", all.x = TRUE, sort = FALSE)
  } else if(as.numeric(years[i]) > 2020){
    data <- merge(io, oil[year==2020, .(comm_code, food_share)], by = "comm_code", all.x = TRUE, sort = FALSE)
  } else {
    data <- merge(io, oil[year==as.numeric(years[i]), .(comm_code, food_share)], by = "comm_code", all.x = TRUE, sort = FALSE)
  }
  
  data <- cbind(data, as.matrix(Y[[i]][,fd$area=="China, mainland"]))
  # data[, food_share_fao := `41_food` / (`41_food` + `41_other`)]
  # data[, `:=`(food = `41_food`, other = `41_other`)]
  data[!is.na(food_share), `:=`(food = round((food + other) * food_share),
              other = round((food + other) * (1-food_share)))]
  Y_new[[i]][, fd$area_code==41 & fd$fd=="food"] <- data$food
  Y_new[[i]][, fd$area_code==41 & fd$fd=="other"] <- data$other
  
  data_l <- cbind(data, as.matrix(Y_l[[i]][,fd_l$area=="China, mainland"]))
  data_l[!is.na(food_share), `:=`(food = round((food + other) * food_share),
                                other = round((food + other) * (1-food_share)))]
  Y_l_new[[i]][, fd_l$area_code==41 & fd_l$fd=="food"] <- data_l$food
  Y_l_new[[i]][, fd_l$area_code==41 & fd_l$fd=="other"] <- data_l$other
}

# compare old and new values
for(i in seq_along(Y)){
  food <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="food"])
  other <- sum(Y[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="other"])
  share <- food / (food + other)
  food_new <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="food"])
  other_new <- sum(Y_new[[i]][io$comm_code %in% oil$comm_code, fd$area_code==41 & fd$fd=="other"])
  share_new <- food_new / (food_new + other_new)
  print(paste0(years[i], ": ", round(food/1000000), "/", round(other/1000000), " Mt, ", round(share*100), "% // ",
               round(food_new/1000000), "/", round(other_new/1000000), " Mt, ", round(share_new*100), "%"))
}

saveRDS(Y_new, "/mnt/nfs_fineprint/tmp/fabio/v1.2/Y.rds")
saveRDS(Y_l_new, "/mnt/nfs_fineprint/tmp/fabio/v1.2/losses/Y.rds")


