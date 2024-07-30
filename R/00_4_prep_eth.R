
# Biofuels -----------------------------------------------------------------

library("data.table")
library("stringr" )
# library("openxlsx")
source("R/00_system_variables.R")

path_biofuels <- "input/biofuels/"


# EIA data ----------------------------------------------------------------

biofuels_eia_cols <- c("character", "character", "character", rep("character", length(1980:2022)))

biofuels_eia <- fread(paste0(path_biofuels, "eia_biofuels_2024.csv"), header = TRUE,
                   check.names = FALSE, colClasses = biofuels_eia_cols, na.strings = c("-", "--", "", "NA"))

biofuels_eia <- biofuels_eia[,(as.character(years)) := lapply(.SD,as.numeric),.SDcols=as.character(years)]
# biofuels_eia[, country := str_trim(country)]

if(!biofuels_eia[[3]][1] == "Afghanistan" &&
  !biofuels_eia[[3]][nrow(biofuels_eia)] == "Zimbabwe") {
  stop("CSV not read in successfully")
}

saveRDS(biofuels_eia, paste0(path_biofuels, "biofuels_eia.rds"))

# # Compare to previous data
# biofuels_eia_comp <- read.xlsx(paste0(path_biofuels, "EIA_Biofuels_production.xlsx"),
#                        colNames = FALSE, rows = 9:235, cols = c(2, 4:38))
# for(num in 2:36) class(biofuels_eia_comp[[num]]) <- "numeric"
# names(biofuels_eia_comp) <- c("country", paste0("y", 1980:2014))


# IEA data ----------------------------------------------------------------

biofuels_iea_cols <- c("character", "NULL", "character", "character",
  "NULL", "character", "character", "character",
  "integer", "NULL", "numeric", "NULL", "NULL")
biofuels_iea <- fread(paste0(path_biofuels, "iea_renewables_2024.csv"),
  colClasses = biofuels_iea_cols)
biofuels_iea <- subset(biofuels_iea, PRODUCT == "BIOGASOL")[, -2]
biofuels_iea <- subset(biofuels_iea, PRODUCT %in% c("BIOGASOL", "BIODIESEL", "OBIOLIQ", "BIOJETKERO", "BIOGASES") & 
                         Value != 0 & !is.na(Value))

library(tidyverse)
iea <- biofuels_iea %>% 
  group_by(UNIT, Flow, Product) %>% 
  summarise(value = sum(Value, na.rm = T)) %>% 
  spread(Product, value)
fwrite(iea, paste0(path_biofuels, "iea.csv"))


biofuels_iea_extent <- min(biofuels_iea$TIME):max(biofuels_iea$TIME)
biofuels_iea <- dcast(biofuels_iea, Country ~ TIME, value.var = "Value", fun.aggregate = sum)
names(biofuels_iea) <- c("country", biofuels_iea_extent)
saveRDS(biofuels_iea, paste0(path_biofuels, "biofuels_iea.rds"))

# # Compare to previous data
# biofuels_iea_comp <- read.xlsx(paste0(path_biofuels, "IEA_Biogasoline_production.xlsx"),
#                        colNames = FALSE, rows = 8:167, cols = c(1, 3:28))
# for(num in 2:27) class(biofuels_iea_comp[[num]]) <- "numeric"
# names(biofuels_iea_comp) <- c("country", paste0("y", 1990:2015))
