
# Biofuels -----------------------------------------------------------------

library("data.table")
library("stringr" )
# library("openxlsx")
source("R/00_system_variables.R")

path_biofuels <- "input/biofuels/"


# EIA data ----------------------------------------------------------------
# Source: https://www.eia.gov/international/data/world/biofuels/biofuels-production?pd=79&p=000001g00006&u=1&f=A&v=mapbubble&a=-&i=none&vo=value&t=C&g=none&l=249-ruvvvvvfvtvnvv1vrvvvvfvvvvvvfvvvou20evvvvvvvvvvnvuvs0008&s=315532800000&e=1640995200000&ev=false&

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




# IEA data ----------------------------------------------------------------
# Source: https://www.iea.org/data-and-statistics/data-product/world-energy-statistics

biofuels_iea_cols <- c("character", "character", "character",
  "character", "character", "character", "character")
biofuels_iea <- fread(paste0(path_biofuels, "/iea_biofuels_2024.csv"),
  colClasses = biofuels_iea_cols, skip = 4)

saveRDS(biofuels_iea, paste0(path_biofuels, "biofuels_iea.rds"))

