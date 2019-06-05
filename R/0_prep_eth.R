
# Ethanol -----------------------------------------------------------------

library(data.table)
# library(openxlsx)
path_eth <- "input/ethanol/"


# EIA data ----------------------------------------------------------------

eth1_extent <- 1980:2014
eth1_cols <- c("NULL", "character", "NULL", rep("numeric", length(eth1_extent)))
eth1_prod <- fread(paste0(path_eth, "eia_biofuels_production.csv"),
                   skip = 8, check.names = FALSE, nrows = 227,
                   colClasses = eth1_cols, na.strings = c("-", "--", "", "NA"))

names(eth1_prod) <- c("country", eth1_extent)

if(!eth1_prod[[1]][1] == "Afghanistan" &&
   !eth1_prod[[1]][nrow(eth1_prod)] == "Zimbabwe") {
  stop("CSV not read in successfully")
}

saveRDS(eth1_prod, paste0(path_eth, "eth_eia.rds"))

# # Compare to previous data
# eth1_comp <- read.xlsx(paste0(path_eth, "EIA_Biofuels_production.xlsx"),
#                        colNames = FALSE, rows = 9:235, cols = c(2, 4:38))
# for(num in 2:36) class(eth1_comp[[num]]) <- "numeric"
# names(eth1_comp) <- c("country", paste0("y", 1980:2014))


# IEA data ----------------------------------------------------------------

eth2_cols <- c("NULL", "NULL", "character", "NULL",
               "NULL", "NULL", "character", "NULL",
               "integer", "NULL", "numeric", "NULL", "NULL")
eth2_prod <- fread(paste0(path_eth, "iea_renewables_production.csv"),
                   colClasses = eth2_cols)
eth2_prod <- subset(eth2_prod, PRODUCT == "BIOGASOL")[, -2]
eth2_extent <- min(eth2_prod$TIME):max(eth2_prod$TIME)
eth2_prod <- dcast(eth2_prod, COUNTRY ~ TIME, value.var = "Value")
names(eth2_prod) <- c("country", eth2_extent)

saveRDS(eth2_prod, paste0(path_eth, "eth_iea.rds"))

# # Compare to previous data
# eth2_comp <- read.xlsx(paste0(path_eth, "IEA_Biogasoline_production.xlsx"),
#                        colNames = FALSE, rows = 8:167, cols = c(1, 3:28))
# for(num in 2:27) class(eth2_comp[[num]]) <- "numeric"
# names(eth2_comp) <- c("country", paste0("y", 1990:2015))
