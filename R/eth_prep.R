
# Ethanol -----------------------------------------------------------------

library(readr)
# library(openxlsx)

path <- "input/ethanol/"


# EIA data ----------------------------------------------------------------

eth1_extent <- 1980:2014
eth1_cols <- paste0("_c_", strrep("d", length(eth1_extent)))
eth1_prod <- read_csv(paste0(path, "eia_biofuels_production.csv"),
                      skip = 8, na = c("-", "--", "", "NA"),
                      col_names = FALSE, n_max = 227,
                      col_types = eth1_cols)
names(eth1_prod) <- c("country", paste0("y", eth1_extent))

if(!eth1_prod[[1]][1] == "Afghanistan" &&
   !eth1_prod[[1]][nrow(eth1_prod)] == "Zimbabwe")
  stop("CSV not read in successfully")

saveRDS(eth1_prod, paste0(path, "eth1_prod.rds"))

# # Compare to previous data
# eth1_prod_comp <- read.xlsx("EIA_Biofuels_production.xlsx",
#                             colNames = FALSE, rows = 9:235, cols = c(2, 4:38))
# for(num in 2:36) class(eth1_prod_comp[[num]]) <- "numeric"
# names(eth1_prod_comp) <- c("country", paste0("y", 1980:2014))
# eth1_prod_comp[-1] - eth1_prod[-1]


# IEA data ----------------------------------------------------------------

eth2_cols <- "__c___f_i_d__"
eth2_prod <- read_csv(paste0(path, "iea_renewables_production.csv"),
                      col_types = eth2_cols)
eth2_prod <- subset(eth2_prod, PRODUCT == "BIOGASOL")[-2]
eth2_extent <- min(eth2_prod$TIME):max(eth2_prod$TIME)
eth2_prod <- reshape2::dcast(eth2_prod, COUNTRY ~ TIME, value.var = "Value")
names(eth2_prod) <- c("country", paste0("y", eth2_extent))

saveRDS(eth2_prod, paste0(path, "eth2_prod.rds"))

# # Compare to previous data
# eth2_prod_comp <- read.xlsx("IEA_Biogasoline_production.xlsx",
#                             colNames = FALSE, rows = 8:167, cols = c(1, 3:28))
# for(num in 2:27) class(eth2_prod_comp[[num]]) <- "numeric"
# names(eth2_prod_comp) <- c("country", paste0("y", 1990:2015))
# eth2_prod_comp[-1] - eth2_prod[c(-1, -28, -29)]
