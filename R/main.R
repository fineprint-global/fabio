
# 0_prep ------------------------------------------------------------------

library("data.table") # 1.12.0
library("comtradr") # 0.2.2
# library("openxlsx") # 4.1.0

source("R/0_prep_functions.R")

# Will download required ZIP files and convert the contents to RDS
source("R/0_prep_fao.R")

# Requires EIA and IEA CSV files that are available from:
# https://www.eia.gov/opendata/qb.php?category=2135203 (in 1000 bbl/d)
# http://dx.doi.org/10.1787/data-00550-en
source("R/0_prep_eth.R")

# Requires BACI92 ZIP files as well as data.table::rbindlist(comtrade)
source("R/0_prep_trade.R")

# Depends on outputs produced in step 0
source("R/1_tidy_fao.R")
source("R/1_tidy_eth.R")
source("R/1_tidy_trade.R")

# Build full BTD, integrating trade data
source("R/2_build_btd.R")

# Build full CBS, integrating production data, etc.
source("R/3_build_cbs.R")

# Estimate trade shares from the CBS
source("R/4_estimate_btd.R")

# Balance trade using RAS
source("R/5_balance.R")

# Create the supply structure
source("R/6_supply.R")

# Create the use structure
source("R/7_use.R")

# Build MRIO blocks
source("R/8_mrio.R")
