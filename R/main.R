
# 0_prep ------------------------------------------------------------------

# Will download required ZIP files and convert the contents to RDS
source("R/00_1_prep_fao.R")
source("R/00_2_prep_fao_reshape.R")

# Requires BACI92 ZIP files as well as data.table::rbindlist(comtrade)
source("R/00_3_prep_trade.R")

# Requires EIA and IEA CSV files that are available from:
# https://www.eia.gov/opendata/qb.php?category=2135203 (in 1000 bbl/d)
# http://dx.doi.org/10.1787/data-00550-en
source("R/00_4_prep_eth.R")

# Will download required ZIP files and convert the contents to RDS
source("R/00_5_prep_fish.R")
source("R/00_6_labels.R")
source("R/00_7_prep_spatial_NPK.R")
rm(list = ls()); gc()


# 1_tidy ------------------------------------------------------------------

# Depends on outputs produced in step 0
source("R/01_1_tidy_fao.R")
source("R/01_2_tidy_trade.R")
source("R/01_3_tidy_eth.R")
source("R/01_4_tidy_fish.R")
rm(list = ls()); gc()

# Build full BTD ---------------------------------------------------------
source("R/02_build_btd.R"); rm(list = ls()); gc()

# Build full CBS ---------------------------------------------------------
source("R/03_1a_build_cbs.R"); rm(list = ls()); gc()
source("R/03_1b_balance_cbs.R"); rm(list = ls()); gc()

# Build full SUA ---------------------------------------------------------
source("R/03_2a_build_tcf_sua.R"); rm(list = ls()); gc()
source("R/03_2b_build_sua.R"); rm(list = ls()); gc()

# Estimate BTD from totals -----------------------------------------------
source("R/04_estimate_btd.R"); rm(list = ls()); gc()

# Balance trade using RAS ------------------------------------------------
source("R/05_balance_btd.R"); rm(list = ls()); gc()

# Allocate re-exports ----------------------------------------------------
source("R/06_re-exports.R"); rm(list = ls()); gc()

# Create the supply structure --------------------------------------------
source("R/07_1_supply_cbs.R"); rm(list = ls()); gc()
source("R/07_2_supply_sua.R"); rm(list = ls()); gc()

# Create the use structure -----------------------------------------------
source("R/08_1a_use_cbs.R"); rm(list = ls()); gc()

# Build multi-regional supply, use and IO tables -------------------------
source("R/09_mrsut.R"); rm(list = ls()); gc()
source("R/10_mrio.R"); rm(list = ls()); gc()

# Derive Leontief inverses -----------------------------------------------
source("R/11_leontief_inverse.R"); rm(list = ls()); gc()

# Prepare env. extensions ------------------------------------------------
source("R/12_1_land_mass_water.R"); rm(list = ls()); gc()
source("R/12_2_NPK_spatial.R"); rm(list = ls()); gc()
source("R/12_3_NPK_fertilizer_application.R"); rm(list = ls()); gc()
source("R/12_4_NP_balance.R"); rm(list = ls()); gc()
source("R/12_5_ghg.R"); rm(list = ls()); gc()
source("R/12_6_biodiversity.R"); rm(list = ls()); gc()
source("R/13_extensions_main.R"); rm(list = ls()); gc()



