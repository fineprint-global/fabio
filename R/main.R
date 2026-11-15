# 0_prep ------------------------------------------------------------------

# Will download ZIP files from FAOSTAT
source("R/00_01_prep_fao.R")
source("R/00_02_prep_fao_reshape.R")
rm(list = ls()); gc()

# Requires downloading BACI ZIP files available from:
# https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
source("R/00_03_prep_trade.R")
rm(list = ls()); gc()

# Requires EIA and IEA CSV files available from:
# https://www.eia.gov/opendata/qb.php?category=2135203 (in 1000 bbl/d)
# http://dx.doi.org/10.1787/data-00550-en
source("R/00_04_prep_eth.R")

# Will download ZIP files from FAOSTAT
source("R/00_05_prep_fish.R")
source("R/00_06_labels.R")
rm(list = ls()); gc()

# Requires downloading some data (see script)
source("R/00_07_prep_spatial_NPK.R"); rm(list = ls()); gc()
source("R/00_08_prep_gloria_e.R"); rm(list = ls()); gc()
# Eager staging of the value-added extension's external inputs (Brazil IBGE SUTs,
# Canada cansim slices, OECD SUT, Eurostat NAMA): always re-downloads + overwrites
# into input/value_added/. Read later by 14_3/14_4 (pure readers). Self-contained
# (sources R/00_value_added_config.R); year scope follows the FABIO years.
source("R/00_09_prep_value_added.R"); rm(list = ls()); gc()
source("R/00_10_prep_feed.R"); rm(list = ls()); gc()


# 1_tidy ------------------------------------------------------------------
source("R/01_1_tidy_fao.R"); rm(list = ls()); gc()
source("R/01_2_tidy_trade.R"); rm(list = ls()); gc()
source("R/01_3_tidy_eth.R"); rm(list = ls()); gc()
source("R/01_4_tidy_fish.R"); rm(list = ls()); gc()


# Build full BTD ---------------------------------------------------------
source("R/02_build_btd.R"); rm(list = ls()); gc()

# Build full CBS ---------------------------------------------------------
source("R/03_1a_build_cbs.R"); rm(list = ls()); gc()
source("R/03_1b_balance_cbs.R"); rm(list = ls()); gc()

# # Build full SUA ---------------------------------------------------------
# source("R/03_2a_build_tcf_sua.R"); rm(list = ls()); gc()
# source("R/03_2b_build_sua.R"); rm(list = ls()); gc()

# Build a gap-filled and balanced trade dataset --------------------------
source("R/04_estimate_btd.R"); rm(list = ls()); gc()
source("R/05_balance_btd.R"); rm(list = ls()); gc()
source("R/06_re-exports.R"); rm(list = ls()); gc()

# Create the supply structure --------------------------------------------
source("R/07_1_supply_cbs.R"); rm(list = ls()); gc()
# source("R/07_2_supply_sua.R"); rm(list = ls()); gc()

# Create the use structure -----------------------------------------------
source("R/08_1a_use_cbs.R"); rm(list = ls()); gc()

# Build multi-regional supply, use and IO tables -------------------------
source("R/09_mrsut.R"); rm(list = ls()); gc()
source("R/10_mrio.R"); rm(list = ls()); gc()

# Derive Leontief inverses -----------------------------------------------
source("R/11_leontif_inverse.R"); rm(list = ls()); gc()

# Prepare env. extensions ------------------------------------------------
source("R/12_01_land_mass_water.R"); rm(list = ls()); gc()
# Script 12_02 doesn't need to be re-run at every update.
# source("R/12_02_NPK_spatial.R"); rm(list = ls()); gc()
source("R/12_03_NPK_fertilizer_application.R"); rm(list = ls()); gc()
source("R/12_04_NP_balance.R"); rm(list = ls()); gc()
source("R/12_05_ghg.R"); rm(list = ls()); gc()
source("R/12_06_biodiversity_ibif.R"); rm(list = ls()); gc()
source("R/12_07_biodiversity_tidy.R"); rm(list = ls()); gc()
source("R/12_08_biodiversity_lc_fd.R"); rm(list = ls()); gc()
source("R/12_09_ecosystem_services.R"); rm(list = ls()); gc()
# Compile extensions into E.rds / ex_labels.csv
source("R/12_10_extensions_main.R"); rm(list = ls()); gc()

# Value-added / producer-price pipeline (folded in) ----------------------
# Each script sources R/00_value_added_config.R itself, so the rm() pattern
# between them is safe. Order is load-bearing:
#   price stage (13_) -> FABIOv2 producer total values (the 14_1-14_3 handoff)
source("R/13_1_FAOstat_producer_prices_USD.R"); rm(list = ls()); gc()
source("R/13_2_clean_bilateral_trade_prices.R"); rm(list = ls()); gc()
source("R/13_3_FABIO_v2_price_extension.R"); rm(list = ls()); gc()
#   value-added stage (14_) -> per-base/per-source VA, then COMBINED synthesis
source("R/14_1_value_added_FABIO_v2_MRIOTs.R"); rm(list = ls()); gc()
source("R/14_2_value_added_FABIO_v2_FSDN.R"); rm(list = ls()); gc()
source("R/14_3_value_added_FABIO_v2_national_SUTs.R"); rm(list = ls()); gc()
source("R/14_4_value_added_FABIO_v2_synthesis.R"); rm(list = ls()); gc()
#   last mile -> six CBS value-added extensions in data/extensions/cbs/
source("R/14_5_value_added_extensions.R"); rm(list = ls()); gc()
