# Load FAO data -----------------------------------------------------------

library("data.table")
source("R/00_prep_functions.R")
path_fish <- "input/fish/"
path_fish23 <- paste0(path_fish, "2023/")


# Settings ----------------------------------------------------------------

files <- c(
  "prod" = "GlobalProduction_2025.1.0.zip", #total primary production (capture + aquaculture)
  "prod_proc" = "FI_Trade_PP_2025.1.0.zip", #processed production
  "trad" = "FI_Trade_Partners_2025.1.0.zip", #trade by partner country (since 2019)
  "trad_aggr" = "FI_Trade_2025.1.0.zip", #countries' total import and export
  "isscaap" = "GlobalProduction_2025.1.0.zip", #catch/aquaculture metadata items
  "isscfc" = "FI_Trade_PP_2025.1.0.zip" #trade/processed metadata items
)

# Files to extract from the ZIP archives
extr <- c("Global_production_quantity.csv",
          "TRADE_PP_QUANTITY.csv",
          "TRADE_PARTNERS_QUANTITY.csv",
          "TRADE_QUANTITY.csv",
          "CL_FI_SPECIES_GROUPS.csv",
          "CL_FI_COMMODITY_ISSCFC.csv"
)

name <- names(files)

# Links to the files
links <- c(rep("http://www.fao.org/fishery/static/Data/", length(files)))

#############

# Column types to possibly skip some
col_types <- list(
  "prod" = c(rep("character", 5), "numeric", "numeric", "character"),
  "prod_proc" = c(rep("character", 4), "numeric", "character", "numeric"),
  "trad" = c(rep("character", 5), "numeric", "character", "numeric"),
  "trad_aggr" = c(rep("character", 4), "numeric", "character", "numeric"),
  "isscaap" = rep("character", 36),
  "isscfc" = c(rep("character", 8), "logical", "character")
)


# Execute -----------------------------------------------------------------

fa_dl(file = files, link = links, path = path_fish)


fa_extract(path_in = path_fish, files = files,
           path_out = path_fish, name = name, extr = extr, col_types = col_types, #read_method = read_method,
           rm = FALSE)





# Add 2023 trade/processed metadata items ---------------------------------
## necessary because it includes HS correspondence
#### (removed by FAO in 2024 publication)
#### (separate step necessary because otherwise the files are overwritten due to same naming)

files23 <- c("isscfc23" = "FI_Trade_PP_2023.1.0.zip")

extr23 <- c("CL_FI_COMMODITY_ISSCFC.csv")

name23 <- names(files23)

links23 <- c(rep("http://www.fao.org/fishery/static/Data/", length(files23)))

col_types23 <- list("isscfc23" = c(rep("character", 9), "logical", "character"))

fa_dl(file = files23, link = links23, path = path_fish23)

fa_extract(path_in = path_fish23, files = files23,
           path_out = path_fish23, name = name23, extr = extr23, col_types = col_types23, #read_method = read_method,
           rm = FALSE)
