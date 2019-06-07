
library(data.table)
source("R/1_tidy_functions.R")

flow_pref <- function(x, pref = "Import") {

  x[, id := paste(from_code, to_code, item_code, year, sep = "_")]

  to_kick <- x[imex != pref & id %in% x[imex == pref, id], id]
  cat("Dropping ", length(to_kick), " observations as preference is given to ",
      pref, ".\n", sep = "")

  return(x[imex == pref | !id %in% to_kick])
}

regions <- fread("inst/regions_full.csv")
items <- fread("inst/items_full.csv")


# BTD ---------------------------------------------------------------------

btd <- readRDS("data/tidy/btd_tidy.rds")

# Change from reporting & partner country to receiving & supplying country
btd[, `:=`(from = ifelse(imex == "Import", partner, reporter),
           from_code = ifelse(imex == "Import", partner_code, reporter_code),
           to = ifelse(imex == "Import", reporter, partner),
           to_code = ifelse(imex == "Import", reporter_code, partner_code),
           reporter = NULL, reporter_code = NULL,
           partner = NULL, partner_code = NULL)]

# Give preference to imports flows over export flows
btd <- flow_pref(btd, pref = "Import")

# Exclude intra-regional trade flows
btd <- dt_filter(btd, from_code != to_code)


# Forestry ----------------------------------------------------------------

fore <- readRDS("data/tidy/fore_trad_tidy.rds")

# Change from reporting & partner country to receiving & supplying country
fore[, `:=`(from = ifelse(imex == "Import", partner, reporter),
            from_code = ifelse(imex == "Import", partner_code, reporter_code),
            to = ifelse(imex == "Import", reporter, partner),
            to_code = ifelse(imex == "Import", reporter_code, partner_code),
            reporter = NULL, reporter_code = NULL,
            partner = NULL, partner_code = NULL)]

# Give preference to imports flows over export flows
fore <- flow_pref(fore, pref = "Import")

# Exclude intra-regional trade flows
fore <- dt_filter(fore, from_code != to_code)

# Merge btd and fore
# Estimate missing kg & head values based on prices


# Ethanol -----------------------------------------------------------------

comtrade <- readRDS("data/tidy/comtrade_tidy.rds")
baci <- readRDS("data/tidy/baci_tidy.rds")

if(year < 1995) {
  eth <- comtrade[grep("2207", item_code), ]

  # Change from reporting & partner country to receiving & supplying country
  eth[, `:=`(from = ifelse(imex == "Import", partner, reporter),
             from_code = ifelse(imex == "Import", partner_code, reporter_code),
             to = ifelse(imex == "Import", reporter, partner),
             to_code = ifelse(imex == "Import", reporter_code, partner_code),
             reporter = NULL, reporter_code = NULL,
             partner = NULL, partner_code = NULL)]

  # Give preference to imports flows over export flows
  eth <- flow_pref(eth, pref = "Import")

  # Exclude intra-regional trade flows
  eth <- dt_filter(eth, from_code != to_code)

  # Estimate missing kg based on prices
}

if(year >= 1995) {
  eth <- baci[grep("2207[0-9]*", category), ]
  eth[, `:=`(item = "Alcohol, Non-Food",
             item_code = 2659)]
  # Estimate missing kg based on prices
}


year <- 2000
for(year in years) {

  btd <- btd_full[year == 2000,]



  # btd <- btd[item_code %in% items$item_code, ]

}
