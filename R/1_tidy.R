
library(data.table)
source("R/1_tidy_f.R")


# Colnames ----------------------------------------------------------------

rename_cbs <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Year" = "year",
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "losses",
  # "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses" = "other",
  "Processing" = "processing"
)

rename_btd <- c(
  "Reporter Country Code" = "reporter_code",
  "Reporter Countries" = "reporter",
  "Partner Country Code" = "partner_code",
  "Partner Countries" = "partner",
  "Item Code" = "item_code",
  "Item" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value"
)

rename_oth <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  # "Element Code" = "element_code",
  "Element" = "element",
  # "Year Code" = "year_code",
  "Year" = "year",
  "Unit" = "unit",
  # "Flag" = "flag",
  "Value" = "value"
)


# CBS ---------------------------------------------------------------------

cbs <- rbind(readRDS("input/fao/cbs_crop.rds"),
                 readRDS("input/fao/cbs_live.rds"))

cbs <- dt_rename(cbs)

# Adjust countries (merge Ethiopia, kick China, kick country groups)
# Country concordance

cbs <- dcast(cbs,
             area_code + area + item_code + item + year ~ element,
             value.var = "value")

dt_replace(cbs, is.na, 0)

# Balance total_supply, adjust stock_variation
# Cap some variables at 0
# Balance some variables


# BTD ---------------------------------------------------------------------

btd_prod <- readRDS("input/fao/btd_prod.rds")

btd_prod <- dt_rename(btd_prod)

# Adjust countries (merge Ethiopia, kick China, kick country groups)

dt_filter(btd_prod, !item %in% items)
dt_filter(btd_prod, value > 0)

btd_prod[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

# Apply TCF

# Aggregate to CBS items

btd <- dcast(btd_prod,
             reporter_code + reporter + partner_code + partner +
               item_code + item + year + imex ~ unit,
             value.var = "value")
btd <- dt_rename(btd, c("1000 Head" = "k_cap", "1000 US$" = "k_usd",
                        "Head" = "cap", "tonnes" = "tonnes"), drop = FALSE)

dt_replace(cbs, is.na, 0)


# Forestry ----------------------------------------------------------------

# Production
#
fore_prod <- readRDS("input/fao/fore_prod.rds")

fore_prod <- dt_rename(fore_prod)

# Adjust countries (merge Ethiopia, kick China, kick country groups)

dt_filter(fore_prod, !item %in% items)
dt_filter(fore_prod, value > 0)
dt_filter(fore_prod, unit == "m3")

fore_prod[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

fore_prod <- dcast(fore_prod,
                   area_code + area + item_code + item + year ~ imex,
                   value.var = "value")
fore_prod <- dt_rename(fore_prod, c("Export" = "exports", "Import" = "imports",
                                    "Production" = "production"), drop = FALSE)

dt_replace(fore_prod, is.na, 0)


# Trade
#
fore_trad <- readRDS("input/fao/fore_trad.rds")

fore_trad <- dt_rename(fore_trad)

# Adjust countries (merge Ethiopia, kick China, kick country groups)

dt_filter(fore_trad, !item %in% items)

fore_trad[, imex := factor(gsub("^(Import|Export) (.*)$", "\\1", element))]

fore_trad <- dcast(fore_trad,
                   reporter_code + reporter + partner_code + partner +
                     item_code + item + year ~ imex,
                   value.var = "value")
fore_trad <- dt_rename(fore_trad, c("Export" = "exports", "Import" = "imports",
                                    "Production" = "production"), drop = FALSE)


# Crops -------------------------------------------------------------------

crop <- rbind(readRDS("input/fao/crop_prod.rds"),
              readRDS("input/fao/crop_proc.rds"))

crop <- dt_rename(crop, rename_oth, drop = TRUE)

# Country / Area adjustments
crop <- area_kick(crop, code = 351, pattern = "China", groups = TRUE)
crop <- area_merge(crop, orig = 62, dest = 238, pattern = "Ethiopia")

crop <- dt_filter(crop, value > 0)

# Item concordance and TCF

crop[, value := value * tcf]

crop <- crop[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]


crop_prim <- readRDS("input/fao/crop_prim.rds")

crop_prim <- dt_rename(crop_prim)

dt_filter(crop_prim, !item %in% items)
dt_filter(crop_prim, !is.na)
dt_filter(crop_prim, element != "Yield")

# Item concordance

crop_prim <- crop_prim[, list(value = sum(value)),
                       by = .(area_code, area, element, year, unit, item_code, item)]

crop <- rbind(crop, crop_prim)

# Filter to values > 0?


# Livestock ---------------------------------------------------------------

live_prod <- rbind(readRDS("input/fao/live_prod.rds"),
                   readRDS("input/fao/live_proc.rds"),
                   readRDS("input/fao/live_prim.rds"))

live <- dt_rename(live)

# Adjust countries (merge Ethiopia, kick China, kick country groups)

dt_filter(live, !item %in% items)
dt_filter(live, !is.na)
dt_filter(live, value != 0)

live[unit == "1000 Head", value := value / 1000]
live[unit == "1000 Head", unit == "Head"]


# Bio-Ethanol -------------------------------------------------------------


