
library(data.table)


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

crop <- dt_rename(crop)

# Adjust countries (merge Ethiopia, kick China, kick country groups)

dt_filter(crop, !is.na)
dt_filter(crop, value != 0)

# Item concordance and TCF

crop[, value := value * tcf]

crop <- crop[, list(value = sum(value)),
             by = .(area_code, area, element, year, unit, item_code, item)]


crop_prim <- readRDS("input/fao/crop_prim.rds")

crop_prim <- dt_rename(crop_prim)

dt_filter(crop, !is.na)
dt_filter(crop_prim, item_code %in% item_conc)
dt_filter(crop_prim, element != "Yield")

# Item concordance

crop_prim <- crop_prim[, list(value = sum(value)),
                       by = .(area_code, area, element, year, unit, item_code, item)]

crop_prim[, unit := ifelse(element == "Area harvested", "ha", "tonnes")]

rbind(crop, crop_prim)


# Livestock ---------------------------------------------------------------

live_prod <- readRDS("input/fao/live_prod.rds")
live_proc <- readRDS("input/fao/live_proc.rds")

live_prim <- readRDS("input/fao/live_prim.rds")
