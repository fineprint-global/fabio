library("data.table")
library("ggplot2")
library("dplyr")
source("R/01_tidy_functions.R")

isscaap <- fread("inst/items_fish_isscaap.csv")
species <- fread("inst/items_fish_species.csv")
tcf <- fread("inst/items_fish_hs.csv")


# Colnames ---------------------------------------------------------------------

rename <- c(
  "COUNTRY.UN_CODE" = "country",
  "MEASURE" = "unit",
  "PERIOD" = "year",
  "VALUE" = "value",
  "STATUS" = "status",
  #prod:
  "SPECIES.ALPHA_3_CODE" = "species",
  "AREA.CODE" = "water_area",
  "PRODUCTION_SOURCE_DET.CODE" = "prod_source", #CAPTURE, FRESHWATER, BRACKISHWATER, MARINE, AQUACULTURE
  # prod_proc, trad:
  "COMMODITY.FAO_CODE" = "commodity",
  "TRADE_FLOW.ALPHA_CODE" = "flow",
  "COUNTRY_REPORTER.UN_CODE" = "country_reporter",
  "COUNTRY_PARTNER.UN_CODE" = "country_partner",
  #items_aap:
  "3A_Code" = "species",
  "Taxonomic_Code" = "taxon_code",
  "Name_En" = "name",
  "Scientific_Name" = "name_scientific",
  "Major_Group" = "group",
  "ISSCAAP_Group_En" = "isscaap_name",
  "CPC_Class_En" = "cpc_name",
  #items_fc:
  "Code" = "commodity",
  "HS_2017" = "hs_code",
  "ISSCAAP" = "isscaap_code",
  "Name_En" = "name",
  "Observations" = "observ",
  "CPCv2.1" = "cpc_code"
)


# Load data --------------------------------------------------------------------

prod <- readRDS("input/fish/prod.rds")
prod <- dt_rename(prod, rename, drop = TRUE)
prod <- prod[value > 0]

prod_proc <- readRDS("input/fish/prod_proc.rds")
prod_proc <- dt_rename(prod_proc, rename, drop = TRUE)
prod_proc <- prod_proc[value > 0]

trad <- readRDS("input/fish/trad.rds")
trad <- dt_rename(trad, rename, drop = TRUE)
trad <- trad[value > 0]

trad_aggr <- readRDS("input/fish/trad_aggr.rds")
trad_aggr <- dt_rename(trad_aggr, rename, drop = TRUE)
trad_aggr <- trad_aggr[value > 0]

#aquatic animals and plants (metadata items for prod):
items_aap <- readRDS("input/fish/isscaap.rds")
items_aap <- dt_rename(items_aap, rename, drop = TRUE)

#fisheries commodities (metadata items for prod_proc, trad):
items_fc <- readRDS("input/fish/isscfc.rds")
items_fc <- dt_rename(items_fc, rename, drop = TRUE)
isscfc23 <- readRDS("input/fish/2023/isscfc23.rds")
isscfc23 <- dt_rename(isscfc23, rename, drop = TRUE)


##################
# PREP METADATA  # 
##################

# # Prep CPC metadata ------------------------------------------------------------
# 
# colnames(cpc) <- c("cpc_code", "cpc_name")
# cpc[cpc_name == "Cold-water shrimps and prawns (<i>Pandalus spp.</i>, <i>Crangon crangon</i>), live, fresh or chilled",
#     cpc_name := "Cold-water shrimps and prawns (Pandalus spp., Crangon crangon), live, fresh or chilled"]
# cpc[cpc_name == "Lobsters (<i>Homarus spp.</i>), live, fresh or chilled",
#     cpc_name := "Lobsters (Homarus spp.), live, fresh or chilled"]
# cpc[cpc_name == "Cuttle fish and squid, live, fresh or chilled",
#     cpc_name := "Cuttlefish and squid, live, fresh or chilled"]
# cpc[cpc_name == "Cuttle fish and squid, frozen, smoked, dried, salted or in brine",
#     cpc_name := "Cuttlefish and squid, frozen, smoked, dried, salted or in brine"]
# 
# cpc <- cpc[cpc_name %in% items_aap$cpc_name | cpc_code %in% items_fc$cpc_code]
# cpc <- cpc[order(cpc_code)]
# cpc <- cpc[!duplicated(cpc_code) & !duplicated(cpc_name)]
# 
# write.csv(cpc, "data/fish_cpc.csv", row.names = FALSE)
# 
# 
# # Prep ASFIS metadata ----------------------------------------------------------
# 
# colnames(asfis) <- c("isscaap_code", "taxon_code", "species", "name_scientific", "name", "family", "order", "fishstat")
# setcolorder(asfis,
#             c("species", "name", "name_scientific", "taxon_code", "family", "order"))
# 
# report_species <- unique(prod$species) #all species reported in prod(uction data)
# 
# asfis <- asfis[species %in% report_species]
# asfis <- asfis[, c("species", "family", "order")]
# 
# families <- unique(asfis$family)
# families <- families[order(families)]
# 
# 
# # Build metadata for primary production: items_aap -----------------------------
# 
# items_aap <- items_aap[species %in% report_species]
# # add ISSCAAP-code:
# items_aap <- merge(items_aap, isscaap, by = "isscaap_name", all.x = TRUE) # 202 rows have empty isscaap_name (all of group_major "AMPHIBIA, REPTILIA") => all.x = TRUE to keep them nonetheless
# # add CPC-code:
# items_aap <- merge(items_aap, cpc, by = "cpc_name", all.x = TRUE)
# # add families and order:
# items_aap <- merge(items_aap, asfis, by = "species")
# 
# items_aap <- items_aap[order(species)]
# setcolorder(items_aap,
#             c("species", "name", "name_scientific", "taxon_code", "family", "order", "isscaap_name", "isscaap_code", "cpc_name", "cpc_code", "group"))
# 
# write.csv(items_aap, "data/items_aap.csv", row.names = FALSE)
# 
# 
# # Build metadata for processed production and trade: items_fc ------------------
# 
# items_fc <- merge(items_fc, isscfc23[, c("commodity", "hs_code")], 
#                   by = "commodity", all.x = TRUE) #add HS17 codes from 2023 metadata to 2024 metadata
# 
# items_fc[cpc_code == 4920, cpc_code := 492]
# 
# report_commod <- unique(c(prod_proc$commodity, trad$commodity, trad_aggr$commodity)) #all commodities ever reported
# items_fc <- items_fc[commodity %in% report_commod]
# 
# sum(items_fc$observ)
# items_fc[, observ := NULL]
# setcolorder(items_fc, c("commodity", "cpc_code", "hs_code"))
# 
# items_fc[is.na(hs_code)]
# # => five commodities added (and reported) in 2024 with empty hs_code!! (because not present in 2023 data)
# # add missing HS manually:
# items_fc[is.na(hs_code), "hs_code"] <- c("0307.21", "0307.21", "0307.22", "0307.29", "0306.99")
# 
# write.csv(items_fc, "data/items_fc.csv", row.names = FALSE)



################
# ADD METADATA #
################

# CPC + ISSCAAP codes in prod/prod_proc/trad/trad_aggr -------------------------

prod <- merge(prod, items_fish[, .(species, isscaap_code = isscaap_division_code, isscaap_name = isscaap_division_name, 
                                   cpc_code = cpc_group_code, cpc_group = cpc_group_name)], by = "species", all.x = TRUE)
setcolorder(prod, c("country", "species", "isscaap_code", "isscaap_name", "cpc_code", "cpc_group"))

prod_proc <- merge(prod_proc, items_fc[, .(commodity, isscaap_code, cpc_code, hs_code)],
                   by = "commodity", all.x = TRUE)
prod_proc <- merge(prod_proc, items_fish[, .(isscaap_code = isscaap_, isscaap_division_code, isscaap_name = isscaap_division_name)],
                   by = "isscaap_code", all.x = TRUE)
prod_proc <- merge(prod_proc, items_fish[, .(cpc_code = cpc_group_code, cpc_group = cpc_group_name)],
                   by = "cpc_code", all.x = TRUE)

setcolorder(prod_proc, c("country", "commodity", "isscaap_code", "cpc_code",
                         "hs_code", "unit", "year", "value", "status"))

trad <- merge(trad, items_fc[, c("commodity", "isscaap_code", "cpc_code", "hs_code")],
              by = "commodity", all.x = TRUE)
setcolorder(trad, c("country_reporter", "country_partner", "flow", "commodity", "isscaap_code", "cpc_code", 
                    "hs_code", "unit", "year", "value", "status"))

trad_aggr <- merge(trad_aggr, items_fc[, c("commodity", "isscaap_code", "cpc_code", "hs_code")],
                   by = "commodity", all.x = TRUE) %>% rename("country" = "country_reporter")
setcolorder(trad_aggr, c("country", "flow", "commodity", "isscaap_code", "cpc_code",
                         "hs_code", "unit", "year", "value", "status"))


saveRDS(prod, "data/tidy/fish_prod.rds")
saveRDS(prod_proc, "data/tidy/fish_prod_proc.rds")
saveRDS(trad, "data/tidy/fish_trad.rds")
saveRDS(trad_aggr, "data/tidy/fish_trad_aggr.rds")



# CPC code overview ------------------------------------------------------------

cpc_summary <- copy(cpc) %>%
  mutate(in_prod_aap = cpc_code %in% prod$cpc_code) %>%
  mutate(in_prod_proc_fc = cpc_code %in% prod_proc$cpc_code) %>%
  mutate(in_trad_fc = cpc_code %in% trad$cpc_code) %>%
  mutate(in_trad_aggr_fc = cpc_code %in% trad_aggr$cpc_code)

write.csv(cpc_summary, "data/cpc_summary_aap_fc.csv", row.names = FALSE)





# Aggregate prod by isscaap codes
prod_agg <- prod[, .(value = na_sum(value)), 
                 by = .(country, year, unit, isscaap_code, prod_source)]


prod_proc_agg <- prod_proc[, .(value = na_sum(value)), 
                           by = .(country, year, unit, isscaap_code)]




# prep prod --------------------------------------------------------------------

cpc_in_prod <- unique(prod$cpc_code) #for prod_proc, see below

prod <- prod[!is.na(cpc_code) & !cpc_code %in% c(491, 492)] %>%   # remove groups "MAMMALIA" and "AMPHIBIA, REPTILIA", and corals and sponges
  mutate(cpc_isscaap = if_else(cpc_code < 430, #add column with either isscaap for finfish...
                               isscaap_code,
                               cpc_code)) %>%  #... or cpc for non-fish species
  mutate(cpc_isscaap = if_else(cpc_isscaap %in% c(451, 452, 453),  #add Sea cucumbers, Sea urchins, and Jellyfish...
                               459,  #...to Other aquatic invertebrates
                               cpc_isscaap)) %>%
  mutate(cpc_isscaap = if_else(prod_source == "CAPTURE",
                               paste0(cpc_isscaap, "c"),
                               paste0(cpc_isscaap, "a"))) %>%
  mutate(cpc_isscaap = case_when(
    cpc_isscaap == "24a" ~ "25a",  #little reporting of 24a (Shads AquaC) => add to 25a (Misc diadromous fish AquaC)
    cpc_isscaap == "32a" & country == "352" ~ "32c", #Iceland reported once 4 tons of farmed cod - add to caught cod
    cpc_isscaap == "32a" & country == "578" ~ "39a", #Norway reports moderate amounts of cod farming - add to 39a (Marine fish AquaC)
    cpc_isscaap == "34a" ~ "39a",  #dataset contains only six entries for 34a (Misc demersal fish AquaC) => add to 39a (Marine fish AquaC)
    cpc_isscaap == "35a" ~ "35c",  #remove herrings/sardines/anchovies from aquaculture (only one entry) 
    TRUE ~ cpc_isscaap))

prod <- prod[!cpc_isscaap %in% c("447a"),   #remove octopus from aquaculture (only non-commercial experiments in Spain)
             .(sum(value)), by = c("country", "cpc_isscaap", "year")] %>%
  rename(value = V1)


# prep prod_proc ---------------------------------------------------------------

prod_proc <- prod_proc[!cpc_code %in% c(cpc_in_prod, "21526")] %>%   #remove all primary production, and marine mammal fats and oils
  mutate(cpc_code = fish_fillet_meat(cpc_code)) %>%
  mutate(cpc_isscaap = paste(cpc_code, isscaap_code, sep = "-")) %>%   #add column with cpc-isscaap-pairs
  mutate(cpc_isscaap = cpc_isscaap_comb(cpc_isscaap)) %>%
  mutate(cpc_isscaap = final_aggr(cpc_isscaap))

prod_proc <- prod_proc[, .(sum(value)), by = c("country", "cpc_isscaap", "year")] %>%
  rename(value = V1)





# save -------------------------------------------------------------------------

saveRDS(prod, "data/tidy/prod_final.rds")
saveRDS(prod_proc, "data/tidy/prod_proc_final.rds")
saveRDS(trad, "data/tidy/trad_final.rds")
saveRDS(trad_aggr, "data/tidy/trad_aggr_final.rds")
