
# Tidying -----------------------------------------------------------------

library(data.table) # 1.12.0
regions <- fread("inst/regions_full.csv", encoding = "UTF-8")
years <- 1986:2013


# CBS --------------------------------------------------------------------

cbs_crop <- readRDS("input/fao/cbs_crop.rds")
cbs_live <- readRDS("input/fao/cbs_live.rds")
cbs_raw <- rbind(cbs_crop, cbs_live)
rm(cbs_crop, cbs_live)

# Remove country groups (>= 5000)
cbs_raw <- cbs_raw[cbs_raw[[1]] < 5000, ]
# Widen the data
cbs <- dcast(cbs_raw,
             `Area Code` + Area + `Item Code` + Item + Year ~ Element,
             value.var = "Value")

cat("Setting NA values in the cbs object to 0\n")
replace_dt(cbs, 0)

# Rename the columns
rename <- c(
  "Area Code" = "area_code",
  "Area" = "area",
  "Item Code" = "item_code",
  "Item" = "item",
  "Year" = "year",
  "Production" = "production",
  "Import Quantity" = "imports",
  "Export Quantity" = "exports",
  "Domestic supply quantity" = "total_supply",
  "Losses" = "waste",
  "Food supply quantity (tonnes)" = "food",
  "Stock Variation" = "stock_withdrawal",
  "Feed" = "feed",
  "Seed" = "seed",
  "Other uses" = "other",
  "Processing" = "processing"
)
names(cbs) <- c(rename[names(cbs)])

# Change the supply element from "prod + imp - exp" to "prod + imp"
all.equal(cbs$total_supply, cbs$production + cbs$imports - cbs$exports)
cbs$total_supply <- cbs$production + cbs$imports

# Adjust stock variation from representing stock-withdrawals to stock-additions
cbs$stock_var <- -cbs$stock_withdrawal

# Cap out stock-additions to total supply
sum(cbs$stock_var > cbs$total_supply)
cbs$stock_var <- ifelse(cbs$stock_var > cbs$total_supply,
                        cbs$total_supply, cbs$stock_var)

# Set negative values of variables other than stock-changes to 0
for(i in c("production", "imports", "exports", "total_supply", "waste",
             "food", "feed", "seed", "other", "processing"))
  set(cbs, which(cbs[[i]] < 0), i, 0)

# Take supply and stock-changes as given and rebalance uses
uses <- cbs[, c("exports", "food", "feed", "seed", "waste", "processing", "other")]
cbs[, c("exports", "food", "feed", "seed", "waste", "processing", "other")] <-
  uses / rowSums(uses) * (cbs$total_supply - cbs$stock_var)
cat("Setting NaN values in the cbs object to 0\n")
replace_dt(cbs, 0, is.nan)

# Match country names
cbs$area <- regions$name[match(cbs$area_code, regions$code)]

# Merge variations of countries
# merge_areas(cbs, orig = 206, dest = 276, "Sudan")
# merge_areas(cbs, orig = 62, dest = 238, "Ethiopia")

# Detect missing years for countries
missing <- data.frame(years = years)
for(code in unique(cbs$area_code)) {
  missing_years <- which(!years %in% unique(cbs[area_code == code]$year))
  if(length(missing_years > 0))
    missing[missing_years, as.character(code)] <- TRUE
}
cat("Missing years found for the following regions:\n",
    paste(regions$name[match(names(missing)[-1], regions$code)],
          colSums(missing[, -1], na.rm = TRUE), collapse = ", "),
    "\n")
cat("Missing regions found for the following years:\n",
    paste(years,
          rowSums(missing[, -1], na.rm = TRUE), collapse = ", "),
    "\n")

# If they are missing for >= 6 years do not estimate via extrapolation

# Fill missing years
# possibly use MA or RW?
for(applicable_region) {
  # Years are either missing towards the end or towards the start
  if(min(missing_year) == min(years))
    use_next_available
  else if(max(missing_year) == max(years))
    use_last_available
  # Included for the sake of completeness
  else
    use_average
}

