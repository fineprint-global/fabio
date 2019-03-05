
library(data.table) # 1.12.0
regions <- fread("inst/regions_full.csv", encoding = "UTF-8")
years <- 1986:2013

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
merge_areas(cbs, orig = 206, dest = 276, "Sudan")
merge_areas(cbs, orig = 62, dest = 238, "Ethiopia")


