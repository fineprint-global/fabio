library(data.table)
library(tidyverse)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# set input path
path_geo <- "/mnt/nfs_fineprint/tmp/geo_data/"

# Match IDs from soil dataset to soil types (~7hrs)---------------------
# Here, the IDs given in the HWSD2 raster are matched to their datapoints and
# aggregated to the same level as the NPKgrids datesets.

r_soils <- rast(paste0(path_geo, "HWSD2/HWSD2.bil"))
lookup <- fread(paste0(path_geo, "HWSD2/HWSD2_SMU.csv"))
lookup <- lookup[, .(id = as.numeric(HWSD2_SMU_ID), soil_type = as.numeric(WRB2_CODE))]

# # test on smaller set of data 
# ext_austria <- ext(9.5, 17, 46.3, 49.1)

# r_small <- crop(r_soils, ext_austria)

# create temp folder
dir.create("temp")

# set up temporary memory for chunking data
terraOptions(tempdir = "temp", progress = 1)

# Apply substitution and force writing to disk -> this makes it safer as the
# chunks are not saved in memory which may be corrupted
r_soils <- subst(
   r_soils,
   from = lookup$id,
   to = lookup$soil_type,
   filename = "data/NPK/HWSD2_soiltype.tif",
   overwrite = TRUE,
   wopt = list(datatype = "INT2S", gdal = c("COMPRESS=LZW"))
 )

# delete temp folder
unlink("temp", recursive = TRUE)

# Aggregate to same level as NPK data, using mode of aggregated cells
r_soils <- aggregate(r_soils, fact = 5, fun = "modal")

# # Force flush to disk to avoid dangling temp files
terra::writeRaster(r_soils, filename = "data/NPK/HWSD2_soiltype.tif", overwrite = TRUE)

 
# Create country mapping -------------------------
# Here the countries from the NPK datasets are mapped to the country borders from
# the rnaturalearth package using random sampling. This creates a country mapping
# that enables the use of original country borders when aggregating the NPK dataset

countries_NPK <- rast(paste0(path_geo, "Countries_2018.nc"))
countries_dt <- as.data.table(countries_NPK, xy = TRUE)
country_borders <- ne_countries(scale = "medium", returnclass = "sf")[, c("iso_a3_eh", "geometry")]
 
# get 1000 random points from the NPK country file
set.seed(90)
sampled_points <- countries_dt[!is.na(country), .SD[sample(.N, min(1000, .N))], by = country]
sampled_points_sf <- st_as_sf(sampled_points, coords = c("x", "y"), crs = 4326)
 
# perform a spatial join between the random points and borders
joined_sf <- st_join(sampled_points_sf, country_borders, join = st_intersects)
joined_dt <- as.data.table(joined_sf)
 
# count majority matches
match_counts <- joined_dt[, .N, by = .(country, iso_a3_eh)]
 
# map countries to points (using majority of points in case of mismatch)
country_mapping <- match_counts[order(-N), .SD[1], by = country][, N := NULL]
setorder(country_mapping, country, iso_a3_eh)
saveRDS(country_mapping, "data/NPK/country_mapping.rds")

rm(list = ls())
gc()
