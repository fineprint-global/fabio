# This script downloads, aggregates and tidies Gridded Livestock of the World v4

library("tidyverse")
library("data.table")

items <- fread("inst/items_full_123.csv")


# add stocks from GLW -----------
# Goal: get per capita values per feedtype, multiply with faostat stocks (they differ from GLW)
## Download and aggregate GLW data -----
# Download settings 
base            <- "https://dataverse.harvard.edu"
dataverse_alias <- "glw_4"
out_root        <- "input/GLW"
overwrite       <- FALSE   # TRUE to re-download files that already exist

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Helper: GET + parse JSON 
get_json <- function(url, query = NULL) {
  r <- GET(url, query = query, timeout(120))
  stop_for_status(r)
  fromJSON(content(r, "text", encoding = "UTF-8"), simplifyVector = FALSE)$data
}

# Layer 1: list the datasets inside the dataverse 
message("Listing datasets in dataverse '", dataverse_alias, "' ...")
datasets <- get_json(paste0(base, "/api/dataverses/", dataverse_alias, "/contents"))
message("Found ", length(datasets), " datasets.\n")

# Helper: derive a clean folder name from a dataset title 
species_from_title <- function(title) {
  sp <- sub("^Global\\s+(.*?)\\s+distribution.*$", "\\1",
            title, ignore.case = TRUE)
  if (identical(sp, title)) sp <- title            # fallback if no match
  sp <- tolower(trimws(sp))
  gsub("[^a-z0-9]+", "_", sp)                      # filesystem-safe
}

# Layer 2: for each dataset, list and download its files 
for (ds in datasets) {
  pid <- paste0(ds$protocol, ":", ds$authority, "/", ds$identifier)
  
  meta   <- get_json(paste0(base, "/api/datasets/:persistentId/"),
                     query = list(persistentId = pid))
  ver    <- meta$latestVersion
  fields <- ver$metadataBlocks$citation$fields
  title  <- Find(function(f) identical(f$typeName, "title"), fields)$value
  
  species <- species_from_title(title)
  dest_dir <- file.path(out_root, species)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  files <- ver$files
  message(sprintf("[%s]  %s  (%d files)", species, title, length(files)))
  
  for (f in files) {
    fname <- if (!is.null(f$dataFile$filename)) f$dataFile$filename else f$label
    fid   <- f$dataFile$id
    dest  <- file.path(dest_dir, fname)
    
    if (file.exists(dest) && !overwrite) {
      message("   skip (exists): ", fname)
      next
    }
    
    message("   downloading: ", fname)
    resp <- GET(paste0(base, "/api/access/datafile/", fid),
                query = list(format = "original"),
                write_disk(dest, overwrite = TRUE),
                timeout(600))
    if (http_error(resp)) {
      warning("   FAILED (", status_code(resp), "): ", fname)
      if (file.exists(dest)) file.remove(dest)
    }
  }
}

message("\nDone. Files saved under: ", normalizePath(out_root))


## Aggregate GLW by country ---------------
# Settings 
in_root  <- "input/GLW"          # where the per-species folders live
method   <- "Da"                 # "Da" (dasymetric, standard) or "Aw" (areal-weighted) -> no difference at national level
regions_current <- fread("inst/regions_full.csv")[current == TRUE]

# Country polygons from terra
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries <- countries[, c("iso_a3_eh", "admin")]
names(countries)[1:2] <- c("iso_a3", "country")

#  Loop over species folders 
species_dirs <- list.dirs(in_root, recursive = FALSE)
stopifnot(length(species_dirs) > 0)

results <- list()

for (sp_dir in species_dirs) {
  species <- basename(sp_dir)
  
  tifs <- list.files(sp_dir, pattern = "\\.tif$",
                     full.names = TRUE, ignore.case = TRUE)
  if (length(tifs) == 0) {
    message("No .tif in ", sp_dir, " - skipping.")
    next
  }
  
  # Prefer the requested method (Da/Aw); fall back to the first tif found.
  pick <- grep(paste0("_", method, "\\.tif$"), tifs,
               ignore.case = TRUE, value = TRUE)
  tif  <- if (length(pick) > 0) pick[1] else tifs[1]
  
  message(sprintf("[%s] %s", species, basename(tif)))
  
  r <- rast(tif)
  r <- ifel(r < 0, NA, r)                       # drop NoData (-9999) if present
  polys <- st_transform(countries, crs(r))
  
  # exact_extract fractionally weights border pixels by coverage, so animals
  # in a boundary pixel are split between countries instead of double-counted.
  polys[[species]] <- exact_extract(r, polys, "sum", progress = FALSE)
  
  results[[species]] <- st_drop_geometry(polys[, c("iso_a3", "country", species)])
}

#  Combine into one wide table (country x species) 
country_totals_glw <- Reduce(function(a, b) full_join(a, b, by = c("iso_a3", "country")),
                             results)

country_totals_glw <- melt(as.data.table(country_totals_glw),
                           id.vars = c("iso_a3", "country"),
                           variable.name = "species",
                           value.name = "stocks")

# round to whole animals and order by cattle if present
country_totals_glw[, stocks := round(stocks)]

# Aggregate RoW
country_totals_glw[, iso3c := regions_current$iso3c[match(iso_a3, regions_current$iso3c)]]
country_totals_glw[is.na(iso3c), `:=` (iso3c = "ROW", country = "RoW")]
country_totals_glw[iso3c == "AUS", `:=` (country = "Australia")]
country_totals_glw <- country_totals_glw[ , .(stocks = sum(stocks, na.rm = TRUE)),
                                          by = .(iso3c, country, species)]

saveRDS(country_totals_glw, "data/tidy/glw_tidy.rds")

# tidy
rm( countries, datasets, ds, f, fields, files, meta, polys,
    r,  results, ver, country_totals_glw, regions_current)