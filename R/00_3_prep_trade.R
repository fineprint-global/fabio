
# Trade -------------------------------------------------------------------

library("data.table")
# library("comtradr")
source("R/00_prep_functions.R")
path_trade <- "input/trade/"


# BACI92 ------------------------------------------------------------------

file <- c("baci_full" = "BACI_HS92_V202301.zip")

pattern <- "(BACI_HS92_Y[0-9]+)([.]csv)"
pattern <- "(BACI_HS92_Y[0-9]{4}_V202301)([.]csv)"


extr <- unzip(paste0(path_trade, file), list = TRUE)[[1]]
extr <- extr[grep(pattern, extr)]

# name <- gsub(pattern, "\\1", extr)
name <- names(file)

col_types <- rep(list(c("integer", "integer", "integer", "integer",
                           "numeric", "numeric")), length(extr))

baci_full <- fa_extract(path_in = path_trade, files = file, path_out = path_trade,
  name = name, extr = extr, col_types = col_types, stack = TRUE)

baci_sel <- readRDS(baci_full)
# Select fish (30___) and ethanol (2207__)
baci_sel <- baci_sel[grep("^(30[1-5]..|2207..)", baci_sel$k), ]
saveRDS(baci_sel, paste0(path_trade, "baci_sel.rds"))

