
# Trade -------------------------------------------------------------------

library("data.table")
# library("comtradr")
source("R/00_prep_functions.R")
path_trade <- "input/trade/"


# BACI92 ------------------------------------------------------------------

file <- c("baci_full" = "BACI_HS92_V202501.zip")
name <- names(file)
link <- "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/"

# download file
options(timeout = max(600, getOption("timeout")))
fa_dl(file = file, link = link, path = path_trade)

pattern <- "(BACI_HS92_Y[0-9]{4}_V202501)([.]csv)"

# unzip data files
extr <- unzip(paste0(path_trade, file), list = TRUE)[[1]]
extr <- extr[grep(pattern, extr)]
col_types <- rep(list(c("integer", "integer", "integer", "integer",
                           "numeric", "numeric")), length(extr))
baci_full <- fa_extract(path_in = path_trade, files = file, path_out = path_trade,
  name = name, extr = extr, col_types = col_types, stack = TRUE)

# unzip auxiliary files
extr <- unzip(paste0(path_trade, file), list = TRUE)[[1]]
extr <- extr[-grep(pattern, extr)]
baci_full <- fa_extract(path_in = path_trade, files = file, path_out = path_trade,
                        name = name, extr = extr, stack = FALSE)

# select fish (30___), fish meal (230120), fish oil (1504__), inedible fish products (51191), 
# fish preparations (1604__), mollusc preparations (1605__), and ethanol (2207__) data
baci_full <- readRDS("input/trade/baci_full.rds")
baci_sel <- baci_full[grep("^(30[1-9]..|2207..|1504..|230120|51191|1604..|1605..)", baci_full$k), ]
saveRDS(baci_sel, paste0(path_trade, "baci_sel.rds"))

