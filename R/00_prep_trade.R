
# Trade -------------------------------------------------------------------

library("data.table")
library("comtradr")
source("R/00_prep_functions.R")
path_trade <- "input/trade/"


# BACI92 ------------------------------------------------------------------

#file <- c("baci_full" = "BACI_HS92_V202001.zip")
file <- c("baci_full" = "BACI_HS92_V202201.zip")

pattern <- "(BACI_HS92_Y[0-9]+)([.]csv)"
pattern <- "(BACI_HS92_Y[0-9]{4}_V202201)([.]csv)"


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


# Comtrade ----------------------------------------------------------------

# Register token - look in "~/.Renviron"
COMTRADE_TOKEN <- Sys.getenv("COMTRADE_TOKEN")
if(nchar(COMTRADE_TOKEN) == 0) {
  stop("No 'COMTRADE_TOKEN' found in the environment. ",
    "Please provide a token to use the Comtrade API.")
} else {ct_register_token(COMTRADE_TOKEN)}

# Loop over possible reporters to circumvent API restrictions
reporters <- readLines("inst/comtrade_reporters.txt", encoding = "UTF-8")
comtrade <- vector("list", ceiling(length(reporters) / 5))
j <- 1
for(i in seq(1, length(reporters), by = 5)) {
  cat("Downloading Comtrade data:", i, "of", length(reporters), "\n")
  comtrade[[j]] <- ct_search(
    reporters = reporters[i:min(c(i + 4, length(reporters)))],
    partners = "All",
    trade_direction = "All",
    start_date = "1986-01-01",
    end_date = "1990-12-31",
    commod_codes = c("0301", "0302", "0303", "0304", "0305", "2207")
  )
  comtrade[[j]] <- rbind(
    comtrade[[j]],
    ct_search(
      reporters = reporters[i:min(c(i + 4, length(reporters)))],
      partners = "All",
      trade_direction = "All",
      start_date = "1991-01-01",
      end_date = "1994-12-31",
      commod_codes = c("0301", "0302", "0303", "0304", "0305", "2207")
    )
  )
  j <- j + 1
}
comtrade <- data.table::rbindlist(comtrade)
saveRDS(comtrade, paste0(path_trade, "comtrade.rds"))
