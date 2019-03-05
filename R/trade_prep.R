
# Trade -------------------------------------------------------------------

source("R/prep.R")
path <- "input/trade/"
library(comtradr)
ct_register_token(Sys.getenv("COMTRADE_TOKEN"))


# BACI92 ------------------------------------------------------------------

file <- c("baci_full" = "baci92_trade.zip")

pattern <- "(baci92_[0-9]+)([.]csv)"

extr <- unzip(paste0(path, file), list = TRUE)[[1]]
extr <- extr[grep(pattern, extr)]

# name <- gsub(pattern, "\\1", extr)
name <- names(file)

col_types <- rep("iiiidd", length(extr))

baci_full <- fa_extract(file, path, name,
                         extr = extr, col_types = col_types, stack = TRUE)

baci_sel <- readRDS(baci_full)
# Select fish (30___) and ethanol (2207_)
baci_sel <- baci_sel[grep("^(30[1-4]..|2207.)", baci_sel$hs6), ]
saveRDS(baci_sel, paste0(path, "baci_sel.rds"))


# Comtrade ----------------------------------------------------------------

# Loop over possible reporters to circumvent API restrictions
reporters <- readLines(paste0(path, "comtrade_reporters.txt"))
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
    commod_codes = c("0301", "0302", "0303", "0304", "2207")
  )
  comtrade[[j]] <- rbind(
    comtrade[[j]],
    ct_search(
      reporters = reporters[i:min(c(i + 4, length(reporters)))],
      partners = "All",
      trade_direction = "All",
      start_date = "1991-01-01",
      end_date = "1994-12-31",
      commod_codes = c("0301", "0302", "0303", "0304", "2207")
    )
  )
  j <- j + 1
}
comtrade <- data.table::rbindlist(comtrade)
saveRDS(comtrade, paste0(path, "comtrade.rds"))
