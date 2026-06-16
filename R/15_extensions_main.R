library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

# Prepping E_labels --------------
# Check that the same files are present in sua and cbs folders

nms_cbs    <- gsub(".rds", "", list.files("data/extensions/cbs",    pattern = "\\.rds$"))
nms_sua    <- gsub(".rds", "", list.files("data/extensions/sua",    pattern = "\\.rds$"))
nms_fd_cbs <- gsub(".rds", "", list.files("data/extensions/fd_cbs", pattern = "\\.rds$"))
nms_fd_sua <- gsub(".rds", "", list.files("data/extensions/fd_sua", pattern = "\\.rds$"))

# CBS-only stressors -----------------------------------------------------------
# Value-added (and any other monetary CBS-level) extensions live only in the
# v2/CBS tree: they have no meaningful SUA disaggregation, so no
# data/extensions/sua/ counterpart is produced (see 14_5_value_added_extensions.R).
# The cbs<->sua parity check below tolerates EXACTLY these names as cbs-only,
# and the SUA / v2_525 E is compiled WITHOUT them.  Keep this list in sync with
# 14_5_value_added_extensions.R and with inst/E_labels_initial.csv.
#
# TWELVE names: the three VA strands x two ISIC levels, once per base (GLORIA
# and EXIOBASE).  The _gloria and _exiobase rows are ALTERNATIVE estimates of
# the same quantity (different upstream MRIO base) — they are not additive.
cbs_only <- c(
  "VA_wages_isic_a_gloria",   "VA_capital_isic_a_gloria",   "VA_tls_isic_a_gloria",
  "VA_wages_isic_c_gloria",   "VA_capital_isic_c_gloria",   "VA_tls_isic_c_gloria",
  "VA_wages_isic_a_exiobase", "VA_capital_isic_a_exiobase", "VA_tls_isic_a_exiobase",
  "VA_wages_isic_c_exiobase", "VA_capital_isic_c_exiobase", "VA_tls_isic_c_exiobase")

# Parity check: the cbs side may carry the cbs_only extras and nothing else; the
# fd pair must still match exactly (VA extensions are production-side, not fd).
pairs <- list(
  list(a = setdiff(nms_cbs, cbs_only), b = nms_sua,    name_a = "cbs",    name_b = "sua"),
  list(a = nms_fd_cbs,                 b = nms_fd_sua, name_a = "fd_cbs", name_b = "fd_sua")
)

msg <- ""
for (p in pairs) {
  if (!identical(p$a, p$b)) {
    only_in_a <- setdiff(p$a, p$b)
    only_in_b <- setdiff(p$b, p$a)
    msg <- paste0(msg, "File lists are NOT identical between '", p$name_a, "' and '", p$name_b, "':\n")
    if (length(only_in_a) > 0) msg <- paste0(msg,"  Only in ", p$name_a, ": ", paste(only_in_a, collapse = ", "), "\n")
    if (length(only_in_b) > 0) msg <- paste0(msg,"  Only in ", p$name_b, ": ", paste(only_in_b, collapse = ", "), "\n")
  }
}
if (nchar(msg) > 0) stop(msg) else message("All file lists are identical within pairs (cbs-only extras allowed).")

# Verify every cbs_only name is actually present on the cbs side (catches a
# rename / missing build before it silently drops out of E).
missing_cbs_only <- setdiff(cbs_only, nms_cbs)
if (length(missing_cbs_only) > 0)
  stop("cbs_only stressor(s) declared but absent from data/extensions/cbs: ",
       paste(missing_cbs_only, collapse = ", "),
       ".\nRun R/14_5_value_added_extensions.R first.")

nms_cbs_all <- copy(nms_cbs)    # CBS stressor set, INCLUDING value-added
nms     <- copy(nms_sua)        # SUA stressor set, EXCLUDING value-added
nms_fd <- copy(nms_fd_sua)
rm(nms_sua, nms_fd_cbs, nms_fd_sua, p, pairs, msg)

# E_label checks -----------
E_labels <- fread("inst/E_labels_initial.csv")
E_fd_labels <- fread("inst/E_fd_labels_initial.csv")

# check if E_labels is fully updated -- every file (on EITHER side) needs a row.
all_stressors <- union(nms_cbs_all, nms)
missing_labels <- setdiff(all_stressors, E_labels$Stressor)
missing_labels_fd <- setdiff(nms_fd, E_fd_labels$Stressor)

if (length(missing_labels) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels,    collapse = ", "), " is present in extension folder but not in E_labels")
if (length(missing_labels_fd) > 0) stop("E_labels needs to be updated -> ", paste(missing_labels_fd, collapse = ", "), " is present in fd extension folder but not in E_labels")

# Two label tables: CBS (incl. value-added) for the v2 tree, SUA for v2_525.
# Each is filtered and ordered to match its E's row order (= the file order
# returned by list.files for that folder).
E_labels_cbs <- E_labels[Stressor %in% nms_cbs_all][order(match(Stressor, nms_cbs_all)), ]
E_labels_sua <- E_labels[Stressor %in% nms][order(match(Stressor, nms)), ]
E_fd_labels  <- E_fd_labels[Stressor %in% nms_fd][order(match(Stressor, nms_fd)), ]

# check that the ordering worked
if (!all(E_labels_cbs$Stressor == nms_cbs_all)) stop("Re-do column ordering for E_labels_cbs to match cbs files")
if (!all(E_labels_sua$Stressor == nms))         stop("Re-do column ordering for E_labels_sua to match sua files")
if (!all(E_fd_labels$Stressor == nms_fd))       stop("Re-do column ordering for E_fd_labels to match files")

rm(missing_labels, missing_labels_fd)

# Compile all extensions -----------------
files_cbs <- list.files("data/extensions/cbs", pattern = "\\.rds$", full.names = TRUE)
data_cbs <- lapply(files_cbs, readRDS)

files_fd_cbs <- list.files("data/extensions/fd_cbs", pattern = "\\.rds$", full.names = TRUE)
data_fd_cbs <- lapply(files_fd_cbs, readRDS)

files_sua <- list.files("data/extensions/sua", pattern = "\\.rds$", full.names = TRUE)
data_sua <- lapply(files_sua, readRDS)

files_fd_sua <- list.files("data/extensions/fd_sua", pattern = "\\.rds$", full.names = TRUE)
data_fd_sua <- lapply(files_fd_sua, readRDS)

# Combine.  E_cbs carries the value-added rows; E_sua does not.
E_cbs <- compile_extension(data_cbs, files_cbs)
E_fd_cbs <- compile_extension(data_fd_cbs, files_fd_cbs)

E_sua <- compile_extension(data_sua, files_sua)
E_fd_sua <- compile_extension(data_fd_sua, files_fd_sua)

# Final guard: each compiled E's row order must equal its label table.
if (!all(rownames(E_cbs[[as.character(years[1])]]) == E_labels_cbs$Stressor))
  stop("E_cbs row order does not match E_labels_cbs.")
if (!all(rownames(E_sua[[as.character(years[1])]]) == E_labels_sua$Stressor))
  stop("E_sua row order does not match E_labels_sua.")

# save -- v2 (CBS) gets the value-added stressors; v2_525 (SUA) does not.
saveRDS(E_cbs, paste0(output_dir, "E.rds"))
saveRDS(E_fd_cbs, paste0(output_dir, "E_fd.rds"))

saveRDS(E_sua, paste0(output_dir_v525, "E.rds"))
saveRDS(E_fd_sua, paste0(output_dir_v525, "E_fd.rds"))

fwrite(E_labels_cbs, paste0(output_dir, "ex_labels.csv"))
fwrite(E_fd_labels,  paste0(output_dir, "ex_fd_labels.csv"))

fwrite(E_labels_sua, paste0(output_dir_v525, "ex_labels.csv"))
fwrite(E_fd_labels,  paste0(output_dir_v525, "ex_fd_labels.csv"))