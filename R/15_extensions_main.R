library(data.table)
library(tidyverse)
source("R/00_system_variables.R")
source("R/01_tidy_functions.R")
source("R/00_prep_functions.R")

# Prepping E_labels ------------------------------------------------------------
# The cbs and sua extension folders carry the same stressor set, so both trees
# (v2 CBS and v2_525 SUA) share one label ordering.

nms_cbs    <- gsub(".rds", "", list.files("data/extensions/cbs",    pattern = "\\.rds$"))
nms_sua    <- gsub(".rds", "", list.files("data/extensions/sua",    pattern = "\\.rds$"))
nms_fd_cbs <- gsub(".rds", "", list.files("data/extensions/fd_cbs", pattern = "\\.rds$"))
nms_fd_sua <- gsub(".rds", "", list.files("data/extensions/fd_sua", pattern = "\\.rds$"))

# Parity check: 
# sua (and fd_sua) must be a subset of cbs (and fd_cbs), since cbs can carry
# additional stressors (e.g. nms_cbs length 135 vs nms_sua length 123).
pairs <- list(
  list(a = nms_cbs,    b = nms_sua,    name_a = "cbs",    name_b = "sua"),
  list(a = nms_fd_cbs, b = nms_fd_sua, name_a = "fd_cbs", name_b = "fd_sua")
)


msg <- ""
for (p in pairs) {
  only_in_b <- setdiff(p$b, p$a)
  if (length(only_in_b) > 0) {
    msg <- paste0(msg, "'", p$name_b, "' contains stressors not present in '", p$name_a, "':\n")
    msg <- paste0(msg, "  Only in ", p$name_b, ": ", paste(only_in_b, collapse = ", "), "\n")
  }
}
if (nchar(msg) > 0) stop(msg) else message("All ", "'b' file lists are subsets of their 'a' counterparts.")

# nms_cbs and nms_sua (and the fd equivalents) are now tracked separately,
# since sua can have fewer stressors than cbs.
nms_cbs    <- copy(nms_cbs)     # cbs-side stressor set (superset)
nms_sua    <- copy(nms_sua)     # sua-side stressor set (subset of cbs)
nms_fd_cbs <- copy(nms_fd_cbs)  # fd_cbs-side stressor set (superset)
nms_fd_sua <- copy(nms_fd_sua)  # fd_sua-side stressor set (subset of fd_cbs)
rm(p, pairs, msg)


# E_label checks ---------------------------------------------------------------
E_labels    <- fread("inst/E_labels_initial.csv")
E_fd_labels <- fread("inst/E_fd_labels_initial.csv")

# Every file (in either tree) needs a label row; check cbs and sua separately.
missing_labels_cbs    <- setdiff(nms_cbs,    E_labels$Stressor)
missing_labels_sua    <- setdiff(nms_sua,    E_labels$Stressor)
missing_labels_fd_cbs <- setdiff(nms_fd_cbs, E_fd_labels$Stressor)
missing_labels_fd_sua <- setdiff(nms_fd_sua, E_fd_labels$Stressor)
if (length(missing_labels_cbs) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels_cbs,    collapse = ", "), " is present in cbs extension folder but not in E_labels")
if (length(missing_labels_sua) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels_sua,    collapse = ", "), " is present in sua extension folder but not in E_labels")
if (length(missing_labels_fd_cbs) > 0) stop("E_labels needs to be updated -> ", paste(missing_labels_fd_cbs, collapse = ", "), " is present in fd_cbs extension folder but not in E_labels")
if (length(missing_labels_fd_sua) > 0) stop("E_labels needs to be updated -> ", paste(missing_labels_fd_sua, collapse = ", "), " is present in fd_sua extension folder but not in E_labels")

# One label table per tree (cbs / sua), each filtered & ordered to match that
# tree's own file order, since the two trees can differ in length.
E_labels_cbs    <- E_labels[Stressor %in% nms_cbs][order(match(Stressor, nms_cbs)), ]
E_labels_sua    <- E_labels[Stressor %in% nms_sua][order(match(Stressor, nms_sua)), ]
E_fd_labels_cbs <- E_fd_labels[Stressor %in% nms_fd_cbs][order(match(Stressor, nms_fd_cbs)), ]
E_fd_labels_sua <- E_fd_labels[Stressor %in% nms_fd_sua][order(match(Stressor, nms_fd_sua)), ]
if (!all(E_labels_cbs$Stressor == nms_cbs))       stop("Re-do column ordering for E_labels_cbs to match files")
if (!all(E_labels_sua$Stressor == nms_sua))       stop("Re-do column ordering for E_labels_sua to match files")
if (!all(E_fd_labels_cbs$Stressor == nms_fd_cbs)) stop("Re-do column ordering for E_fd_labels_cbs to match files")
if (!all(E_fd_labels_sua$Stressor == nms_fd_sua)) stop("Re-do column ordering for E_fd_labels_sua to match files")
rm(missing_labels_cbs, missing_labels_sua, missing_labels_fd_cbs, missing_labels_fd_sua)

# Compile all extensions -------------------------------------------------------
files_cbs    <- list.files("data/extensions/cbs",    pattern = "\\.rds$", full.names = TRUE)
files_fd_cbs <- list.files("data/extensions/fd_cbs", pattern = "\\.rds$", full.names = TRUE)
files_sua    <- list.files("data/extensions/sua",    pattern = "\\.rds$", full.names = TRUE)
files_fd_sua <- list.files("data/extensions/fd_sua", pattern = "\\.rds$", full.names = TRUE)

E_cbs    <- compile_extension(lapply(files_cbs,    readRDS), files_cbs)
E_fd_cbs <- compile_extension(lapply(files_fd_cbs, readRDS), files_fd_cbs)
E_sua    <- compile_extension(lapply(files_sua,    readRDS), files_sua)
E_fd_sua <- compile_extension(lapply(files_fd_sua, readRDS), files_fd_sua)

# Row order guard: each compiled tree's row order must equal its own label
# order (cbs against E_labels_cbs, sua against E_labels_sua), since the two
# trees can have different numbers/sets of rows.
if (!all(rownames(E_cbs[[as.character(years[1])]]) == E_labels_cbs$Stressor))
  stop("E_cbs row order does not match E_labels_cbs.")
if (!all(rownames(E_sua[[as.character(years[1])]]) == E_labels_sua$Stressor))
  stop("E_sua row order does not match E_labels_sua.")

# save BAMBOO version ----------------------------------------------------------
saveRDS(E_cbs,    paste0(output_dir, "E_bamboo.rds"))
saveRDS(E_fd_cbs, paste0(output_dir, "E_bamboo_fd.rds"))
saveRDS(E_sua,    paste0(output_dir_v525, "E_bamboo.rds"))
saveRDS(E_fd_sua, paste0(output_dir_v525, "E_bamboo_fd.rds"))

# Write each tree's label table
fwrite(E_labels_cbs,    paste0(output_dir, "ex_bamboo_labels.csv"))
fwrite(E_fd_labels_cbs, paste0(output_dir, "ex_bamboo_fd_labels.csv"))
fwrite(E_labels_sua,    paste0(output_dir_v525, "ex_bamboo_labels.csv"))
fwrite(E_fd_labels_sua, paste0(output_dir_v525, "ex_bamboo_fd_labels.csv"))

# save public version ----------------------------------------------------------
# Extensions that can be shared as of 2026-06-25.
public_extensions <- c("biomass", "ghg", "gwp", "ibif", "land", "LC-Impact",
                       "luc", "nutrient_pollution", "water")

public_rows_cbs    <- E_labels_cbs$Compartment    %in% public_extensions
public_rows_sua    <- E_labels_sua$Compartment    %in% public_extensions
public_rows_fd_cbs <- E_fd_labels_cbs$Compartment %in% public_extensions
public_rows_fd_sua <- E_fd_labels_sua$Compartment %in% public_extensions

# Subset each tree's compiled extensions with that tree's own public_rows mask.
saveRDS(lapply(E_cbs,    `[`, public_rows_cbs,    ), paste0(output_dir,      "E.rds"))
saveRDS(lapply(E_fd_cbs, `[`, public_rows_fd_cbs, ), paste0(output_dir,      "E_fd.rds"))
saveRDS(lapply(E_sua,    `[`, public_rows_sua,    ), paste0(output_dir_v525, "E.rds"))
saveRDS(lapply(E_fd_sua, `[`, public_rows_fd_sua, ), paste0(output_dir_v525, "E_fd.rds"))

# Write cbs-derived public labels
fwrite(E_labels_cbs[public_rows_cbs,],       paste0(output_dir,      "ex_labels.csv"))
fwrite(E_fd_labels_cbs[public_rows_fd_cbs,], paste0(output_dir,      "ex_fd_labels.csv"))
fwrite(E_labels_sua[public_rows_sua,],       paste0(output_dir_v525, "ex_labels.csv"))
fwrite(E_fd_labels_sua[public_rows_fd_sua,], paste0(output_dir_v525, "ex_fd_labels.csv"))
