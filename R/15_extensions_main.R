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

# Parity check: cbs<->sua and fd_cbs<->fd_sua must match exactly.
pairs <- list(
  list(a = nms_cbs,    b = nms_sua,    name_a = "cbs",    name_b = "sua"),
  list(a = nms_fd_cbs, b = nms_fd_sua, name_a = "fd_cbs", name_b = "fd_sua")
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
if (nchar(msg) > 0) stop(msg) else message("All file lists are identical within pairs.")

nms    <- copy(nms_cbs)     # shared production-side stressor set (cbs == sua)
nms_fd <- copy(nms_fd_sua)  # shared fd stressor set (fd_cbs == fd_sua)
rm(nms_sua, nms_fd_cbs, p, pairs, msg)

# E_label checks ---------------------------------------------------------------
E_labels    <- fread("inst/E_labels_initial.csv")
E_fd_labels <- fread("inst/E_fd_labels_initial.csv")

# Every file needs a label row.
missing_labels    <- setdiff(nms,    E_labels$Stressor)
missing_labels_fd <- setdiff(nms_fd, E_fd_labels$Stressor)
if (length(missing_labels) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels,    collapse = ", "), " is present in extension folder but not in E_labels")
if (length(missing_labels_fd) > 0) stop("E_labels needs to be updated -> ", paste(missing_labels_fd, collapse = ", "), " is present in fd extension folder but not in E_labels")

# One label table each, filtered & ordered to match the file order.
E_labels    <- E_labels[Stressor %in% nms][order(match(Stressor, nms)), ]
E_fd_labels <- E_fd_labels[Stressor %in% nms_fd][order(match(Stressor, nms_fd)), ]
if (!all(E_labels$Stressor == nms))       stop("Re-do column ordering for E_labels to match files")
if (!all(E_fd_labels$Stressor == nms_fd)) stop("Re-do column ordering for E_fd_labels to match files")
rm(missing_labels, missing_labels_fd)

# Compile all extensions -------------------------------------------------------
files_cbs    <- list.files("data/extensions/cbs",    pattern = "\\.rds$", full.names = TRUE)
files_fd_cbs <- list.files("data/extensions/fd_cbs", pattern = "\\.rds$", full.names = TRUE)
files_sua    <- list.files("data/extensions/sua",    pattern = "\\.rds$", full.names = TRUE)
files_fd_sua <- list.files("data/extensions/fd_sua", pattern = "\\.rds$", full.names = TRUE)

E_cbs    <- compile_extension(lapply(files_cbs,    readRDS), files_cbs)
E_fd_cbs <- compile_extension(lapply(files_fd_cbs, readRDS), files_fd_cbs)
E_sua    <- compile_extension(lapply(files_sua,    readRDS), files_sua)
E_fd_sua <- compile_extension(lapply(files_fd_sua, readRDS), files_fd_sua)

# Row order guard: compiled row order must equal the shared label order.
if (!all(rownames(E_cbs[[as.character(years[1])]]) == E_labels$Stressor))
  stop("E_cbs row order does not match E_labels.")
if (!all(rownames(E_sua[[as.character(years[1])]]) == E_labels$Stressor))
  stop("E_sua row order does not match E_labels.")

# save BAMBOO version ----------------------------------------------------------
saveRDS(E_cbs,    paste0(output_dir, "E_bamboo.rds"))
saveRDS(E_fd_cbs, paste0(output_dir, "E_bamboo_fd.rds"))

# saveRDS(E_sua,    paste0(output_dir_v525, "E_bamboo.rds"))
# saveRDS(E_fd_sua, paste0(output_dir_v525, "E_bamboo_fd.rds"))

fwrite(E_labels,    paste0(output_dir, "ex_bamboo_labels.csv"))
fwrite(E_fd_labels, paste0(output_dir, "ex_bamboo_fd_labels.csv"))

# fwrite(E_labels,    paste0(output_dir_v525, "ex_bamboo_labels.csv"))
# fwrite(E_fd_labels, paste0(output_dir_v525, "ex_bamboo_fd_labels.csv"))

# save public version ----------------------------------------------------------
# Extensions that can be shared as of 2026-06-25.
public_extensions <- c("biomass", "ghg", "gwp", "ibif", "land", "LC-Impact",
                       "luc", "nutrient_pollution", "water")
public_rows    <- E_labels$Compartment    %in% public_extensions
public_rows_fd <- E_fd_labels$Compartment %in% public_extensions

saveRDS(lapply(E_cbs,    `[`, public_rows,    ), paste0(output_dir,      "E.rds"))
saveRDS(lapply(E_fd_cbs, `[`, public_rows_fd, ), paste0(output_dir,      "E_fd.rds"))
saveRDS(lapply(E_sua,    `[`, public_rows,    ), paste0(output_dir_v525, "E.rds"))
saveRDS(lapply(E_fd_sua, `[`, public_rows_fd, ), paste0(output_dir_v525, "E_fd.rds"))

fwrite(E_labels[public_rows,],       paste0(output_dir,      "ex_labels.csv"))
fwrite(E_fd_labels[public_rows_fd,], paste0(output_dir,      "ex_fd_labels.csv"))
fwrite(E_labels[public_rows,],       paste0(output_dir_v525, "ex_labels.csv"))
fwrite(E_fd_labels[public_rows_fd,], paste0(output_dir_v525, "ex_fd_labels.csv"))