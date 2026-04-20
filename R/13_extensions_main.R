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

nms <- copy(nms_sua)
nms_fd <- copy(nms_fd_sua)
rm(nms_cbs, nms_sua, nms_fd_sua, nms_fd_cbs, p, pairs, msg)

# E_label checks -----------
E_labels <- fread("inst/E_labels.csv")
E_fd_labels <- fread("inst/E_fd_labels.csv")

# check if E_labels is fully updated
missing_labels <- E_labels[!Stressor %in% nms]
missing_labels_fd <- E_fd_labels[!Stressor %in% nms_fd]

if (nrow(missing_labels) > 0)    stop("E_labels needs to be updated -> ", paste(missing_labels,    collapse = ", "), " is present in extension folder but not in E_labels")
if (nrow(missing_labels_fd) > 0) stop("E_labels needs to be updated -> ", paste(missing_labels_fd, collapse = ", "), " is present in fd extension folder but not in E_labels")

# exclude possible extra lines in E_labels
E_labels <- E_labels[Stressor %in% nms]
E_fd_labels <- E_fd_labels[Stressor %in% nms_fd]

# sort labels in the same order as the files
E_labels <- E_labels[order(match(E_labels$Stressor, nms)), ]
E_fd_labels <- E_fd_labels[order(match(E_fd_labels$Stressor, nms_fd)), ]

# check that the ordering worked
if (!all(E_labels$Stressor == nms)) stop("Re-do column ordering for E_labels to match files")
if (!all(E_fd_labels$Stressor == nms_fd)) stop("Re-do column ordering for E_fd_labels to match files")
  
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

# Combine
E_cbs <- compile_extension(data_cbs, files_cbs)  
E_fd_cbs <- compile_extension(data_fd_cbs, files_fd_cbs)  

E_sua <- compile_extension(data_sua, files_sua)  
E_fd_sua <- compile_extension(data_fd_sua, files_fd_sua)  

# save
saveRDS(E_cbs, paste(output_dir, "E.rds"))
saveRDS(E_fd_cbs, paste(output_dir, "E_fd.rds"))

saveRDS(E_sua, paste(output_dir_v525, "E.rds"))
saveRDS(E_fd_sua, paste(output_dir_v525, "E_fd.rds"))

fwrite(E_labels, paste(output_dir, "ex_labels.csv"))
fwrite(E_fd_labels, paste(output_dir, "ex_fd_labels.csv"))

fwrite(E_labels, paste(output_dir_v525, "ex_labels.csv"))
fwrite(E_fd_labels, paste(output_dir_v525, "ex_fd_labels.csv"))







