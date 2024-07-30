# define system variables ----------------------------------------------------------------

# Determine the output directory based on the branch
branch_name <- system("git symbolic-ref --short HEAD", intern = TRUE)
if (branch_name == "data-1986-2013") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/8613"
} else if (branch_name == "data-2010-current") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current"
} else {
  stop("Unknown branch!")
}

# Ensure the directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# # Use the output_dir in your code
# write.csv(data, file.path(output_dir, "output.csv"))


# Determine years
years <- 2010:2022