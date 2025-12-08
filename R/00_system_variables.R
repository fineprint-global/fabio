# define system variables ----------------------------------------------------------------

# Determine the output directory based on the branch
branch_name <- system("git symbolic-ref --short HEAD", intern = TRUE)
if (branch_name == "data-1986-2013") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/8613"
} else if (branch_name == "data-2010-current") {
  output_dir <- "/mnt/nfs_fineprint/tmp/fabio/v2/"
} else {
  stop("Unknown branch!")
}

# Ensure the directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# # Use the output_dir in your code
# write.csv(data, file.path(output_dir, "output.csv"))


# Determine years
years <- 2010:2022



# Recursive sum over vectors with NA, returns NA if all values are NA
na_sum <- function(..., rowwise = TRUE) {
  dots <- list(...)
  if(length(dots) == 1) { # Base
    ifelse(all(is.na(dots[[1]])), NA_real_, sum(dots[[1]], na.rm = TRUE))
  } else { # Recurse
    if(rowwise) {
      x <- do.call(cbind, dots)
      return(apply(x, 1, na_sum))
    }
    return(na_sum(vapply(dots, na_sum, double(1L))))
  }
}

# Aggregate a matrix based on its column names
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }
