# This script creates a function for inter and extrpolating values in a data.table
# Optional TODO: combine into one function

library(data.table)


interpolate <- function(var, x) {  
  # Identify available (non-NA) values
  x[, available := !is.na(get(var))]
  
  # Process only groups with at least two available values
  interpolated <- x[, if (sum(available) >= 2) {
    known_x <- year[available]
    known_y <- get(var)[available]
    
    # Linear interpolation with rule = 2 (allows extrapolation)
    interpolated_y <- approx(known_x, known_y, year, rule = 2)$y
    
    # Replace extrapolated values with nearest known value
    interpolated_y[year < min(known_x)] <- known_y[which.min(known_x)]
    interpolated_y[year > max(known_x)] <- known_y[which.max(known_x)]
    
    .(year, interpolated_value = interpolated_y)
  } else {
    .(year, interpolated_value = get(var))  # Keep original values if interpolation isn't possible
  }, by = .(iso3c, item)]
  
  # Merge interpolated values back into the main dataset
  x[interpolated, on = .(iso3c, item, year), 
    (var) := i.interpolated_value]
  
  # Remove the temporary 'available' column
  x[, available := NULL]
  
  return(x)  
}


extrapolate <- function(var, x) {  
  # Identify available (non-NA) values
  x[, available := !is.na(get(var))]
  
  # Identify groups with exactly **one** available value
  single_value_groups <- x[, .(non_na_count = sum(available)), by = .(iso3c, item)][non_na_count == 1]
  
  # Extract and apply the single available value for those groups
  extrapolated <- x[single_value_groups, on = .(iso3c, item), 
                    .(iso3c, item, year, extrapolated_value = rep(na.omit(get(var)), .N)), 
                    by = .(iso3c, item)]
  
  # Merge extrapolated values back into the main dataset using a join
  x[extrapolated, on = .(iso3c, item, year), (var) := i.extrapolated_value]
  
  # Remove the temporary column
  x[, available := NULL]
  
  return(x)  
}
