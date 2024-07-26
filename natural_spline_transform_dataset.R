library(splines)

# Define the function to transform the dataset
natural_spline_transform_dataset <- function(data, feature_var, knots_chosen, boundary_knots) {
  # Create the spline basis for the feature variable
  spline_basis <- ns(data[[feature_var]], knots = knots_chosen, Boundary.knots = boundary_knots)
  
  # Convert the spline basis to a data frame
  spline_basis_df <- as.data.frame(spline_basis)
  
  # Generate names for the spline basis columns
  col_names <- paste0(feature_var, "_spline", seq_len(ncol(spline_basis_df)))
  names(spline_basis_df) <- col_names
  
  # Combine the original dataset with the spline basis data frame
  data_transform <- cbind(data, spline_basis_df)
  
  # Remove the original feature variable from the transformed dataset
  data_transform <- data_transform[, !names(data_transform) %in% feature_var]
  
  # Return the transformed dataset
  return(data_transform)
}

# Example usage
# Assuming `data` is your original dataset
# result <- select_optimal_knots(data, "tw", "hval", 20)
# transformed_data <- transform_dataset(data, "hval", result$knots_chosen, result$boundary_knots)

# Check the dimensions of the transformed dataset
# print(dim(transformed_data))
