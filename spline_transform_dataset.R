library(splines)

spline_transform_dataset <- function(data, feature_var, knots_chosen, boundary_knots) {
  
  spline_basis <- bs(data[[feature_var]], knots = knots_chosen, Boundary.knots = boundary_knots)
  
  spline_basis_df <- as.data.frame(spline_basis)
  
  # Generate names for the spline basis columns
  col_names <- paste0(feature_var, "_spline", seq_len(ncol(spline_basis_df)))
  names(spline_basis_df) <- col_names
  
  data_transform <- cbind(data, spline_basis_df)
  
  # Remove the original feature variable from the transformed dataset
  data_transform <- data_transform[, !names(data_transform) %in% feature_var]
  
  return(data_transform)
}

