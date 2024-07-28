library(splines)

natural_spline_transform_dataset <- function(data, feature_var, knots_chosen, boundary_knots) {
  
  spline_basis <- ns(data[[feature_var]], knots = knots_chosen, Boundary.knots = boundary_knots)
  
  spline_basis_df <- as.data.frame(spline_basis)
  
  col_names <- paste0(feature_var, "_spline", seq_len(ncol(spline_basis_df)))
  names(spline_basis_df) <- col_names
  
  data_transform <- cbind(data, spline_basis_df)
  
  data_transform <- data_transform[, !names(data_transform) %in% feature_var]
  
  return(data_transform)
}
