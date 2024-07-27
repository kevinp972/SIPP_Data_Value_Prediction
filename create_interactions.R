create_interactions <- function(data, response_var, exclude_vars) {
  
  all_vars <- setdiff(names(data), c(response_var, exclude_vars))
  
  interaction_terms <- as.data.frame(model.matrix(~ .^2 - 1, data[, all_vars]))
  
  interaction_colnames <- colnames(interaction_terms)
  interaction_terms <- interaction_terms[, !interaction_colnames %in% all_vars]
  
  data_with_interactions <- cbind(data, interaction_terms)
  
  return(data_with_interactions)
}