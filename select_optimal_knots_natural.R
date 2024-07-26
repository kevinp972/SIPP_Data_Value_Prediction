library(splines)
library(ggplot2)  

select_optimal_knots_natural <- function(data, response_var, feature_var, max_knots, k = 10) {
  set.seed(123)
  n <- nrow(data)
  ii <- sample(rep(1:k, length = n))
  
  # Initialize boundary knots based on the feature variable
  min_value <- min(data[[feature_var]], na.rm = TRUE)
  max_value <- max(data[[feature_var]], na.rm = TRUE)
  boundary_knots <- c(min_value, max_value)
  
  # Initialize MSE matrix
  MSE <- matrix(ncol = max_knots - 1, nrow = k)
  
  for (i in 1:k) {
    test <- (ii == i)
    train <- (ii != i)
    for (num_knots in 2:max_knots) {
      # Define knots
      knots <- seq(from = min_value, to = max_value, length = num_knots + 2)
      knots <- knots[2:(num_knots + 1)] # Remove endpoints
      
      # Train the model
      formula_str <- paste(response_var, "~ ns(", feature_var, ", knots = knots, Boundary.knots = boundary_knots)", sep = "")
      model <- lm(as.formula(formula_str), data = data[train,])
      
      # Make Predictions
      pred <- predict(model, newdata = data[test,])
      
      # Calculate and store MSE
      MSE[i, num_knots - 1] <- mean((data[[response_var]][test] - pred)^2)
    }
  }
  
  # Average MSE across folds
  mean_MSE <- colMeans(MSE)
  
  # Create the plot using ggplot2
  mse_df <- data.frame(Knots = 2:max_knots, Mean_MSE = mean_MSE)
  p <- ggplot(mse_df, aes(x = Knots, y = Mean_MSE)) +
    geom_line() +
    geom_point() +
    labs(title = "MSE vs Number of Knots",
         x = "Number of Knots",
         y = "Mean Squared Error") +
    theme_minimal()
  
  # Optimal number of knots
  optimal_knots <- which.min(mean_MSE) + 1 # Adding 1 to get the number of knots excluding boundary knots
  final_num_knots <- optimal_knots + 2 # Including boundary knots
  
  # Define final knots
  knots_chosen <- seq(from = min_value, to = max_value, length = final_num_knots)
  knots_chosen <- knots_chosen[2:(final_num_knots - 1)]
  
  return(list(knots_chosen = knots_chosen, boundary_knots = boundary_knots, plot = p))
}

# Example usage
# Assuming `data_no1` is your dataframe
# response_var = "tw"
# feature_var = "hval"
# max_knots = 20
# result <- select_optimal_knots(data_no1, response_var, feature_var, max_knots)

# Display the chosen knots and boundary knots
# print(result$knots_chosen)
# print(result$boundary_knots)

# Print or plot the generated plot
# print(result$plot)  # For viewing the plot in RStudio or Jupyter Notebook
