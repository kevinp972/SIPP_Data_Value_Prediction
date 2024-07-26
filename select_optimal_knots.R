library(splines)

# Define the function to select the optimal knots
select_optimal_knots <- function(data, response_var, feature_var, max_knots, k = 10) {
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
      model <- lm(as.formula(paste(response_var, "~ bs(", feature_var, ", knots = knots, Boundary.knots = boundary_knots)")), data = data[train,])
      
      # Make Predictions
      pred <- predict(model, newdata = data[test,])
      
      # Calculate and store MSE
      MSE[i, num_knots - 1] <- mean((data[[response_var]][test] - pred)^2)
    }
  }
  
  # Average MSE across folds
  mean_MSE <- colMeans(MSE)
  
  # Optimal number of knots
  optimal_knots <- which.min(mean_MSE) + 1 # Adding 1 to get the number of knots excluding boundary knots
  final_num_knots <- optimal_knots + 2 # Including boundary knots
  
  # Define final knots
  knots_chosen <- seq(from = min_value, to = max_value, length = final_num_knots)
  knots_chosen <- knots_chosen[2:(final_num_knots - 1)]
  
  return(list(knots_chosen = knots_chosen, boundary_knots = boundary_knots, mean_MSE = mean_MSE)
}

# Example usage
# data_no1 <- your_data_frame_here
# result <- select_optimal_knots(data_no1, response_var = "tw", feature_var = "hval", max_knots = 20)

# Display the chosen knots and boundary knots
# print(result$knots_chosen)
# print(result$boundary_knots)
