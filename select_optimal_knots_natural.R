library(splines)
library(ggplot2)  

select_optimal_knots_natural <- function(data, response_var, feature_var, max_knots, k = 10) {
  set.seed(123)
  n <- nrow(data)
  ii <- sample(rep(1:k, length = n))
  
  min_value <- min(data[[feature_var]], na.rm = TRUE)
  max_value <- max(data[[feature_var]], na.rm = TRUE)
  boundary_knots <- c(min_value, max_value)
  
  MSE <- matrix(ncol = max_knots - 1, nrow = k)
  
  for (i in 1:k) {
    test <- (ii == i)
    train <- (ii != i)
    for (num_knots in 2:max_knots) {
      knots <- seq(from = min_value, to = max_value, length = num_knots + 2)
      knots <- knots[2:(num_knots + 1)]
      
      formula_str <- paste(response_var, "~ ns(", feature_var, ", knots = knots, Boundary.knots = boundary_knots)", sep = "")
      model <- lm(as.formula(formula_str), data = data[train,])
      
      pred <- predict(model, newdata = data[test,])
      
      MSE[i, num_knots - 1] <- mean((data[[response_var]][test] - pred)^2)
    }
  }
  
  mean_MSE <- colMeans(MSE)
  
  mse_df <- data.frame(Knots = 2:max_knots, Mean_MSE = mean_MSE)
  p <- ggplot(mse_df, aes(x = Knots, y = Mean_MSE)) +
    geom_line() +
    geom_point() +
    labs(title = "MSE vs Number of Knots",
         x = "Number of Knots",
         y = "Mean Squared Error") +
    theme_minimal()
  
  optimal_knots <- which.min(mean_MSE) + 1
  final_num_knots <- optimal_knots + 2
  
  knots_chosen <- seq(from = min_value, to = max_value, length = final_num_knots)
  knots_chosen <- knots_chosen[2:(final_num_knots - 1)]
  
  return(list(knots_chosen = knots_chosen, boundary_knots = boundary_knots, plot = p))
}
