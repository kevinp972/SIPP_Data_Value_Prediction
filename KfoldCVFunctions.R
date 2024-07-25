library(glmnet)
library(MASS)

compute_ridge_mspe <- function(data, response_var, k = 10, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  id <- sample(rep(1:k, length=n))
  MSPE.ridge <- rep(NA, k)
  
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    
    y <- data[[response_var]]
    X <- as.matrix(data[ , !(names(data) %in% response_var)])
    X.train <- as.matrix(X[train,])
    y.train <- y[train]
    X.test <- X[test,]
    
    ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0)  
    ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
    
    pr.ridge <- predict(ridge, newx=X.test)
    
    MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
  }
  
  return(mean(MSPE.ridge))
}

compute_lasso_mspe <- function(data, response_var, k = 10, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  id <- sample(rep(1:k, length=n))
  MSPE.lasso <- rep(NA, k)
  
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    
    y <- data[[response_var]]
    X <- as.matrix(data[ , !(names(data) %in% response_var)])
    X.train <- as.matrix(X[train,])
    y.train <- y[train]
    X.test <- X[test,]
    
    lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
    lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
    
    pr.lasso <- predict(lasso, newx=X.test)
    
    MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  }
  
  return(mean(MSPE.lasso))
}

compute_forward_stepwise_mspe <- function(data, response_var, k = 10, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  id <- sample(rep(1:k, length=n))
  MSPE.stepwise_forward <- rep(NA, k)
  
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    
    full <- lm(as.formula(paste(response_var, "~ .")), data=data[train,])
    null <- lm(as.formula(paste(response_var, "~ 1")), data=data[train,])
    
    forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')
    
    pr.stepwise_forward <- predict(forward, newdata=data[test,])
    
    MSPE.stepwise_forward[f] <- mean((data[test, response_var] - pr.stepwise_forward)^2)
  }
  
  return(mean(MSPE.stepwise_forward))
}

compute_backward_stepwise_mspe <- function(data, response_var, k = 10, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  id <- sample(rep(1:k, length=n))
  MSPE.stepwise_backward <- rep(NA, k)
  
  for (f in 1:k) {
    test <- (id == f)
    train <- (id != f)
    
    full <- lm(as.formula(paste(response_var, "~ .")), data=data[train,])
    null <- lm(as.formula(paste(response_var, "~ 1")), data=data[train,])
    
    backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = FALSE, direction='backward')
    
    pr.stepwise_backward <- predict(backward, newdata=data[test,])
    
    MSPE.stepwise_backward[f] <- mean((data[test, response_var] - pr.stepwise_backward)^2)
  }
  
  return(mean(MSPE.stepwise_backward))
}