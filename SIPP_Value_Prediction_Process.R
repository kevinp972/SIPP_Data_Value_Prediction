## Open Data Set 
Sys.setenv(lang = "en_US")
setwd("C:/Users/Kevin Peng/OneDrive/Documents/School/UCSD/Third Year Summer/Econ 178")
data <- read.table('data_tr.txt', head = T)[,-1]

### Visualize the first six rows of the dataset 
head(data)

## Outcome 
summary(data$tw)
## Visulize the variable in an histogram...
hist(data$tw)
## Outliers with enormous wealth

write.csv(summary(data), "summary(data).csv")
## Need to pay attention to ira and nohs and smcol and male and col

## let's check whether removing different multicollinear terms changes things
k <- 10
set.seed(123)
rand <- sample(nrow(data), floor(nrow(data)/k))
train <- setdiff(c(1:nrow(data)), rand)
y_rand <- data$tw[rand]

regnohs <- lm(tw ~ 1 + hs + smcol + col, data = data[train,])
reghs <- lm(tw ~ 1 + nohs + smcol + col, data = data[train,])
regsmcol <- lm(tw ~ 1 + nohs + hs + col, data = data[train,])
regcol <- lm(tw ~ 1 + nohs + hs + smcol, data = data[train,])

prnohs <- predict(regnohs, newdata = data[rand,])
prhs <- predict(reghs, newdata = data[rand,])
prsmcol <- predict(regsmcol, newdata = data[rand,])
prcol <- predict(regcol, newdata = data[rand,])

MSEnohs <- mean((y_rand-prnohs)^2)
MSEhs <- mean((y_rand-prhs)^2)
MSEsmcol <- mean((y_rand-prsmcol)^2)
MSEcol <- mean((y_rand-prcol)^2)

## Remove hs and hequity to deal with perfect multicollinearity
data <- data[, !(names(data) %in% c("hs", "hequity"))]
head(data)

# We do k-fold cross validation to choose among ridge, lasso, stepwise backward and stepwise forward.
library(MASS)
library(glmnet)
n <- nrow(data)
k <- 10 
set.seed(123)
id <- sample(rep(1:k, length= n))
MSPE.stepwise_backward <- MSPE.stepwise_forward <- MSPE.lasso <- MSPE.ridge <- rep(NA, k)
for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  # Stepwise
  full <- lm(tw ~ ., data=data[train,])
  null <- lm(tw ~ 1, data=data[train,])
  
  forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
  backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
  pr.stepwise_forward <- predict(forward, newdata=data[test,])
  pr.stepwise_backward <- predict(backward, newdata=data[test,])
  
  ## Do with LASSO/Ridge
  y <- data$tw
  X <- as.matrix(data[,-1])
  X.train <- as.matrix(X[train,])
  y.train <- y[train]
  X.test <- X[test,]
  
  ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0) 
  lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
  
  ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
  lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
  
  pr.lasso <- predict(lasso, newx=X.test)
  pr.ridge <- predict(ridge, newx=X.test)
  
  MSPE.stepwise_backward[f] <- mean((y[test] - pr.stepwise_forward)^2)
  MSPE.stepwise_forward[f] <- mean((y[test] - pr.stepwise_backward)^2)
  MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
}

## Check the best
c(mean(MSPE.ridge), mean(MSPE.lasso), mean(MSPE.stepwise_forward), mean(MSPE.stepwise_backward))
## Stepwise Forward Regression is the best out of the four

## Look at step forward
## Fit on the ENTIRE dataset your solution 
null <- lm(tw ~ 1, data = data)
full <- lm(tw ~., data = data)
baseline0 <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')
baseline0
summary(baseline0)
# write.csv(baseline0$coefficients, "summary(data).csv")
## Study possible outliers 
plot(baseline0)

## Now that we have the baseline model, we inspect each variable and its relationship with tw
# install.packages("tidyverse")
library(tidyverse)
boxplot(data$tw)$out
ggplot(data = data) + 
  geom_point(mapping = aes(x = ira, y = tw)) ## linear
ggplot(data = data) + 
  geom_point(mapping = aes(x = nifa, y = tw)) ## linear
ggplot(data = data) + 
  geom_point(mapping = aes(x = inc, y = tw)) ## nearly linear
ggplot(data = data) + 
  geom_point(mapping = aes(x = educ, y = tw)) ## predictor x has a small range
ggplot(data = data) + 
  geom_point(mapping = aes(x = age, y = tw)) ## linear

## tw ~ hval
ggplot(data = data) + 
  geom_point(mapping = aes(x = hval, y = tw)) ## splines

## look at the graph: many outliers (tw>1000000)
## Now we remove tw>1000000
data_no1 = subset(data, data$tw<1000000)
summary(data_no1$hval)

## With the outliers removed, let's try finding the optimal number of knots (code is from discussion sessions)
library(splines)
k <- 10
set.seed(123)
n <- nrow(data_no1)
ii <- sample(rep(1:k, length = n))
max_knots <- 20
MSE <- matrix(ncol = max_knots - 1, nrow = k)

for (i in 1:k) {
  test <- (ii == i)
  train <- (ii != i)
  for(num_knots in 2:max_knots) {
    # Adds 2 for the two endpoints
    knots <- seq(from = 0, to = 300000, length = num_knots + 2)
    # Removes the endpoints from knots, it's accounted for in Boundary.knots
    knots <- knots[2:num_knots+1]
    # Train the model
    model <- lm(tw ~ bs(hval, knots = knots, Boundary.knots = c(0,300000)), data = data_no1[train,])
    # Make Predictions
    pred <- predict(model, newdata = data_no1[test,])
    # Get the MSE of the num_knoth^th model for the jth fold
    MSE[i, num_knots - 1] <- mean((data_no1$tw[test] - pred)^2)
  } 
}

MSE <- colMeans(MSE)
plot(MSE)
cbind(2:(max_knots), MSE) ## 2:max_knots represent the number of knots
which.min(MSE) ## 2th index; which means 3 knots (excluding boundary knots)

final_num_knots <- which.min(MSE) + 1 + 2 ## 5 knots including boundary knots
knots_chosen <- seq(from = 0, to = 300000, length = final_num_knots)
knots_chosen <- knots_chosen[2:final_num_knots-1]
model <- lm(tw ~ bs(hval, knots = knots_chosen, Boundary.knots = c(0,300000)), data = data)

x.grid <- seq(from = min(data$hval), to = max(data$hval), length = 7933)
pr <- predict(model, newdata = list(hval = x.grid))
ggplot(data = data) + 
  geom_point(mapping = aes(x = hval, y = tw)) +
  geom_line(mapping = aes(x=x.grid, y = pr), lwd = 1, color = "Blue")

plot(model)

## k-fold cross validation to test splines vs linear
n <- nrow(data)
k <- 10
set.seed(123)
id <- sample(rep(1:k, length = n))
mspe_hvsplines <- mspe_hvlinear <- rep(NA, k)

for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  splineshv <- lm(tw ~ bs(hval, knots = knots_chosen, Boundary.knots = c(0,300000)), data = data[train,])
  linearhv <- lm(tw ~ hval, data = data[train,])
  prsplineshv <- predict(splineshv, newdata = data[test,])
  prlinearhv <- predict(linearhv, newdata = data[test,])
  mspe_hvsplines[f] = mean((data$tw[test] - prsplineshv)^2)
  mspe_hvlinear[f] = mean((data$tw[test] - prlinearhv)^2)
}

c(mean(mspe_hvsplines), mean(mspe_hvlinear))

## now we add the splines of hval to our baseline model(GAM)
library(splines)
hval.spline <- bs(data$hval, knots = knots_chosen, Boundary.knots = c(0,300000))
hval.spline <- as.data.frame(hval.spline)
# This gives names to hval.spline before merging with the original dataset.
# This will ensure the names are given by hval_spline# rather than some arbitrary naming scheme.
names(hval.spline) <- paste0("hval_spline", 1:7) ## # of knots + 4 degrees of freedom
# Combine dataset with hval.spline
data_transform <- cbind(data, hval.spline)
# Remove hval from transformed data
data_transform <- subset(data_transform, select = -hval)
dim(data_transform)

## cross validation for ridge lasso and step for hval transformed data
library(MASS)
library(glmnet)
n <- nrow(data_transform)
k <- 10 
set.seed(123)
id <- sample(rep(1:k, length= n))
MSPE.stepwise_backward <- MSPE.stepwise_forward <- MSPE.lasso <- MSPE.ridge <- rep(NA, k)
for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  # Stepwise
  full <- lm(tw ~ ., data=data_transform[train,])
  null <- lm(tw ~ 1, data=data_transform[train,])
  
  forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
  backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
  pr.stepwise_forward <- predict(forward, newdata=data_transform[test,])
  pr.stepwise_backward <- predict(backward, newdata=data_transform[test,])
  
  ## Do with LASSO/Ridge
  y <- data_transform$tw
  X <- as.matrix(data_transform[,-1])
  X.train <- as.matrix(X[train,])
  y.train <- y[train]
  X.test <- X[test,]
  
  ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0) 
  lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
  
  ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
  lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
  
  pr.lasso <- predict(lasso, newx=X.test)
  pr.ridge <- predict(ridge, newx=X.test)
  
  MSPE.stepwise_backward[f] <- mean((y[test] - pr.stepwise_forward)^2)
  MSPE.stepwise_forward[f] <- mean((y[test] - pr.stepwise_backward)^2)
  MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
}

## Check the best
c(mean(MSPE.ridge), mean(MSPE.lasso), mean(MSPE.stepwise_forward), mean(MSPE.stepwise_backward))
## splines of hval is a success!

## INTERACTIONS

## add all interactions
data_int <- data_transform
for (i in 2:15){
  # Convert columns to numeric (to prevent integer overflow)
  col1 <- as.numeric(data[, i])
  col2_to_16 <- as.matrix(data[, (i + 1):16])
  
  # Calculate interactions
  interaction <- col1 * col2_to_16
  # Name the interaction columns
  col_names <- paste0(names(data)[i], '_times_', names(data)[(i + 1):16])
  colnames(interaction) <- col_names
  # Add to transformed data
  data_int <- cbind(data_int, interaction)
}
dim(data_int)

## select features (stepwise)
library(MASS)
null <- lm(tw ~ 1, data = data_int)
full <- lm(tw ~ ., data = data_int)
forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')

summary(forward)
# write.csv(forward$coefficients, "interactionfeatureselection.csv")
plot(forward)

## k-fold cross validation to test stepwise vs lasso
n <- nrow(data_int)
k <- 10
set.seed(123)
id <- sample(rep(1:k, length = n))
mspe_fullint <- mspe_intlasso <- rep(NA, k)
library(MASS)
library(glmnet)

for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  null <- lm(tw ~ 1, data = data_int[train,])
  full <- lm(tw ~ ., data = data_int[train,])
  fullint <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')
  prfullint <- predict(fullint, newdata = data_int[test,])
  mspe_fullint[f] = mean((data_int$tw[test] - prfullint)^2)
  
  xx.tr <- data_int[train,-1]
  y.tr <-  data_int$tw[train]
  xx.te <- data_int[test,-1]
  lasso.cv <- cv.glmnet(x=as.matrix(xx.tr), y=y.tr, nfolds=k, alpha=1)
  lasso <- glmnet(x = xx.tr, y = y.tr, lambda = lasso.cv$lambda.min, alpha = 1)
  pr.lasso <- predict(lasso, newx=as.matrix(xx.te))
  mspe_intlasso[f] = mean((data_int$tw[test] - pr.lasso)^2)
}

c(mean(mspe_intlasso),mean(mspe_fullint))

## now add only the statistically significant interactions
data_int2 <- data_transform

# Calculate interactions
data_int2$interaction_inc_e401 <- data$inc * data$e401
data_int2$interaction_inc_hmort <- as.numeric(data$inc) * as.numeric(data$hmort)
data_int2$interaction_inc_age <- data$inc * data$age
data_int2$interaction_inc_col <- data$inc * data$col
data_int2$interaction_educ_hval <- data$educ * data$hval
data_int2$interaction_educ_nifa <- data$educ * data$nifa
data_int2$interaction_e401_nifa <- data$e401 * data$nifa
data_int2$interaction_ira_males <- data$ira * data$male

## cross validation for ridge lasso and step for selected dataset
library(MASS)
library(glmnet)
n <- nrow(data_int2)
k <- 10 
set.seed(123)
id <- sample(rep(1:k, length= n))
MSPE.stepwise_backward <- MSPE.stepwise_forward <- MSPE.lasso <- MSPE.ridge <- rep(NA, k)
for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  # Stepwise
  full <- lm(tw ~ ., data=data_int2[train,])
  null <- lm(tw ~ 1, data=data_int2[train,])
  
  forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
  backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
  pr.stepwise_forward <- predict(forward, newdata=data_int2[test,])
  pr.stepwise_backward <- predict(backward, newdata=data_int2[test,])
  
  ## Do with LASSO/Ridge
  y <- data_int2$tw
  X <- as.matrix(data_int2[,-1])
  X.train <- as.matrix(X[train,])
  y.train <- y[train]
  X.test <- X[test,]
  
  ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0) 
  lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
  
  ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
  lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
  
  pr.lasso <- predict(lasso, newx=X.test)
  pr.ridge <- predict(ridge, newx=X.test)
  
  MSPE.stepwise_backward[f] <- mean((y[test] - pr.stepwise_forward)^2)
  MSPE.stepwise_forward[f] <- mean((y[test] - pr.stepwise_backward)^2)
  MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
}

## Check the best
c(mean(MSPE.ridge), mean(MSPE.lasso), mean(MSPE.stepwise_forward), mean(MSPE.stepwise_backward))

## finalizing the model
## select features (stepwise)
library(MASS)
null <- lm(tw ~ 1, data = data_int2)
full <- lm(tw ~ ., data = data_int2)
forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')

summary(forward)

selected_covariates <- attr(forward$terms, "term.labels")
selected_data <- data_int2[, c("tw", selected_covariates)]

## cross validation for ridge lasso and step for selected dataset
library(MASS)
library(glmnet)
n <- nrow(selected_data)
k <- 10 
set.seed(123)
id <- sample(rep(1:k, length= n))
MSPE.stepwise_backward <- MSPE.stepwise_forward <- MSPE.lasso <- MSPE.ridge <- rep(NA, k)
for (f in 1:k){
  test <- (id == f)
  train <- (id != f)
  
  # Stepwise
  full <- lm(tw ~ ., data=selected_data[train,])
  null <- lm(tw ~ 1, data=selected_data[train,])
  
  forward <- stepAIC(null, scope=list(lower=null, upper=full), trace = F, direction='forward')
  backward <- stepAIC(full, scope=list(lower=null, upper=full), trace = F, direction='backward')
  pr.stepwise_forward <- predict(forward, newdata=selected_data[test,])
  pr.stepwise_backward <- predict(backward, newdata=selected_data[test,])
  
  ## Do with LASSO/Ridge
  y <- selected_data$tw
  X <- as.matrix(selected_data[,-1])
  X.train <- as.matrix(X[train,])
  y.train <- y[train]
  X.test <- X[test,]
  
  ridge.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 0) 
  lasso.cv <- cv.glmnet(x = X.train, y = y.train, nfolds = k, alpha = 1)
  
  ridge <- glmnet(x = X.train, y = y.train, lambda = ridge.cv$lambda.min, alpha = 0)
  lasso <- glmnet(x = X.train, y = y.train, lambda = lasso.cv$lambda.min, alpha = 1)
  
  pr.lasso <- predict(lasso, newx=X.test)
  pr.ridge <- predict(ridge, newx=X.test)
  
  MSPE.stepwise_backward[f] <- mean((y[test] - pr.stepwise_forward)^2)
  MSPE.stepwise_forward[f] <- mean((y[test] - pr.stepwise_backward)^2)
  MSPE.lasso[f] <- mean((y[test] - pr.lasso)^2)
  MSPE.ridge[f] <- mean((y[test] - pr.ridge)^2)
}

## Check the best
c(mean(MSPE.ridge), mean(MSPE.lasso), mean(MSPE.stepwise_forward), mean(MSPE.stepwise_backward))

## FINAL MODEL
library(MASS)
null <- lm(tw ~ 1, data = data_int2)
full <- lm(tw ~ ., data = data_int2)
FINAL <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')

summary(FINAL)

## get predictions
data_te <- read.table("data_for_prediction.txt", header = TRUE, sep = "\t", dec = ".")[,-1]

## Remove hs and hequity to deal with perfect multicollinearity
data_te <- data_te[, !(names(data_te) %in% c("hs", "hequity"))]
head(data_te)
final_num_knots <- 5 ## 5 knots including boundary knots
knots_chosen <- seq(from = 0, to = 300000, length = final_num_knots)
knots_chosen <- knots_chosen[2:final_num_knots-1]
library(splines)
hval.spline <- bs(data_te$hval, knots = knots_chosen, Boundary.knots = c(0,300000))
hval.spline <- as.data.frame(hval.spline)
# This gives names to hval.spline before merging with the original dataset.
# This will ensure the names are given by hval_spline# rather than some arbitrary naming scheme.
names(hval.spline) <- paste0("hval_spline", 1:7) ## # of knots + 4 degrees of freedom
# Combine dataset with hval.spline
datate_transform <- cbind(data_te, hval.spline)
# Remove hval from transformed data
datate_transform <- subset(datate_transform, select = -hval)

datate_transform$interaction_inc_e401 <- data_te$inc * data_te$e401
datate_transform$interaction_inc_hmort <- as.numeric(data_te$inc) * as.numeric(data_te$hmort)
datate_transform$interaction_inc_age <- data_te$inc * data_te$age
datate_transform$interaction_inc_col <- data_te$inc * data_te$col
datate_transform$interaction_educ_hval <- data_te$educ * data_te$hval
datate_transform$interaction_educ_nifa <- data_te$educ * data_te$nifa
datate_transform$interaction_e401_nifa <- data_te$e401 * data_te$nifa
datate_transform$interaction_ira_males <- data_te$ira * data_te$male

FINAL_PREDICT <- predict(FINAL, newdata=datate_transform)
data_te <- cbind(FINAL_PREDICT, data_te)
write.csv(data_te, "my predictions.csv")
write.table(FINAL_PREDICT, file = 'my_predictions.txt')
