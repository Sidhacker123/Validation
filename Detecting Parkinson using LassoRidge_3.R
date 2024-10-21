install.packages('glmnet')
install.packages('Matrix')
install.packages('caret')
install.packages('ggplot')
library(glmnet)

library(caret)

Park_Train <- read.csv('parkinsons_train.csv')
Park_Test <- read.csv('parkinsons_test.csv')

Test_X <- Park_Test[, !names(Park_Test) %in% c('name', 'status')]
Test_Y <- Park_Test$status

set.seed(4)
trainIndex <- createDataPartition(Park_Train$status, p = 0.5, list = FALSE)
Park_Train_Set <- Park_Train[trainIndex, ]
Park_Validate_Set <- Park_Train[-trainIndex, ]

Train_X <- Park_Train_Set[, !names(Park_Train_Set) %in% c('name', 'status')]
Train_Y <- Park_Train_Set$status

Validate_X <- Park_Validate_Set[, !names(Park_Validate_Set) %in% c('name', 'status')]
Validate_Y <- Park_Validate_Set$status

Ridge_CV <- function(Train_X, Train_Y, Validate_X, Validate_Y) {
  lam_MSE <- c()
  lambda_grid <- seq(1, 100, by = 1)
  set.seed(4)
  fit_ridge <- cv.glmnet(as.matrix(Train_X), Train_Y, alpha = 0, lambda = lambda_grid, 
                         nfolds = 5, type.measure = "mse")
  optimal_lam <- fit_ridge$lambda.min
  return(optimal_lam)
}

optimal_lambda <- Ridge_CV(Train_X, Train_Y, Validate_X, Validate_Y)

ridge_model <- glmnet(as.matrix(Train_X), Train_Y, alpha = 0, lambda = optimal_lambda)
Park_ridge_pred <- predict(ridge_model, as.matrix(Test_X))

Park_Coef <- as.vector(coef(ridge_model)[-1])
Park_Intercept <- coef(ridge_model)[1]

Park_ridge_pred_rounded <- ifelse(Park_ridge_pred > 0.5, 1, 0)
cm <- confusionMatrix(as.factor(Park_ridge_pred_rounded), as.factor(Test_Y))

cat("Optimal Tuning Parameter (Lambda):", optimal_lambda, "\n")
cat("Coefficients:\n", Park_Coef, "\n")
cat("Intercept:\n", Park_Intercept, "\n")
cat("Confusion Matrix:\n")
print(cm$table)


### Lasso Regression


Lasso_CV <- function(Train_X, Train_Y, Validate_X, Validate_Y) {
  lam_MSE <- c()
  lambda_grid <- seq(1, 100, by = 1)
  set.seed(0)
  fit_lasso <- cv.glmnet(as.matrix(Train_X), Train_Y, alpha = 1, lambda = lambda_grid, 
                         nfolds = 5, type.measure = "mse")
  optimal_lam <- fit_lasso$lambda.min
  return(optimal_lam)
}

optimal_lambda <- Lasso_CV(Train_X, Train_Y, Validate_X, Validate_Y)

lasso_model <- glmnet(as.matrix(Train_X), Train_Y, alpha = 1, lambda = optimal_lambda)
Park_lasso_pred <- predict(lasso_model, as.matrix(Test_X))

Park_Coef <- as.vector(coef(lasso_model)[-1])
Park_Intercept <- coef(lasso_model)[1]

Park_lasso_pred_rounded <- ifelse(Park_lasso_pred > 0.5, 1, 0)
cm <- confusionMatrix(as.factor(Park_lasso_pred_rounded), as.factor(Test_Y))

cat("Optimal Tuning Parameter (Lambda):", optimal_lambda, "\n")
cat("Coefficients:\n", Park_Coef, "\n")
cat("Intercept:\n", Park_Intercept, "\n")
cat("Confusion Matrix:\n")
print(cm$table)
