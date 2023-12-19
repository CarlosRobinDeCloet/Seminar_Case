library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)
library(readxl)
library(olsrr)


#results <- data.frame(matrix(ncol = 6, nrow = 0))
#x <- c("SKU", "Estimator","Dataset","Lambda",  "RMSE", "Rsqaure" )
#colnames(results) <- x


#############################################################################
# Performs OLS General to specific, Ridge and LASSO regression.             #
#                                                                           #
# Specify the dependent variable for regression on line 64, 65, 66 and 120  #  
# by selecting the variable from the data.                                  #
# Results of the regressions can be found in the variable 'Results'         #
# Beware! When rerunning the model, run the model from line 28!             #
#                                                                           #
#############################################################################


#Import data and convert to log except for the promo columns
#datasetS02 <- read_excel("Erasmus/Eco/Pre master/Blok 4/Seminar case/datasetS02.xlsx")
dat <- datasetS02
logDat <- datasetS02
logDat[,2:17] <-log(dat[,2:17])
logDat[,26:27] <-log(logDat[,26:27])
logDat[,29:30] <-log(logDat[,29:30])

#Remove the week number
logDat <-logDat[,c(2:31)]

#Add the lags to the dataset
for(i in 1:ncol(logDat))
{       
  logDat <- cbind(logDat, lag(logDat[,i]))
}


#Renaming the columns
colnames(logDat) <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8",         
                      "price_1","price_2","price_3","price_4","price_5","price_6","price_7", "price_8",         
                      "promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8"  ,       
                      "sales_s","price_s","promo_s","sales_f", "price_f","promo_f",
                      "sales_1_lag","sales_2_lag","sales_3_lag","sales_4_lag","sales_5_lag","sales_6_lag","sales_7_lag","sales_8_lag",         
                      "price_1_lag","price_2_lag","price_3_lag","price_4_lag","price_5_lag","price_6_lag","price_7_lag", "price_8_lag",         
                      "promo_1_lag","promo_2_lag","promo_3_lag","promo_4_lag","promo_5_lag","promo_6_lag","promo_7_lag","promo_8_lag"  ,       
                      "sales_s_lag","price_s_lag","promo_s_lag","sales_f_lag", "price_f_lag","promo_f_lag") 

#Split the dataset
train = logDat[2:78,] # Create the training data and removing first week due to lack of lags
test = logDat[79:104,] # Create the test data



#Create the vectors containing true observations. CHANGE THIS PART FOR DIFFERENT SKU!
#Is also the dependent variable in the regression
y_train = train$sales_8
y_test = test$sales_8
sku="sales_8"

#Remove the sales data (not of the lag of course)
trainWithoutSales = train[, c(9:60)]
trainWithoutSales <- subset (trainWithoutSales, select = -c(sales_s,sales_f))
x_train = as.matrix(trainWithoutSales)

testWithoutSales = test[, c(9:60)]
testWithoutSales <- subset (testWithoutSales, select = -c(sales_s,sales_f))
x_test = as.matrix(testWithoutSales)



# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, estimator, dataset, sku, lambda1) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  r_square <- 1 - SSE / SST
  rMSE = sqrt(SSE/length(true))
  
  # Model performance metrics
  result1 = data.frame(
    SKU=sku,
    Estimator= estimator,
    Dataset =dataset,
    Lamda = lambda1,
    RMSE= rMSE,
    R_square = r_square)
  return(result1)
}



######### Reduced OLS ############
#Complete OLS Model

OLSmodel <- lm(sales_8 ~ price_1+	price_2	+price_3+	price_4	+price_5+	price_6+	price_7+	price_8+
                 +promo_1+	+	promo_3+	promo_4+	promo_5	+promo_6+	promo_7+	promo_8+	price_s	+promo_s+	price_f+	promo_f+
                 sales_1_lag+	sales_2_lag+	sales_3_lag+	sales_4_lag+	sales_5_lag+	sales_6_lag+	sales_7_lag+	sales_8_lag+
                 price_1_lag+	price_2_lag+	price_3_lag+	price_4_lag+	price_5_lag+	price_6_lag+	price_7_lag+	price_8_lag+
                 promo_1_lag+	promo_2_lag+	promo_3_lag+	promo_4_lag+	promo_5_lag+	promo_6_lag+	promo_7_lag+	promo_8_lag+
                 sales_s_lag+	price_s_lag+	promo_s_lag+	sales_f_lag+	price_f_lag+	promo_f_lag
               , data = ols_train)

#Select 5% significant to select the parameters
reducedOLSModelObject <- ols_step_backward_p(OLSmodel, details = FALSE, prem = 0.05)
reducedOLSModel <-reducedOLSModelObject$model

# Prediction and evaluation on train data
predictions_train = predict(reducedOLSModel, newdata = ols_train)
result = eval_results(ols_y_train, predictions_train,  "OLS", "Training", sku, 0 )
results <- rbind(results, result)

# Prediction and evaluation on test data
predictions_test = predict(reducedOLSModel, newdata = ols_test)
result = eval_results(ols_y_test, predictions_test, "OLS", "Test", sku, 0)
results <- rbind(results, result)



######### Ridge ############
# Setting the range of lambda values
lambdas <- 10^seq(3, -2, by = -.1)

# Train ridge regression on training model
ridge_reg <- glmnet(x_train, y_train, alpha = 0, lambda  = lambdas, standardize = TRUE, standardize.response = TRUE)

# Using cross validation glmnet on Ridge to find optimal lambda
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas, nfolds = 78)
best_lambda_ridge <- ridge_cv$lambda.min


# Gives coefficients of the Ridge regression
coef_ridge = coef(ridge_reg, s = best_lambda_ridge)

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = best_lambda_ridge, newx = x_train)
result = eval_results(y_train, predictions_train,  "Ridge", "Training", sku, best_lambda_ridge )
results <- rbind(results, result)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = best_lambda_ridge, newx = x_test)
result = eval_results(y_test, predictions_test, "Ridge", "Test", sku, best_lambda_ridge)
results <- rbind(results, result)



######### Lasso ############

#Train LASSO model on training data
lasso_reg <- glmnet(x_train, y_train, alpha = 1, lambda  = lambdas, standardize = TRUE, standardize.response = TRUE)

# Using cross validation glmnet on LASSO to find best lambda
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, nfolds = 78)
best_lambda_lasso <- lasso_reg$lambda.min

# Gives coefficients of the LASSO regression
coef_lasso = coef(lasso_reg,s = best_lambda_lasso)

# Prediction and evaluation on train data
predictions_train <- predict(lasso_reg, s = best_lambda_lasso, newx = x_train)
result = eval_results(y_train, predictions_train, "Lasso", "Training", sku, best_lambda_lasso)
results <- rbind(results, result)

# Prediction and evaluation on test data
predictions_test <- predict(lasso_reg, s = best_lambda_lasso, newx = x_test)
result = eval_results(y_test, predictions_test, "Lasso", "Test", sku, best_lambda_lasso)
results <- rbind(results, result)


