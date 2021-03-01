###############################################
# Superstore Sales Data Analysis
###############################################

# Removing installed lists
rm(list=ls())

# Install google drive packages
install.packages("googledrive", repos = "https://cran.rstudio.com/")
install.packages("janitor")
install.packages("plyr")
install.packages('psych')
install.packages("dplyr")

# Loading libraries
library("googledrive")
library("janitor")
library("plyr")
library("dplyr")
library(psych)
library(ggplot2)

################################################
### Reading csv file
################################################

#output the current working path
getwd()

#setup a new working path
setwd("C:/Users/padma/Documents/Fall 2020 - sem 2/Data analytics/Project/report")

# Reading the .csv file from google drive link
superstore_data <- read.csv(file = "superstore")

# Formatting column names by replacing space with underscore
superstore_data = clean_names(superstore_data)

# View record
View(superstore_data)

# Output the top rows
head(superstore_data)

# Features of Data
names(superstore_data)

# Summarize structure of an object
str(superstore_data)

# Get the number of rows and coulmns of data
dim(superstore_data)
###############################################################################
### Check for missing Records
#       Only postal_code has missing records and postal code is not useful for 
#       Statistical Analysis.
###############################################################################
na_count = sapply(superstore_data, function(x) sum(is.na(x)))
na_count = data.frame(na_count)
na_count

# Removing these ID columns as they are not useful for statistical Analysis
superstore_data$row_id <- NULL
superstore_data$order_id <- NULL
superstore_data$order_date <- NULL
superstore_data$customer_id <- NULL
superstore_data$customer_name <- NULL
superstore_data$postal_code <- NULL
superstore_data$product_id <- NULL
superstore_data$product_name <- NULL
head(superstore_data)

# Get the number of rows and coulmns of data
dim(superstore_data)

###############################################################################
# Hypothesis Testing
###############################################################################
#Anova1 - Sales vs Category 
#F-Test Hypothesis
#Null Hypothesis: The mean of sales in all category is same.
#Alternate Hypothesis: The mean of sales in all category is not same, i.e. atleast one or two groups have
# diffrent mean.

# Creating new anovadf1 dataframe
anovadf1 <- superstore_data[, c("sales", "category")]
# Removing space between category groups
#anovadf1$category = sub(' ', '', anovadf1$category)
head(anovadf1)
anovadf1
sales = anovadf1$sales
category = anovadf1$category
# Diffrences among category groups are not visible through side by side box plots
plot(sales~category)

# Build anova model for sales~ category
anova1=lm(sales~category)
summary(anova1)

# Residual Analysis checking constance variance and predicted vs residuals
# From the plot it is clear that spread is not constant
plot(fitted(anova1), rstandard(anova1), main = "Predicted vs Residuals Plot")
abline(a=0, b=0, col='red')

# Normality Q-Q plot
# From the plot we could see that points are not around the line
qqnorm(rstandard(anova1))
qqline(rstandard(anova1), col=2)

# Performing Transformation

# log Transformation on Sales
anova11=lm(log(sales)~category)
summary(anova11)

# sqrt Transformation
anova12=lm(sqrt(sales)~category)
summary(anova12)

# inverse Transformation
anova13=lm((1/sales)~category)
summary(anova13)

# The log transformation has shown improvement
# Residual Analysis- Could observe log transformed sales variables are scattered around zero line
plot(fitted(anova11), rstandard(anova11))
abline(a=0, b=0, col='red')

# Normality Distribution Q-Q plot - could see points are distrubted around normal line
qqnorm(rstandard(anova11))
qqline(rstandard(anova11), col=2)

plot(log(sales)~category)
# Fitted anova model with 95% confidence level
summary(anova11)

#run anova
anovaa = aov(anova11)
summary(anovaa)

# From the summary output, one can interpret that there is a significant difference (i.e. P < 0.001)
# between the category
# F-Test Hypothesis
# We could observe that F-test p-value < 0.05 so, we reject NULL hypothesis and accept 
# that at least 2 category groups have diffrent average sales

#tukey's test
data.test1 <- TukeyHSD(anovaa, conf.level=0.95)
data.test1
#From the Tukey's test with log transformation of Sales, we conclude that there is significant difference in all category group 
#at adjusted p-value < 0.05.
#The technology category has higher sales and office-supplies category has least sales.

#################################################################################################

#Anova2 - Sales vs Market 

#F-Test Hypothesis
#Null Hypothesis: Mean of all market groups are the same.
#Alternate Hypothesis: Not all the market groups of means are the same.

# Creating new anovadf2 dataframe
anovadf2 <- superstore_data[, c("sales", "market")]
head(anovadf2)
sales1 = anovadf2$sales
market = anovadf2$market
# Diffrences among market groups are not visible through side by side box plots
plot(sales1~market)

# Build anova model for sales~ market
anova2=lm(sales1~market)
summary(anova2)

# Residual Analysis checking constance variance and predicted vs residuals
# From the plot it is clear that spread is not constant
plot(fitted(anova2), rstandard(anova2), main = "Predicted vs Residuals Plot")
abline(a=0, b=0, col='red')

# Normality Q-Q plot
# From the plot we could see that points are not around the line
qqnorm(rstandard(anova2))
qqline(rstandard(anova2), col=2)

# Performing Transformation

# log Transformation on Sales1
anova21=lm(log(sales1)~market)
summary(anova21)

# sqrt Transformation
anova22=lm(sqrt(sales1)~market)
summary(anova22)

# inverse Transformation
anova23=lm((1/sales1)~market)
summary(anova23)

# The log transformation has shown improvement
# Residual Analysis- Could observe variables are scattered around zero line
plot(fitted(anova21), rstandard(anova21))
abline(a=0, b=0, col='red')

# Normality Distribution Q-Q plot - could see points are distrubted around normal line
qqnorm(rstandard(anova21))
qqline(rstandard(anova21), col=2)

# Fitted anova model with 95% confidence level
summary(anova21)
plot(log(sales1)~ market)
describe(anovadf2)

#run anova 
anovaaa = aov(anova21)
summary(anovaaa)
# F-Test Hypothesis
# We could observe that F-test p-value < 2.2e-16 so, we reject NULL hypothesis and accept 
# that at least 2 market groups have diffrent average sales

#tukey's test 
data.test <- TukeyHSD(anovaaa, conf.level=0.95)
data.test
plot(data.test)

#From the Tukey's test with log transformation of Sales, we conclude that there is significant difference in all other market group 
#at adjusted p-value < 0.05, except between the groups Canada-Africa, EMEA-Africa, US-Africa, EU-APAC, EMEA-Canada, LATAM-Canada, 
#US-EMEA and US-Canada.
#The APAC Market group has larger sales and Africa has least number of sales.


############################################################
# Data Preprocessing
############################################################
# As we are using Market and region for further ignoring rest of the coulmns
superstore_data$ship_date <- NULL
superstore_data$customer_name <- NULL
superstore_data$order_priority <- NULL
superstore_data$city <- NULL
superstore_data$state <- NULL
superstore_data$country <- NULL

###############################################################
# Creating N-1 Dummy Variables
###############################################################

# Creating dummy variables
library(tidyverse)
# Fetching Categorical Variables
categorical=superstore_data %>% select_if(negate(is.numeric))

# Chi-Square-Test to check the relationship between market and region
chitest1 = table(categorical$market, categorical$region)
chisq.test(chitest1)
# As p-value is less than 0.05 indicates market and region are not independent
# hence ignoring market for further process
categorical$market <- NULL

# Chi-Square-Test to check the relationship between Category and Sub-Category
chitest2 = table(categorical$category, categorical$sub_category)
chisq.test(chitest2)
# As p-value is less than 0.05 indicates Category and Sub-Category are not independent
# hence ignoring category for further process
categorical$category <- NULL

# Creating n-1 dummy variables
dummydf<- data.frame(sapply(categorical,function(x) data.frame(model.matrix(~x-1,data =categorical))[,-1]))
dim(dummydf)

dummydf = clean_names(dummydf)
names(dummydf)

######################################################################
# Correlation
######################################################################

checknumericvar = sapply(superstore_data, is.numeric)
# Fetching numeric features
numericvar = superstore_data[checknumericvar]

# checking Correlation between numeric features
corr=cor(numericvar)
#install.packages("corrplot")
library(corrplot)
corrplot(corr, method="circle")
corr

# Applying Transformation to improve weak correlation
# Transformation Quantity
quantity1 = log(numericvar$quantity)
cor(numericvar$profit, quantity1)

quantity2 = sqrt(numericvar$quantity)
cor(numericvar$profit, quantity2)

quantity3 = (1/numericvar$quantity)
cor(numericvar$profit, quantity3)

# Transformation Sales
sales1 = log1p(numericvar$sales)
cor(numericvar$profit, sales1)

sales2 = sqrt(numericvar$sales)
cor(numericvar$profit, sales2)

sales3 = (1/numericvar$sales)
cor(numericvar$profit, sales3)

# Transformation Discount
discount1 = log1p(numericvar$discount)
cor(numericvar$profit, discount1)

discount2 = sqrt(numericvar$discount)
cor(numericvar$profit, discount2)

discount3 = (1/numericvar$discount)
cor(numericvar$profit, discount3)

# Transformation of shipping cost
shipping_cost1 = log1p(numericvar$shipping_cost)
cor(numericvar$profit, shipping_cost1)

shipping_cost2 = sqrt(numericvar$shipping_cost)
cor(numericvar$profit, shipping_cost2)

shipping_cost3 = (1/numericvar$shipping_cost)
cor(numericvar$profit, shipping_cost3)

# No improvement in corr of Qunatity hence removing quantity feature
numericvar$quantity <- NULL
corr=cor(numericvar)
corr

final_df=data.frame(numericvar, dummydf)
head(final_df)

#######################################################
# Building a model
#######################################################

library(car)

# Function to rebuild model by removing variables having p-value >= 0.05
remove_non_sig_var <- function(model, df)
{
  all_x_variables <- names(model[[1]])[-1]  # names of all X variables
  # Get the summary of variables
  modelsummary <- summary(model)  # fetching summary of model
  pvalues <- modelsummary[[4]][, 4]  # getting all pvalues
  non_sig_x_var <- character()  # init variables that aren't statsitically significant
  non_sig_x_var <- names(which(pvalues >= 0.05)) # fetch records which are having p-value >= 0.05
  non_sig_x_var <- non_sig_x_var[!non_sig_x_var %in% "(Intercept)"] 
  # If there are any non-significant variables, 
  while(length(non_sig_x_var) > 0){
    all_x_variables <- all_x_variables[!all_x_variables %in% non_sig_x_var[1]]
    regformula <- as.formula(paste("profit ~ ", paste (all_x_variables, collapse=" + "), sep=""))  # new formula
    newmodel <- lm(regformula, data=df)  # re-build model with new formula
    # Get the non-significant vars from the rebuilt model to loop through again.
    newmodelsummary <- summary(newmodel)
    pvalues <- newmodelsummary[[4]][,4]
    non_sig_x_var <- character()
    non_sig_x_var <- names(which(pvalues >= 0.05))
    non_sig_x_var <- non_sig_x_var[!non_sig_x_var %in% "(Intercept)"]
  }
  return(newmodel)
}


#number of total sales data
dim(final_df)
#Fetching 70% of total sales row data as train data
train.data = final_df[1:floor(0.7*nrow(final_df)),]
nrow(train.data)
#Fetching rest 30% of row data as test data
test.data = final_df[floor(0.7*nrow(final_df)+1):nrow(final_df),]
nrow(test.data)

# Building Full Model
fullmodel = lm(profit~., data = train.data)
summary(fullmodel) #Adj-R2 = 0.3337

# Checking for multicollinearity
vifs=vif(fullmodel)
vifs

fullcolumnames = names(vifs)
fullcolumnames

# Running backward elimination model by P-Value >= 0.05 and rebuild a model
eliminationmodel = remove_non_sig_var(fullmodel, train.data)
summary(eliminationmodel) # Adj-R2 = 0.3338

#Performing Residual Analysis on eliminationmodel
res1=rstandard(eliminationmodel)
#Constant Varience
plot(fitted(eliminationmodel), res1, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for reduced model
qqnorm(res1)
qqline(res1,col=2)

# Try Transformation
# Applying log transformation on Y variable could see there is no improvement
eliminationmodel_var <- names(eliminationmodel[[1]])[-1] 
str(eliminationmodel_var)
logformula1 <- as.formula(paste("log(profit) ~ ", paste (eliminationmodel_var, collapse=" + "), sep=""))  # new formula
logeliminationmodel3 <- lm(logformula1, data=train.data)
summary(logeliminationmodel3) 

#Performing Residual Analysis on logeliminationmodel3
reslog1=rstandard(logeliminationmodel3)
#Constant Varience
plot(fitted(logeliminationmodel3), reslog1, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for transformed model
qqnorm(reslog1)
qqline(reslog1,col=2)

#SQRT Transformation on Y variable
sqrtformula1 <- as.formula(paste("sqrt(profit) ~ ", paste (eliminationmodel_var, collapse=" + "), sep=""))  # new formula
sqrteliminationmodel3 <- lm(sqrtformula1, data=train.data)
summary(sqrteliminationmodel3)

#Performing Residual Analysis on sqrteliminationmodel3
ressqrt1=rstandard(sqrteliminationmodel3)
#Constant Varience
plot(fitted(sqrteliminationmodel3), ressqrt1, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for transformed model
qqnorm(ressqrt1)
qqline(ressqrt1,col=2)

# Inverse Transformation on Y variable
invformula1 <- as.formula(paste("(1/profit) ~ ", paste (eliminationmodel_var, collapse=" + "), sep=""))  # new formula
inveliminationmodel3 <- lm(invformula1, data=train.data)
summary(inveliminationmodel3)

#Performing Residual Analysis on inveliminationmodel3
resinv1=rstandard(inveliminationmodel3)
#Constant Varience
plot(fitted(inveliminationmodel3), resinv1, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for inv transformed model
qqnorm(resinv1)
qqline(resinv1,col=2)

# The transformations on Y has not shown any improvement in linearity and normality.
# Check for influential points in elimination model
cooksd1 = cooks.distance(eliminationmodel)
n = nrow(train.data)
plot(cooksd1, main="Influential Points")
abline(h = 4/n, lty=2, col="green")
influential_points1 = as.numeric(names(cooksd1[cooksd1 > (4/n)]))
#influential_points1
newtrain.data12 <- train.data[-influential_points1,]
nrow(newtrain.data12)

# Rebuilding model after removing influential points
eliminationmodel_var <- names(eliminationmodel[[1]])[-1] 
regformula1 <- as.formula(paste("profit ~ ", paste (eliminationmodel_var, collapse=" + "), sep=""))  # new formula
eliminationmodel3 <- lm(regformula1, data=newtrain.data12)
summary(eliminationmodel3) # Adj-R2 = 0.557

# As there are still non significant variables rebuilding model by removing them
eliminationmodel4 = remove_non_sig_var(eliminationmodel3, newtrain.data12)
summary(eliminationmodel4) # Adj-R2 = 0.557

#Performing Residual Analysis on elliminationmodel
res2=rstandard(eliminationmodel4)
#Constant Varience
plot(fitted(eliminationmodel4), res2, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for reduced model
qqnorm(res2)
qqline(res2,col=2)

############################################################################
# Feature Selection - Stepwise Both Model
############################################################################
# Buidling stepwise both model
# Fetching subset of useful variables from train data set
train1.data=subset(train.data ,select= fullcolumnames)
train1.data$profit = train.data$profit
fullmdl <- lm(profit~., data=train1.data)
stepwisebothmodel = step(fullmdl, direction="both", trace=F)
summary(stepwisebothmodel) # Adj-R2 0.3339

#Performing Residual Analysis on backwrdmodel
res3=rstandard(stepwisebothmodel)
#Constant Varience
plot(fitted(stepwisebothmodel), res3, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for reduced model
qqnorm(res3)
qqline(res3,col=2)

#Taking the vif for all columns built in the stepwise both model
vifs2=vif(stepwisebothmodel)
vifs2

# Check for influential points in stepwise both model
cooksd2 = cooks.distance(stepwisebothmodel)
n = nrow(train1.data)
plot(cooksd2, main="Influential Points")
abline(h = 4/n, lty=2, col="green")
influential_points2 = as.numeric(names(cooksd2[cooksd2 > (4/n)]))
newtrain.data2 <- train1.data[-influential_points2,]
nrow(newtrain.data2)

# REbuildig model by removing influential points
stepwisebothmodel12 <- lm(profit~., data=newtrain.data2)
stepwisebothmodel14 = step(stepwisebothmodel12, direction="both", trace=F)
summary(stepwisebothmodel14) # Adj-R2 0.5549

# Rebuilding model by removing non-significant variables
stepwisebothmodel15 = remove_non_sig_var(stepwisebothmodel14, newtrain.data2)
summary(stepwisebothmodel15) # Adj-R2 = 0.5548

#Performing Residual Analysis on backwrdmodel
resboth=rstandard(stepwisebothmodel15)
#Constant Varience
plot(fitted(stepwisebothmodel15), resboth, main="Plot residuals vs predicted values")
abline(a=0, b=0, col='red')

#Check normal Distribution for reduced model
qqnorm(resboth)
qqline(resboth,col=2)


################################################################################
# RMSE Calculation on train.data
################################################################################
y1 = predict.glm(eliminationmodel4, train.data)
y2 = predict.glm(stepwisebothmodel15, train.data)
y = train.data$profit
rmse_1 = sqrt(((y-y1)%*%(y-y1)) /nrow(train.data))
rmse_1
rmse_2 = sqrt(((y-y2)%*%(y-y2)) /nrow(train.data))
rmse_2

#################################################################################
# RMSE Calculation on test.data
#################################################################################
x1 = predict.glm(eliminationmodel4, test.data)
x2 = predict.glm(stepwisebothmodel15, test.data)
x = test.data$profit
rmse_1 = sqrt(((x-x1)%*%(x-x1)) /nrow(test.data))
rmse_1
rmse_2 = sqrt(((x-x2)%*%(x-x2)) /nrow(test.data))
rmse_2

############################################################################
# Regularization
############################################################################
library(caret)
dummies <- dummyVars(profit~., data = train.data)
train_dummies = predict(dummies, newdata = train.data)
test_dummies = predict(dummies, newdata = test.data)
print(dim(train_dummies)); print(dim(test_dummies))

# Custom function to Compute R-square from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  # Calculate R-square value
  R_square <- 1 - SSE / SST
  # Calculate RMSE
  RMSE = sqrt(SSE/nrow(df))
  # Model performance metrics RMSE and R_square
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# installing packages
#install.packages("glmnet")
library(glmnet)
x_train = as.matrix(train_dummies)
y_train = train.data$profit
x_test = as.matrix(test_dummies)
y_test = test.data$profit
############################################################################
# Ridge Regression
############################################################################
grid <- 10^seq(2, -3, by = -.1)
# The alpha=0 implies Ridge penalty
ridge_reg = glmnet(x_train, y_train, nlambda = 100, alpha = 0, family = 'gaussian', lambda = grid)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = grid)
plot(cv_ridge)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda
ridge_coeff = predict(cv_ridge, s=optimal_lambda, type="coefficients")[1:37, ]

# Prediction and evaluation on train data ridge model
predictions_train <- predict(ridge_reg, s=optimal_lambda, newx=x_train)
eval_results(y_train, predictions_train, train.data)
# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s=optimal_lambda, newx=x_test)
eval_results(y_test, predictions_test, test.data)

##############################################################################
# Lasso Regression
##############################################################################
grid <- 10^seq(2, -3, by = -.1)
#lambdas <-10^seq(10, -2, length = 100)
# Setting alpha = 1 implies lasso penalty
lasso_reg <- cv.glmnet(x_train, y_train, alpha=1, lambda=grid, standardize=TRUE, nfolds=10)
plot(lasso_reg)
lambda_best <- lasso_reg$lambda.min
lambda_best
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)
lasso.coef = predict(lasso_model, s=lambda_best, type="coefficients")[1:37, ]
lasso.coef[lasso.coef !=0]


# Prediction and evaluation on train data using Lasso model
predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train)
eval_results(y_train, predictions_train, train.data)
#Prediction and evaluation on test data
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test.data)


# compare co-efficients
library(data.table)
coef = data.table(lasso = lasso.coef,
                  ridge = ridge_coeff)
coef[,feature := names(ridge_coeff)]
to_plot = melt(coef, id.vars='feature', variable.name = 'model', value.name='coefficient')
ggplot(to_plot, aes(x=feature, y=coefficient, fill=model)) + coord_flip() +
  geom_bar(stat = 'identity') + facet_wrap(~model) + guides(fill=FALSE)

