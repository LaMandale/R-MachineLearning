library(data.table)      # easy to load large files
library(xts)             # time series objects
library(dplyr)           # illustrate dplyr and piping
library(ggplot2)         # draw fancy graphics & Charts
library(PerformanceAnalytics)

rv_data <- fread("oxfordmanrealizedvolatilityindices.csv")

rv_subset <- rv_data %>% 
  filter(Symbol == ".BVSP") %>%
  select(Date = V1, 
         RV = rv5, 
         close_price,
         open_to_close)

# format Date 
rv_subset$Date <- as.Date(rv_subset$Date)

rv_subset <- as.xts(rv_subset)
rv_subset$RV <- rv_subset$RV^.5 * sqrt(252)
rv_subset$rets <- CalculateReturns(rv_subset$close_price)

par(mfrow = c(3, 1) )
plot(rv_subset$rets)
plot(rv_subset$open_to_close)
plot(rv_subset$RV)

##################################
#                                #
#       Moving Averages          #
#                                #
##################################
# PART A
# Moving Averages of RV
rv_subset$MA_RV1 <- frollmean(rv_subset$RV, 1)
rv_subset$MA_RV5 <- frollmean(rv_subset$RV, 5)
rv_subset$MA_RV22 <- frollmean(rv_subset$RV, 22)
rv_subset$MA_RV66 <- frollmean(rv_subset$RV, 66)
rv_subset$MA_RV132 <- frollmean(rv_subset$RV, 132)
rv_subset$MA_RV264 <- frollmean(rv_subset$RV, 264)

# Lagged Moving Averages of RV 
rv_subset$MA_RV1 <- stats::lag(rv_subset$MA_RV1, 1)
rv_subset$MA_RV5 <- stats::lag(rv_subset$MA_RV5, 1)
rv_subset$MA_RV22 <- stats::lag(rv_subset$MA_RV22, 1)
rv_subset$MA_RV66 <- stats::lag(rv_subset$MA_RV66, 1)
rv_subset$MA_RV132 <- stats::lag(rv_subset$MA_RV132, 1)
rv_subset$MA_RV264 <- stats::lag(rv_subset$MA_RV264, 1)

# PART B
# Moving Averages of DAILY RETURNS
rv_subset$MA_Rets1 <- frollmean(rv_subset$rets, 1)
rv_subset$MA_Rets5 <- frollmean(rv_subset$rets, 5)
rv_subset$MA_Rets22 <- frollmean(rv_subset$rets, 22)
rv_subset$MA_Rets66 <- frollmean(rv_subset$rets, 66)
rv_subset$MA_Rets132 <- frollmean(rv_subset$rets, 132)
rv_subset$MA_Rets264 <- frollmean(rv_subset$rets, 264)

# Lagged Moving Averages of RV 
rv_subset$MA_Rets1 <- stats::lag(rv_subset$MA_Rets1, 1)
rv_subset$MA_Rets5 <- stats::lag(rv_subset$MA_Rets5, 1)
rv_subset$MA_Rets22 <- stats::lag(rv_subset$MA_Rets22, 1)
rv_subset$MA_Rets66 <- stats::lag(rv_subset$MA_Rets66, 1)
rv_subset$MA_Rets132 <- stats::lag(rv_subset$MA_Rets132, 1)
rv_subset$MA_Rets264 <- stats::lag(rv_subset$MA_Rets264, 1)

# PART C 
# Moving Averages of INTRADAY RETURNS
rv_subset$MA_OtC1 <- frollmean(rv_subset$open_to_close, 1)
rv_subset$MA_OtC5 <- frollmean(rv_subset$open_to_close, 5)
rv_subset$MA_OtC22 <- frollmean(rv_subset$open_to_close, 22)
rv_subset$MA_OtC66 <- frollmean(rv_subset$open_to_close, 66)
rv_subset$MA_OtC132 <- frollmean(rv_subset$open_to_close, 132)
rv_subset$MA_OtC264 <- frollmean(rv_subset$open_to_close, 264)

# Lagged Moving Averages of INTRADAY RETURNS
rv_subset$MA_OtC1 <- stats::lag(rv_subset$MA_OtC1, 1)
rv_subset$MA_OtC5 <- stats::lag(rv_subset$MA_OtC5, 1)
rv_subset$MA_OtC22 <- stats::lag(rv_subset$MA_OtC22, 1)
rv_subset$MA_OtC66 <- stats::lag(rv_subset$MA_OtC66, 1)
rv_subset$MA_OtC132 <- stats::lag(rv_subset$MA_OtC132, 1)
rv_subset$MA_OtC264 <- stats::lag(rv_subset$MA_OtC264, 1)

                          #############################
###########################

# Lagged Moving Averages Plots Realized Volatility
plot(rv_subset$MA_RV66, 
     col = "#646FD4", 
     main = "Lagged Moving Average of Realized Volatility",
     xlab = "Date", ylab = "RV",
     lwd = 2)
lines(rv_subset$MA_RV132, col = "#FF6FB5", lwd = 2)
lines(rv_subset$MA_RV264, col = "#36AE7C", lwd = 2)

# Lagged Moving Averages Plots Daily Returns
plot(rv_subset$MA_Rets66, 
     col = "#646FD4", 
     main = "Lagged Moving Average of Daily Returns",
     xlab = "Date", ylab = "Daily Returns",
     lwd = 2)
lines(rv_subset$MA_Rets132, col = "#FF6FB5", lwd = 2)
lines(rv_subset$MA_Rets264, col = "#36AE7C", lwd = 2)

# Lagged Moving Averages Plots Intraday Returns
plot(rv_subset$MA_OtC66, 
     col = "#646FD4", 
     main = "Lagged Moving Average of Intraday Returns",
     xlab = "Date", ylab = "Intraday Returns",
     lwd = 2,
     type = "s")
lines(rv_subset$MA_OtC132, col = "#FF6FB5", lwd = 2)
lines(rv_subset$MA_OtC264, col = "#36AE7C", lwd = 2)

                          #############################
###########################

#Additional indicators using (TTR) Library
require(TTR)
# We tried several indicators in order to see if Volatility and Returns could give us more information on index overall situation.

# Relative Strength Index --->
# The relative strength index (RSI) is a momentum indicator used in technical analysis that measures the magnitude of recent price changes to evaluate overbought or oversold conditions in the price of a stock or other asset.
rsi <- RSI(rv_subset$close_price)
# -> plot(rsi, type = "s") #Ploting the RSI
# With the RSI, we can see that the index is frequently underbought (under 20 RSI), and has very high peaks above 80 which could be explained by speculation and uncertainty within the market.  

# Bollinger Bands --->
# Bollinger Bands are a way to compare a security's volatility and price levels over a period of time. 
BB <- BBands(rv_subset$close_price, n = 22, sd = 2)
df <- data.frame(BB)

#Long-term Bands are "collapsing" as they are smashed by the density huge amounts of datas. Thus, Bollinger Bands do not give us additional information 
#We decided to not use them in our case study.
plot(df$mavg, type = "s",lwd = 1)
lines(df$dn, col = "#FF6FB5", lwd = 1) 
lines(df$up, col = "#36AE7C", lwd = 1)

                          #############################
###########################

# Splitting the data into training and testing sets
train_data <- rv_subset["/2019", ]
test_data <-  rv_subset["2020/", ]

# Cleaning training data
train_data <- na.omit(train_data)


#######################Developing our linear ML models#########################

#Let's start using the basic HAR model

HAR_RV <- lm(RV ~ MA_RV1 + MA_RV5 + MA_RV22,
             data = train_data)
summary(HAR_RV)

#Let's use an enhanced HAR model with our new variables

EN_HAR_RV <- lm(RV ~ MA_RV1 + MA_RV5 + MA_RV22 + MA_RV66 + MA_RV132 + MA_Rets1 + MA_Rets5 + MA_Rets22 + MA_OtC1 + MA_OtC5 + MA_OtC22,
                data = train_data)
summary(EN_HAR_RV)

#Predicting RV in the training sample with simple HAR model
train_data$pred_HAR_RV <- as.numeric(predict(HAR_RV))

#Predicting RV in the training sample with enhanced HAR model we created 
train_data$pred_EN_HAR_RV <- as.numeric(predict(EN_HAR_RV))



############### RIDGE REGRESSION ##############

#Let's start using "glmnet" library to go a little more in depth with our regression
# Side-note : might be useful to update native package 'Rcpp' for "glmnet" to work properly
library(glmnet)

#Let's find ridge penalty
EN_HAR_RV_RIDGE <- glmnet(x=data.matrix(train_data[,c('MA_RV1','MA_RV5','MA_RV22','MA_RV66','MA_RV132','MA_Rets1','MA_Rets5','MA_Rets22','MA_OtC1','MA_OtC5','MA_OtC22')]),y = train_data$RV,alpha=0)
summary(EN_HAR_RV_RIDGE)

#We'll now run k-fold cross validation in order to find an optimal value for Lambda (penalty in ridge regression)
cv_EN_HAR_RV_RIDGE <- cv.glmnet(x=data.matrix(train_data[,c('MA_RV1','MA_RV5','MA_RV22','MA_RV66','MA_RV132','MA_Rets1','MA_Rets5','MA_Rets22','MA_OtC1','MA_OtC5','MA_OtC22')]),y = train_data$RV,alpha=0)
optimal_penalty <- cv_EN_HAR_RV_RIDGE$lambda.min

#let's visualize the mean squared error given lambda
plot(cv_EN_HAR_RV_RIDGE)

#now let's develop our optimal lasso regression model
EN_HAR_RV_RIDGE <- glmnet(x=data.matrix(train_data[,c('MA_RV1','MA_RV5','MA_RV22','MA_RV66','MA_RV132','MA_Rets1','MA_Rets5','MA_Rets22','MA_OtC1','MA_OtC5','MA_OtC22')]),y = train_data$RV,alpha=0,lambda=optimal_penalty)
coef(EN_HAR_RV_RIDGE)

#Let's store our result in our train data table
train_data$pred_EN_HAR_RV_RIDGE <- as.numeric(predict(EN_HAR_RV_RIDGE, s = optimal_penalty, newx=data.matrix(train_data[,c('MA_RV1','MA_RV5','MA_RV22','MA_RV66','MA_RV132','MA_Rets1','MA_Rets5','MA_Rets22','MA_OtC1','MA_OtC5','MA_OtC22')])))



#Plotting our predictions and actual values in the mean time for year 2019
plot(train_data["2019", 
                c("RV", "pred_HAR_RV","pred_EN_HAR_RV","pred_EN_HAR_RV_RIDGE")], 
     col=c("black", "red","blue","cyan"),
     lwd = c(1,1), 
     main = "Actual vs predicted RVs", 
     legend.loc = "topleft")

#We could also do a lasso regression for optional variable reduction

#######################Developing our non linear ML models#########################

#Let's start with bagging/random forest
library(rpart)
library(caret)
library(randomForest)
library(ROCR)
library(gbm)

# to use the randomForest function, let's set the right data format
#we develop a model with all our previously used predictors in enhanced HAR model
fm.class <- as.formula( train_data$MA_RV1 + train_data$MA_RV5 + train_data$MA_RV22 + train_data$MA_RV66 + train_data$MA_RV132 + train_data$MA_Rets1 + train_data$MA_Rets5 + train_data$MA_Rets22 + train_data$MA_OtC1 + train_data$MA_OtC5 + train_data$MA_OtC22)

xtrain_rf <- model.matrix(fm.class, data = train_data)
ytrain_rf <- train_data$RV
xtest_rf <- model.matrix(fm.class, data = test_data)
ytest_rf <- test_data$RV






#Calculating and generating functions to get Mean Squared Error as well as R Squared

# create functions to calculate R2 and RMSE
MSE = function(y_actual, y_predict){
  sqrt(mean((y_actual-y_predict)^2))
}

# recall that R2 is given by 1 - SSR/SST
RSQUARE = function(y_actual,y_predict){
  1 - sum( (y_actual-y_predict)^2)/sum( (y_actual-mean(y_actual))^2)
}

MSE(train_data$RV, train_data$pred_HAR_RV)
RSQUARE(train_data$RV, train_data$pred_HAR_RV)