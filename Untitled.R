library(data.table)      # easy to load large files
library(xts)             # time series objects
library(dplyr)           # illustrate dplyr and piping
library(ggplot2)         # draw fancy graphics & Charts
library(PerformanceAnalytics)

rv_data <- fread("oxfordmanrealizedvolatilityindices.csv")

rv_subset <- rv_data %>% 
  filter(Symbol == ".SPX") %>%
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