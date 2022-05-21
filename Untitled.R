library(data.table)      # easy to load large files
library(xts)             # time series objects
library(dplyr)           # illustrate dplyr and piping
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
rv_subset$MA_RV20 <- frollmean(rv_subset$RV, 20)
rv_subset$MA_RV40 <- frollmean(rv_subset$RV, 40)
rv_subset$MA_RV80 <- frollmean(rv_subset$RV, 80)
rv_subset$MA_RV160 <- frollmean(rv_subset$RV, 160)

# Lagged Moving Averages of RV 
rv_subset$MA_RV1 <- stats::lag(rv_subset$MA_RV1, 1)
rv_subset$MA_RV5 <- stats::lag(rv_subset$MA_RV5, 1)
rv_subset$MA_RV20 <- stats::lag(rv_subset$MA_RV20, 1)
rv_subset$MA_RV40 <- stats::lag(rv_subset$MA_RV40, 1)
rv_subset$MA_RV80 <- stats::lag(rv_subset$MA_RV80, 1)
rv_subset$MA_RV160 <- stats::lag(rv_subset$MA_RV160, 1)

# PART B
# Moving Averages of DAILY RETURNS
rv_subset$MA_Rets1 <- frollmean(rv_subset$rets, 1)
rv_subset$MA_Rets5 <- frollmean(rv_subset$rets, 5)
rv_subset$MA_Rets22 <- frollmean(rv_subset$rets, 22)
rv_subset$MA_Rets44 <- frollmean(rv_subset$rets, 44)
rv_subset$MA_Rets88 <- frollmean(rv_subset$rets, 88)
rv_subset$MA_Rets186 <- frollmean(rv_subset$rets, 186)

# Lagged Moving Averages of RV 
rv_subset$MA_Rets1 <- stats::lag(rv_subset$MA_Rets1, 1)
rv_subset$MA_Rets5 <- stats::lag(rv_subset$MA_Rets5, 1)
rv_subset$MA_Rets22 <- stats::lag(rv_subset$MA_Rets22, 1)
rv_subset$MA_Rets44 <- stats::lag(rv_subset$MA_Rets44, 1)
rv_subset$MA_Rets88 <- stats::lag(rv_subset$MA_Rets88, 1)
rv_subset$MA_Rets186 <- stats::lag(rv_subset$MA_Rets186, 1)

# PART C 
# Moving Averages of INTRADAY RETURNS
rv_subset$MA_OtC1 <- frollmean(rv_subset$open_to_close, 1)
rv_subset$MA_OtC5 <- frollmean(rv_subset$open_to_close, 5)
rv_subset$MA_OtC20 <- frollmean(rv_subset$open_to_close, 20)
rv_subset$MA_OtC40 <- frollmean(rv_subset$open_to_close, 40)
rv_subset$MA_OtC80 <- frollmean(rv_subset$open_to_close, 80)
rv_subset$MA_OtC160 <- frollmean(rv_subset$open_to_close, 160)

# Lagged Moving Averages of INTRADAY RETURNS
rv_subset$MA_OtC1 <- stats::lag(rv_subset$MA_OtC1, 1)
rv_subset$MA_OtC5 <- stats::lag(rv_subset$MA_OtC5, 1)
rv_subset$MA_OtC20 <- stats::lag(rv_subset$MA_OtC20, 1)
rv_subset$MA_OtC40 <- stats::lag(rv_subset$MA_OtC40, 1)
rv_subset$MA_OtC80 <- stats::lag(rv_subset$MA_OtC80, 1)
rv_subset$MA_OtC160 <- stats::lag(rv_subset$MA_OtC160, 1)

#Additional indicators uusing (TTR) Library
require(TTR)

# Relative Strength Index
rsi <- RSI(rv_data$close_price) # Draw un petit plot sympa avec le RSI ???

# MACD
macd  <- MACD(rv_data$close_price, 12, 26, 9, maType="EMA" ) # Draw un petit plot sympa avec MACD ??? Variable rv_data$close_price à confirmer

#Mosaique résumé des graphs à faire