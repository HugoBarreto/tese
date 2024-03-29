# Get Index data from Yahoo Finance
#
# This script will import price data from different markets from Yahoo Finance.
# The resulting datasets are serialized (saved) in rds files, to be used in the next steps.

## MAIN OPTIONS (fell free to edit it)
first_date <- '2015-10-01'
last_date <- '2022-09-30'
last_date_BGS <- '2022-10-03' # fetching american stock data from yahoo misses last date https://github.com/joshuaulrich/quantmod/issues/258
tickers <- c('VTI','QQQ','URTH', 'AGG','SCHP','GLD','TLT','BTC-USD','ETH-USD')

training_date_period <- "2016-10/2021-09"
test_date_period <- "2021-10/"

# VTI - Vanguard Total Stock Market ETF (US equities market)
# QQQ - Invesco QQQ Trust
# URTH - iShares MSCI World ETF | cover 85% of the developed world's market capitalization
# TLT - iShares 20+ Year Treasury Bond ETF | market-weighted index of debt issued by the US Treasury with remaining maturities of 20 years or more.
# AGG - iShares Core U.S. Aggregate Bond ETF | Tracks an index of US investment-grade bonds
# SCHP - Schwab U.S. TIPS ETF | cheaper expenses than TIP
# GLD - SPDR Gold Trust | Comparable ETFs: IAU, GLDM, SGOL

## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)
library(xts)

# change working directory to the script's parent folder location
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')

# download price data for "my_ticker"
raw_data <- BatchGetSymbols(tickers,
                            first.date = first_date,
                            last.date = last_date_BGS,
                            thresh.bad.data = 0.5)

# Since 16/03/2022, the complete ETH price history is not available in the source anymore. Luckly, I have stored
# its historical prices
hist_data_raw <- read_rds('data/raw-data-eth-hist.rds')
hist_data <- reshape.wide(hist_data_raw$df.tickers)$price.adjusted
hist_data_eth <- as.xts(x = subset(hist_data, select= `ETH-USD`),
                    order.by = subset(hist_data, select=ref.date, drop = TRUE))

# Omit weekends, when legacy markets do not trade. Consider only weekdays trading data
prices <- reshape.wide(raw_data$df.tickers)$price.adjusted
prices <- as.xts(x = subset(prices, select= -ref.date),
                 order.by = subset(prices, select=ref.date, drop = TRUE))

prices$`ETH-USD` <- rbind(hist_data_eth[is.na(prices$`ETH-USD`)], prices$`ETH-USD`[!is.na(prices$`ETH-USD`)])

prices <- na.omit(prices)
stopifnot(index(head(prices, 1)) == as.Date(first_date))
stopifnot(index(tail(prices, 1)) >= as.Date(last_date))
prices <- prices[paste0("/",last_date)]

# Create dataframe containing discrete daily returns
prices_rets <- prices2returns(prices) %>%
  na.omit()

prices_logrets <- prices2logreturns(prices) %>%
  na.omit()

prices_training <- prices[training_date_period]
prices_rets_training <- prices_rets[training_date_period]
prices_logrets_training <- prices_logrets[training_date_period]

prices_test <- prices[test_date_period]
prices_rets_test <- prices_rets[test_date_period]
prices_logrets_test <- prices_logrets[test_date_period]

# save data into file
write_rds(raw_data, 'data/raw-data.rds')

write_rds(prices, 'data/prices.rds')
write_rds(prices_training, 'data/prices_training.rds')
write_rds(prices_test, 'data/prices_test.rds')

write_rds(prices_rets, 'data/ret.rds')
write_rds(prices_rets_training, 'data/ret_training.rds')
write_rds(prices_rets_test, 'data/ret_test.rds')

write_rds(prices_logrets, 'data/logret.rds')
write_rds(prices_logrets_training, 'data/logret_training.rds')
write_rds(prices_logrets_test, 'data/logret_test.rds')

effr <- read.csv('data/DFF.csv')
effr <- xts(effr$DFF, order.by=as.Date(effr$DATE))/100
effr <- (effr+1)^(1/360)-1

# Return on day t+1 is traded on day t. Hence the lag.
MM <- (1000*cumprod(na.fill(lag.xts(effr), 0) + 1))[index(prices)] %>% `colnames<-`("MM")
write_rds(MM[training_date_period], 'data/MM_training.rds')
write_rds(MM[test_date_period], 'data/MM_test.rds')