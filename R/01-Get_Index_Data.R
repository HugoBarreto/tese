# Get Index data from Yahoo Finance
#
# This script will import price data from different markets from Yahoo Finance.
# The resulting datasets are serialized (saved) in rds files, to be used in the next steps.
#
#! This script is no longer the main one in this step of the study. It remains here to show what has been done before data corruption
#! in the origin. The correct one to replicate the study is 01b-Get_Index_data.R

## MAIN OPTIONS (fell free to edit it)

first_date <- '2015-10-01'
last_date <- '2021-07-01' # fetching american stock data from yahoo misses last date https://github.com/joshuaulrich/quantmod/issues/258
tickers <- c('URTH', 'EEM', 'SPY', 'IEF', 'AGG', 'TIP', 'GLD', 'GSG', 'BTC-USD', 'ETH-USD')


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

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data for "my_ticker"
raw_data <- BatchGetSymbols(tickers,
                         first.date = first_date,
                         last.date = last_date)

# Omit weekends, when legacy markets do not trade. Consider only weekdays trading data
prices <- reshape.wide(raw_data$df.tickers)$price.adjusted %>%
  na.omit()

prices <- as.xts(x = subset(prices, select= -ref.date),
                    order.by = subset(prices, select=ref.date, drop = TRUE))

stopifnot(index(tail(prices, 1)) == "2021-06-30")

# Create dataframe containing discrete daily returns
prices_rets <- discrete_returns(prices) %>%
  na.omit()

pricesB2021Q2 <- prices["/2021-03"]
prices2021Q2 <- prices["2021-04/"]

prices_retsB2021Q2 <- prices_rets["/2021-03"]
prices_rets2021Q2 <- prices_rets["2021-04/"]

# save data into file
write_rds(raw_data, 'data/raw-data.rds')
write_rds(pricesB2021Q2, 'data/prices.rds')
write_rds(prices2021Q2, 'data/prices2021Q2.rds')
write_rds(prices_retsB2021Q2, 'data/ret.rds')
write_rds(prices_rets2021Q2, 'data/ret2021Q2.rds')
write_rds(index(prices_rets2021Q2), 'data/dates_2021Q2')
# write_rds(tickers, 'data/tickers.rds')