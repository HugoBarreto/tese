# A Garch Tutorial with R - Get Index data from Yahoo Finance
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will import price data for market index Ibovespa (or any other) from Yahoo Finance.
#
# The resulting dataset is serialized (saved) in a rds file named data/RAC-GARCH-Data.rds,
# to be used in the next step.

## MAIN OPTIONS (fell free to edit it)

first_date <- '2015-10-01'
last_date <- '2021-01-31'
tickers <- c('URTH', 'EEM', 'SPY', 'IEF', 'AGG', 'TIP', 'GLD', 'GSG', 'BTC-USD', 'ETH-USD')


## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)

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
df_prices <- reshape.wide(raw_data$df.tickers)$price.adjusted %>%
  na.omit()

df_prices <- as.xts(x = subset(df_prices, select= -ref.date),
                    order.by = subset(df_prices, select=ref.date, drop = TRUE))

# Create dataframe containing discrete daily returns
df_ret <- discrete_returns(df_prices) %>%
  na.omit()

# save data into file
write_rds(raw_data, 'data/raw-data.rds')
write_rds(df_prices, 'data/prices.rds')
write_rds(df_ret, 'data/ret.rds')
# write_rds(tickers, 'data/tickers.rds')