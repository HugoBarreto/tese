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

# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data for "my_ticker"
l_out <- BatchGetSymbols(tickers,
                         first.date = first_date,
                         last.date = last_date)

# select columns and calculated log_ret and arim_ret
df_data <- l_out$df.tickers %>%
  mutate(log_ret = log(price.adjusted/dplyr::lag(price.adjusted) ),
         arim_ret = price.adjusted/dplyr::lag(price.adjusted) - 1)

df_prices <- reshape.wide(df_data)$price.adjusted %>%
  na.omit()

row.names(df_prices) <- NULL

df_logret <- select(df_prices, !'ref.date') %>%
  as.matrix() %>%
  price2ret() %>%
  as_tibble() %>%
  mutate(ref.date = df_prices$ref.date[-1]) %>%
  na.omit()

row.names(df_logret) <- NULL

# save data into file
data_out <- 'data/raw-data.rds'
write_rds(df_data, data_out)

prices_out <- 'data/prices.rds'
write_rds(df_prices, prices_out)

logret_out <- 'data/logret.rds'
write_rds(df_logret, logret_out)

