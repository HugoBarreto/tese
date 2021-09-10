library(tidyverse)
library(xts)
library(portfolioBacktest)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/portfolio_functions.R')

# Importing data
tickers <- read_rds('data/tickers.rds')

prices <- read_rds('data/prices.rds')
prices2021Q2 <- read_rds('data/prices2021Q2.rds')
empirical_prices <- rbind(prices,prices2021Q2)

simulated_returns <- read_rds('data/simulated_returns.rds')
dates_2021Q2 <- read_rds('data/dates_2021Q2')

#######
prices <- prices[,tickers]
empirical_prices <- empirical_prices[, tickers]
#######

# wrap xts in a nested list used as input in backtest function
empirical_prices <- list("empirical_data"=list("adjusted"=empirical_prices))

# Transform the simulated_returns 3-D array into a list of xts objects using
# simulation_dates as index to all instances
simulated_returns <- split.along.dim(simulated_returns, 3) %>%
  lapply(., as.xts, order.by=dates_2021Q2)

# Use only last 252 days of trading data to optimize initial portfolio weights
prices <- tail(prices, 251)

# Get a list whose each element is a xts object that contains realized and
# simulated prices, providing distinct simulated price action based on our models.
simulate_prices <- function(sim_returns, prices) {
  last_prices <- tail(prices,1) %>% coredata %>% drop
  sim_prices <- discrete_returns2prices(sim_returns, last_prices)
  result <- rbind(prices, sim_prices)
  list("adjusted"=result)
}


simulated_prices <- lapply(simulated_returns, simulate_prices, prices)

# Backtest portfolios
portfolios <- list("Uniform"  = uniform_portfolio_fun,
                   "Quintile"  = quintile_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun,
                   "RiskParity" = risk_parity_portfolio_fun)

bt_sim <- portfolioBacktest(portfolios, simulated_prices,
                        show_progress_bar = TRUE,
                        paral_portfolios = 4)

bt_emp <- portfolioBacktest(portfolios, empirical_prices,
                            show_progress_bar = TRUE)

write_rds(bt_sim, 'data/backtest_portfolios_simulated_data.rds')
write_rds(bt_emp, 'data/backtest_portfolios_realized_data.rds')

# plot cumulative returns chart (realized data)
backtestChartCumReturns(bt_emp)

# plot max drawdown chart (realized data)
backtestChartDrawdown(bt_emp)

# bactest summary - Meadian and Mean
bt_summary_mean <- backtestSummary(bt_sim, summary_fun = mean)
bt_summary_median <- backtestSummary(bt_sim, summary_fun = median)













#
#
# # Construct an equal weight portfolio
# weights <- rep(1/nTickers, nTickers)
#
# cumalative_returns <- apply(simulated_returns, 3, function(ret)
#   sum(log(1 + (exp(ret) - 1) %*% weights)))
#
# VaR = 100 * quantile(cumalative_returns, c(0.10, 0.05, 0.01))
#
# sprintf('Maximum Simulated Loss: %8.4f%s'   , 100*min(cumalative_returns), '%')
# sprintf('Maximum Simulated Gain: %8.4f%s' ,  100*max(cumalative_returns), '%')
# sprintf('     Simulated 90%% VaR: %8.4f%s'  ,  VaR[1], '%')
# sprintf('     Simulated 95%% VaR: %8.4f%s'  ,  VaR[2], '%')
# sprintf('     Simulated 99%% VaR: %8.4f%s',  VaR[3], '%')
#
# plot(ecdf(cumalative_returns), cex=0, col='red')
# xlabel('Logarithmic Return')
# ylabel('Probability')
# title ('Simulated One-Month Global Portfolio Returns CDF')
#
#
#
#
