library(tidyverse)
library(xts)
library(portfolioBacktest)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/portfolio_functions.R')

## Aux Functions ------------------------------------------------------------------------------------------------------
# Get a list whose each element is a xts object that contains realized and
# simulated prices, providing distinct simulated price action based on our models.
simulate_prices <- function(sim_returns, prices) {
  last_prices <- tail(prices,1) %>% coredata %>% drop
  sim_prices <- logret2price(sim_returns, last_prices)[-1,] # drop the last training price (it's on the merge)
  result <- rbind(prices, sim_prices)
  list("adjusted"=result)
}

# add to each portfolio, a "analysis" list
portfolio_analysis <- function(bt) {
  bt_attributes <- attributes(bt)
  bt <- lapply(bt, function(.list){
    .names <- names(.list)
    .list["analysis"] = list(NULL)
    .list[c("analysis", .names)]
  })
  attributes(bt) <- bt_attributes
  return(bt)
}

add_analysis <- function(bt, name, fun) {

  fun_each_portfolio <- function(bt_portfolio){
    names_tmp <- names(bt_portfolio$analysis)
    bt_portfolio$analysis <- c(bt_portfolio$analysis,
                               list(fun(bt_portfolio[setdiff(names(bt_portfolio), "analysis")])))
    names(bt_portfolio$analysis) <- c(names_tmp, name)
    return(bt_portfolio)
  }

  bt_attributes <- attributes(bt)
  bt <- lapply(bt, fun_each_portfolio)
  attributes(bt) <- bt_attributes
  return(bt)
}


#### Portfolio Analysis Producer Functions
get_VaR <- function(port_datasets, alpha=0.95) {
  # Bind all wealth derived from simulations. Exclude initial date
  wealth <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1]
  # Calculate VaR for each row
  xts(apply(wealth, 1, quantile, probs=(1-alpha), na.rm=T), index(wealth))
}


get_CVaR <- function(port_datasets, alpha=0.95) {
  # Bind all wealth derived from simulations. Exclude initial date
  wealth <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1]
  # Calculate VaR for each row
  VaR <- apply(wealth, 1, quantile, probs=(1-alpha), na.rm=T)
  # Calculate Conditional Var a.k.a. Expected Shortfall
  coredata(wealth)[wealth >= VaR] <- NA
  xts(rowMeans(wealth, na.rm = T), index(wealth))
}

## End Aux Functions --------------------------------------------------------------------------------------------------

# Importing data
prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
prices <- rbind(prices_training,prices_test)

tickers <- names(prices)
test_dates <- index(prices_test)

simulated_returns <- read_rds('data/simulated_returns.rds')

# Transform the simulated_returns 3-D array into a list of xts objects using
# simulation_dates as index to all instances
simulated_returns <- split.along.dim(simulated_returns, 3) %>%
  lapply(as.xts, order.by=test_dates)


#? Use only last 252 days of trading data to optimize initial portfolio weights
prices_training <- tail(prices_training, 252)
prices <- prices[paste0(index(prices_training)[1],"/")] # Analyse performance on the same data period.

# Get Test Prices (wrap xts in a nested list used as input in backtest function)
realized_prices <- list("empirical_data"=list("adjusted"=prices))

# Get Simulated Prices
simulated_prices <- lapply(simulated_returns, simulate_prices, prices_training)


# Backtest portfolios
portfolios <- list("Quintile"  = quintile_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun,
                   "RiskParity" = risk_parity_portfolio_fun)

# Perform Backtest
bt_sim <- portfolioBacktest(portfolios, simulated_prices,
                            benchmark = "1/N",
                            rebalance_every=5,
                            show_progress_bar = TRUE,
                            paral_portfolios = 4)

bt_emp <- portfolioBacktest(portfolios, realized_prices,
                            benchmark = "1/N",
                            rebalance_every=5,
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





################################## Stagging ####################################

prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
prices <- rbind(prices_training,prices_test)


#? Use only last 252 days of trading data to optimize initial portfolio weights
prices_training <- tail(prices_training, 252)
prices <- prices[paste0(index(prices_training)[1],"/")] # Analyse performance on the same data period.

# Get Test Prices (wrap xts in a nested list used as input in backtest function)
realized_prices <- list("empirical_data"=list("adjusted"=prices))

# Get Simulated Prices
simulated_prices <- lapply(simulated_returns, simulate_prices, prices_training)
simulated_prices <- simulated_prices[1:100]


bt_sim <- portfolioBacktest(portfolios, simulated_prices,
                            benchmark = "1/N",
                            rebalance_every=5,
                            show_progress_bar = TRUE,
                            paral_portfolios = 4)

bt_sim <- portfolio_analysis(bt_sim)

#####

backtestTable(bt_sim, measures = names(bt_sim[[1]][[1]]$performance)) #
backtestSummary(bt_sim)$performance_summary


bt_sim <- add_analysis(bt_sim, name = "VaR", fun = get_VaR) %>%
  add_analysis(name = "CVaR", fun = get_CVaR)






































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
