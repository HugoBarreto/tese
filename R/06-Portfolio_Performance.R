library(bizdays)
library(portfolioBacktest)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/portfolio_functions.R')

# Importing data
tickers <- read_rds('data/tickers.rds')
df_prices <- read_rds('data/prices.rds')
simulated_returns <- read_rds('data/simulated_returns.rds')

###
df_prices <- df_prices[,tickers]
###

# Create simulation dates based on NYSE trading calendar
cal <- create.calendar(name='NYSECalendar', holidays=holidayNYSE(2021),
                       weekdays=c('sunday', 'saturday'),
                       adjust.from=preceding, adjust.to=following)
last_day <- tail(df_prices, 1) %>% index()
simulation_horizon <- dim(simulated_returns)[1]

simulation_dates <- bizseq(offset(last_date, 1, cal),
                           offset(last_date, simulation_horizon, cal),
                           cal)

# Transform the simulated_returns 3-D array into a list of xts objects using
# simulation_dates as index to all instances
simulated_returns <- split.along.dim(simulated_returns, 3) %>%
  lapply(., as.xts, order.by=simulation_dates)


# Get a list whose each element is a xts object that contains realized and
# simulated prices, providing distinct simulated price action based on our models.
simulate_prices <- function(sim_returns, prices) {
  last_prices <- tail(prices,1) %>% coredata %>% drop
  sim_prices <- discrete_returns2prices(sim_returns, last_prices)
  result <- rbind(prices, sim_prices)
  list("adjusted"=result)
}


simulated_prices <- lapply(simulated_returns, simulate_prices, df_prices)

# Backtest portfolios
portfolios <- list("Quintile"  = quintile_portfolio_fun,
                   "GMVP"      = GMVP_portfolio_fun,
                   "Markowitz" = Markowitz_portfolio_fun)

bt <- portfolioBacktest(portfolios, simulated_prices, benchmark = "uniform", show_progress_bar = TRUE)











# Construct an equal weight portfolio
weights <- rep(1/nTickers, nTickers)

cumalative_returns <- apply(simulated_returns, 3, function(ret)
  sum(log(1 + (exp(ret) - 1) %*% weights)))

VaR = 100 * quantile(cumalative_returns, c(0.10, 0.05, 0.01))

sprintf('Maximum Simulated Loss: %8.4f%s'   , 100*min(cumalative_returns), '%')
sprintf('Maximum Simulated Gain: %8.4f%s' ,  100*max(cumalative_returns), '%')
sprintf('     Simulated 90%% VaR: %8.4f%s'  ,  VaR[1], '%')
sprintf('     Simulated 95%% VaR: %8.4f%s'  ,  VaR[2], '%')
sprintf('     Simulated 99%% VaR: %8.4f%s',  VaR[3], '%')

plot(ecdf(cumalative_returns), cex=0, col='red')
xlabel('Logarithmic Return')
ylabel('Probability')
title ('Simulated One-Month Global Portfolio Returns CDF')