# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')

# Importing data
tickers <- read_rds('data/tickers.rds')
simulated_returns <- read_rds('data/simulated_returns.rds')

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