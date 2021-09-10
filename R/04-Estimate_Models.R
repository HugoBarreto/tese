# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

library(tidyverse)
library(FinTS)
library(texreg)
library(rugarch)
library(Rsafd)
library(copula)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/garch_fcts.R')

# Importing returns data
price_returns <- read_rds('data/ret.rds')

## Only for staging phase - Not Final
price_returns <- price_returns[,c('SPY','BTC-USD', 'ETH-USD')]
tickers <- exclude_element(names(price_returns), 'ref.date')

write_rds(tickers, 'data/tickers.rds')

# Modeling OPTIONS
ar_lag <- 1 # lag used for ar term in mean equation (0 in paper)
ma_lag <- 1 # lag used for ma term in mean equation (0 in paper)
arch_lag <- 1 # lag in arch effect (1 in paper)
garch_lag <- 1 # lag in garch effect (1 in paper)
garch_model <- c('gjrGARCH') # see rugarch manual for more
distribution_to_estimate <- 'std' # distribution used in all models
#my_html_file <- 'tabs/tab04-estimation_garch.html' # where to save html file?
# END OPTIONS

# close all opened windows
graphics.off()


# define garch estimation function
estimate_garch <- function(data,
                           ticker,
                           ar_lag=0,
                           ma_lag=0,
                           arch_lag=1,
                           garch_lag=1,
                           garch_model='sGARCH',
                           distribution_to_estimate='norm') {

  message('Asset: ', ticker, ' | ', 'Estimating ARMA(',ar_lag,',', ma_lag, ')',
          '-', garch_model, '(', arch_lag, ',', garch_lag, ') ',
          'dist = ', distribution_to_estimate)

  # estimate model
  my_spec <- ugarchspec(mean.model = list(armaOrder = c(ar_lag, ma_lag)),
                        variance.model = list(model = garch_model,
                                              garchOrder = c(arch_lag, garch_lag)),
                        distribution.model = distribution_to_estimate)

  my_garch <- ugarchfit(spec = my_spec, data = data[,ticker])

  return(my_garch)
}



# get all combinations of models
models_to_estimate <- expand_grid(ticker = tickers,
                       ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       garch_model,
                       distribution_to_estimate)


# Estimate all GARCH models
garch_models <- pmap(.l = models_to_estimate, .f = estimate_garch, price_returns)
names(garch_models) <- tickers

write_rds(garch_models, 'data/garch_models.rds')

## Utilizando o default da função gpd.tails
## Uma melhora seria escolher os thresholds com base no resultado
## da função shape.plot

# Estimate the Semi-Parametric CDFs
models_residuals <- map(garch_models, (function (model) as.numeric(residuals(model, standardize=TRUE))))
residuals_dist <- map(models_residuals, gpd.tail, plot=F)

write_rds(residuals_dist, 'data/residuals_dist.rds')

# Apply respective semi-parametric cdf to each model residuals in order to get a
# R.V. U with multivariate uniform distribution
U.T <- map2(models_residuals, residuals_dist, gpd.2p)
U <- t(matrix(unlist(U.T, use.names = F), nrow = length(U.T), byrow = T))

# Fit student-t Copula
fitted_copula <- fitCopula(tCopula(dim=length(tickers)), U)

write_rds(fitted_copula, 'data/fitted_copula.rds')

