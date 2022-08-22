# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

library(tidyverse)
library(rugarch)
library(Rsafd)
library(copula)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/garch_fcts.R')

# Importing data
logret_test <- read_rds('data/logret_test.rds')
tickers <- names(logret_test)

selected_dynamic_models <- read_rds('data/selected_dynamic_models.rds')
residuals_dist <- read_rds('data/residuals_dist.rds')
fitted_copula <-read_rds('data/fitted_copula.rds')

# Set simulation horizon based on test data (2021 4th quarter NYSE trading calendar)

##### Simulation Parameters #####
nTickers <-  length(tickers)
nTrials <- 2000
horizon <- nrow(logret_test)
################################



# Simulate nTrials*horizon independent samples from vector (u_1, ..., u_d) with
# distribution C (a student-t copula)
U <- rCopula(nTrials*horizon, copula = fitted_copula@copula)



# Apply the quantile function (inverse cdf) to each U vector dimension, simulating
# innovations to the garch models that carry the copula dependence structure
stopifnot(residuals_dist %>% names == tickers) # Confirm that tickers are in the same order of residuals_dist

Z <- sapply(seq_along(tickers),
                 function(i)
                   matrix(gpd.2q(U[,i], residuals_dist[[i]]), horizon, nTrials),
            simplify = "array",
            USE.NAMES = FALSE)

# Verify that innovations have mean 0 and sigma 1
mu <- apply(Z, 3, mean)
sigma <- apply(Z, 3, sd)
mu_paths <-  apply(Z, c(2,3), mean)
sigma_paths <-  apply(Z, c(2,3), sd)

# Plot the paths' mean of innovations hist for each asset
par(mfrow=c(3,3))
for (i in seq_along(mu)) {
  asset <- tickers[i]
  hist(mu_paths[,i], main = bquote(paste("Histogram of ", mu, " from ", .(asset) , " innovations" )), xlab = expression(mu))
  abline(v = mu[i], col="red", lwd=2)
}

# Plot the paths' sigma of innovations hist for each asset
par(mfrow=c(3,3))
for (i in seq_along(sigma)) {
  asset <- tickers[i]
  hist(sigma_paths[,i], main = bquote(paste("Histogram of ", sigma, " from ", .(asset) , " innovations")), xlab = expression(sigma))
  abline(v = sigma[i], col="red", lwd=2)
}



# Simulate 'nTrials' independent paths starting from sample data with
# time horizon 'horizon' for each asset individual returns
stopifnot(selected_dynamic_models %>% names == tickers) # Confirm that tickers are in the same order of residuals_dist

simulations <- lapply(1:nTickers,
              function(j) ugarchsim(selected_dynamic_models[[j]][[1]],
                                    n.sim = horizon, m.sim = nTrials,
                                    startMethod = "sample",
                                    custom.dist = list(name = "sample",
                                                       distfit = Z[,,j])))

simulated_returns <- sapply(simulations, function(x) fitted(x), simplify = "array")
dimnames(simulated_returns)[[3]] <-  tickers

simulated_returns <- aperm(simulated_returns, c(1,3,2))

write_rds(simulated_returns, 'data/simulated_returns.rds')
