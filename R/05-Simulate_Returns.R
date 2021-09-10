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

# Importing data
tickers <- read_rds('data/tickers.rds')
garch_models <- read_rds('data/garch_models.rds')
garch_residuals_dist <- read_rds('data/residuals_dist.rds')
fitted_copula <-read_rds('data/fitted_copula.rds')
dates_2021Q2 <- read_rds('data/dates_2021Q2')

# Set simulation horizon based on 2021 2nd quarter NYSE trading calendar


# Simulation Parameters
nTickers <-  length(tickers)
nTrials <- 2000
horizon <- length(dates_2021Q2)

# Simulate nTrials*horizon independent samples from vector (u_1, ..., u_d) with
# distribution C (a student-t copula)
U <- rCopula(nTrials*horizon, copula = fitted_copula@copula)

# Apply the quantile function (inverse cdf) to each U vector dimension, simulating
# innovations to the garch models that carry the copula dependence structure
Z <- sapply(seq_along(tickers),
                 function(i)
                   matrix(gpd.2q(U[,i], garch_residuals_dist[[i]]), horizon, nTrials),
            simplify = "array",
            USE.NAMES = FALSE)

# Verify that innovations have mean 0 and sigma 1
mu <- apply(Z, 3, mean)
sigma <- apply(Z, 3, sd)
mu_paths <-  apply(Z, c(2,3), mean)
sigma_paths <-  apply(Z, c(2,3), sd)

for (i in seq_along(mu)) {
  asset <- tickers[i]
  hist(mu_paths[,i], main = bquote(paste("Histogram of ", mu, " from ", .(asset) , " innovations" )))
  abline(v = mu[i], col="red", lwd=2)

}

for (i in seq_along(sigma)) {
  hist(sigma_paths[,i], main = bquote(paste("Histogram of ", sigma, " from ", .(asset) , " innovations" )))
  abline(v = sigma[i], col="red", lwd=2)
}

# Simulate 'nTrials' independent paths starting from sample data with
# time horizon 'horizon' for each asset individual returns
simulations <- lapply(1:nTickers,
              function(j) ugarchsim(garch_models[[j]],
                                    n.sim = horizon, m.sim = nTrials,
                                    startMethod = "sample",
                                    custom.dist = list(name = "sample",
                                                       distfit = Z[,,j])))

simulated_returns <- sapply(simulations, function(x) fitted(x), simplify = "array")
dimnames(simulated_returns)[[3]] <-  tickers

simulated_returns <- aperm(simulated_returns, c(1,3,2))

write_rds(simulated_returns, 'data/simulated_returns.rds')
