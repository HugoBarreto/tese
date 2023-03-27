# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

library(tidyverse)
library(xts)
library(rugarch)
library(Rsafd)
library(copula)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/garch_fcts.R')

# For reproducibility concerns
set.seed(42)

# Importing data
prices <- read_rds('data/prices.rds')
prices_test <- read_rds('data/prices_test.rds')
logret_training <- read_rds('data/logret_training.rds')
logret_test <- read_rds('data/logret_test.rds')
MM_training <- read_rds('data/MM_training.rds')
MM_test <- read_rds('data/MM_test.rds')

MM <- rbind(MM_training, MM_test)
logret <- rbind(logret_training, logret_test)
tickers <- names(prices)

# Dates that is the origin of simulations
dates <- c(tail(index(logret_training),1),head(index(logret_test),-1))
dates.sim <- dates #tail(dates,20) # Useful to simulate only a subset - Stage only

selected_dynamic_models <- read_rds('data/selected_dynamic_models.rds')
residuals_dist <- read_rds('data/residuals_dist.rds')
fitted_copula <-read_rds('data/fitted_copula.rds')

##### Simulation Parameters #####
nTickers <-  length(tickers)
out.sample <- nrow(logret_test)
n.old <-  nrow(logret_training)
# Simulations performed per analysed date
nTrials <- 1000
# Set simulation max. horizon
max.horizon <- 21
################################

## Aux Functions ------------------------------------------------------------------------------------------------------
# Simulate log returns using sample data as starting method with uGARCHfilter
ugarchfilter.sim <- function(uGARCHfilter, uGARCHspec, n.old, ...){
  m <- uGARCHfilter@model$maxOrder
  presigma <- tail(head(sigma(uGARCHfilter), n.old), m)
  prereturns <- tail(head(uGARCHfilter@model$modeldata$data, n.old),m)
  preresiduals <- tail(head(residuals(uGARCHfilter), n.old),m)

  ugarchpath(uGARCHspec, presigma=presigma, prereturns=prereturns,
             preresiduals=preresiduals, ...)
}

# Simulate multiple paths of an asset given its filter based on its dynamic model
# Depends on: nTrials, tickers, out.sample, max.horizon, n.old
simulate_asset_paths <- function(d.sim, asset, asset.filter, asset.spec, Z){
  asset.index <- which(asset==tickers)
  horizon <- min(max.horizon, out.sample-d.sim+1)

  innovations <- Z[,,asset.index][d.sim:(d.sim+horizon-1),1:nTrials,drop=FALSE]
  sim <- ugarchfilter.sim(asset.filter, asset.spec, n.old=n.old+(d.sim-1),
                          n.sim = horizon, m.sim = nTrials,
                          custom.dist = list(name = "Innovations", distfit = innovations))
}

# Generate assets simulated prices for a specific date
generate_date_sim_prices <- function(date, date.simulations.logret){
  d.index <- which(date == index(prices))
  initial.date <- d.index - 251 # backtest lookback period (252) - 1

  generate.asset.sim.prices <- function(asset, sim.logret) {
    d.price <- prices[d.index, asset] %>% coredata %>% drop
    logreturns2prices(sim.logret, d.price)[-1,,drop=FALSE]
  }

  sim.raw.prices <- sapply(tickers, function(asset){
    generate.asset.sim.prices(asset,date.simulations.logret[[asset]])
  }, simplify = "array") #%>%
  sim.raw.prices <- aperm(sim.raw.prices, c(1,3,2))

  sim.horizon <- nrow(sim.raw.prices)
  lookback.prices <- prices[initial.date:d.index, tickers] %>% coredata
  dates <- index(prices)[initial.date:(d.index+sim.horizon)]

  MM <- generate_date_sim_MM(date)[dates]
  MM.names <- names(MM)

  apply(sim.raw.prices,3, function(sim.raw){
    dim(sim.raw) <- c(sim.horizon,length(tickers))
    res <- xts(rbind(lookback.prices, sim.raw), order.by = dates)
    res <- merge(res, MM) %>%
      `colnames<-`(c(tickers, MM.names))
    attributes(res) <- c(attributes(res), list("investable.assets"=tickers,
                                               "alternative.assets"=MM.names))
    list("prices"=res)
  }, simplify = FALSE)
}

generate_date_sim_MM <- function(date){
  d.sim <- which(date == dates)
  horizon <- min(max.horizon, out.sample-d.sim+1)

  d.index <- which(date == index(MM))

  MM.lookback <- MM[1:d.index] %>% coredata
  MM.sim <- as.numeric(MM[d.index])*cumprod(1 +
    rep(
      prices2returns(MM)[d.index],
      horizon
    ))
  xts(c(MM.lookback, MM.sim),
      order.by = index(MM)[1:(d.index+horizon)]) %>%
    `colnames<-`(names(MM))
}
## End Aux Functions --------------------------------------------------------------------------------------------------

# Simulate nTrials*out.sample independent samples from vector (u_1, ..., u_d) with
# distribution C (a student-t copula)
U <- rCopula(nTrials*out.sample, copula = fitted_copula@copula)

# Confirm that tickers are in the same order of residuals_dist
stopifnot(names(residuals_dist) == tickers)

# Apply the quantile function (inverse cdf) to each U vector dimension, simulating
# innovations to the garch models that carry the copula dependence structure
Z <- sapply(seq_along(tickers),
                 function(i)
                   matrix(gpd.2q(U[,i], residuals_dist[[i]]),
                          out.sample, nTrials),
            simplify = "array",
            USE.NAMES = FALSE)

# Verify that innovations have mean 0 and sigma 1
mu <- apply(Z, 3, mean)
sigma <- apply(Z, 3, sd)
mu_paths <-  apply(Z, c(2,3), mean)
sigma_paths <-  apply(Z, c(2,3), sd)

# Plot the paths' mean of innovations hist for each asset
pdf("figs/assetsGarchInnovationsMean.pdf")
par(mfrow=c(3,3))
for (i in seq_along(mu)) {
  asset <- tickers[i]
  hist(mu_paths[,i], main =
         bquote(paste("Histogram of ", mu, " from ", .(asset) ,
                      " innovations" )), xlab = expression(mu))
  abline(v = mu[i], col="red", lwd=2)
}
dev.off()

# Plot the paths' sigma of innovations hist for each asset
pdf("figs/assetsGarchInnovationsSigma.pdf")
par(mfrow=c(3,3))
for (i in seq_along(sigma)) {
  asset <- tickers[i]
  hist(sigma_paths[,i], main =
         bquote(paste("Histogram of ", sigma, " from ", .(asset) ,
                      " innovations")), xlab = expression(sigma))
  abline(v = sigma[i], col="red", lwd=2)
}
dev.off()

# Confirm that tickers are in the same order of residuals_dist
stopifnot(names(selected_dynamic_models) == tickers)

# Extract necessary inputs for simulation from each asset
assets.sim.inputs <- lapply(tickers, function(asset) {
  asset.garch_model <- selected_dynamic_models[[asset]][[1]]
  asset.spec <- getspec(asset.garch_model)
  setfixed(asset.spec) <- as.list(coef(asset.garch_model))
  asset.filter <- ugarchfilter(asset.spec, logret[,asset], n.old=n.old)
  list(asset=asset, filter=asset.filter, spec=asset.spec)
}) %>% set_names(tickers)

##########################################################################################################
########## The following section is computational and memory intensive - Recommended to NOT Run ##########
##########################################################################################################

# Simulate 'nTrials' independent paths with time horizon up to 'max.horizon'
# for all assets for each starting date in 'dates'
simulations <- lapply(dates.sim, function(d) {
  d.sim <- which(d == dates)
  lapply(assets.sim.inputs, function(sim.inputs){
    asset <- sim.inputs$asset
    asset.filter <- sim.inputs$filter
    asset.spec <- sim.inputs$spec

    simulate_asset_paths(d.sim, asset, asset.filter, asset.spec, Z)
  })
}) %>% set_names(dates.sim)

# Get price simulations used to backtest portfolios for each date in 'dates'
simulations.prices <- lapply(dates.sim, function(last.observed.date){
  date <- as.character(last.observed.date)

  date.simulations.logret <- simulations[[date]] %>% lapply(fitted)
  generate_date_sim_prices(date, date.simulations.logret)
}) %>% set_names(dates.sim)


write_rds(U, 'data/U-rCopula.rds')
write_rds(simulations, 'data/simulations.rds')
write_rds(simulations.prices, 'data/simulated_prices.rds')
write_rds(assets.sim.inputs, 'data/assets_filter_spec_list.rds')
# rm(U, Z, simulations) # free memory
############################################################################




























############################################################################
generate_date_sim_info <- function(date, assets, date.simulations.logret, prices){
  d.index <- which(date == index(prices))
  initial.date <- d.index - 251 # backtest lookback period (252) - 1

  generate.asset.sim.prices <- function(asset, sim.logret) {
    d.price <- prices[d.index, asset] %>% coredata %>% drop
    logreturns2prices(sim.logret, d.price)[-1,]
  }

  sim.raw.prices <- sapply(assets, function(asset){
    generate.asset.sim.prices(asset,date.simulations.logret[[asset]])
  }, simplify = "array") %>% aperm(c(1,3,2))

  sim.horizon <- nrow(sim.raw.prices)
  lookback.prices <- prices[initial.date:d.index, assets] %>% coredata
  dates <- index(prices)[initial.date:(d.index+sim.horizon)]

  apply(sim.raw.prices,3, function(sim.raw){
    dim(sim.raw) <- c(sim.horizon,length(assets))
    res <- rbind(lookback.prices, sim.raw)
    list("adjusted"=xts(res, order.by = dates))
  }, simplify = FALSE)
}


generate_date_sim_prices2 <- function(...){
  p.args <- list(...)
  date <- p.args$date
  assets <- p.args$assets
  prices <- p.args$prices
  date.simulations <- p.args$date.simulations

  d.index <- which(date == index(prices))
  initial.date <- d.index - 251

  info.extractor <- fitted
  info.transformer <- function(asset, sim.logret) {
    d.price <- prices[d.index, asset] %>% coredata %>% drop
    logreturns2prices(sim.logret, d.price)[-1,]
  }

  base.info <- prices
  result.wraper <- function(res) list("adjusted"=res)

  generate_date_sim_info(date, assets, date.simulations, info.extractor,
                         info.transformer, base.info, d.index, initial.date,
                         result.wraper)
}

generate_date_sim_sigma <- function(...){
  p.args <- list(...)
  date <- p.args$date
  assets <- p.args$assets
  sigmas <- p.args$sigmas
  date.simulations <- p.args$date.simulations

  d.index <- which(date == index(sigmas))
  initial.date <- d.index

  info.extractor <- sigma
  info.transformer <- function(asset, sim.info) sim.info

  result.wraper <- function(res) res

  generate_date_sim_info(date, assets, date.simulations, info.extractor)
}



generate_date_sim_info <- function(date, assets, date.simulations, info.extractor,
                                   info.transformer, base.info, d.index,
                                   initial.date, result.wraper){

  sim.raw.info <- sapply(assets, function(asset){
    info <- info.extractor(date.simulations[[asset]])
    info.transformer(asset, info)
  }, simplify = "array") %>% aperm(c(1,3,2))

  sim.horizon <- nrow(sim.raw.info)

  previous.info <- base.info[initial.date:d.index, assets] %>% coredata
  dates <- index(base.info)[initial.date:(d.index+sim.horizon)]

  # result.wraper
  apply(sim.raw.info,3, function(sim.raw){
    dim(sim.raw) <- c(sim.horizon,length(assets))
    res <- rbind(previous.info, sim.raw)
    result.wraper(xts(res, order.by = dates))

  }, simplify = FALSE)
}

