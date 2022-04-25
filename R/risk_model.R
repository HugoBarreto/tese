# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

library(xts)
library(tidyverse)
library(FinTS)
library(texreg)
library(rugarch)
library(Rsafd)
library(copula)


# Modeling OPTIONS
garch_model_default_spec <- list(ar_lag = 1, ma_lag = 1,
                                 arch_lag = 1, garch_lag = 1,
                                 garch_model = c('gjrGARCH'),
                                 distribution_to_estimate = 'std')
# END OPTIONS

##################################
# define garch estimation function
estimate_garch <- function(data, ticker, ar_lag=0, ma_lag=0, arch_lag=1,
                           garch_lag=1, garch_model='sGARCH',
                           distribution_to_estimate='norm') {

  message('Asset: ', ticker, ' | ', 'Estimating ARMA(',ar_lag,',', ma_lag, ')',
          '-', garch_model, '(', arch_lag, ',', garch_lag, ') ',
          'dist = ', distribution_to_estimate)

  # estimate model
  model_spec <- ugarchspec(mean.model = list(armaOrder = c(ar_lag, ma_lag)),
                        variance.model = list(model = garch_model,
                                              garchOrder = c(arch_lag, garch_lag)),
                        distribution.model = distribution_to_estimate)

  garch_fit <- ugarchfit(spec = model_spec, data = data[,ticker])

  return(garch_fit)
}

##############################################
# Fit risk model based on assets' returns data
risk_model.fit <- function(assets_returns, model_to_update=list()){
  tickers <- names(assets_returns)

  # # only update sample data
  # if(!is_empty(model_to_update)){
  #  garch_models_to_update <-  model_to_update$garch_models_fitted
  #  old_models_spec <- lapply(garch_models_to_update, function)
  # }

  # get all combinations of models
  garch_models_to_estimate <- do.call(expand_grid,
                                      c(list(ticker=tickers), garch_model_default_spec))

  # Estimate all GARCH models
  garch_models_fitted <- pmap(.l = garch_models_to_estimate, .f = estimate_garch, assets_returns)
  names(garch_models_fitted) <- tickers

  # Estimate the Semi-Parametric CDFs
  garch_residuals <- map(garch_models_fitted, (function (model) as.numeric(residuals(model, standardize=TRUE))))
  residuals_dist <- map(garch_residuals, gpd.tail, plot=F)


  # Apply respective semi-parametric cdf to each model residuals in order to get a
  # R.V. U with multivariate uniform distribution
  U.T <- map2(garch_residuals, residuals_dist, gpd.2p)
  U <- t(matrix(unlist(U.T, use.names = F), nrow = length(U.T), byrow = T))

  # Fit student-t Copula
  copula_fitted <- fitCopula(tCopula(dim=length(tickers)), U)

  return(list(garch_models_fitted=garch_models_fitted, residuals_dist=residuals_dist, copula_fitted=copula_fitted))
}

##################################################################
# Simulate assets' returns nSim times based on a fitted risk model
risk_model.simulate <- function(risk_model.fitted, horizon, nSim){
  garch_models_fitted <- risk_model.fitted$garch_models_fitted
  residuals_dist <-  risk_model.fitted$residuals_dist
  copula_fitted <-  risk_model.fitted$copula_fitted

  tickers <- names(garch_models_fitted)
  nTickers <-  length(tickers)

  # Simulate nSim*horizon independent samples from vector (u_1, ..., u_d) with
  # distribution C (a student-t copula)
  U <- rCopula(nSim*horizon, copula = copula_fitted@copula)

  # Apply the quantile function (inverse cdf) to each U vector dimension, simulating
  # innovations to the garch models that carry the copula dependence structure
  Z <- sapply(seq_along(tickers),
              function(i)
                matrix(gpd.2q(U[,i], residuals_dist[[i]]), horizon, nSim),
              simplify = "array",
              USE.NAMES = FALSE)

  # Simulate 'nSim' independent paths starting from sample data with
  # time horizon 'horizon' for each asset individual returns
  simulate_asset_garch_model_with_custom_dist <- function(j) {
    ugarchsim(garch_models_fitted[[j]],
              n.sim = horizon, m.sim = nSim,
              startMethod = "sample",
              custom.dist = list(name = "sample",
                                 # if Z is not matrix, it's dropping the dim becoming a vector
                                 # must force to be 1xnSim matrix
                                 distfit = if(!is.matrix(Z[,,j])) matrix(Z[,,j], nrow = 1)
                                 else Z[,,j]))
    }

  simulations <- lapply(1:nTickers, simulate_asset_garch_model_with_custom_dist)


  simulated_returns <- sapply(simulations, function(x) fitted(x), simplify = "array")
  dimnames(simulated_returns)[[3]] <-  tickers

  simulated_returns <- aperm(simulated_returns, c(1,3,2))

  return(simulated_returns)
}

risk_model.mspe <- function(predictions, realized, horizons){
  # prediction's horizon and number of assets must be equal to realized data
  stopifnot(dim(predictions)[1:2] == dim(realized))
  # Horizons must be in bound of both predictions and realized
  stopifnot(dim(predictions)[1] >= max(horizons))

  diff_square <- sweep(predictions, 1:2, realized)^2
  mspe <- apply(diff_square, 1:2, mean)
  mspe[horizons,]
}

#########################################################
### Evaluate the risk model based on different parameters
evaluate_risk_model <- function(assets_returns, sample_rolling_window=252, horizons=c(1,5,20), eval_freq=1, optim_freq=20, nSim=1000){

  N <- ncol(assets_returns)
  T <- nrow(assets_returns)

  min_horizon <- min(horizons)

  optimize_indices  <- seq(from = sample_rolling_window, to = T, by = optim_freq)

  # initial model
  model_mspe <-  array(dim = c(T, length(horizons), N),
                       ### index(assets_returns) não funcionando como gostaria
                       dimnames = list(index(assets_returns), paste0("T+", horizons), colnames(assets_returns)))


  # simulate, eval and update model until necessary
  for(t in sample_rolling_window:(T-min_horizon)){

    # refit model
    if (t %in% optimize_indices) {
      data_window <- assets_returns[(t-sample_rolling_window+1):t]
      model <- risk_model.fit(data_window)
    }

    # simulate data horizon up to realized available data
    sim_horizons <- horizons[horizons+t <= T]
    max_sim_horizon <-  max(sim_horizons)

    simulations <- risk_model.simulate(model, max_sim_horizon, nSim)

    realized_window <- assets_returns[(t+min_horizon):(t+max_sim_horizon)]

    # store mspe for each asset on every available horizon for each evaluation date.
    model_mspe[t,1:length(sim_horizons),] <- risk_model.mspe(simulations, realized_window, sim_horizons)
  }

  model_mspe[sample_rolling_window:(T-min_horizon),,]
}


