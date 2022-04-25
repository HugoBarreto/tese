library(xts)
library(portfolioBacktest)
library(riskParityPortfolio)

# download price data
faang_data <- stockDataDownload(c("GOOG", "NFLX", "AAPL", "AMZN", "FB"),
                                from = "2014-01-01", to = "2019-06-25")

# define portfolios to be backtested
# risk parity portfolio
risk_parity <- function(dataset) {
  prices <- dataset$adjusted
  log_returns <- diff(log(prices))[-1]
  return(riskParityPortfolio(cov(log_returns))$w)
}

# tangency portfolio (maximum sharpe ratio)
library(quadprog)
max_sharpe_ratio <- function(dataset) {
  prices <- dataset$adjusted
  log_returns <- diff(log(prices))[-1]
  N <- ncol(prices)
  Sigma <- cov(log_returns)
  mu <- colMeans(log_returns)
  if (all(mu <= 1e-8))
    return(rep(0, N))
  Dmat <- 2 * Sigma
  Amat <- diag(N)
  Amat <- cbind(mu, Amat)
  bvec <- c(1, rep(0, N))
  dvec <- rep(0, N)
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w <- res$solution
  return(w/sum(w))
}

# call portfolioBacktest and benchmark against the uniform (1/N) portfolio
bt <- portfolioBacktest(list("risk parity portfolio" = risk_parity,
                             "tangency portfolio"    = max_sharpe_ratio),
                        list(faang_data),
                        T_rolling_window = 12*20,
                        optimize_every = 3*20, rebalance_every = 3*20)



### Denoting diff between fit and filter objects

sspec <- getspec(rmodel$garch_models_fitted$SPY)
fit1 <- ugarchfit(sspec, assets_returns[1:200, "SPY"])
spec1 <- getspec(fit1)
setfixed(spec1) <- coef(fit1)

fit2 <- ugarchfit(spec1, assets_returns[2:201, "SPY"], fit.control = list(fixed.se=1))

filter1 <- ugarchfit(spec1, assets_returns[2:201, "SPY"])

sim <- ugarchsim(filter1, 10,startMethod = "sample")

set.seed(1)
sim <- ugarchsim(fit1, 10,startMethod = "sample")
set.seed(1)
simf <- ugarchsim(fit2, 10,startMethod = "sample")


cbind(fitted(sim), fitted(simf))








######### Simulating with filtered data - mwe code snippet

#### Initial parameters

data("sp500ret")

model_spec <- ugarchspec()

sample_rolling_window <- 252
horizon <- 20
optim_freq <- 20
nSim <- 1000

T <- nrow(sp500ret)
optimize_indices  <- seq(from = sample_rolling_window, to = T, by = optim_freq)

#### How I would like to work

for(t in sample_rolling_window:(T-horizon)){
  data_window <- sp500ret[(t-sample_rolling_window+1):t]

  if (t %in% optimize_indices) {
    garch_fit <- ugarchfit(spec = model_spec, data = data_window)
    simulations <- ugarchsim(garch_fit, n.sim = horizon, m.sim = 1000, startMethod = "sample")
  } else {
    fit_spec <- getspec(garch_fit)
    setfixed(fit_spec) <- coef(garch_fit)
    garch_filter <- ugarchfilter(fit_spec, data_window)
    simulations <- ugarchsim(garch_filter, n.sim = horizon, m.sim = 1000, startMethod = "sample")
  }

  # Use simulations result to continue my analysis
  # ...
}

#### How I would expect to work, but I`m not so sure
for(t in sample_rolling_window:(T-horizon)){
  data_window <- sp500ret[(t-sample_rolling_window+1):t]

  if (t %in% optimize_indices) {
    garch_fit <- ugarchfit(spec = model_spec, data = data_window)
    simulations <- ugarchsim(garch_fit, n.sim = horizon, m.sim = 1000, startMethod = "sample")
  } else {
    fit_spec <- getspec(garch_fit)
    setfixed(fit_spec) <- coef(garch_fit)
    garch_filter <- ugarchfilter(fit_spec, data_window)
    m <- garch_filter@model$maxOrder
    presigma <- tail(garch_filter@fit$sigma, m)
    prereturns <- tail(garch_filter@fit$sigma, m)
    preresiduals <- tail(garch_filter@model$modeldata$data, m)
    simulations <- ugarchsim(garch_filter, n.sim = horizon, m.sim = 1000, startMethod = "sample",
                             presigma = presigma, prereturns = prereturns, preresiduals = preresiduals)
  }

  # Use simulations result to continue my analysis
  # ...
}



#### sgarchsim code snippet

if(startMethod == "unconditional"){
  z = rbind(matrix(0, nrow = m, ncol = m.sim), z)
} else{
  z = rbind(matrix(tail(fit@fit$z, m), nrow = m, ncol = m.sim), z)
}

# create the presample information
if(!is.na(presigma[1])){
  presigma = as.vector(presigma)
  # ...
}
if(!is.na(prereturns[1])){
  prereturns = as.vector(prereturns)
  # ...
}
if(!is.na(preresiduals[1])){
  preresiduals = as.vector(preresiduals)
  # ...
  preres = matrix(preresiduals, nrow = m)
}

# ...
for(i in 1:m.sim){
  if(is.na(preresiduals[1])){
    if(startMethod[1] == "unconditional"){
      preres = as.numeric(z[1:m, i])*presigma
    } else{
      preres = tail(resids, m)
    }
  }
  res = c(preres, rep(0, n))

  ans1 = try(.C("sgarchsimC", model = as.integer(modelinc[1:21]), pars = as.double(ipars[,1]), idx = as.integer(idx[,1]-1), h = as.double(h),

                z = as.double(z[,i]), res = as.double(res), e = as.double(res*res),

                vexdata = as.double(vexsim[[i]]), T = as.integer(n+m), m = as.integer(m), PACKAGE = "rugarch"), silent = TRUE)

  # ...
}

# sgarchsimC code

void sgarchsimC(int *model, double *pars, int *idx, double *h, double *z, double *res, double *e,
                double *vexdata, int *T, int *m)
{
  int i;
  for (i=*m; i<*T; i++)
  {
    sgarchfilter(model, pars, idx, vexdata, e, *T, i, h);
    res[i]=pow(h[i], 0.5)*z[i];
    e[i] = res[i]*res[i];
  }
}
