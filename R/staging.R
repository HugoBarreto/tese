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
