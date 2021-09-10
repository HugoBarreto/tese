library(quadprog)
library(riskParityPortfolio)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')

# uniform portfolio function
uniform_portfolio_fun <- function(dataset) {
  N <- ncol(dataset$adjusted)
  return(rep(1/N, N))
}

# define quintile portfolio
quintile_portfolio_fun <- function(dataset) {
  X <- discrete_returns(dataset$adjusted)[-1]  # compute returns
  N <- ncol(X)
  # design quintile portfolio
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}

# define GMVP (with heuristic not to allow shorting)
GMVP_portfolio_fun <- function(dataset) {
  X <- discrete_returns(dataset$adjusted)[-1]  # compute returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}

# define Markowitz mean-variance portfolio
# Markowitz_portfolio_fun <- function(dataset) {
#   X <- discrete_returns(dataset$adjusted)[-1]  # compute returns
#   mu <- colMeans(X)  # compute mean vector
#   Sigma <- cov(X)  # compute the SCM
#   # design mean-variance portfolio
#   w <- Variable(nrow(Sigma))
#   prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
#                   constraints = list(w >= 0, sum(w) == 1))
#   result <- solve(prob)
#   return(as.vector(result$getValue(w)))
# }

# define Markowitz mean-variance portfolio
Markowitz_portfolio_fun <- function(dataset) {
  X <- discrete_returns(dataset$adjusted)[-1]  # compute returns
  N <- ncol(X)
  Sigma <- cov(X)
  mu <- colMeans(X)
  if (all(mu <= 1e-8))
    return(rep(0, N))
  Dmat <- 2 * Sigma
  A.sumW <- rep(1, N)
  Amat <- cbind(A.sumW, diag(N))
  bvec <- c(1, rep(0, N))
  dvec <- 0.5 * mu
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w <- res$solution
  return(w/sum(w))
}

risk_parity_portfolio_fun <- function(dataset){
  X <- discrete_returns(dataset$adjusted)[-1]  # compute returns
  Sigma <- cov(X)
  w <- riskParityPortfolio(Sigma)$w
  w <- as.vector(w)
  return(w/sum(w))
}
