library(quadprog)
library(riskParityPortfolio)

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')

##### NOTE
# For all portfolio functions, it's considered that
# dataset[[1]] is the adjusted price xts
##### END NOTE

# uniform portfolio function | Also implemented as benchmark
ewp <- function(dataset, ...) {
  d <- ncol(dataset[[1]])
  return(rep(1/d, d))
}


# define Markowitz mean-variance portfolio
Markowitz.p <- function(dataset, lambda=0.5, ...) {
  X <- price2logret(dataset[[1]])[-1]  # compute returns
  d <- ncol(X)
  Sigma <- cov(X)
  mu <- colMeans(X)

  if (all(mu <= 1e-8))
    return(rep(0, d))

  # onde vai o lambda?
  Dmat <- 2 * Sigma
  A.sumW <- rep(1, d)
  Amat <- cbind(A.sumW, diag(d))
  bvec <- c(1, rep(0, d))
  dvec <- 0.5 * mu
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w <- res$solution
  return(w/sum(w))
}


max.sharpe.ratio.p <- function(dataset, ...) {
  log_returns <- price2logret(dataset[[1]])[-1]
  d <- ncol(log_returns)
  Sigma <- cov(log_returns)
  mu <- colMeans(log_returns)

  if (all(mu <= 1e-8))
    return(rep(0, d))

  Dmat <- 2 * Sigma
  Amat <- cbind(mu, diag(d))
  bvec <- c(1, rep(0, d))
  dvec <- rep(0, d)
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w <- res$solution
  return(w/sum(w))
}

# define GMVP (with heuristic not to allow shorting)
gmvp <- function(dataset, ...) {
  X <- price2logret(dataset[[1]])[-1]  # compute returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}



rpp.vanilla <- function(dataset, ...){
  X <- price2logret(dataset[[1]])[-1]  # compute returns
  Sigma <- cov(X)
  w <- riskParityPortfolio(Sigma)$w
  w <- as.vector(w)
  return(w/sum(w))
}

rpp.naive <- function(dataset, ...){
  X <- price2logret(dataset[[1]])[-1]  # compute returns
  Sigma <- cov(X)
  w <- riskParityPortfolio(Sigma, formulation="diag")$w
  w <- as.vector(w)
  return(w/sum(w))
}

####################################################################

IVP_portfolio_fun <- function(data, ...) {
  X <- diff(log(dataset[[1]]))[-1]
  sigma <- sqrt(diag(cov(X)))
  w <- 1/sigma
  w <- w/sum(w)

  leverage <- parent.frame(n = 1)$leverage
  if (is.null(leverage) || is.infinite(leverage))
    return(w)
  else
    return(w * leverage)
}


# define quintile portfolio
quintile.p <- function(dataset, ...) {
  X <- price2logret(dataset[[1]])[-1]  # compute returns
  N <- ncol(X)
  # design quintile portfolio
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}