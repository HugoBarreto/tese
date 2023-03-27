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
  prices <- dataset[["prices"]]
  investable.assets <- attr(prices, "investable.assets")
  alternative.assets <- attr(prices, "alternative.assets")

  investable.index <- which(colnames(prices) %in% investable.assets)
  alternative.index <- which(colnames(prices) %in% alternative.assets)

  w <- array(0, dim = length(c(investable.assets,alternative.assets)))
  d <- ncol(prices[,investable.assets])

  w[investable.index] <- 1/d
  return(w)
}


# define Markowitz mean-variance portfolio
Markowitz.p <- function(dataset, lambda=0.5, ...) {
  prices <- dataset[["prices"]]
  investable.assets <- attr(prices, "investable.assets")
  alternative.assets <- attr(prices, "alternative.assets")

  investable.index <- which(colnames(prices) %in% investable.assets)
  alternative.index <- which(colnames(prices) %in% alternative.assets)

  w <- array(0, dim = length(c(investable.assets,alternative.assets)))
  d <- ncol(prices[,investable.assets])

  X <- prices2logreturns(prices[,investable.assets])[-1]

  Sigma <- cov(X)
  mu <- colMeans(X)

  if (all(mu <= 1e-8) & lambda!=0){
    w[alternative.index] <- 1
    return(w/sum(w))
  }

  Dmat <- 2 * Sigma
  A.sumW <- rep(1, d)
  Amat <- cbind(A.sumW, diag(d))
  bvec <- c(1, rep(0, d))
  dvec <- lambda * mu
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w[investable.index] <- res$solution
  w[which(abs(w) < 1e-12)] <- 0
  return(w/sum(w))
}


max.sharpe.ratio.p <- function(dataset, ...) {
  prices <- dataset[["prices"]]
  investable.assets <- attr(prices, "investable.assets")
  alternative.assets <- attr(prices, "alternative.assets")

  investable.index <- which(colnames(prices) %in% investable.assets)
  alternative.index <- which(colnames(prices) %in% alternative.assets)

  w <- array(0, dim = length(c(investable.assets,alternative.assets)))
  d <- ncol(prices[,investable.assets])

  X <- prices2logreturns(prices[,investable.assets])[-1]

  Sigma <- cov(X)
  mu <- colMeans(X)

  if (all(mu <= 1e-8)){
    w[alternative.index] <- 1
    return(w/sum(w))
  }

  Dmat <- 2 * Sigma
  Amat <- cbind(mu, diag(d))
  bvec <- c(1, rep(0, d))
  dvec <- rep(0, d)
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w[investable.index] <- res$solution
  return(w/sum(w))
}

# define GMVP (with heuristic not to allow shorting)
gmvp <- function(dataset, ...) {
  return(Markowitz.p(dataset, lambda = 0, ...))
}

# Risk Parity Portfolio - Vanilla
rpp.vanilla <- function(dataset, ...){
  prices <- dataset[["prices"]]
  investable.assets <- attr(prices, "investable.assets")
  alternative.assets <- attr(prices, "alternative.assets")

  investable.index <- which(colnames(prices) %in% investable.assets)
  alternative.index <- which(colnames(prices) %in% alternative.assets)

  w <- array(0, dim = length(c(investable.assets,alternative.assets)))
  d <- ncol(prices[,investable.assets])

  X <- prices2logreturns(prices[,investable.assets])[-1]

  Sigma <- cov(X)
  w[investable.index] <- riskParityPortfolio(Sigma)$w
  return(w/sum(w))
}

# Risk Parity Portfolio - Naive
rpp.naive <- function(dataset, ...){
  prices <- dataset[["prices"]]
  investable.assets <- attr(prices, "investable.assets")
  alternative.assets <- attr(prices, "alternative.assets")

  investable.index <- which(colnames(prices) %in% investable.assets)
  alternative.index <- which(colnames(prices) %in% alternative.assets)

  w <- array(0, dim = length(c(investable.assets,alternative.assets)))
  d <- ncol(prices[,investable.assets])

  X <- prices2logreturns(prices[,investable.assets])[-1]

  Sigma <- cov(X)
  w[investable.index] <- riskParityPortfolio(Sigma, formulation="diag")$w
  return(w/sum(w))
}
