library(CVXR)

# define quintile portfolio
quintile_portfolio_fun <- function(dataset) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  N <- ncol(X)
  # design quintile portfolio
  ranking <- sort(colMeans(X), decreasing = TRUE, index.return = TRUE)$ix
  w <- rep(0, N)
  w[ranking[1:round(N/5)]] <- 1/round(N/5)
  return(w)
}

# define GMVP (with heuristic not to allow shorting)
GMVP_portfolio_fun <- function(dataset) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}

# define Markowitz mean-variance portfolio
Markowitz_portfolio_fun <- function(dataset) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  mu <- colMeans(X)  # compute mean vector
  Sigma <- cov(X)  # compute the SCM
  # design mean-variance portfolio
  w <- Variable(nrow(Sigma))
  prob <- Problem(Maximize(t(mu) %*% w - 0.5*quad_form(w, Sigma)),
                  constraints = list(w >= 0, sum(w) == 1))
  result <- solve(prob)
  return(as.vector(result$getValue(w)))
}