#' Convert prices to discrete returns
#'
#' @param prices series of price data. prices can be a column vector or a matrix
#'
#' @return NUMOBS-1 Vector or (NUMOBS-1)-by-Col Matrix
#' @export
#'
#' @examples
#' x <- 1:12 ; dim(x) <- c(3,4)
#' discrete_returns(x)
discrete_returns <- function(prices) {
  if ("xts" %in% class(prices)){
    return(prices/lag.xts(prices) - 1)
  }
  prices[-1,]/prices[-nrow(prices),] - 1
}

#' Convert discrete returns to prices
#'
#' @param returns series of discrete returns data.
#' @param initial_prices vector of initial prices, must be equal to ncol(returns)
#'
#' @return array of prices not including initial prices
#' @export
#'
#' @examples
#' x <- 1:12 ; dim(x) <- c(3,4)
#' discrete_returns(x)
discrete_returns2prices <- function(returns, initial_prices){
  prices <- cumprod(returns + 1) %*% diag(initial_prices)
  attributes(prices) <- attributes(returns)
  prices
}

#' Exclude elements from vector
#'
#' @param vector target vector
#' @param e elements to be excluded
#'
#' @return vector without element e
#' @export
exclude_element <- function(vector, e=NULL) {
  vector[!(vector %in% e)]
}

#' Split array in one of its dimension transforming into a list of arrays
#' with one less dimension.
#'
#' @param a an array
#' @param d dimension
#'
#' @return list of arrays
#' @export
split.along.dim <- function(a, d) {
  setNames(lapply(split(a, arrayInd(seq_along(a), dim(a))[, d]),
                  array, dim = dim(a)[-d], dimnames(a)[-d]),
           dimnames(a)[[d]])
  }

#' Convert prices to log returns
#'
#' @param p Time series of price data. p can be a column vector or a matrix
#'
#' @return NUMOBS-1 Vector or (NUMOBS-1)-by-Col Matrix
#' @export
#'
#' @examples
#' x <- 1:12 ; dim(x) <- c(3,4)
#' price2ret(x)
price2ret <- function(p){
  diff(log(p))
}

#' Convert returns to prices
#'
#' @param r Time series array of returns. r can be a column vector or a matrix
#'
#' @return NUMOBS+1 Vector or (NUMOBS+1)-by-Col Matrix
#' @export
#'
#' @examples
#' x <- 1:12 ; dim(x) <- c(3,4)
#' ret2price(my.price2ret(x))
ret2price <- function(r, startPrice=NULL){
  if(length(dim(r)) <= 1){
    if(is.null(startPrice)){
      startPrice <- 1
    }
    c(startPrice, startPrice*exp(1)**cumsum(r))
  }
  else {
    if(is.null(startPrice)){
      startPrice <- rep(1, dim(r)[2])
    }
    rbind(startPrice, t(startPrice* t(exp(1)**apply(r, 2, function(x) cumsum(x)))), deparse.level = 0)
  }
}

#' Returns a percentage as character vector
#'
#' @param x A numeric
#'
#' @return A string with percentages
#' @export
#'
#' @examples
#' my_perc(0.1)
my_perc <- function(x) {
  require(scales)
  x <- scales::percent(x, accuracy = 0.01)
}

#' Reformats rugarch output to texreg
#'
#' <https://stackoverflow.com/questions/57312645/how-to-export-garch-output-to-latex>
#'
#' @param fit Rugarch model object
#' @param include.rsquared Should include rquared?
#' @param include.loglike Should include loglike?
#' @param include.aic  Should include AIC?
#' @param include.bic Should include BIC?
#'
#' @return A texreg friendly object
#' @export
#'
#' @examples
extract.rugarch <- function(fit,
                            include.rsquared = TRUE,
                            include.loglike = TRUE,
                            include.aic = TRUE,
                            include.bic = TRUE) {

  require(texreg)

  # extract coefficient table from fit:
  coefnames <- rownames(as.data.frame(fit@fit$coef))
  coefs <- fit@fit$coef
  se <- as.vector(fit@fit$matcoef[, c(2)])
  pvalues <-  as.vector(fit@fit$matcoef[, c(4)])       # numeric vector with p-values

  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.rsquared == TRUE) {
    r2 <-  1 - (var(fit@fit$residuals) / var(y))
    gof <- c(gof, r2)
    gof.names <- c(gof.names, "R^2")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.loglike == TRUE) {
    loglike <- fit@fit$LLH
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- infocriteria(fit)[c(1)]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }

  if (include.bic == TRUE) {
    bic <- infocriteria(fit)[c(2)]
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }

  # include distribution and type variance
  # browser()
  #   variance_model <- fit@model$modeldesc$vmodel
  #   type_dist <- fit@model$modeldesc$distribution
  #   gof <- c(gof, variance_model, type_dist)
  #   gof.names <- c(gof.names, "Variance Model", 'Distribution')
  #   gof.decimal <- c(gof.decimal, TRUE, TRUE)

  # create texreg object:
  tr <- createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pvalues,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = gof.decimal
  )
  return(tr)
}