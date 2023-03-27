##### Price and (log|discrete) returns convertion functions
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
prices2returns <- function(prices) {
  if ("xts" %in% class(prices)){
    return(prices/xts::lag.xts(prices) - 1)
  }
  prices <- as.matrix(prices)
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
returns2prices <- function(returns, initial_prices){
  prices <- cumprod(returns + 1) %*% diag(initial_prices)
  attributes(prices) <- attributes(returns)
  prices
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
#' prices2logreturns(x)
prices2logreturns <- function(p){
  diff(log(p))
}

#' Convert log returns to prices
#'
#' @param r Time series array of returns. r can be a column vector or a matrix
#'
#' @return NUMOBS+1 Vector or (NUMOBS+1)-by-Col Matrix
#' @export
#'
#' @examples
#' x <- 1:12 ; dim(x) <- c(3,4)
#' logreturns2prices(my.price2ret(x))
logreturns2prices <- function(r, startPrice=NULL){
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
    rbind(startPrice,
          # t occurs bc startPrice is multiplied col-wise
          t(startPrice* t(exp(1)** array(apply(r, 2, cumsum),
                                         dim = dim(r), dimnames=dimnames(r)))),
          deparse.level = 0)
  }
}


#' Convert log-returns to returns
#'
#' @param logret Array of log-returns. logret can be a column vector or a matrix
#'
#' @export
logreturns2returns <- function(logret){
  exp(logret) - 1
}

#' Convert returns to log-returns
#'
#' @param ret Array of returns. ret can be a column vector or a matrix
#'
#' @export
returns2logreturns <- function(ret){
  log(ret+1)
}


# Transform log-return to discrete return as a string (latex readable) in scientific format
logreturns2returnsPercentage <- function(.logret, digits=3) {
  logreturns2returns(.logret) %>%
    (function(ret) paste(format(ret*100, scientific=T, digits=digits),'\\%'))
}

##############################

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
  x <- scales::percent(x, accuracy = 0.01)
}
