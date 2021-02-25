
price2ret <- function(p){
  diff(log(p))
}

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

risk.model.fit <- function(data, data.is.log.return = FALSE) {
  returns <- data

  if (!data.is.log.return) {
    returns <- price2ret(data)
  }

  T <- dim(returns)[1]
  index <- 1


}


