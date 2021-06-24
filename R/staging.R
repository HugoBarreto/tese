

risk.model.fit <- function(data, data.is.log.return = FALSE) {
  returns <- data

  if (!data.is.log.return) {
    returns <- price2ret(data)
  }

  T <- dim(returns)[1]
  index <- 1


}