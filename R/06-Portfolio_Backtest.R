library(rlang)
library(tidyverse)
library(xts)
library(portfolioBacktest)
library(PerformanceAnalytics)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/portfolio_functions.R')

constraints.file <- "tabs/bt_constraints.txt"

# Portfolios' performance measures for further analysis
table_measures <- c("Sharpe ratio","max drawdown","annual return",
                    "annual volatility","turnover","CAGR", "error")

## Aux Functions ------------------------------------------------------------------------------------------------------
# Getter function to access specific attributes
get_backtestTable <- attr_getter("backtestTable")
get_analysis <- attr_getter("analysis")

# lapply that keep obj attributes
lapply_ka <- function(obj, .f, ...){
  .obj <- lapply(obj, .f, ...)
  attributes(.obj) <- attributes(obj)
  return(.obj)
}

# Split .list names into chunks for partial processing
split_into_chuncks <- function(.list, chunk.size=ceiling(length(.list)/10)){
  .list.names <- names(.list)
  len <- length(.list.names)
  split(.list.names, f=rep(1:(len %/% chunk.size + 1), each=chunk.size,
                     length.out=len))
}

# Check if backtests constraints were respected (shows table)
check_constraints <- function(bt){
  add_performance(bt, name = "avgLeverage", desired_direction = -1, fun = function(w_bop,...) if(anyNA(w_bop)) NA else mean(rowSums(abs(w_bop)))) %>%
    add_performance(name = "FullAlloc?", desired_direction = -1, fun = function(w_bop,...) if(anyNA(w_bop)) NA else mean(rowSums(w_bop))) %>%
    add_performance(name = "Shortsell?", desired_direction = -1, fun = function(w_bop,...) if(anyNA(w_bop)) NA else as.numeric(any(w_bop < -1e-8))) %>%
    backtestSummary(summary_fun = mean) %>%
    summaryTable(measures = c("avgLeverage", "FullAlloc?", "Shortsell?"))
}

# Add additional performance metrics - CAGR + Geometric SR - to each bt run
add_performances <- function(bt){
  add_performance(bt, name = "CAGR", fun = function(return, performance,...) {
    if(is.na(performance["annual return"]) || is.null(performance["annual return"]))
      return(NA)
    if(performance["annual return"]==0) return(0)
    PerformanceAnalytics::Return.annualized(return, scale = 252, geometric = TRUE)
  }) %>%
    add_performance(name = "Geometric SR", fun = function(return, performance,...) {
      if(is.na(performance["Sharpe ratio"]) || is.null(performance["Sharpe ratio"]))
        return(NA)
      if(performance["Sharpe ratio"]==0 ) return(0)
      PerformanceAnalytics::SharpeRatio.annualized(return, scale = 252, geometric = TRUE)
    })
}

# add to backtest, a "backtestTable" attribute | good for piping operations
add_backtestTable_attr <- function(bt, measures=NULL){
  attr(bt, "backtestTable") <-  backtestTable(bt, measures = measures)
  bt
}
# add to each portfolio, a "analysis" attribute
add_analysis_attr <- function(bt) {
  is_null_analysis <- sapply(bt, function(port) is.null(get_analysis(port)))
  if (!any(is_null_analysis)) return(bt)

  .bt <- map2(bt, is_null_analysis, function(port, is_null){
    if(is_null) attr(port, "analysis") <- list()
    port
  })
  attributes(.bt) <- attributes(bt)
  return(.bt)
}

add_analysis <- function(bt, name, fun, ...) {
  # make this function support dynamic dots
  dots <- list2(...)

  fun_each_portfolio <- function(bt_portfolio){
    port_analysis <- get_analysis(bt_portfolio)
    args <- c(dots, tibble::lst(port_analysis=port_analysis))
    attr(bt_portfolio, "analysis") <- c(port_analysis,
                                        # execute a function that are unaware to dynamic dots
                                        tibble::lst(!!name := exec(function(...) fun(bt_portfolio, ...), !!!args)))
    return(bt_portfolio)
  }

  lapply_ka(bt, fun_each_portfolio)
}

add_multiple_analysis <- function(bt, analysis_options) {
  pmap(analysis_options, function(...) {
    args <- list(...)
    stat <- match.arg(args$stat, c("CVaR","VaR", "CumReturns"))
    name <- if(is.null(args$horizon) | is.na(args$horizon)) paste0(stat,'_wealth') else paste0(stat, '_h', sprintf("%02d", args$horizon))
    args$stat <- NULL

    switch (stat,
            CVaR = function(.bt) add_analysis(.bt, name = name, fun = get_CVaR, !!!args),
            VaR = function(.bt) add_analysis(.bt, name = name, fun = get_VaR, !!!args),
            CumReturns = function(.bt) add_analysis(.bt, name = name, fun = get_cumreturns, !!!args),
            stop("Option not available on multiple addition. Try adding separatly with \'add_analysis\' function"))
  }) %>%
  purrr::reduce(function(.bt, add_analysis_fun) add_analysis_fun(.bt), .init = bt)
}

#### Portfolio Analysis Producer Functions
get_cumreturns <- function(port_datasets, horizon=1, ...) {
  args <- list2(...)
  if(!is.null(args$port_analysis)){
    name <- if(is.null(horizon) | is.na(horizon)) "CumReturns_wealth" else paste0("CumReturns_h", sprintf("%02d", horizon))
    if(!is.null(args$port_analysis[[name]])) return(args$port_analysis[[name]])
  }

  returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$return))
  # cumulative returns
  return(as.xts(rollapplyr(as.zoo(returns)+1, width = horizon, prod) - 1))

}

get_VaR <- function(port_datasets, alpha=0.95, horizon=NULL, ...) {
  args <- list2(...)
  if(!is.null(args$port_analysis)){
    name <- if(is.null(horizon) | is.na(horizon)) "VaR_wealth" else paste0("VaR_h", sprintf("%02d", horizon))
    if(!is.null(args$port_analysis[[name]])) return(args$port_analysis[[name]])
  }

  if(is.null(horizon) | is.na(horizon)){
    # Bind all wealth derived from simulations. Exclude first date (initial endowment of 1)
    cum_returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1] - 1
  }
  else {
    # Get rolling 'horizon' of days cumulative return
    cum_returns <- get_cumreturns(port_datasets, horizon, !!!args)
  }
  # Calculate VaR for each date
  if (length(cum_returns)[1L] == 0)
    return(NA)
  VaR <- apply(cum_returns, 1, quantile, probs=(1-alpha), na.rm=T)
  VaR <- if(!is.matrix(VaR)) as.matrix(VaR) else t(VaR)
  xts(VaR, index(cum_returns), dimnames=list(NULL, alpha))
}

get_CVaR <- function(port_datasets, alpha=0.95, horizon=NULL, ...) {
  args <- list2(...)
  if(is.null(horizon) | is.na(horizon)){
    # Bind all wealth derived from simulations. Exclude first date (initial endowment of 1)
    cum_returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1]
  }
  else {
    # Get rolling 'horizon' of days cumulative return
    cum_returns <- get_cumreturns(port_datasets, horizon, !!!args)
  }
  # Get VaR for each date
  VaR <- get_VaR(port_datasets, alpha, horizon, !!!args)

  # Calculate Conditional Var a.k.a. Expected Shortfall for each alpha
  if(all(is.na(VaR)))
    return(NA)
  cvar <- apply(VaR, 2, function(VaR, cum_returns){
    coredata(cum_returns)[cum_returns >= coredata(VaR)] <- NA
    .cvar <- rowMeans(cum_returns, na.rm = T)
    na.fill(.cvar, VaR[is.na(.cvar)])
  }, cum_returns)
  dim(cvar) <- dim(VaR)
  dimnames(cvar) <- dimnames(VaR)
  xts(cvar, index(cum_returns))
}
## End Aux Functions --------------------------------------------------------------------------------------------------

# Importing data
prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
MM_training <- read_rds('data/MM_training.rds')
MM_test <- read_rds('data/MM_test.rds')

tickers <- names(prices_test)
MM.names <- names(MM_test)
all.names <- c(tickers, MM.names)

# Complete sets
prices_training <- merge(prices_training, MM_training) %>% `colnames<-`(all.names)
prices_test <- merge(prices_test, MM_test) %>% `colnames<-`(all.names)
prices <- rbind(prices_training,prices_test)


# Info used in portfolio functions
attributes(prices_training) <- c(attributes(prices_training),
                                 list("investable.assets"=tickers,
                                      "alternative.assets"=MM.names))

attributes(prices) <- c(attributes(prices),
                        list("investable.assets"=tickers,
                             "alternative.assets"=MM.names))

# Backtest portfolios
portfolios <- list("msr" = max.sharpe.ratio.p,
                   "gmv" = gmvp,
                   "rpn" = rpp.naive,
                   "rpv" = rpp.vanilla,
                   "ew" = ewp)

portfolios.names <- names(portfolios)

################
# Objetivos:
# - Backtest all training data and sampled windows.

# Define backtest dataset list for training and test data.
bt_training.datasets <- list("complete_data"=list("prices"=prices_training))
bt_test.datasets <- list("empirical_data"=list(
  "prices"=prices[paste0(head(index(tail(prices_training, 252)),1),"/")]))


training.resample <- financialDataResample(bt_training.datasets[[1]],
                                           N_sample = 10, T_sample = 252*2,
                                           num_datasets = 100)

bt_training.datasets <- c(bt_training.datasets,training.resample)

# FAZER: resample training dataset - ampliar os backtests !!!!!!!!!!!!!!!!!!!!!!

# Perform Backtest - Training data
bt_training.20 <- portfolioBacktest(portfolios, bt_training.datasets,
                            rebalance_every=5, optimize_every = 20) %>%
  add_performances %>% # CAGR + Geometric SR
  set_names(portfolios.names) %>%
  add_backtestTable_attr(table_measures)

bt_training.5 <- portfolioBacktest(portfolios, bt_training.datasets,
                               rebalance_every=5, optimize_every = 5) %>%
  add_performances %>%
  set_names(portfolios.names) %>%
  add_backtestTable_attr(table_measures)

bt_training.1 <- portfolioBacktest(portfolios, bt_training.datasets,
                               rebalance_every=1, optimize_every = 1) %>%
  add_performances %>%
  set_names(portfolios.names) %>%
  add_backtestTable_attr(table_measures)

write_rds(bt_training.20, 'data/backtest_training_opt20.rds')
write_rds(bt_training.5, 'data/backtest_training_opt5.rds')
write_rds(bt_training.1, 'data/backtest_training_opt1.rds')

# Check hypothesized constraints
sink(constraints.file)
cat("Training bt - opt 20\n")
check_constraints(bt_training.20)
cat("=======================================\n")
cat("Training bt - opt 5\n")
check_constraints(bt_training.5)
cat("=======================================\n")
cat("Training bt - opt 1\n")
check_constraints(bt_training.1)
cat("=======================================\n")
sink()

# Options of relevant stat about port on each dataset
analysis_options <- expand_grid(stat="CumReturns", horizon=c(1,5,21))

# Perform Backtest - Test data | Add backtestTable to bt and analysis to each dataset of bt
bt_test <- portfolioBacktest(portfolios, bt_test.datasets,
                             rebalance_every=1, optimize_every = 1) %>%
  add_performances %>%
  set_names(portfolios.names) %>%
  add_backtestTable_attr(table_measures) %>%
  add_analysis_attr %>%
  add_multiple_analysis(analysis_options)

sink(constraints.file, append = TRUE)
cat("Test bt - opt 1\n")
check_constraints(bt_test)
cat("=======================================\n")
sink()

write_rds(bt_test, 'data/backtest_test.rds')

##########################################################################################################
########## The following section is computational and memory intensive - Recommended to NOT Run ##########
##########################################################################################################
# expand options to simulation backtests --- the order of c("VaR","CVaR") matters for code performance
analysis_options <- analysis_options %>%
  bind_rows(expand_grid(stat=c("VaR","CVaR"), alpha=list(c(0.90,0.95,0.99)), horizon=c(1,5,21)))

# Get datasets for simulations backtest
bt_simulations.multi_datasets <- read_rds('data/simulated_prices.rds')

chunks <- split_into_chuncks(bt_simulations.multi_datasets, chunk.size = 8)

for(g in names(chunks)) {
    message('Simulating chunk:', g)
    message('First date in group:', chunks[[g]][1])

    bt_chunk <-
      lapply(chunks[[g]], function(bt.date){
        portfolioBacktest(portfolios, bt_simulations.multi_datasets[[bt.date]],
                          rebalance_every=1, optimize_every = 1,
                          paral_portfolios = 5) %>%
          set_names(portfolios.names)
      }) %>%
      set_names(chunks[[g]]) %>%
      lapply(function(date.bt){
        add_analysis_attr(date.bt) %>%
          add_multiple_analysis(analysis_options)
      })


    # Save partial bt with attributes
    write_rds(bt_chunk, paste0('data/backtest_simulations_part', g,'.rds'))

    bt_chunk <-
    lapply(bt_chunk, function(date.sim){
      lapply_ka(date.sim, function(port) {port[] <- NULL;port})
    })

    # Save partial bt only with attributes
    write_rds(bt_chunk, paste0('data/backtest_simulations_only_performance_part', g,'.rds'))

    rm(bt_chunk)
    gc(verbose = TRUE, reset = TRUE)
}

bt_simulations <- list()

for(g in names(chunks)){
  bt_simulations <- c(
    bt_simulations,
    read_rds(paste0('data/backtest_simulations_only_performance_part', g,'.rds')))
}

# Save bt with only attributes, no more individual datasets
write_rds(bt_simulations, 'data/backtest_simulations_only_performance.rds')













# TESTING - APAGAR
chunks <- split_into_chuncks(tail(bt_simulations.multi_datasets), chunk.size=1)

#### APAGAR
bt_simulations <- read_rds('data/backtest_simulations_only_performance.rds')

# All available measures table_measures - APAGAR
# table_measures <- c(
#   "Sharpe ratio","max drawdown","annual return","annual volatility",
#   "Sortino ratio","downside deviation","Sterling ratio","Omega ratio",
#   "VaR (0.95)","CVaR (0.95)","rebalancing period","turnover","ROT (bps)",
#   "CAGR","Geometric SR","error","cpu time")



date.bt <- bt_simulations[[1]]
an <- get_analysis(bt_simulations[[1]][[1]])
get_analysis(bt_simulations[[17]][[2]])


debugonce(get_CVaR)
debug(get_CVaR)
undebug(get_CVaR)






data(SP500_symbols)  # load the SP500 symbols
# download data from internet
SP500 <- stockDataDownload(stock_symbols = SP500_symbols,
                           from = "2008-12-01", to = "2018-12-01")

my_dataset_list <- financialDataResample(SP500,
                                         N_sample = 50, T_sample = 252*2,
                                         num_datasets = 10)








# # Open issue in xts github
# .x <- .xts(c(1:5,NaN),1:6)
# .x <- cbind(.x,.x)
# rollapply(.x, width = 6, prod)
