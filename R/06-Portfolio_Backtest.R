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

constraints.file <- "tabs/bt_sim_constraints.txt"
constraints.file.real <- "tabs/bt_real_constraints.txt"

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

# Get a list whose each element is a xts object that contains realized and
# simulated prices, providing distinct simulated price action based on our models.
simulate_prices <- function(sim_returns, prices) {
  last_prices <- tail(prices,1) %>% coredata %>% drop
  sim_returns_dates <- index(sim_returns)
  sim_prices <- logret2price(sim_returns, last_prices)[-1,] # drop the last training price (it's on the merge)
  result <- rbind(prices, xts(sim_prices, order.by = sim_returns_dates))
  list("adjusted"=result)
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
    if(performance["annual return"]==0) return(0)
    PerformanceAnalytics::Return.annualized(return, scale = 252, geometric = TRUE)
  }) %>%
    add_performance(name = "Geometric SR", fun = function(return, performance,...) {
      if(performance["Sharpe ratio"]==0) return(0)
      PerformanceAnalytics::SharpeRatio.annualized(return, scale = 252, geometric = TRUE)
    })
}

# add to each portfolio, a "analysis" attribute
portfolio_analysis <- function(bt) {
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
    attr(bt_portfolio, "analysis") <- c(port_analysis,
                                        # execute a function that are unaware to dynamic dots
                                        tibble::lst(!!name := exec(function(...) fun(bt_portfolio, ...), !!!dots)))
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
    reduce(function(.bt, add_analysis_fun) add_analysis_fun(.bt), .init = bt)
}

#### Portfolio Analysis Producer Functions
get_cumreturns <- function(port_datasets, horizon=1, ...) {
  returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$return))
  # cumulative returns
  return(na.omit(rollapply(returns+1, width = horizon, prod)) - 1)
}


get_VaR <- function(port_datasets, alpha=0.95, horizon=NULL, ...) {
  if(is.null(horizon) | is.na(horizon)){
    # Bind all wealth derived from simulations. Exclude first date (initial endowment of 1)
    cum_returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1] - 1
  }
  else {
    # Get rolling 'horizon' of days cumulative return
    cum_returns <- get_cumreturns(port_datasets, horizon)
  }
  # Calculate VaR for each date
  VaR <- apply(cum_returns, 1, quantile, probs=(1-alpha), na.rm=T)
  VaR <- if(!is.matrix(VaR)) as.matrix(VaR) else t(VaR)
  xts(VaR, index(cum_returns), dimnames=list(NULL, alpha))
}


get_CVaR <- function(port_datasets, alpha=0.95, horizon=NULL, ...) {
  if(is.null(horizon) | is.na(horizon)){
    # Bind all wealth derived from simulations. Exclude first date (initial endowment of 1)
    cum_returns <- do.call(cbind, lapply(port_datasets, function(single_bt) single_bt$wealth))[-1]
  }
  else {
    # Get rolling 'horizon' of days cumulative return
    cum_returns <- get_cumreturns(port_datasets, horizon)
  }
  # Get VaR for each date
  VaR <- get_VaR(port_datasets, alpha, horizon)

  # Calculate Conditional Var a.k.a. Expected Shortfall for each alpha
  cvar <- apply(VaR, 2, function(VaR, cum_returns){
    coredata(cum_returns)[cum_returns >= coredata(VaR)] <- NA
    rowMeans(cum_returns, na.rm = T)
  }, cum_returns)
  xts(cvar, index(cum_returns))
}

## End Aux Functions --------------------------------------------------------------------------------------------------

# Backtest portfolios
portfolios <- list("msr" = max.sharpe.ratio.p,
                   "gmv" = gmvp,
                   "rpn" = rpp.naive,
                   "rpv" = rpp.vanilla)

port_name <- c(names(portfolios), "ew")

# Importing data
prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
prices <- rbind(prices_training,prices_test)

tickers <- names(prices)
test_dates <- index(prices_test)

# Use only last 252 days of trading data to optimize initial portfolio weights
prices_training <- tail(prices_training, 252)
prices <- prices[paste0(index(prices_training)[1],"/")] # Analyse performance on the same data period.

# Get Test Prices (wrap xts in a nested list used as input in backtest function)
realized_prices <- list("empirical_data"=list("adjusted"=prices))

##########################################################################################################
########## The following section is computational and memory intensive - Recommended to NOT Run ##########
##########################################################################################################

# Import simulated returns and then
# transform the 3-D array into a list of xts objects using
# simulation_dates as index to all instances
simulated_logreturns <- read_rds('data/simulated_logreturns.rds') %>%
  split.along.dim(3) %>%
  lapply(as.xts, order.by=test_dates) # %>% `[`(1:100) # STAGE ONLY

# Get Simulated Prices
simulated_prices <- lapply(simulated_logreturns, simulate_prices, prices_training)

# Perform Backtest
### Realized data
bt_real_20 <- portfolioBacktest(portfolios, realized_prices,
                            benchmark = "1/N",
                            rebalance_every=5,
                            optimize_every = 20,
                            show_progress_bar = TRUE)

bt_real_5 <- portfolioBacktest(portfolios, realized_prices,
                               benchmark = "1/N",
                               rebalance_every=5,
                               optimize_every = 5,
                               show_progress_bar = TRUE)

bt_real_1 <- portfolioBacktest(portfolios, realized_prices,
                               benchmark = "1/N",
                               rebalance_every=1,
                               optimize_every = 1,
                               show_progress_bar = TRUE)

names(bt_real_1)[5] <- names(bt_real_5)[5] <- names(bt_real_20)[5] <- "ew"

# Add custom performance metrics
bt_real_20 <- add_performances(bt_real_20)
bt_real_5 <- add_performances(bt_real_5)
bt_real_1 <- add_performances(bt_real_1)

write_rds(bt_real_20, 'data/backtest_portfolios_realized_data_opt20.rds')
write_rds(bt_real_5, 'data/backtest_portfolios_realized_data_opt5.rds')
write_rds(bt_real_1, 'data/backtest_portfolios_realized_data_opt1.rds')

# Measures to generate the backtest table attribute to each bt
table_measures <- c(names(bt_real_20[[1]][[1]]$performance), "error", "cpu time")

# Add backtestTable (list of matrices of each performance metric for each dataset and portfolio) as attribute to bt
attr(bt_real_20, "backtestTable") <-  backtestTable(bt_real_20, measures = table_measures)
attr(bt_real_5, "backtestTable") <-  backtestTable(bt_real_5, measures = table_measures)
attr(bt_real_1, "backtestTable") <-  backtestTable(bt_real_1, measures = table_measures)

# Add the analysis attribute to each bt's port
bt_real_20 <- portfolio_analysis(bt_real_20)
bt_real_5 <- portfolio_analysis(bt_real_5)
bt_real_1 <- portfolio_analysis(bt_real_1)

analysis_options <- expand_grid(stat="CumReturns", horizon=c(1,5,20))

bt_real_20 <- add_multiple_analysis(bt_real_20, analysis_options)
bt_real_5 <- add_multiple_analysis(bt_real_5, analysis_options)
bt_real_1 <- add_multiple_analysis(bt_real_1, analysis_options)

write_rds(bt_real_20, 'data/backtest_portfolios_realized_data_opt20_plus_attributes.rds')
write_rds(bt_real_5, 'data/backtest_portfolios_realized_data_opt5_plus_attributes.rds')
write_rds(bt_real_1, 'data/backtest_portfolios_realized_data_opt1_plus_attributes.rds')

# Check hypothesized constraints
sink(constraints.file.real)
cat("Real bt - opt 20\n")
check_constraints(bt_real_20)
cat("=======================================\n")
cat("Real bt - opt 5\n")
check_constraints(bt_real_5)
cat("=======================================\n")
cat("Real bt - opt 1\n")
check_constraints(bt_real_1)
cat("=======================================\n")
sink()

# Clean memory space for further processing
rm(bt_real_20, bt_real_5, bt_real_1); gc(reset = TRUE)





# expand options to simulation data backtest
analysis_options <- analysis_options %>%
  bind_rows(expand_grid(stat=c("CVaR","VaR"), alpha=list(c(0.90,0.95,0.99)), horizon=c(1,5,20))) %>%
  bind_rows(expand_grid(stat=c("CVaR","VaR"), alpha=list(c(0.90,0.95,0.99))))

###### Sim data - opt 20
bt_sim_20 <- portfolioBacktest(portfolios, simulated_prices,
                               benchmark = "1/N",
                               rebalance_every=5,
                               optimize_every = 20,
                               show_progress_bar = TRUE,
                               paral_portfolios = 4)
names(bt_sim_20)[5] <- "ew"


# Check hypothesized constraints - Output results are saved in constraints.file
sink(constraints.file)
cat("Sim bt - opt 20\n")
check_constraints(bt_sim_20)
cat("=======================================\n")
sink()


# Save bt
write_rds(bt_sim_20, 'data/backtest_portfolios_simulated_data_opt20.rds')

bt_sim_20 <- add_performances(bt_sim_20)
attr(bt_sim_20, "backtestTable") <-  backtestTable(bt_sim_20, measures = table_measures)

bt_sim_20 <- portfolio_analysis(bt_sim_20)

bt_sim_20 <- add_multiple_analysis(bt_sim_20, analysis_options)

# Save bt with attributes
write_rds(bt_sim_20, 'data/backtest_portfolios_simulated_data_opt20_plus_attributes.rds')

bt_sim_20 <- lapply_ka(bt_sim_20, function(port) {port[] <- NULL;port})

# Save bt with only attributes, no more individual datasets
write_rds(bt_sim_20, 'data/bt_sim_opt20_only_backtestTable_and_analysis.rds')

rm(bt_sim_20); gc(reset = TRUE)



###### Sim data - opt 5
bt_sim_5 <- portfolioBacktest(portfolios, simulated_prices,
                            benchmark = "1/N",
                            rebalance_every=5,
                            optimize_every = 5,
                            show_progress_bar = TRUE,
                            paral_portfolios = 4)
names(bt_sim_5)[5] <- "ew"


sink(constraints.file, append = TRUE)
cat("Sim bt - opt 5\n")
check_constraints(bt_sim_5)
cat("=======================================\n")
sink()


# Save bt
write_rds(bt_sim_5, 'data/backtest_portfolios_simulated_data_opt5.rds')

bt_sim_5 <- add_performances(bt_sim_5)
attr(bt_sim_5, "backtestTable") <-  backtestTable(bt_sim_5, measures = table_measures)

bt_sim_5 <- portfolio_analysis(bt_sim_5)
bt_sim_5 <- add_multiple_analysis(bt_sim_5, analysis_options)

# Save bt with attributes
write_rds(bt_sim_5, 'data/backtest_portfolios_simulated_data_opt5_plus_attributes.rds')

bt_sim_5 <- lapply_ka(bt_sim_5, function(port) {port[] <- NULL;port})

# Save bt with only attributes, no more individual datasets
write_rds(bt_sim_5, 'data/bt_sim_opt5_only_backtestTable_and_analysis.rds')

rm(bt_sim_5); gc(reset = TRUE)






###### Sim data - opt 1
bt_sim_1 <- portfolioBacktest(portfolios, simulated_prices,
                              benchmark = "1/N",
                              rebalance_every=1,
                              optimize_every = 1,
                              show_progress_bar = TRUE,
                              paral_portfolios = 4)
names(bt_sim_1)[5] <- "ew"

sink(constraints.file, append = TRUE)
cat("Sim bt - opt 1\n")
check_constraints(bt_sim_1)
cat("=======================================\n")
sink()


# Save bt
write_rds(bt_sim_1, 'data/backtest_portfolios_simulated_data_opt1.rds')

bt_sim_1 <- add_performances(bt_sim_1)
attr(bt_sim_1, "backtestTable") <-  backtestTable(bt_sim_1, measures = table_measures)

bt_sim_1 <- portfolio_analysis(bt_sim_1)
bt_sim_1 <- add_multiple_analysis(bt_sim_1, analysis_options)

# Save bt with attributes
write_rds(bt_sim_1, 'data/backtest_portfolios_simulated_data_opt1_plus_attributes.rds')

bt_sim_1 <- lapply_ka(bt_sim_1, function(port) {port[] <- NULL;port})

# Save bt with only attributes, no more individual datasets
write_rds(bt_sim_1, 'data/bt_sim_opt1_only_backtestTable_and_analysis.rds')

rm(bt_sim_1); gc(reset = TRUE)
