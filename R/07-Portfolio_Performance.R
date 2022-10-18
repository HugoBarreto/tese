library(rlang)
library(RColorBrewer)
library(kableExtra)
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

portfolios_performance_tex_file <- 'tabs/portfolios_performance.tex'

def.par <- par(no.readonly = TRUE) # save default, for future resetting
a4w <- 21/2.54
a4h <- 29.7/2.54
mod <- 1.5 # modify plot's height and width in proportion to the A4 dimensions
mod2 <- 2.5

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

# Not exported function from pkg 'portfolioBacktest'
backtestSummarySinglePortfolio <- function(res_table, portfolio_name, summary_fun) {
  performance_names <- setdiff(names(res_table), c("error", "error_message"))  # ignore two non-numerical metrics
  performance <- rep(NA, length(performance_names))
  names(performance) <- performance_names

  fail_mask <- res_table$error[, portfolio_name]
  failure_rate <- mean(fail_mask)
  if (failure_rate < 1)
    for (metric in performance_names)
      performance[metric] <- summary_fun(na.omit(res_table[[metric]][!fail_mask, portfolio_name]))

  # fix names if necessary (to be removed)
  if (any(performance_names == "cpu_time"))
    stop("Performance name cpu_time deprecated")
  #performance_names[which(performance_names == "cpu_time")] <- "cpu time"
  #names(performance) = performance_names


  return(c(performance, "failure rate" = failure_rate))
}

# Slight modification of 'backtestSummary' function - accepts an already calculated backtest Table of performance criteria
backtestSummaryFrombacktestTable <- function (bt, bt_table, portfolio_indexes = NA, portfolio_names = NA,
                                              summary_fun = median, show_benchmark = TRUE) {
  if (anyNA(portfolio_names) && anyNA(portfolio_indexes))
    portfolio_indexes <- setdiff(1:length(bt), attr(bt, "benchmark_index"))
  if (!anyNA(portfolio_indexes))
    portfolio_names <- names(bt)[portfolio_indexes]
  if (show_benchmark)
    portfolio_names <- c(portfolio_names, names(bt)[attr(bt, "benchmark_index")])

  performance <- lapply(portfolio_names, function(portfolio_name)
    backtestSummarySinglePortfolio(bt_table, portfolio_name, summary_fun))
  performance_summary <- do.call(cbind, performance)
  colnames(performance_summary) <- portfolio_names
  return(list(performance_summary = performance_summary, error_message = bt_table$error_message))
}

# Get a single measure from a list of bts
get_merged_measure <- function(list_bt, measure){
  do.call(data.frame,
          lapply(list_bt, function(bt) get_backtestTable(bt)[[measure]])) %>%
    as.matrix()
}

### Graph functions
# Modified backtestsBoxPlot function that takes a list of bt as input instead of only one.
listBacktestsBoxPlot <- function (list_bt, measure = "Sharpe ratio", ref_portfolio = NULL,
                                  type = c("simple","ggplot2"), sorted=FALSE, colors.order=NULL, summary_fun=median,...)
{
  res_table <- get_merged_measure(list_bt, measure)
  if (length(res_table) == 0)
    stop(measure, " does not match a single performance measure")
  if (!is.null(ref_portfolio)) {
    if (!ref_portfolio %in% colnames(res_table))
      stop("Reference portfolio does not exist.")
    res_table <- res_table - res_table[, ref_portfolio]
    measure <- paste0("Excess ", measure, " (w.r.t. reference portfolio ",
                      ref_portfolio, ")")
  }
  cols_order <-  if(sorted) names(sort(apply(res_table, 2, summary_fun))) else colnames(res_table)[ncol(res_table):1]

  params <- list(res_table[, cols_order], ...)
  switch(match.arg(type), simple = {
    if (is.null(params$main)) params$main <- measure
    if (is.null(params$las)) params$las <- 1
    if (is.null(params$cex.axis)) params$cex.axis <- 0.8
    if (is.null(params$horizontal)) params$horizontal <- TRUE
    if (is.null(params$outline)) params$outline <- FALSE
    if (is.null(params$col)) params$col <- topo.colors(ncol(res_table))
    if (!is.null(params$order.colors)) params$col <-  params$col[match(cols_order, params$order.colors)]
    print(params$col)
    mar <- if (is.null(params$mar)) c(3, 5, 3, 1) else params$mar
    old_par <- par(mar = mar)
    do.call(boxplot, params)
    par(old_par)
    if(sorted & is.null(params$order.colors)) cols_order
  }, ggplot2 = {
    if (is.null(params$alpha)) params$alpha <- 0.4
    limits <- apply(res_table, 2, function(x) {
      lquartile <- quantile(x, 0.25, na.rm = TRUE)
      uquartile <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- uquartile - lquartile
      c(limit_min = min(x[x > lquartile - 1.6 * IQR], na.rm = TRUE),
        limit_max = max(x[x < uquartile + 1.6 * IQR],
                        na.rm = TRUE))
    })
    plot_limits <- c(min(limits["limit_min", ]), max(limits["limit_max",]))
    df <- as.data.frame.table(res_table)
    if(sorted){ # Not working
      stop("Not implemented yet.")
      # ggplot(df, aes(x = .data$Var2, y = .data$Freq, fill = .data$Var2)) +
      #   geom_boxplot(show.legend = FALSE) +
      #   geom_point(size = 0.5, alpha = params$alpha, show.legend = FALSE) +
      #   scale_x_discrete(limits = cols_order) +
      #   coord_flip(ylim = plot_limits) +
      #   labs(title = measure, x = NULL, y = NULL)
    }
    else{
      ggplot(df, aes(x = .data$Var2, y = .data$Freq, fill = .data$Var2)) +
        geom_boxplot(show.legend = FALSE) +
        geom_point(size = 0.5, alpha = params$alpha, show.legend = FALSE) +
        scale_x_discrete(limits = rev(levels(df$Var2))) +
        coord_flip(ylim = plot_limits) +
        labs(title = measure, x = NULL, y = NULL)
    }
  }, stop("Boxplot type unknown."))
}

# Gráfico de retornos acumulados realizados vs. simulados (com VaR e CVaR)
real_vs_sim_returns_plot <- function(port_sim, port_real, port_name=NULL, alpha=0.95, horizon=1, plot=TRUE) {
  # Names Suffixes
  horizon_suf <- paste0('_h', sprintf("%02d", horizon))

  # Names of Analysis objs
  n.cumret <- paste0('CumReturns', horizon_suf)
  n.cvar <- paste0('CVaR', horizon_suf)
  n.var <- paste0('VaR', horizon_suf)

  alpha.char <- as.character(alpha)

  # Get analysis
  port_real_analysis <- get_analysis(port_real)
  port_sim_analysis <- get_analysis(port_sim)

  # Real return values lower than VaR
  real_ret_lower_var <- which(port_real_analysis[[n.cumret]] < port_sim_analysis[[n.var]][,alpha.char])

  if(plot){
    # Plot ranges
    points.min <- min(port_sim_analysis[[n.cvar]][,alpha.char], port_real_analysis[[n.cumret]])
    real_range <- range(port_real_analysis[[n.cumret]])

    y.min <- points.min - diff(real_range)*0.02
    data_range <- range(y.min, real_range)
    y.max <- data_range[2] + diff(data_range)*0.13

    n <- nrow(port_real_analysis[[n.cumret]])

    # Plot Simulation Data
    plot.new()
    plot.window(xaxs = "i", ylim=c(y.min, y.max), xlim=range(1,n)+c(-1,1))

    bplot <- boxplot(t(port_sim_analysis[[n.cumret]]), outline=FALSE, whisklty=0, staplelty=0, add=TRUE, at=1:n, xaxt='n')
    axis(side=1, at=round(seq(1,n, length.out=9)), labels=bplot$names[round(seq(1,n, length.out=9))])

    xi <- seq_along(bplot$n)

    lines(xi, port_sim_analysis[[n.var]][,alpha.char], col="red", lty=2)
    points(xi, port_sim_analysis[[n.cvar]][,alpha.char], col="red", pch=20, type = 'o')

    # Plot Realized Data (same plot)
    if (length(real_ret_lower_var) > 0){
      points(setdiff(xi, real_ret_lower_var), port_real_analysis[[n.cumret]][-real_ret_lower_var], col="blue", pch=19)
      points(real_ret_lower_var, port_real_analysis[[n.cumret]][real_ret_lower_var], bg="yellow", pch=21)
    } else {
      points(setdiff(xi, real_ret_lower_var), port_real_analysis[[n.cumret]], col="blue", pch=19)
    }

    # Provide legend and title
    lgd.text <- c("1st, 2nd, 3rd quartiles of Sim. Cumulative Returns",
                  as.expression(rlang::exprs("Realized Cumulative Returns">="VaR"[!!alpha.char],
                                             "Realized Cumulative Returns < VaR"[!!alpha.char],
                                             "VaR"[!!alpha.char], "ES"[!!alpha.char])))

    lgd <- legend("topleft", legend = lgd.text, bty = 'n', ncol = 2,
                  lty=c(rep(0,3),2,1),
                  seg.len = 1.4,
                  pch = c(22,19,21,NA,20), col = c("black", "blue", "black", "red", "red"),
                  pt.bg = c("lightgray", NA,"yellow",NA,NA), pt.cex = c(3.2, rep(1,4)),
                  x.intersp=0.5)
    legend(lgd$rect$left, lgd$rect$top, legend = "", bty = 'n',
           pch = NA, lwd = 3, seg.len = 1.4,
           x.intersp=0.5)
    title(paste(horizon,"days", port_name, "cumulative returns vs. its Simulated results"))
  }

  return(list(
    portfolio=port_name,
    alpha=alpha,
    horizon=horizon,
    points_lower_var_count=length(real_ret_lower_var),
    points_lower_var_percentage=length(real_ret_lower_var)/n
  ))
}

# Weights Plot
weights_plot <- function(bt_run,...){
  chart.StackedBar(bt_run$w_bop,
                   space = 0, border=NA,
                   colorset = brewer.pal(9, name="Paired"),
                   xaxs = "i", yaxs = "i", xaxt='n',ylab="", ...)
}

# Graph aux fun
plot_text_factory <- function(.text, ...) {
  function(index) {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', xaxs = "i", yaxs = "i")
    text(0.5, 0.5, .text[index], ...)
  }
}

plot_margin_factory <- function(.text, ...) {
  function(index) {
    mtext(.text[index], ...)
  }
}

# reduce_datasets <- function(bt, datasets_index) {
#   lapply_ka(bt, function(.list) .list[datasets_index])
# }

## End Aux Functions --------------------------------------------------------------------------------------------------

# Backtest portfolios
portfolios <- list("msr" = max.sharpe.ratio.p,
                   "gmv" = gmvp,
                   "rpn" = rpp.naive,
                   "rpv" = rpp.vanilla)

port_name <- c(names(portfolios), "ew")

# # Importing data
# prices_training <- read_rds('data/prices_training.rds')
# prices_test <- read_rds('data/prices_test.rds')
# prices <- rbind(prices_training,prices_test)
#
# tickers <- names(prices)
#
# # Use only last 252 days of trading data to optimize initial portfolio weights
# prices_training <- tail(prices_training, 252)
# prices <- prices[paste0(index(prices_training)[1],"/")] # Analyse performance on the same data period.

################################################################################
################################################################################
################################################################################

# Importing backtests based on realized data
bt_real_20 <- read_rds('data/backtest_portfolios_realized_data_opt20_plus_attributes.rds')
bt_real_5 <- read_rds('data/backtest_portfolios_realized_data_opt5_plus_attributes.rds')
bt_real_1 <- read_rds('data/backtest_portfolios_realized_data_opt1_plus_attributes.rds')

# Importing only backtestTable and analysis as attributes from backtests of simulated data
bt_sim_20 <- read_rds('data/bt_sim_opt20_only_backtestTable_and_analysis.rds')
bt_sim_5 <- read_rds('data/bt_sim_opt5_only_backtestTable_and_analysis.rds')
bt_sim_1 <- read_rds('data/bt_sim_opt1_only_backtestTable_and_analysis.rds')


# names(bt_real_20) <- paste0(names(bt_real_20), "_20")
# names(bt_real_5) <- paste0(names(bt_real_5), "_5")
# names(bt_real_1) <- paste0(names(bt_real_1), "_1")
#
# names(bt_sim_20) <- paste0(names(bt_sim_20), "_20")
# names(bt_sim_5) <- paste0(names(bt_sim_5), "_5")
# names(bt_sim_1) <- paste0(names(bt_sim_1), "_1")

list_bt <- list(opt20=bt_sim_20, opt05=bt_sim_5, opt01=bt_sim_1)

# backtest summary - Median
lapply(list_bt, function(bt)
  backtestSummaryFrombacktestTable(bt, get_backtestTable(bt), summary_fun = median) %>%
    summaryTable(measures=c("Sharpe ratio", "annual return", "annual volatility", "max drawdown", "CAGR")) %>%
    as_tibble(rownames="portfolio")) %>%
  bind_rows(.id = "backtest") %>%
  group_by(backtest) %>%
  arrange(desc(`Sharpe ratio`), .by_group = TRUE) %>%
  ungroup() %>%
  dplyr::select(!(backtest)) %>%
  mutate(`max drawdown` = -`max drawdown`) %>%
  mutate(across(!c(portfolio, `Sharpe ratio`), ~ scales::percent(.x, big.mark = ',', accuracy = 0.01))) %>%
  kbl(caption="Portfolios Median Performance on Simulated Data",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Portfolio","Sharpe Ratio","Annual Return","Volatility", "Max Drawdown", "CAGR"),
      align="lccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=setNames(rep(length(port_name), 3),
                           c("Optimized weights every 1 day", "Optimized weights every 5 days", "Optimized weights every 20 days"))) %>%
  kable_styling(latex_options = "scale_down") %>%
  cat(file = portfolios_performance_tex_file)

########### Graphs ################
# Sim Data - Boxplots
# Boxplot comparing portfolios' Sharpe ratio - all opt
pdf("figs/boxplot-sharpe.pdf", width = a4w/2, height = a4h/3.3)
order.colors <- listBacktestsBoxPlot(list_bt, measure = "Sharpe ratio", sorted = TRUE, type = "simple")
dev.off()

# Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-CAGR.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "CAGR", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()

# Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-annual-return.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "annual return", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()

# Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-annual-volatility.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "annual volatility", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()

# Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-max-drawdown.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "max drawdown", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()

# Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-turnover.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "turnover", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()


# Cumulative Returns chart (realized data)
pdf("figs/cumulativereturn_chart.pdf")
backtestChartCumReturn(bt_real_1) +
  theme_bw() +  theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggtitle("Portfolios Cumulative Return on Realized Data")
dev.off()


# Max Drawdown chart (realized data)
pdf("figs/drawdown_chart.pdf")
backtestChartDrawdown(bt_real_1) +
  theme_bw() +  theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggtitle("Portfolios Drawdown on Realized Data")
dev.off()

### Weights plot
# All Realized Portfolios in all opt setup
plot_top_margin <- plot_margin_factory(c("opt every 20 days", "opt every 5 days", "opt every 1 day"),
                                       side=3, line=1)

par(mar=c(1,2,2,1))
pdf("figs/weights_plot.pdf", width = a4w*mod, height = a4h*mod)
layout(cbind(rep(0,6), rbind(rep(0,3), matrix(1:15, ncol = 3))),
       widths = c(0.04, rep((1-0.04)/3,3)), heights = c(0.02, rep((1-0.02)/5,5)))
pmap(list(real_port=c(bt_real_20, bt_real_5, bt_real_1), p.name=rep(port_name, 3), index=1:15),
     function(real_port, p.name, index) {
       weights_plot(real_port$empirical_data, legend=FALSE)
       if(index %% 5 == 1) plot_top_margin(index %/% 5 + 1)
       if(index <= 5) mtext(p.name, side = 2, line = 3, las=2)
     })
dev.off()
par(def.par)  # reset to default


### Real vs Sim returns All portfolios - opt 1
## horizon 1
pdf("figs/real_vs_sim_allPort_h01.pdf",  width = a4w*mod, height = a4h*mod)
layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
var_transgression_a95_h01 <- pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=1),
     real_vs_sim_returns_plot)
dev.off()

## horizon 5
pdf("figs/real_vs_sim_allPort_h05.pdf",  width = a4w*mod, height = a4h*mod)
layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
var_transgression_a95_h05 <- pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=5),
     real_vs_sim_returns_plot)
dev.off()

## horizon 20
pdf("figs/real_vs_sim_allPort_h20.pdf",  width = a4w*mod, height = a4h*mod)
layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
var_transgression_a95_h20 <- pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=20),
     real_vs_sim_returns_plot)
dev.off()


## all horizons - 1,5,20
pdf("figs/real_vs_sim_allPort.pdf", width = a4w*mod2, height = a4h*mod2)
layout(matrix(1:15, ncol = 3))
pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=1),
     real_vs_sim_returns_plot)
pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=5),
     real_vs_sim_returns_plot)
pmap(list(port_sim=bt_sim_1, port_real=bt_real_1, port_name=port_name, alpha=0.95, horizon=20),
     real_vs_sim_returns_plot)
dev.off()








weights_plot <- function(bt_run,...){
  n <- nrow(bt_run$w_bop)
  n.opt <- nrow(bt_run$w_optimized)

  barplot(bt_run$w_bop,
          space=0, ylim=c(0,1),
          col = brewer.pal(9, name="Paired"), #rev(col_assets),
          border = brewer.pal(9, name="Paired"), #rev(col_assets),
          xaxs = "i", yaxs = "i", axis.lty=1, xaxt='n',
          main = "", ...)
  if(n.opt == n) {
    axis(side=1, at=1:n, labels=FALSE)
    axis(side=1, at=round(seq(1,n, length.out=15)), labels=index(bt_run$w_bop)[round(seq(1,n, length.out=15))])
  }
  else axis(side=1, at=c(1, which(index(bt_run$w_bop) %in% index(bt_run$w_optimized)), n),
            labels=c(index(bt_run$w_bop)[1], index(bt_run$w_optimized)[-1], index(bt_run$w_bop)[n]))
  box()
}


weights_plot_brew <- function(bt_run,...){
  chart.StackedBar(bt_run$w_bop,
                   space = 0, border=NA,
                   colorset = brewer.pal(9, name="Paired"),
                   xaxs = "i", yaxs = "i", xaxt='n',ylab="", ...)
}


weights_plot(bt_real_1$ew$empirical_data)
weights_plot_brew(bt_real_5$ew$empirical_data, legend=FALSE)


chart.StackedBar(bt_real_1$ew$empirical_data$w_bop,
                 space = 0, #border=NA,
                 colorset = brewer.pal(9, name="Paired"),
                 border = brewer.pal(9, name="Paired"),
                 xaxs = "i", yaxs = "i", xaxt='n',ylab="", axis.lty=1, legend=FALSE)



log_returns <- price2logret(prices)[-1]
log_returns.mean <- lapply(log_returns, function(x) na.omit(apply.rolling(x,252))) %>%
  do.call(cbind.xts, .) %>%
  `colnames<-`(colnames(log_returns))


plot(log_returns.mean)



# # Rearrange backtests by same portfolio - different opt
# bt_sim_by_port <- invoke_map(map(port_name, function(port) {function(bts) bts[str_starts(names(bts), port)]}),
#                              .x=list(list(c(bt_sim_20, bt_sim_5, bt_sim_1)))) %>%
#   `names<-`(port_name)
#
# bt_real_by_port <- invoke_map(map(port_name, function(port) {function(bts) bts[str_starts(names(bts), port)]}),
#                              .x=list(list(c(bt_real_20, bt_real_5, bt_real_1)))) %>%
#   `names<-`(port_name)
#
# bt_msr <-list(sim=bt_sim_by_port[["msr"]], real=bt_real_by_port[["msr"]])
# bt_gmv <-list(sim=bt_sim_by_port[["gmv"]], real=bt_real_by_port[["gmv"]])
# bt_rpn <-list(sim=bt_sim_by_port[["rpn"]], real=bt_real_by_port[["rpn"]])
# bt_rpv <-list(sim=bt_sim_by_port[["rpv"]], real=bt_real_by_port[["rpv"]])
# bt_ew <- list(sim=bt_sim_by_port[["ew"]], real=bt_real_by_port[["ew"]])
# rm(bt_sim_by_port, bt_real_by_port)
#
# ### Real vs Sim returns same portfolio - all opt setup
# ## horizon 1,5,20 - same alpha (0.95)
# # Present
# plot_realVsSim_samePort_allOpt <- function(bt, port_name){
#   layout(matrix(1:9, byrow = TRUE, ncol = 3))
#   pmap(list(port_sim=bt$sim, port_real=bt$real, port_name=port_name, alpha=0.95, horizon=1),
#        real_vs_sim_returns_plot)
#   pmap(list(port_sim=bt$sim, port_real=bt$real, port_name=port_name, alpha=0.95, horizon=5),
#        real_vs_sim_returns_plot)
#   pmap(list(port_sim=bt$sim, port_real=bt$real, port_name=port_name, alpha=0.95, horizon=20),
#        real_vs_sim_returns_plot)
# }
#
# plot_realVsSim_samePort_allOpt(bt_msr, names(bt_msr$sim))
#
# # Appendix
# plot_realVsSim_samePort_allOpt(bt_gmv, names(bt_gmv$sim))
# plot_realVsSim_samePort_allOpt(bt_rpn, names(bt_rpn$sim))
# plot_realVsSim_samePort_allOpt(bt_rpv, names(bt_rpv$sim))
# plot_realVsSim_samePort_allOpt(bt_ew, names(bt_ew$sim))



# ### Weights plot - Cont.
# # All portfolios for each opt setup
# layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
# map(bt_real_20, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
# map(bt_real_5, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
# map(bt_real_1, function(real_port) weights_plot(real_port$empirical_data))
#
# # All opt setup for each portfolio
# layout(matrix(c(rep(1:2, each=2), c(0,3,3,0)), byrow = TRUE, ncol = 4))
# map(bt_msr$real, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:2, each=2), c(0,3,3,0)), byrow = TRUE, ncol = 4))
# map(bt_ew$real, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:2, each=2), c(0,3,3,0)), byrow = TRUE, ncol = 4))
# map(bt_gmv$real, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:2, each=2), c(0,3,3,0)), byrow = TRUE, ncol = 4))
# map(bt_rpn$real, function(real_port) weights_plot(real_port$empirical_data))
#
# layout(matrix(c(rep(1:2, each=2), c(0,3,3,0)), byrow = TRUE, ncol = 4))
# map(bt_rpv$real, function(real_port) weights_plot(real_port$empirical_data))
