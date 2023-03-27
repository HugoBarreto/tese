library(rlang)
library(RColorBrewer)
library(kableExtra)
library(tidyverse)
library(xts)
library(portfolioBacktest)
library(riskParityPortfolio)
library(PerformanceAnalytics)
library(RcppRoll)
library(fitHeavyTail)
library(mvtnorm)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/portfolio_functions.R')

portfolios_performance_training_tex_file <- 'tabs/portfolios_performance.tex'
portfolios_performance_test_tex_file <- 'tabs/portfolios_performance_test.tex'
portfolios_rlv_tex_file <- 'tabs/portfolios_points_lower_var.tex'
portfolios_models_plv_tex_file <- 'tabs/portfolios_models_plv.tex'

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

# Rolling Sigma - List index by dates
rollingSigma <- function(df.xts, windowSize=252) {
  windows <- embed(1:nrow(df.xts), windowSize)
  ## convert window matrix to a list
  windowsList <- split(t(windows), rep(1:nrow(windows), each=ncol(windows)))
  ## edit: customize names: "from:to"
  names(windowsList) <- index(df.xts)[unlist(lapply(windowsList, max))]
  invisible(lapply(windowsList, function(x)cov(df.xts[x, ])))
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
  if(measure=="max drawdown") res_table <- -res_table
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
real_vs_sim_returns_plot <- function(portfolio_name=NULL, alpha=0.95, horizon=1, plot=TRUE,
                                     col.outter.band="lightcyan2", col.inner.band="lightblue3",
                                     col.normal="blue", col.over.var="yellow",
                                     col.cvar="red1", col.var="red1",col.median="black") {

  plotAreaCorridor <- function(x, y, col.poly1=col.outter.band,
                               col.poly2=col.inner.band,...) {
    x.pol <- c(x, rev(x), x[1])
    y.pol <- c(y[,1], rev(y[,5]),y[,1][1])
    plot(x, y[,3], type="n", ...)
    polygon(x.pol, y.pol, col=col.poly1, lty=0)

    x.pol <- c(x, rev(x), x[1])
    y.pol <- c(y[,2], rev(y[,4]), y[,2][1])
    polygon(x.pol, y.pol, col=col.poly2, lty=0)

    lines(x, y[,3], col=col.median, lwd=2) # median

    invisible(NULL)
  }

  # Get analysis
  pr.analysis <- get_analysis(bt_test[[portfolio_name]])
  date.ps.analysis <- lapply(bt_simulations,
                             function(date.bt,portfolio_name){
                               get_analysis(date.bt[[portfolio_name]])
                             }, portfolio_name)

  # Names Suffixes
  horizon_suf <- paste0('_h', sprintf("%02d", horizon))

  # Names of Analysis objs
  n.cumret <- paste0('CumReturns', horizon_suf)
  n.cvar <- paste0('CVaR', horizon_suf)
  n.var <- paste0('VaR', horizon_suf)

  alpha <- as.character(alpha)

  # realized cumulative returns
  pr.cumret <- pr.analysis[[n.cumret]]

  # sim VaR and sim CVaR given horizon and alpha
  .f <- function(ps.a, n.stat){
    if(all(!is.na(ps.a[[n.stat]])))
      ps.a[[n.stat]][1,alpha]
  }
  ps.cumret <- do.call(rbind, lapply(date.ps.analysis,
                                     function(ps.a)
                                       if(length(ps.a[[n.cumret]]>0))
                                         ps.a[[n.cumret]][1,]))
  ps.var <- do.call(rbind, lapply(date.ps.analysis, .f, n.var))
  ps.cvar <- do.call(rbind, lapply(date.ps.analysis, .f, n.cvar))

  stopifnot("Sim. and Realized data must have same index"=index(pr.cumret)==index(ps.var))

  # Real return values lower than VaR
  real_ret_lower_var <- which(pr.cumret < ps.var)


  if(plot){
    # Plot ranges
    points.min <- min(ps.cvar, pr.cumret)
    real_range <- range(pr.cumret)

    y.min <- points.min - diff(real_range)*0.02
    data_range <- range(y.min, real_range)
    y.max <- data_range[2] + diff(data_range)*0.13

    # Plot Confidence Bands
    agg <- apply.daily(ps.cumret, FUN=quantile, probs=c(10,25,50,75,90)/100)

    x <- index(agg)
    plotAreaCorridor(x, coredata(agg), xaxt="n", xaxs = "i", xlab="",
                     xlim=range(index(agg))+c(-1,0.5), # doesn't exclude start and end points
                     ylim=c(y.min, y.max), ylab="Portfolio Cumulative Returns")

    xticks <- axTicks(1)
    while (!(all(xticks %in% as.numeric(x)))) {
      xticks[!(xticks %in% as.numeric(x))] <- xticks[!(xticks %in% as.numeric(x))] + 1
    }

    axis(1, xticks, format(x[which(as.numeric(x) %in% xticks)], "%b %Y"))

    lines(x, ps.var, col=col.var, lty=2, lwd=1.2)
    points(x, ps.cvar, col=col.cvar, pch=20, type = 'o')

    # Plot Realized Data (same plot)
    if (length(real_ret_lower_var) > 0){
      points(setdiff(x, x[real_ret_lower_var]), pr.cumret[-real_ret_lower_var], col=col.normal, pch=19)
      points(x[real_ret_lower_var], pr.cumret[real_ret_lower_var], bg=col.over.var, pch=21)
    } else {
      points(x, pr.cumret, col=col.normal, pch=19)
    }

    # Provide legend and title
    lgd.text <- c("Median Cumulative Returns", "25%-75% percentiles band", "10%-90% percentiles band",
                  as.expression(rlang::exprs("Realized Cumulative Returns">="VaR"[!!alpha],
                                             "Realized Cumulative Returns < VaR"[!!alpha],
                                             "VaR"[!!alpha], "ES"[!!alpha])))
    .order <- c(1,2,3,6,4,5,7)
    lgd.text <- lgd.text[.order]

    legend("topleft", legend = lgd.text, bty = 'n', ncol = 2,
           lty=c(1,rep(0,4),2,1)[.order], lwd=c(2,rep(0,6))[.order],
           pch = c(NA,22,22,19,21,NA,20)[.order],
           col = c(col.median, NA, NA, col.normal, "black", col.var, col.cvar)[.order],
           pt.bg = c(NA,col.inner.band, col.outter.band, NA,col.over.var,NA,NA)[.order],
           pt.cex = c(1, rep(2,2), rep(1,4))[.order],
           x.intersp=0.5, text.width = median(strwidth(lgd.text)))

    t.text <- substitute(
      expression(paste(horizon," days ",portfolio_name," cumulative returns, VaR and ES (",alpha==.alpha,")")),
      list(.alpha=alpha, horizon=horizon, portfolio_name=portfolio_name))

    title(eval(t.text))
  }

  invisible(list(
    portfolio=portfolio_name,
    alpha=as.numeric(alpha),
    horizon=horizon,
    var=ps.var,
    cvar=ps.cvar,
    realized_cumret=pr.cumret,
    points_lower_var=real_ret_lower_var,
    points_lower_var_count=length(real_ret_lower_var),
    points_lower_var_percentage=length(real_ret_lower_var)/nrow(pr.cumret)
  ))
}

# Weights Plot
weights_plot <- function(w,...){
  chart.StackedBar(w,
                   space = 0, border=NA,
                   colorset = brewer.pal(ncol(w), name="Paired"),
                   xaxs = "i", yaxs = "i", xaxt='n',ylab="", ...)
}

weights_plot_bt_run <- function(bt_run,...){
  weights_plot(bt_run$w_bop, ...)
}

# Weights plot + Relative Risk Contribution
rollingBarplotPortfolioRisk <- function (w, Sigmas,...)
{

  RRC <- t(sapply(index(w), function(date) {
    .w <- t(as.matrix(w[date,]))
    Sigma <- Sigmas[[as.character(date)]]
    .w * (Sigma %*% .w)
  }))
  RRC <- sweep(RRC, MARGIN = 1, STATS = rowSums(RRC), FUN = "/")
  colnames(RRC) <- colnames(w)
  RRC <- xts(RRC, order.by = index(w))

  p <- weights_plot(RRC, ...)

  invisible(list(RRC=RRC, risk.plot=p))
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

# Function to scale secondary axis
scale_function <- function(x, scale, m1, m2){
  return ((x-m1)*scale + m2)
}

# Function to scale secondary variable values
inv_scale_function <- function(x, scale, m1, m2){
  return ((x - m2)/scale + m1)
}

## End Aux Functions --------------------------------------------------------------------------------------------------

# Backtest portfolios

# # Importing data
assets.filter <- read_rds('data/assets_filter_spec_list.rds')
prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
MM_training <- read_rds('data/MM_training.rds')
MM_test <- read_rds('data/MM_test.rds')

tickers <- names(prices_test)
MM.names <- names(MM_test)
all.names <- c(tickers, MM.names)

effr <- read.csv('data/DFF.csv')
effr <- xts(effr$DFF, order.by=as.Date(effr$DATE))

# Complete sets
prices_training <- merge(prices_training, MM_training) %>% `colnames<-`(all.names)
prices_test <- merge(prices_test, MM_test) %>% `colnames<-`(all.names)
prices <- rbind(prices_training,prices_test)

logret_training <- prices2logreturns(prices_training)[-1]
logret <- prices2logreturns(prices)[-1]
# logret_test <- prices2logreturns(prices_test)[-1]

################################################################################
################################################################################
################################################################################

# # Importing backtests based on realized data
bt_training.20 <- read_rds('data/backtest_training_opt20.rds')
bt_training.5 <- read_rds('data/backtest_training_opt5.rds')
bt_training.1 <- read_rds('data/backtest_training_opt1.rds')
bt_test <- read_rds('data/backtest_test.rds')
bt_simulations <- read_rds('data/backtest_simulations_only_performance.rds')

# # Importing only backtestTable and analysis as attributes from backtests of simulated data
# bt_simulations <- read_rds('data/backtest_simulations_only_performance.rds')

portfolios.names <- names(bt_test)


# names(bt_training.20) <- paste0(names(bt_training.20), "_20")
# names(bt_training.5) <- paste0(names(bt_training.5), "_5")
# names(bt_training.1) <- paste0(names(bt_training.1), "_1")

list_bt <- list(opt20=bt_training.20, opt05=bt_training.5, opt01=bt_training.1)

# backtest summary - Median
lapply(list_bt, function(bt)
  backtestSummaryFrombacktestTable(bt, get_backtestTable(bt), summary_fun = median, portfolio_names = portfolios.names) %>%
    summaryTable(measures=c("Sharpe ratio", "annual return", "annual volatility", "max drawdown", "CAGR"), digits = 4) %>%
    as_tibble(rownames="portfolio")) %>%
  bind_rows(.id = "backtest") %>%
  group_by(backtest) %>%
  arrange(portfolio, .by_group = TRUE) %>%
  ungroup() %>%
  dplyr::select(!(backtest)) %>%
  mutate(`max drawdown` = -`max drawdown`) %>%
  mutate(across(!c(portfolio, `Sharpe ratio`), ~ scales::percent(.x, big.mark = ',', accuracy = 0.01))) %>%
  kbl(caption="Portfolios Median Performance on Samples from Training Data",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Portfolio","Sharpe Ratio","Annual Return","Volatility", "Max Drawdown", "CAGR"),
      align="lccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=setNames(rep(length(portfolios.names), 3),
                           c("Optimized weights every 1 day", "Optimized weights every 5 days", "Optimized weights every 20 days"))) %>%
  kable_styling(latex_options = "scale_down") %>%
  cat(file = portfolios_performance_training_tex_file)

########### Graphs ################
# Training Data - Boxplots
# Boxplot comparing portfolios' Sharpe ratio - all opt
pdf("figs/boxplot-sharpe.pdf", width = a4w/2, height = a4h/3.3)
order.colors <- listBacktestsBoxPlot(list_bt, measure = "Sharpe ratio", sorted = TRUE, type = "simple")
dev.off()
#
# # Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-CAGR.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "CAGR", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()
#
# # Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-annual-return.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "annual return", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()
#
# # Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-annual-volatility.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "annual volatility", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()
#
# # Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-max-drawdown.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "max drawdown", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()
#
# # Boxplot comparing portfolios' turnover - all opt
pdf("figs/boxplot-turnover.pdf", width = a4w/2, height = a4h/3.3)
listBacktestsBoxPlot(list_bt, measure = "turnover", sorted = TRUE, type = "simple",order.colors=order.colors)
dev.off()



# Training Data - Weights plot
# All Realized Portfolios in all opt setup
plot_top_margin <- plot_margin_factory(c("opt every 20 days", "opt every 5 days", "opt every 1 day"),
                                       side=3, line=1)

par(mar=c(1,2,2,1))
pdf("figs/training_weights_plot.pdf", width = a4w*mod, height = a4h*mod)
layout(cbind(rep(0,6), rbind(rep(0,3), matrix(1:15, ncol = 3))),
       widths = c(0.04, rep((1-0.04)/3,3)), heights = c(0.02, rep((1-0.02)/5,5)))
pmap(list(real_port=do.call(c, list_bt), p.name=rep(portfolios.names, 3), index=1:15),
     function(real_port, p.name, index) {
       weights_plot_bt_run(real_port[[1]], legend=FALSE)
       if(index %% 5 == 1) plot_top_margin(index %/% 5 + 1)
       if(index <= 5) mtext(p.name, side = 2, line = 3, las=2)
     })
dev.off()
par(def.par)  # reset to default

# Legend reference
sink("tabs/assets_color_palette.txt")
cat("Assets Colors Palette\n")
cat("=======================================\n")
brewer.pal(10, name="Paired") %>% set_names(names(bt_test$ew$empirical_data$w_optimized))
sink()

# Training Data - Weights plot + RRC plot
windowSize <- 251 # match to the portfolio backtest data
Sigmas <- rollingSigma(logret_training, windowSize)
plot_top_margin2 <- plot_margin_factory(c("Capital", "Relative Risk Contribution"),
                                       side=3, line=1)

par(mar=c(1,2,2,1))
pdf("figs/training_weights_rrc_plot.pdf", width = a4w*mod, height = a4h*mod)
layout(cbind(rep(0,6), rbind(rep(0,2), matrix(1:10,byrow = TRUE, ncol = 2))),
       widths = c(0.04, rep((1-0.04)/2,2)), heights = c(0.02, rep((1-0.02)/5,5)))

pmap(list(real_port=bt_training.1, p.name=portfolios.names, index=1:5),
     function(real_port, p.name, index) {
       weights_plot(real_port[[1]]$w_optimized, legend=FALSE)
       if(index == 1) plot_top_margin2(1)
       mtext(p.name, side = 2, line = 3, las=2)

       rollingBarplotPortfolioRisk(real_port[[1]]$w_optimized, Sigmas,legend=FALSE)
       if(index == 1) plot_top_margin2(2)
     })
dev.off()
par(def.par)  # reset to default


# Test bt - portfolio performance summary
backtestSummaryFrombacktestTable(bt_test, get_backtestTable(bt_test),
                                 summary_fun = median,
                                 portfolio_names = portfolios.names) %>%
  summaryTable(measures=c("Sharpe ratio", "annual return", "annual volatility", "max drawdown", "CAGR"), digits = 4) %>%
  as_tibble(rownames="portfolio") %>%
  mutate(`max drawdown` = -`max drawdown`) %>%
  arrange(portfolio) %>%
  mutate(across(!c(portfolio, `Sharpe ratio`), ~ scales::percent(.x, big.mark = ',', accuracy = 0.01))) %>%
  kbl(caption="Portfolios Performance on Test Data",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Portfolio","Sharpe Ratio","Annual Return","Volatility", "Max Drawdown", "CAGR"),
      align="lccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  kable_styling(latex_options = "scale_down") %>%
  cat(file = portfolios_performance_test_tex_file)


# Test Data - Portfolio Weights
pdf("figs/test_weights_plot.pdf", width = a4w, height = a4w)
layout(matrix(1:4, byrow = TRUE, ncol = 2))
pmap(list(real_port=bt_test, p.name=portfolios.names),
     function(real_port, p.name) {
       if(p.name!="ew"){
         weights_plot_bt_run(real_port[[1]], legend=FALSE)
         title(paste(p.name, "Capital Allocation"))
       }
     })
dev.off()
par(def.par)  # reset to default


# Test Data - Cumulative Returns
pdf("figs/cumulativereturn_chart.pdf")
backtestChartCumReturn(bt_test) +
  scale_y_continuous(name = "Wealth") +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggtitle("Portfolios Wealth on Test Data")
dev.off()


# EFFR plot
pdf("figs/effr.pdf")
ggplot(data=fortify(effr, melt = T)) +
  geom_line(aes(x=.data$Index, y=.data$Value)) +
  scale_y_continuous(name = "EFFR (%)") +
  theme_bw() + theme(panel.grid.minor = element_blank())
dev.off()

# Test Data - Cumulative Returns + EFFR
effr.df <- fortify(effr[index(bt_test[[1]]$empirical_data$return)], melt = T)
wealth <- do.call(cbind, lapply(bt_test, function(x) x[[1]]$wealth))
M1  <- max(wealth)   # Specify max of first y axis
M2 <- max(effr) # Specify max of second y axis
m1  <- min(wealth)   # Specify min of first y axis
m2 <- min(effr) # Specify min of second y axis

# scale variable calculated based on desired mins and maxes
scale = (M2 - m2)/(M1 - m1)

# Plot
pdf("figs/cumulativereturn_effr_chart.pdf")
backtestChartCumReturn(bt_test) +
  geom_line(data=effr.df,
            aes(x=.data$Index, y=inv_scale_function(Value, scale, m1, m2)),
            colour = alpha("Black", 0.4)) +
  scale_y_continuous(
    limits = c(m1, M1),
    # Features of the first axis
    name = "Wealth",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~scale_function(., scale, m1, m2), name="EFFR (%)") #sec.axis = sec_axis(~3.5*(.-0.75)/0.45, name="EFFR (%)")
  ) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggtitle("Portfolios Wealth and EFFR")
dev.off()


# Test Data - Max Drawdown
pdf("figs/drawdown_chart.pdf")
backtestChartDrawdown(bt_test) +
  scale_y_continuous(name = "Drawdown") +
  theme_bw() +  theme(panel.grid.minor = element_blank(), legend.title = element_blank()) +
  ggtitle("Portfolios Drawdown")
dev.off()



### Test Data vs Sim
## each portfolios - all horizons, all alpha (Appendix)
real_vs_sim_results <- lapply(portfolios.names, function(port.name){
  # pdf(paste0("figs/real_vs_sim_",port.name,"_all.pdf"), width = a4w*mod*1.1, height = a4h*mod*1.1)
  plot.input <- expand_grid(.p=port.name, .a=c(0.9,0.95,0.99), .h=c(1,5,21))
  layout(matrix(1:9, ncol = 3, byrow = TRUE))
  res <- pmap(.l=plot.input,
              .f=function(.p,.a,.h)
                real_vs_sim_returns_plot(.p,.a,.h, plot=FALSE)) %>%
    set_names(pmap(plot.input, function(.a, .h, ...) paste0("a",.a*100,"_h",.h)))
  dev.off()
  res
}) %>%
  set_names(portfolios.names)

## horizon 1 - alpha 95
pdf("figs/real_vs_sim_allPort_h01.pdf",  width = a4w*mod, height = a4h*mod)
layout(matrix(c(rep(1:4, each=2), c(0,5,5,0)), byrow = TRUE, ncol = 4))
pmap(list(portfolio_name=portfolios.names, alpha=0.95, horizon=1),
     real_vs_sim_returns_plot)
dev.off()



rvs_tib <- lapply(real_vs_sim_results, function(.p)
  lapply(.p, function(rvs.info){
    plv <- `$`(rvs.info,"points_lower_var")
    tibble(plv_date=as.Date(index(rvs.info[["cvar"]][plv,])),
           cvar=as.numeric(rvs.info[["cvar"]][plv,]),
           ret=as.numeric(rvs.info[["realized_cumret"]][plv,]),
           lag_cummean = c(NA, head(cummean(ret),-1)),
           portfolio=rvs.info[["portfolio"]],
           horizon=rvs.info[["horizon"]],
           alpha=rvs.info[["alpha"]],
           plv.p=rvs.info[["points_lower_var_percentage"]])
  }) %>% bind_rows
) %>% bind_rows

rvs_all_tib <- lapply(real_vs_sim_results, function(.p)
  lapply(.p, function(rvs.info){
    tibble(date=as.Date(index(rvs.info[["var"]])),
           cvar=as.numeric(rvs.info[["cvar"]]),
           var=as.numeric(rvs.info[["var"]]),
           ret=as.numeric(rvs.info[["realized_cumret"]]),
           portfolio=rvs.info[["portfolio"]],
           horizon=rvs.info[["horizon"]],
           alpha=rvs.info[["alpha"]],
           plv = ret < var)
  }) %>% bind_rows
) %>% bind_rows


# Percentage transgression
rvs_tib %>%
  group_by(horizon,portfolio,alpha, plv.p) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = alpha, values_from = plv.p, id_cols = c(portfolio, horizon),
              names_glue = "alpha_{alpha*100}") %>%
  ungroup() %>%
  select(!horizon) %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective VaR",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Portfolio","90% (10%)","95% (5%)","99% (1%)"),
      align="lccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=setNames(rep(length(portfolios.names), 3),
                           c("1-day Cumulative Returns", "5-days Cumulative Returns", "21-days Cumulative Returns"))) %>%
  add_header_above(c(" ", "Confidence Level (Expected Lower Values)" = 3), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T) %>%
  cat(file=portfolios_rlv_tex_file)



rvs_tib %>%
  group_by(horizon,portfolio,alpha, plv.p) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from = alpha, values_from = plv.p, id_cols = c(portfolio, horizon),
              names_glue = "alpha_{alpha*100}") %>%
  ungroup() %>%
  select(!horizon) %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective VaR",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Portfolio","90% (10%)","95% (5%)","99% (1%)"),
      align="lccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=setNames(rep(length(portfolios.names), 3),
                           c("1-day Cumulative Returns", "5-days Cumulative Returns", "21-days Cumulative Returns"))) %>%
  add_header_above(c(" ", "Confidence Level (Expected Lower Values)" = 3), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T)





# CVaR MSE - DELETE
rvs_tib %>%
  group_by(horizon,portfolio,alpha) %>%
  mutate(SE=(cvar-ret)^2, AE=abs(cvar-ret), RAE=AE/abs(ret-lag_cummean), RBias=(cvar-lag_cummean)/abs(lag_cummean)) %>% # APE=AE/abs(ret)*100,
  summarise(RMSD=(mean(SE))^(1/2), CV.RMSD=RMSD/abs(mean(ret)), MAE=mean(AE), MRAE=mean(RAE, na.rm=T), RBias=mean(RBias,na.rm=T)) %>% # MAPE=mean(APE),
  pivot_wider(names_from = alpha, values_from = c(MAE,MRAE,RMSD,CV.RMSD), id_cols = c(portfolio, horizon),
              names_glue = "alpha_{alpha*100}_{.value}") %>%
  ungroup() %>%
  select(!horizon) %>%
  #mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Strategies CVaR MSE",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Confidence Level",rep(c("90%","95%","99%"), 4)),
      align=c("l", rep("c",5*3)),
      digits=c(0, rep(c(rep(4,3),rep(2,3)), length.out=4*3))) %>%
  kable_classic_2(full_width = F, html_font = "helvetica", latex_options ="scale_down ") %>%
  pack_rows(index=setNames(rep(length(portfolios.names), 3),
                           c("1-day Horizon", "5-days Horizon", "21-days Horizon"))) %>%
  add_header_above(c(" "=1, set_names(rep(3, times=4), c("MAE","MRAE","RMSD","CV.RMSD"))), border_left = FALSE, border_right = FALSE) #%>%
  #footnote("The percentage symbol (%) was supressed for a cleaner visualization")




# Graph
rvs_all_tib %>%
  group_by(horizon,portfolio,alpha) %>%
  mutate(plv.p=rollapplyr(plv,width=4*21,FUN=mean, partial=TRUE)) %>%
  mutate(date=as.Date(date)) %>%
  ggplot(aes(date,plv.p)) +
    facet_grid(rows = vars(portfolio), cols = vars(horizon),scales = "free_y") +
    geom_line(aes(group=factor(alpha), col=factor(alpha)))




rvs_tib %>%
  mutate(cvar=-cvar, ret=-ret) %>%
  ggplot(aes(cvar, ret)) +
    geom_point(aes(col=factor(portfolio))) +
    facet_grid(rows = vars(alpha), cols = vars(horizon),scales = "free") +
    geom_line(aes(cvar,cvar),inherit.aes = FALSE) +
    theme_bw() + theme(panel.grid.minor = element_blank(), legend.title = element_blank())

rvs_tib %>%
  group_by(horizon,portfolio,alpha) %>%
  mutate(Bias=mean(cvar-lag_cummean,na.rm=T)) %>%
  filter(horizon==1) %>%
  ggplot(aes(plv_date, group=horizon)) +
    facet_grid(rows = vars(portfolio), cols = vars(alpha),scales = "free_y") +
    geom_step(aes(y=lag_cummean)) +
    geom_point(aes(y=cvar),colour="red1") +
    geom_point(aes(y=ret),colour="yellow") +
    geom_text(aes(label=paste("Bias:",round(Bias,5)), x=head(plv_date, 1),y=0), hjust = "inward") +
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())










### Calculating VaR from different methods

window <- 252*2
probs <- 1-c(0.9,0.95,0.99)

##### Loss Historical Simulation
# Apply w to window of returns => {(t(w)%*%X): Historical port retuns}
# Then retrieve VaR based on empirical data distribution

var_hist <- lapply(portfolios.names,function(port){
  apply.daily(bt_test[[port]]$empirical_data$w_bop, FUN = function(w_bop, window){
    w.index <- which(index(w_bop) == index(logret))
    hist_Lt <- lag.xts(logret)[(w.index-window+1):w.index,] %*% as.vector(w_bop)
    quantile(hist_Lt, probs)
  }, window=window) %>%
    as_tibble(rownames="date") %>%
    mutate(date=as.Date(date), portfolio=port, .before=everything())
}) %>%
  bind_rows() %>%
  rename_with(.cols=!c(date, portfolio),
              .fn = function(name) paste0("alpha_",100-parse_number(name))) %>%
  pivot_longer(!c(date, portfolio), names_to="alpha", values_to = "var",
               names_transform = list(alpha = readr::parse_number)) %>%
  rename(var.hist=var)


# Table
rvs_all_tib %>%
  filter(horizon==1) %>%
  mutate(alpha=alpha*100) %>%
  full_join(var_hist, by=c("portfolio", "alpha", "date"),
             suffix=c(".sim", ".hist")) %>%
  mutate(plv.hist= ret < var.hist, horizon=NULL) %>%
  group_by(portfolio,alpha) %>%
  summarise(plv.model=mean(plv), plv.hist=mean(plv.hist)) %>%
  pivot_wider(names_from = alpha, values_from = c(plv.model, plv.hist),
              id_cols = portfolio, names_glue = "alpha_{alpha}_{.value}") %>%
  select(portfolio, order(readr::parse_number(names(.)[-1])) + 1) %>% # reorder columns to increasing alpha
  ungroup() %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective VaR",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Portfolio", rep(c("Model", "Hist."), 3)),
      align="lcccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  add_header_above(c(" "=1,"90% (10%)"=2,"95% (5%)"=2,"99% (1%)"=2), border_left = FALSE, border_right = FALSE) %>%
  add_header_above(c(" "=1, "Confidence Level (Expected Lower Values)" = 6), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T)

##### Variance-Covariance Model - Normal | Ignoring Time Dependence
varCovar_norm <- lapply(portfolios.names,function(port){
  apply.daily(bt_test[[port]]$empirical_data$w_bop, function(w_bop, window){
    w.index <- which(index(w_bop) == index(logret))
    logret <- lag.xts(logret)[(w.index-window+1):w.index,]

    Sigma <- cov(logret)
    mu <- colMeans(logret)

    w <- as.vector(w_bop)
    qnorm(probs, mean = mu%*%w, sd = t(w)%*%Sigma%*%w) %>%
      set_names(1-probs)
  }, window = window) %>%
    as_tibble(rownames="date") %>%
    mutate(date=as.Date(date), portfolio=port, .before=everything())
}) %>%
  bind_rows() %>%
  rename_with(.cols=!c(date, portfolio),
              .fn = function(name) paste0("alpha_",100*parse_number(name))) %>%
  pivot_longer(!c(date, portfolio), names_to="alpha", values_to = "var",
               names_transform = list(alpha = readr::parse_number)) %>%
  rename(var.varCovarNorm=var)

##### Variance-Covariance Model - Student | Ignoring Time Dependence
varCovar_t <- lapply(portfolios.names,function(port){
  apply.daily(bt_test[[port]]$empirical_data$w_bop, function(w_bop, window){
    w.index <- which(index(w_bop) == index(logret))
    logret <- lag.xts(logret)[(w.index-window+1):w.index,]

    student <- fit_mvt(logret)

    mu <- student$mu
    # Not a scaled t distribution
    Sigma <- student$scatter
    nu <- student$nu

    w <- as.vector(w_bop)
    (mu%*%w + sqrt(t(w)%*%Sigma%*%w) * qt(probs, df = nu)) %>%
      set_names(1-probs)
  }, window = window) %>%
    as_tibble(rownames="date") %>%
    mutate(date=as.Date(date), portfolio=port, .before=everything())
}) %>%
  bind_rows() %>%
  rename_with(.cols=!c(date, portfolio),
              .fn = function(name) paste0("alpha_",100*parse_number(name))) %>%
  pivot_longer(!c(date, portfolio), names_to="alpha", values_to = "var",
               names_transform = list(alpha = readr::parse_number)) %>%
  rename(var.varCovarStudent=var)


# Table
rvs_all_tib %>%
  filter(horizon==1) %>%
  mutate(alpha=alpha*100) %>%
  full_join(varCovar_t, by=c("portfolio", "alpha", "date"),
            suffix=c(".sim", ".hist")) %>%
  mutate(plv.hist= ret < var.varCovarStudent, horizon=NULL) %>%
  group_by(portfolio,alpha) %>%
  summarise(plv.model=mean(plv), plv.hist=mean(plv.hist)) %>%
  pivot_wider(names_from = alpha, values_from = c(plv.model, plv.hist),
              id_cols = portfolio, names_glue = "alpha_{alpha}_{.value}") %>%
  select(portfolio, order(readr::parse_number(names(.)[-1])) + 1) %>% # reorder columns to incressing alpha
  ungroup() %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective VaR",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Portfolio", rep(c("Model", "Hist."), 3)),
      align="lcccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  add_header_above(c(" "=1,"90% (10%)"=2,"95% (5%)"=2,"99% (1%)"=2), border_left = FALSE, border_right = FALSE) %>%
  add_header_above(c(" "=1, "Confidence Level (Expected Lower Values)" = 6), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T)


##### Variance-Covariance Model | EWMA
varCovar_EWMA <- lapply(portfolios.names,function(port, nu){
  alpha <- 0.06
  mu <- rep(0,ncol(logret_training))
  Sigma <- diag(rep(0,ncol(logret_training)))
  apply.daily(na.omit(lag.xts(logret_training)), function(rets){
    rets <- coredata(rets)
    mu <<- rets*alpha + mu*(1-alpha)
    Sigma <<- t(rets-mu)%*%(rets-mu) * alpha + Sigma*(1-alpha)
  })

  apply.daily(bt_test[[port]]$empirical_data$w_bop, function(w_bop){
    w.index <- which(index(w_bop) == index(logret))
    logret <- coredata(lag.xts(logret)[w.index,])

    mu <<- logret*alpha + mu*(1-alpha)
    Sigma <<- t(logret-mu)%*%(logret-mu) * alpha + Sigma*(1-alpha)

    w <- as.vector(w_bop)
    # Scaled t distribution
    (mu%*%w + sqrt(t(w)%*%Sigma%*%w * ((nu-2)/nu))  * qt(probs, df = nu)) %>%
      set_names(1-probs)
  }) %>%
    as_tibble(rownames="date") %>%
    mutate(date=as.Date(date), portfolio=port, .before=everything())
}, nu=5) %>%
  bind_rows() %>%
  rename_with(.cols=!c(date, portfolio),
              .fn = function(name) paste0("alpha_",100*parse_number(name))) %>%
  pivot_longer(!c(date, portfolio), names_to="alpha", values_to = "var",
               names_transform = list(alpha = readr::parse_number)) %>%
  rename(var.varCovarEWMA=var)

rvs_all_tib %>%
  filter(horizon==1) %>%
  mutate(alpha=alpha*100) %>%
  full_join(varCovar_EWMA, by=c("portfolio", "alpha", "date"),
            suffix=c(".sim", ".hist")) %>%
  mutate(plv.hist= ret < var.varCovarEWMA, horizon=NULL) %>%
  group_by(portfolio,alpha) %>%
  summarise(plv.model=mean(plv), plv.hist=mean(plv.hist)) %>%
  pivot_wider(names_from = alpha, values_from = c(plv.model, plv.hist),
              id_cols = portfolio, names_glue = "alpha_{alpha}_{.value}") %>%
  select(portfolio, order(readr::parse_number(names(.)[-1])) + 1) %>% # reorder columns to incressing alpha
  ungroup() %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective VaR",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Portfolio", rep(c("Model", "Hist."), 3)),
      align="lcccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  add_header_above(c(" "=1,"90% (10%)"=2,"95% (5%)"=2,"99% (1%)"=2), border_left = FALSE, border_right = FALSE) %>%
  add_header_above(c(" "=1, "Confidence Level (Expected Lower Values)" = 6), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T)

plot(3:30, sapply(3:30, function(nu){((nu-2)/nu)^(1/2) * qt(0.01, df = nu)}), col="red", pch=16, ylim = c(-5,0))
points(3:30, sapply(3:30, function(nu){ qt(0.01, df = nu)}), col="blue", pch=16)
points(3:30, sapply(3:30, function(nu){ qnorm(0.01)}), col="black", pch=16)




##### Variance-Covariance Model - Student | ARMA-GARCH Fit + Multivariate Student Residuals
########
varCovar_garch_t <- (function(){
  Z <- s <- m <- NULL
  lapply(assets.filter, function(asset){
    Z_asset <- residuals(asset$filter, standardize=TRUE) %>% setNames(asset$asset)
    s_asset <- sigma(asset$filter) %>% setNames(asset$asset)
    m_asset <- fitted(asset$filter) %>% setNames(asset$asset)
    if(is.null(Z)) {
      Z <<- Z_asset
      s <<- s_asset
      m <<- m_asset
    }
    else {
      Z <<- merge(Z, Z_asset)
      s <<- merge(s, s_asset)
      m <<- merge(m, m_asset)
    }
  })

  Z_training <- Z[index(logret_training)]
  student <- fit_mvt(Z_training)

  nu <- student$nu
  # scaled t innovations | $cov != $scatter
  invDelta <- diag(diag(student$cov)^(-1/2))
  Corr <- invDelta%*% student$cov %*%invDelta

  lapply(portfolios.names,function(port){
    apply.daily(bt_test[[port]]$empirical_data$w_bop, function(w_bop){
      MM.col <- which("MM"==names(w_bop))

      w.index <- which(index(w_bop) == index(m))
      w <- as.vector(w_bop)

      Delta <- diag(coredata(s)[w.index,])
      Sigma <- Delta%*%Corr%*%Delta


      (w[MM.col] %*% lag.xts(logret)[index(w_bop), MM.col] + m[w.index,]%*%w[-MM.col] +
          sqrt(t(w[-MM.col])%*%Sigma%*%w[-MM.col]) * qt(probs, df = nu)) %>%
        set_names(1-probs)
    }) %>%
      as_tibble(rownames="date") %>%
      mutate(date=as.Date(date), portfolio=port, .before=everything())
  })
})() %>%
  bind_rows() %>%
  rename_with(.cols=!c(date, portfolio),
              .fn = function(name) paste0("alpha_",100*parse_number(name))) %>%
  pivot_longer(!c(date, portfolio), names_to="alpha", values_to = "var",
               names_transform = list(alpha = readr::parse_number)) %>%
  rename(var.varCovarGarch=var)

# Table
rvs_all_tib %>%
  filter(horizon==1) %>%
  mutate(alpha=alpha*100) %>%
  rename(model_plv=plv) %>%
  full_join(varCovar_norm, by=c("portfolio", "alpha", "date")) %>%
  full_join(varCovar_t, by=c("portfolio", "alpha", "date")) %>%
  # full_join(varCovar_garch_t, by=c("portfolio", "alpha", "date")) %>%
  full_join(varCovar_EWMA, by=c("portfolio", "alpha", "date")) %>%
  full_join(var_hist, by=c("portfolio", "alpha", "date")) %>%
  mutate(across(.cols = starts_with("var."), .fns = ~ ret < .x, .names = "{.col}_plv"), horizon=NULL,cvar=NULL) %>%
  relocate(model_plv, .after = var.varCovarEWMA_plv) %>%
  group_by(portfolio,alpha) %>%
  summarise(across(.cols = ends_with("plv"), .fns = mean)) %>%
  pivot_wider(names_from = alpha, values_from = !c(alpha, portfolio),
              id_cols = portfolio, names_glue = "alpha_{alpha}_{.value}") %>%
  select(portfolio, order(readr::parse_number(names(.)[-1])) + 1) %>% # reorder columns to incressing alpha
  ungroup() %>%
  mutate(across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective estimated VaR",
      format= "html",
      vline = "",
      linesep = "",
      col.names = c("Portfolio", rep(c("Norm","Student","Copula-GARCH","EWMA", "Hist."), 3)),
      align="lcccccc",
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  add_header_above(c(" "=1,"90% (10%)"=5,"95% (5%)"=5,"99% (1%)"=5), border_left = FALSE, border_right = FALSE) %>%
  add_header_above(c(" "=1, "Confidence Level (Theoretical Value)" = 15), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization", footnote_as_chunk = T)





row_groups <- rep(5,3)
names(row_groups) <- c("$alpha=90%$","$alpha=95%$","$alpha=99%$")

rvs_all_tib %>%
  filter(horizon==1) %>%
  mutate(alpha=alpha*100) %>%
  rename(model_plv=plv) %>%
  full_join(varCovar_norm, by=c("portfolio", "alpha", "date")) %>%
  full_join(varCovar_t, by=c("portfolio", "alpha", "date")) %>%
  # full_join(varCovar_garch_t, by=c("portfolio", "alpha", "date")) %>%
  full_join(varCovar_EWMA, by=c("portfolio", "alpha", "date")) %>%
  full_join(var_hist, by=c("portfolio", "alpha", "date")) %>%
  mutate(across(.cols = starts_with("var."), .fns = ~ ret < .x, .names = "{.col}_plv"), horizon=NULL,cvar=NULL) %>%
  relocate(model_plv, .after = var.varCovarEWMA_plv) %>%
  group_by(portfolio,alpha) %>%
  summarise(across(.cols = ends_with("plv"), .fns = mean)) %>%
  arrange(alpha) %>%
  ungroup() %>%
  mutate(alpha=NULL, across(!c(portfolio), ~ .*100)) %>%
  kbl(caption="Percentage of cumulative returns lower than respective estimated VaR",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Portfolio", c("Normal","Student t",  paste0("EWMA", footnote_marker_symbol(1)),
                                   "Copula-GARCH", paste0("Historical Sim.", footnote_marker_symbol(2)))),
      align="lccccc",
      escape = FALSE,
      digits = 2) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=row_groups) %>%
  add_header_above(c(" "=1, "Var-Covariance" = 3, " "=2), border_left = FALSE, border_right = FALSE) %>%
  #add_header_above(c(" "=1, "Confidence Level (Theoretical Value)" = 15), border_left = FALSE, border_right = FALSE) %>%
  footnote("The percentage symbol (%) was suppressed for a cleaner visualization",
           symbol = c("The conditional distribution is Student t with $nu=5$",
                      "The observed window was the prior 504 trading days (2 year)"),
           footnote_as_chunk = T) %>%
  cat(file = portfolios_models_plv_tex_file)


