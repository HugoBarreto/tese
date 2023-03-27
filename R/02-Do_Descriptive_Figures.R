# <Paper Name>
#
# This script will ...
# , producing Figure 01 at the end of its execution

performance_tex_file <- 'tabs/assets_performance.tex'

# load libraries
library(RColorBrewer)
library(cowplot)
library(forecast)
library(xts)
library(rugarch)
library(tidyverse)
library(gridExtra)
library(kableExtra)
library(PerformanceAnalytics)

# close all existing plot windows
graphics.off()

# change directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

# make sure folder "fig" exists
if (!dir.exists('figs')) dir.create('figs')
if (!dir.exists('tabs')) dir.create('tabs')

# source functions
source('R/utils.R')
source('R/garch_fcts.R')

# get price data
logret_training <- read_rds('data/logret_training.rds')
logret_test <- read_rds('data/logret_test.rds')

prices_training <- read_rds('data/prices_training.rds')
prices_test <- read_rds('data/prices_test.rds')
MM_training <- read_rds('data/MM_training.rds')
MM_test <- read_rds('data/MM_test.rds')

prices <- rbind(prices_training,prices_test)
MM <- rbind(MM_training, MM_test)

tickers <- names(prices)
MM.names <- names(MM)
all.names <- c(tickers, MM.names)

prices <- merge(prices, MM) %>% `colnames<-`(all.names)

# Plot measures
a4w <- 21/2.54
a4h <- 29.7/2.54

# For reproducibility concerns
set.seed(42)

## Aux Functions ------------------------------------------------------------------------------------------------------
compute_assets_performance <-  function(returns){
  period_length <-  nrow(returns)
  tibble(
    asset=colnames(returns),
    total_return = prod(returns+1, na.rm = TRUE) - 1,
    annualized_return=mean(returns, na.rm = TRUE)*252,
    vol = sd(returns, na.rm = TRUE) * sqrt(252),
    sharpe = annualized_return/vol,
    cagr = (1 + total_return)^(252/period_length) - 1,
    maxDrawdown = -maxDrawdown(returns),
  ) %>%
    relocate(sharpe, .after = asset) %>%
    relocate(total_return, .after = everything())
}

logreturn_series_plot <- function(data){
  datadf <- fortify.zoo(data)
  names(datadf) <- c("Index", "value")
  ggplot(datadf, aes(x=Index, y=value)) +
    geom_line() +
    ggtitle(colnames(data)) +
    xlab("") + ylab("") + #ylim(c(-1,1)) +
    theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

custom_ggAcf <- function(data){
  ggAcf(data) +
    ggtitle(colnames(data)) +
    xlab("") + ylab("") +
    theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

returns2percentage <- function(.logret, digits=3) {
  logreturns2returns(.logret) %>%
    (function(ret) paste(format(ret*100, scientific=T, digits=digits),'\\%'))
}
## End Aux Functions --------------------------------------------------------------------------------------------------
# Print performance table
bind_rows(lapply(prices2returns(prices), compute_assets_performance)) %>%
  mutate(across(!c(asset, sharpe), .fns = scales::percent, accuracy = 0.01)) %>%
  kbl(caption="Assets performance in 03/10/2016 - 30/09/2022",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Asset","Sharpe Ratio","Annualized Return", "Volatility","CAGR","Max. Drawdown", "Total Return"),
      align="lccrrcr",
      digits = 4) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  cat(file = performance_tex_file)


##### log prices plot #####
first_row <- lapply(rep(10, length(colnames(logret_training))), function(x) x) %>%
  set_names(colnames(logret_training))
first_row <- c(list(date=as.character(index(logret_training)[1])), first_row)

index_log_prices <- tail(rbind(logret_training,logret_test),-1) %>%
  as_tibble(rownames="date") %>%
  mutate(across(.cols = !'date', .fns = function(x) logreturns2prices(x, startPrice = 10)[-1])) %>%
  add_row(!!!first_row, .before = 1) %>%
  mutate(across(.cols = !'date', .fns =log10)) %>%
  pivot_longer(!date, names_to = "asset", values_to = "price") %>%
  mutate(date=as.Date(date)) %>%
  rename(trading.day=date)

pdf("figs/logprices.pdf", width = a4h*0.9, height = a4w*0.9)
ggplot(index_log_prices, aes(x = trading.day, y= price, group=asset)) +
  geom_line(aes(color= asset)) +
  scale_color_brewer(palette="Paired") +
  labs(title = paste0('03/10/2016 - 30/09/2022'),
       x = '',
       y = expression(log[10]~value),
       caption = 'Data from Yahoo Finance') +
  theme_classic()
dev.off()





##### Series log returns #####
logrets_plots <- lapply(logret_training, function(x) logreturn_series_plot(x))
pdf("figs/logrets.pdf", width = a4w*0.9, height = a4w*0.9)
grid.arrange(grobs=logrets_plots, ncol=3)
dev.off()


##### Autocorrelated series (AR-MA) examples ACF plots #####
arma_ex <- ugarchpath(ugarchspec(variance.model=list(garchOrder = c(0,0)),
                                 mean.model = list(armaOrder = c(1,1)),
                                 fixed.pars = list(mu = 0, omega=1, ar1 = 0.5, ma1 = 0.8)),
                      n.sim = 1000, rseed = 42)


## Acf log-returns
stoch_series <- tibble(SWN= fitted(ugarchpath(ugarchspec(variance.model=list(garchOrder = c(0,0)),
                                                   mean.model = list(armaOrder = c(0,0)),
                                                   fixed.pars = list(mu = 0, omega=1)),
                                        n.sim = 1000, rseed = 42))[,1],
                       MA= fitted(ugarchpath(ugarchspec(variance.model=list(garchOrder = c(0,0)),
                                                   mean.model = list(armaOrder = c(0,1)),
                                                   fixed.pars = list(mu = 0, omega=1, ma1 = 0.3)),
                                        n.sim = 1000, rseed = 42))[,1],
                       AR= fitted(ugarchpath(ugarchspec(variance.model=list(garchOrder = c(0,0)),
                                                   mean.model = list(armaOrder = c(1,0)),
                                                   fixed.pars = list(mu = 0, omega=1, ar1 = 0.5)),
                                        n.sim = 1000, rseed = 42))[,1],
                       ARMA= fitted(arma_ex)[,1]) %>%
  as.xts(order.by=as.Date(Sys.Date():(Sys.Date()+999)))

.p <- lapply(stoch_series, function(x) custom_ggAcf(x))
pdf("figs/ACFexample.pdf")
grid.arrange(grobs=.p, ncol=2)
dev.off()

##### ACF Training data plots #####
## Acf log-returns
.p <- lapply(logret_training, function(x) custom_ggAcf(x))
pdf("figs/ACF.pdf")
grid.arrange(grobs=.p, ncol=3)
dev.off()

## Acf abs log-returns
.p <- lapply(logret_training, function(x) custom_ggAcf(abs(x)))
pdf("figs/ACFabs.pdf")
grid.arrange(grobs=.p, ncol=3)
dev.off()

## Acf square log-returns
.p <- lapply(logret_training, function(x) custom_ggAcf(x^2))
pdf("figs/ACFsquare.pdf")
grid.arrange(grobs=.p, ncol=3)
dev.off()



