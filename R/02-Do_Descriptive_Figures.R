# A Garch Tutorial with R - Create Descriptive Figure
# Paper at <https://rac.anpad.org.br/index.php/rac/article/view/1420>
#
# This script will use the financial data from previous script and, additionally,
# import inflation data from the Brazilian Central Bank Database
# <https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries>
# , producing Figure 01 at the end of its execution

# OPTIONS
n_largest <- 10 # number of largest absolute returns to plot

# END OPTIONS

# load libraries
library(cowplot)
library(tidyverse)
library(GetBCBData)
library(forecast)

# close all existing plot windows
graphics.off()

# change directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

# make sure folder "fig" exists
if (!dir.exists('figs')) dir.create('figs')

# source functions
source('R/utils.R')
source('R/garch_fcts.R')

# get price data
df_prices <- read_rds('data/prices.rds')
df_logret <- read_rds('data/logret.rds')
series_name <- names(df_logret)

# log returns plot
df_index <-  select(df_logret, !'ref.date') %>%
  cumsum() %>%
  mutate(ref.date = df_logret$ref.date) %>%
  gather(key = "variable", value = "value", -ref.date)

df_index <-  select(df_logret, !'ref.date') %>%
  ret2price() %>%
  log2() %>%
  as_tibble() %>%
  mutate(ref.date = df_prices$ref.date) %>%
  gather(key = "variable", value = "value", -ref.date)

series_name <- 'teste'

p1 <- ggplot(df_index, aes(x = ref.date, y= value)) +
  geom_line(aes(color= variable)) #+
  # labs(title = paste0('Prices of ', series_name),
       # subtitle = paste0('Total nominal arithmetic return equals to ',
       #                   my_perc(total_ibov_ret),
       #                   ' (', my_perc(ret_ibov_year), ' per year)\n',
       #                   'Total real return, adjusted for inflation, equals to ',
       #                   my_perc(real_ret_ibov),
       #                   ' (', my_perc(real_ret_ibov_year), ' per year)'),
       # x = '',
       # y = 'Index Value',
       # caption = 'Data from Yahoo Finance') +
  # theme_bw(base_family = "TT Times New Roman")

# Autocorrelation plot

ggAcf(df_logret$`BTC-USD`^2)

# calculate largest absolute price variations
largest_tab <- df_prices %>%
  group_by(ticker) %>%
  top_n(abs(log_ret), n = n_largest)

# create second plot
p2 <- ggplot(df_prices,
             aes(x = ref.date, y = log_ret)) +
  geom_line() +
  labs(title = paste0('Nominal Daily Log Returns of ', series_name),
       subtitle = paste0('Red circles represent the largest ', n_largest,
                         ' absolute price variations in the sample'),
       x = '',
       y = 'Log Returns',
       caption = 'Data from Yahoo Finance') +
  theme_bw(base_family = "TT Times New Roman") +
  geom_point(data = largest_tab, aes(x = ref.date, y = log_ret),
             size = 3, color = 'red'  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(size = 'Absolute Price Variation') # +
  scale_color_brewer(palette = 'BrBG')

# bind plots together
p <- plot_grid(p1, p2, nrow = 2,
               labels = 'AUTO')

# show and save
# ERROR in old Code: invalid 'bg' value
#x11() ; p ; ggsave(filename = paste0('figs/fig02_', series_name, '_prices_returns.png'),
 #                  plot = p)

x11() ; p1 ; ggsave(filename = paste0('figs/fig02a_', series_name, '_prices.png'),
                   plot = p1)
x11() ; p2 ; ggsave(filename = paste0('figs/fig02b_', series_name, '_returns.png'),
                    plot = p2)

# build autocorrelagram
p <- ggAcf(x = df_prices$log_ret, lag.max = 10) +
  labs(title = paste0('Autocorrelogram for the Log Returns of ', series_name)) +
  theme_bw(base_family = "TT Times New Roman")

x11()  ; p ; ggsave('figs/fig03_autocorrelation_logret.png')
