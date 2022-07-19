# <Paper Name>
#
# This script will test for arch effects for a given vector of returns, given
# lags and save results in .html file.

## OPTIONS

boxTest_tex_file <- 'tabs/boxTest.tex'
archTest_tex_file <- 'tabs/ARCHTest.tex'

## END OPTIONS

library(tidyverse)
library(knitr)
library(kableExtra)
library(spgs)

dirname(rstudioapi::getActiveDocumentContext()$path) %>% setwd()
setwd('..')

source('R/garch_fcts.R')

## Aux Functions ------------------------------------------------------------------------------------------------------
# Extract test results into list from htest Object
get_box_test_results <- function(boxt) {
  list(
    statistic = boxt$statistic,
    p.value=boxt$p.value
  )
}
## End Aux Functions --------------------------------------------------------------------------------------------------

# create directory
if (!dir.exists(dirname(my_html_file))) dir.create(dirname(my_html_file))

# get price data
logret_training <- read_rds('data/logret_training.rds')

#define lag to be log(N) where N is lenght of data
max_lag <- log(nrow(logret_training)) %>% round()

# do tests
box_tests <- lapply(logret_training,function(x) Box.test(x, lag = max_lag, type = "Ljung-Box", fitdf = 0))
box_tests_abs <- lapply(logret_training,function(x) Box.test(abs(x), lag = max_lag, type = "Ljung-Box", fitdf = 0))

arch_tests <- lapply(logret_training, function(x) do_arch_test(x, max_lag = max_lag))

# Save results in tables
box_test_table <- lapply(box_tests, get_box_test_results) %>% bind_rows(.id = "id") %>% column_to_rownames(var="id")
box_test_abs_table <- lapply(box_tests_abs, get_box_test_results) %>% bind_rows(.id = "id") %>% column_to_rownames(var="id")

row_groups <- setNames(rep(7,length(names(arch_tests))),names(arch_tests))
arch_tests_table <- bind_rows(arch_tests)

# change value 0 for 1e-16 in order to a more beautiful print in table
#box_test_table$p.value[box_test_table$p.value == 0] <- '< 2.2e-16'
# box_test_abs_table$p.value[box_test_abs_table$p.value == 0] <- 1e-16

file_format <- 'latex'

bind_cols(box_test_table, box_test_abs_table) %>%
  round(digits = 4) %>%
  mutate(across(starts_with('p'), format, nsmall=4, digits=5)) %>%
  kbl(format=file_format,
      digits = c(2,4,2,4),
      caption = "Results of Ljung-Box for assets' log-returns and absolute log-returns.",
      vline = "",
      linesep = "",
      col.names = c("Statistic",paste0("p-Value",footnote_marker_number(1,file_format)), "Statistic",paste0("p-Value",footnote_marker_number(1,file_format))),
      align="lcccc",
      escape = F,
      longtable = TRUE) %>%
  kable_classic_2(html_font = "helvetica") %>%
  add_header_above(c(" ", "log-returns" = 2, "abs log-returns" = 2), border_left = FALSE, border_right = FALSE) %>%
  footnote(paste0("The degrees of freedom of all statistics is ", max_lag,
                  ", the choice was made based on the fact that log(N) is approx. this value."),
           'The p-values are rounded to the 4th decimal.',
           footnote_as_chunk=F,
           threeparttable = TRUE) %>%
  cat(., file = boxTest_tex_file)



arch_tests_table %>%
  kbl(format=file_format,
      digits = c(0,2,4),
      caption = "Results of ARCH test for assets.",
      vline = "",
      linesep = "",
      col.names = c("Lag", "LM Statistic", "p-value"),
      align="lcccc",
      escape = F,
      longtable = TRUE) %>%
  kable_classic_2(html_font = "helvetica") %>%
  pack_rows(index=row_groups) %>%
  cat(file = archTest_tex_file)
