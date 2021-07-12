# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

library(tidyverse)
library(FinTS)
library(texreg)
library(rugarch)
library(Rsafd)
library(copula)

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)
setwd('..')

source('R/utils.R')
source('R/garch_fcts.R')

# Importing log returns data
df_logret <- read_rds('data/logret.rds')

## Only for staging phase - Not Final
df_logret <- df_logret[c('SPY','BTC-USD', 'ETH-USD', 'ref.date')]
tickers <- exclude_element(names(df_logret), 'ref.date')

# Modeling OPTIONS
ar_lag <- 1 # lag used for ar term in mean equation (0 in paper)
ma_lag <- 1 # lag used for ma term in mean equation (0 in paper)
arch_lag <- 1 # lag in arch effect (1 in paper)
garch_lag <- 1 # lag in garch effect (1 in paper)
garch_model <- c('gjrGARCH') # see rugarch manual for more
distribution_to_estimate <- 'std' # distribution used in all models
my_html_file <- 'tabs/tab04-estimation_garch.html' # where to save html file?
# END OPTIONS

# close all opened windows
graphics.off()


# define garch estimation function
estimate_garch <- function(data,
                           ticker,
                           ar_lag=0,
                           ma_lag=0,
                           arch_lag=1,
                           garch_lag=1,
                           garch_model='sGARCH',
                           distribution_to_estimate='norm') {

  message('Estimating ARMA(',ar_lag,',', ma_lag, ')', '-',
          garch_model, '(', arch_lag, ',', garch_lag, ') ',
          'dist = ', distribution_to_estimate)

  # estimate model
  my_spec <- ugarchspec(mean.model = list(armaOrder = c(ar_lag, ma_lag)),
                        variance.model = list(model = garch_model,
                                              garchOrder = c(arch_lag, garch_lag)),
                        distribution.model = distribution_to_estimate)

  my_garch <- ugarchfit(spec = my_spec, data = data[[ticker]])

  return(my_garch)
}



# get all combinations of models
models_to_estimate <- expand_grid(ticker = tickers,
                       ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       garch_model,
                       distribution_to_estimate)


# Estimate all GARCH models
l_models <- pmap(.l = models_to_estimate, .f = estimate_garch, df_logret)
names(l_models) <- tickers

# Estimate the Semi-Parametric CDFs

## Utilizando o default da função gpd.tails
## Uma melhora seria escolher os thresholds com base no resultado
## da função shape.plot

models_std_residuals <- map(l_models, (function (model) as.numeric(residuals(model, standardize=TRUE))))
residualsSemiParamDist <- map(models_std_residuals, gpd.tail, plot=F)

# Fit student-t Copula

U <- map2(models_std_residuals, residualsSemiParamDist, gpd.2p)
U.T <- t(matrix(unlist(U, use.names = F), nrow = length(U), byrow = T))

fittedCopula <- fitCopula(tCopula(dim=3), U.T)

#


## Temp
spyResid <- models_std_residuals$SPY

qqnorm(spyResid)
shape.plot(spyResid[spyResid <= 0], tail = "lower")
shape.plot(spyResid[spyResid >= 0], tail = "upper", to=0.96)

spyTailsEst <- gpd.tail(spyResid, lower = -1)

## Plotando dist semi paramétrica de 1 ativo
minProb <- gpd.2p(min(spyResid), spyTailsEst)
lowerThreshProb <- 1 - spyTailsEst$p.larger.lower.thresh
upperThreshProb <- spyTailsEst$p.less.upper.thresh
maxProb <- gpd.2p(max(spyResid), spyTailsEst)

pLowerTail <- seq(minProb, lowerThreshProb, length.out = 200)
pUpperTail <- seq(upperThreshProb, maxProb, length.out = 200)
pInterior <- seq(lowerThreshProb, upperThreshProb, length.out = 200)

plot_data <- bind_rows(
  tibble(q=gpd.2q(pLowerTail, spyTailsEst), p=pLowerTail, color="red"),
  tibble(q=gpd.2q(pInterior, spyTailsEst), p=pInterior, color="black"),
  tibble(q=gpd.2q(pUpperTail, spyTailsEst), p=pUpperTail, color="blue")
)

ggplot(data=plot_data) +
  geom_line(mapping=aes(x=q, y=p), color=plot_data$color) +
  annotate("point", x = spyTailsEst$lower.thresh, y = lowerThreshProb, colour = "blue") +
  annotate("point", x = spyTailsEst$upper.thresh, y = upperThreshProb, colour = "blue")



# save models in file
models_out <- 'data/models.rds'
write_rds(l_models, models_out)



data <- sort(spyResid)
n <- length(data)
l1 <- data[trunc(0.5 * n)]
l2 <- data[trunc(0.98 * n)]
x <- pretty(c(l1, l2), n = 30)
tail(data)

## Format and print a table in html - Reporting code

# # make sure dir "tabs" exists
# if (!dir.exists('tabs')) dir.create('tabs')
#
# # reformat models for texreg
# l_models_tr <- map(l_models, extract.rugarch, include.rsquared = FALSE)
#
# # write custom row
# custom_row <- list('Variance Model' = df_grid$models_to_estimate,
#                    'Distribution' = df_grid$distribution_to_estimate)
# custom_names <- paste0('Model ', 1:length(l_models))
#
#
# # save to html
# htmlreg(l_models,
#         file = my_html_file,
#         custom.gof.rows = custom_row,
#         custom.model.names = custom_names,
#         digits = 3)
#
# # print to screen
# screenreg(l_models,
#           custom.gof.rows = custom_row,
#           custom.model.names = custom_names,
#           digits = 3)
#
#
