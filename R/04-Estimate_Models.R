# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

## OPTIONS
infocriteria_tex_file <- 'tabs/infocriteria.tex'
resdist_params_tex_file <- 'tabs/residuals_dist_params.tex'

## Modeling OPTIONS
ar_lag <- c(0,1) # lag used for ar term in mean equation (0 in paper)
ma_lag <- c(0,1) # lag used for ma term in mean equation (0 in paper)
arch_lag <- c(0,1) # lag in arch effect (1 in paper)
garch_lag <- c(0,1) # lag in garch effect (1 in paper)
garch_model <- c('sGARCH','gjrGARCH') # see rugarch manual for more
distribution_to_estimate <- 'norm' # distribution used in all models
## END OPTIONS


# Libraries Import
library(tidyverse)
library(xts)
library(rugarch)
library(Rsafd)
library(copula)
library(forecast)
library(gridExtra)
library(spgs)
library(kableExtra)
library(GGally)
library(FRAPO)
library(corrplot)

# Setting WD to repo root
dirname(rstudioapi::getActiveDocumentContext()$path) %>% setwd()
setwd('..')

source('R/garch_fcts.R')
source('R/04-Estimate_Models.R')

## Aux Functions ------------------------------------------------------------------------------------------------------
# Function to get models' name
extract_models_name <- function(dynamic_models_to_estimate){
  apply(dynamic_models_to_estimate,1,function(r) paste0(r[1]," ARMA(",r[2],",",r[3],")-",r[6],"(",r[4],",",r[5],")"))
}

# Dynamic model estimation function
estimate_garch <- function(data,
                           ticker,
                           ar_lag=0,
                           ma_lag=0,
                           arch_lag=1,
                           garch_lag=1,
                           garch_model='sGARCH',
                           distribution_to_estimate='norm') {

  message('Asset: ', ticker, ' | ', 'Estimating ARMA(',ar_lag,',', ma_lag, ')',
          '-', garch_model, '(', arch_lag, ',', garch_lag, ') ',
          'dist = ', distribution_to_estimate)

  # estimate model
  my_spec <- ugarchspec(mean.model = list(armaOrder = c(ar_lag, ma_lag)),
                        variance.model = list(model = garch_model,
                                              garchOrder = c(arch_lag, garch_lag)),
                        distribution.model = distribution_to_estimate)

  my_garch <- ugarchfit(spec = my_spec, data = data[,ticker])

  return(my_garch)
}

# Customized filled.contour function which only plots the heatmap and nothing else
custom.filled.contour <- function(x = seq(0, 1, length.out = nrow(z)),
                                  y = seq(0, 1, length.out = ncol(z)),
                                  z,
                                  xlim = range(x, finite = TRUE),
                                  ylim = range(y, finite = TRUE),
                                  zlim = range(z, finite = TRUE),
                                  levels = pretty(zlim, nlevels), nlevels = 20,
                                  color.palette = function(n) hcl.colors(n, "YlOrRd", rev = TRUE),
                                  col = color.palette(length(levels) - 1)
){
  plot.new()
  plot.window(xlim, ylim, xaxs = "i", yaxs = "i")
  .filled.contour(x, y, z, levels, col)
  box()
}

# Plots a string centered in a lightgrey box
text_plot <- function(.text, ...){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', xaxs = "i", yaxs = "i")
  rect(0,0,1,1,border = NA,col='lightgrey')
  text(0.5,0.5, .text, ...)
  box()
}

# Creates a upper triangle plot layout with identifier labels on top and right
make.layout_matrix <- function(n_assets){
  layout_matrix <- matrix(1,(n_assets-1),(n_assets-1))
  layout_matrix[lower.tri(layout_matrix)] <- 0
  layout_matrix[layout_matrix != 0] <- (1 + sum(dim(layout_matrix))):(length(layout_matrix[layout_matrix != 0]) + sum(dim(layout_matrix)))
  layout_matrix <- cbind(rbind(1:ncol(layout_matrix), layout_matrix),
                         c(0,(1+ncol(layout_matrix)):(ncol(layout_matrix)+nrow(layout_matrix))))
  layout_matrix
}

# Transform log-return to discrete return as a string (latex readable) in scientific format
logret2perctRet <- function(.logret) {logret2price(.logret) %>% discrete_returns %>% (function(ret) paste(format(ret*100, scientific=T, digits=3), '\\%'))}
## End Aux Functions --------------------------------------------------------------------------------------------------

# Importing Data
logret_training <- read_rds('../data/logret_training.rds')

## get tickers
tickers <- names(logret_training)


# Dynamic Marginal Models
## -----------------------------------------------------------------------------
## Define all models to estimate
dynamic_models_to_estimate <- expand_grid(ticker = tickers,
                       ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       garch_model,
                       distribution_to_estimate) %>%
  filter(ar_lag!=0 | ma_lag!=0 | arch_lag!=0 | garch_lag!=0) %>%
  # All models considered is heteroscedastic
  filter(arch_lag > 0 | garch_lag > 0) %>%
  # Exclude ARMA models for BTC, ETH and GLD
  filter(!(ticker %in% c("BTC-USD","ETH-USD","GLD") & (ar_lag > 0 | ma_lag > 0)))

## Fit dynamic models
dynamic_models_fit <- tickers %>%
  lapply(function(asset) {
    ticker_models <- dynamic_models_to_estimate %>% filter(ticker==asset)
    ticker_models_fit <- pmap(.l = ticker_models, .f = estimate_garch, logret_training) %>%
      `names<-`(extract_models_name(ticker_models))
    ticker_models_fit}) %>%
  `names<-`(tickers)


## Filter out models that did not converge
dynamic_models_fit_c <- map(dynamic_models_fit,
                            function(asset_models_fit) Filter(function(x) x@fit$convergence==0, asset_models_fit))





# Generate Info criteria Table
## -----------------------------------------------------------------------------
## Extract Info criteria from models fit
dynamic_models_fit_c_infocriteria <-  dynamic_models_fit_c %>%
  lapply(function(asset_models_fit){
    sapply(asset_models_fit, function(fit) infocriteria(fit)[1:2,]) %>%
      t() %>%
      as_tibble(rownames="model") %>%
      arrange(Bayes, Akaike)
    })

## Show Models on table separated by asset
row_groups <- lengths(dynamic_models_fit_c)
## Show only top 5 models
row_groups[] <- 5

dynamic_models_fit_c_infocriteria %>%
  bind_rows(.id="asset") %>%
  group_by(asset) %>%
  slice_head(n=5) %>%
  ungroup() %>%
  dplyr::select(!(asset)) %>%
  kbl(caption="Information Criteria of models fit",
      format= "latex",
      vline = "",
      linesep = "",
      col.names = c("Model","AIC","BIC"),
      align="lcc",
      digits = 4) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  pack_rows(index=row_groups) %>%
  cat(file = infocriteria_tex_file)

# Select each Asset Best Model and Fit semi-parametric dist on Residuals
## -----------------------------------------------------------------------------
## Select Best Models based on BIC
selected_dynamic_models_names <- dynamic_models_fit_c_infocriteria %>%
  bind_rows(.id="asset") %>%
  group_by(asset) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  dplyr::select(model) %>% `[[`(1)


selected_dynamic_models <-  lapply(dynamic_models_fit_c,
                             function(asset_models) asset_models[names(asset_models) %in% selected_dynamic_models_names])

## Fit semi-parametric distribution with GPD tails on residuals.
selected_models_residuals <- lapply(selected_dynamic_models, (function (asset) as.numeric(residuals(asset[[1]], standardize=TRUE))))
residuals_dist <- lapply(selected_models_residuals, gpd.tail, plot=F)

## assert all gpd tails converged and Raise error if convergence fail
sapply(residuals_dist, function(asset_gpd) c(asset_gpd['upper.converged'], asset_gpd['lower.converged'])) %>%
  apply(2, all) %>%
  (function(tails_converged)
    if(any(!tails_converged))
      stop(paste("The Following Tails did not converged:",
                 paste0(names(tails_converged)[which(!tails_converged)], collapse = ', '))))


## Print table with Residuals' GPD fit parameters
lapply(residuals_dist, function(gpd_obj) gpd_obj[grep("par.ests|^(upper|lower).thresh",names(gpd_obj))]) %>%
  lapply(function(gpd_info) append(gpd_info, c(upper=gpd_info[['upper.par.ests']]['lambda'],
                                               upper=gpd_info[['upper.par.ests']]['xi'],
                                               lower=gpd_info[['lower.par.ests']]['lambda'],
                                               lower=gpd_info[['lower.par.ests']]['xi']))) %>%
  lapply(function(gpd_info) gpd_info[-grep("par.ests",names(gpd_info))]) %>%
  lapply(function(gpd_info){
    append(gpd_info, list(
      upper.support=(if(gpd_info[['upper.xi']] >= 0) "\\infty" else
        (((-gpd_info[['upper.lambda']]/gpd_info[['upper.xi']]) + gpd_info[['upper.thresh']]) %>% logret2perctRet)),
      lower.support=(if(gpd_info[['lower.xi']] >= 0) "-\\infty" else
        (((gpd_info[['lower.lambda']]/gpd_info[['lower.xi']]) - gpd_info[['lower.thresh']]) %>% logret2perctRet))))}) %>%
  bind_rows(.id = 'asset') %>%
  relocate(starts_with('lower'), .after=asset) %>%
  kbl(format='html',
      digits = 3,
      caption = "Residuals' GPD fit parameters.",
      vline = "",
      linesep = "",
      col.names = c("Asset","Threshold","\\lambda","\\xi","Lower Bound","Threshold","\\lambda","\\xi","Upper Bound"),
      align="lcccccccc",
      escape = F) %>%
  kable_classic_2(html_font = "helvetica") %>%
  add_header_above(c(" ", "Lower Tail" = 4, "Upper Tail" = 4), border_left = FALSE, border_right = FALSE) %>%
  footnote(paste("Some assets (GLD, QQQ, TLT, URTH, VTI) have a negative fitted \\xi value for the upper tail, which introduces an upper bound.",
                 "However, such limit is far beyond any expected observable value, therefore not violating any underling hypotheses, so the fit is kept as is.")) %>%
  cat(file = resdist_params_tex_file)


# Fit Copula Model
## -----------------------------------------------------------------------------
## Transform residuals sample into Uniform sample
U_hat.T <- map2(selected_models_residuals, residuals_dist, gpd.2p)
U_hat <- t(matrix(unlist(U_hat.T, use.names = F), nrow = length(U_hat.T), byrow = T)) %>%
  `dimnames<-`(list(NULL, names(selected_models_residuals)))

## Plot pairwise filled contour (heatmap) sampled distribution of assets' residuals
t.U_hat <- as_tibble(U_hat) %>% as.list()
DENS_list <- map(t.U_hat,
                 function(.col1, .data) map(.data, kde2d, .col1),
                 t.U_hat)
t.DENS <- DENS_list %>% as_tibble() %>% slice(-n()) %>% dplyr::select(-1)
t.DENS[lower.tri(t.DENS)] <- NA


n_tickers <- length(tickers)
layout_matrix <- make.layout_matrix(n_tickers)


pdf("pairwise_unif_sample_dist.pdf")
par(mar=c(0,0.2,0,0))
layout(layout_matrix,
       widths = c(rep.int(1, ncol(layout_matrix)-1), 0.3),
       heights = c(0.4, rep.int(1, nrow(layout_matrix)-1)))
walk(tickers[-1], function(ticker) text_plot(ticker, cex=1, font=2))
par(mar=c(0.2,0,0,0))
walk(tickers[-n_tickers], function(ticker) text_plot(ticker, cex=1, font=2, srt=270))
par(mar=c(0.2,0.2,0,0))
apply(which(!!layout_matrix[-1,-n_tickers], arr.ind = T), 1,
      function(.row) {
        i <- .row[1]
        j <- .row[2]
        (function(.DENS) custom.filled.contour(.DENS$x, .DENS$y, .DENS$z))(t.DENS[[i,j]][[1]])
      paste(names(t.DENS[j])[1], names(t.DENS[[j]])[i])
      })
dev.off()



## Correlations Plot - Appendix
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

pdf("spearman_rho.pdf")
U_hat %>%
  as_tibble() %>%
  cor(method="spearman") %>%
  corrplot(method = "color",  col=col(200),
           type = 'upper', diag = F, order='hclust',
           tl.col ='black', tl.srt = 45, tl.cex = 0.85,
           addCoef.col = "black")
dev.off()

pdf("kendall_tau.pdf")
U_hat %>%
  as_tibble() %>%
  cor(method="kendall") %>%
  corrplot(method = "color",  col=col(200),
           type = 'upper', diag = F, order='hclust',
           tl.col ='black', tl.srt = 45, tl.cex = 0.85,
           addCoef.col = "black")
dev.off()
## Tail dependence



## Fit Student-t Copula
fitted_copula <- fitCopula(tCopula(dim=length(tickers), dispstr = "un"), U_hat)



# Save Script Output
## -----------------------------------------------------------------------------
write_rds(selected_dynamic_models, 'data/selected_dynamic_models.rds')
write_rds(residuals_dist, 'data/residuals_dist.rds')
write_rds(fitted_copula, 'data/fitted_copula.rds')
