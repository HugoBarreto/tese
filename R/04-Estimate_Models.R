# <Paper Name>
#
# This script will estimate the marginal distribution models for each asset returns.
# First we estimate an ARMA-gjrGARCH model, then we estimate the tail of the
# residuals' distribution as a GPD.

## OPTIONS
infocriteria_tex_file <- "tabs/infocriteria.tex"
resdist_params_tex_file <- "tabs/residuals_dist_params.tex"
boxTest_tex_file <- "tabs/ResidBoxTest.tex"
residualsStats_tex_file <- "tabs/residualsStats_NormTest.tex"
residsQQ1_pdf <- "figs/ResidsQQplot.pdf"
residsQQ2_pdf <- "figs/ResidsQQplot2.pdf"
spearmanRho_corrplot_pdf <- "figs/spearman_rho.pdf"
kendallTau_corrplot_pdf <- "figs/kendall_tau.pdf"
tailsCoeff_pdf <- "figs/tailscoeff.pdf"
tailsCoeffDiff_pdf <- "figs/tailscoeffdiff.pdf"

## Modeling OPTIONS
ar_lag <- c(0,1) # lag used for ar term in mean equation (0 in paper)
ma_lag <- c(0,1) # lag used for ma term in mean equation (0 in paper)
arch_lag <- c(0,1) # lag in arch effect (1 in paper)
garch_lag <- c(0,1) # lag in garch effect (1 in paper)
garch_model <- c('sGARCH','gjrGARCH') # see rugarch manual for more
distribution_to_estimate <- 'norm' # distribution used in all models
## END OPTIONS


# Libraries Import
library(forecast)
library(Rsafd)
library(xts)
library(FRAPO)
library(tidyverse)
library(rugarch)
library(copula)
library(gridExtra)
library(GGally)
library(corrplot)
library(moments)
library(kableExtra)

# Setting WD to repo root
dirname(rstudioapi::getActiveDocumentContext()$path) %>% setwd()
setwd('..')

source('R/garch_fcts.R')

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

# Get specific values from Box test
get_box_test_results <- function(boxt) {
  list(
    statistic = boxt$statistic,
    p.value=boxt$p.value
  )
}

# Transform log-return to discrete return as a string (latex readable) in scientific format
logret2perctRet <- function(.logret) {
  logret2ret(.logret) %>%
  (function(ret) paste(format(ret*100, scientific=T, digits=3), '\\%'))
}

## End Aux Functions --------------------------------------------------------------------------------------------------

# Importing Data
logret_training <- read_rds('data/logret_training.rds')

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

selected_models_residuals <- lapply(selected_dynamic_models, (function (asset) as.numeric(residuals(asset[[1]], standardize=TRUE))))

## Do SWN tests for models residuals
# define lag to be log(N) where N is lenght of data
selected_models_residuals_tib <- as_tibble(selected_models_residuals) %>% slice(-1:-10)
max_lag <- log(nrow(selected_models_residuals_tib)) %>% round()

# do tests
box_tests <- lapply(selected_models_residuals_tib,function(x) Box.test(x, lag = max_lag, type = "Ljung-Box", fitdf = 0))
box_tests_abs <- lapply(selected_models_residuals_tib,function(x) Box.test(abs(x), lag = max_lag, type = "Ljung-Box", fitdf = 0))

# Save results in tables
box_test_table <- lapply(box_tests, get_box_test_results) %>% bind_rows(.id = "id") %>% column_to_rownames(var="id")
box_test_abs_table <- lapply(box_tests_abs, get_box_test_results) %>% bind_rows(.id = "id") %>% column_to_rownames(var="id")

bind_cols(box_test_table, box_test_abs_table) %>%
  round(digits = 4) %>%
  mutate(across(starts_with('p'), format, nsmall=4, digits=5)) %>%
  kbl(format='latex',
      digits = c(2,4,2,4),
      caption = "Results of Ljung-Box for assets' log-returns and absolute log-returns.",
      vline = "",
      linesep = "",
      col.names = c("Statistic","p-Value", "Statistic","p-Value"),
      align="lcccc",
      escape = F,
      longtable = TRUE) %>%
  kable_classic_2(html_font = "helvetica") %>%
  add_header_above(c(" ", "residuals" = 2, "abs residuals" = 2), border_left = FALSE, border_right = FALSE) %>%
  footnote(paste0("The degrees of freedom of all statistics is ", max_lag,
                  ", which is the approx. value of log(N)."),
           footnote_as_chunk=F,
           threeparttable = TRUE) %>%
  cat(., file = boxTest_tex_file)

# Table with sample statistics
norm_test <- function(data){
  test <- shapiro.test(data)
  tibble(stat=test$statistic, p.value=test$p.value)
}

sample_stats <- list(
  mean=mean,
  sd=sd,
  skew=moments::skewness,
  kurtosis=moments::kurtosis,
  test=norm_test
)

selected_models_residuals_tib %>%
  summarise(across(.fns=sample_stats)) %>%
  unpack(cols = ends_with("test"),names_sep = ".") %>%
  pivot_longer(everything(), names_to = c("asset",".value"), names_sep = "_") %>%
  kbl(caption="Summary statistics and the Shapiro-Wilk test results of the dynamic models residuals.",
      format= 'latex',
      vline = "",
      linesep = "",
      col.names = c("", "mean", "sd","skew","kurt","Statistic","p-value"),
      align="lcccr",
      escape=F,
      digits = 4) %>%
  kable_classic_2(full_width = F, html_font = "helvetica") %>%
  footnote("The first 10 values were discarded to avoid any meaningful interference of the starting values on the analysis",
           footnote_as_chunk=F,
           threeparttable = TRUE) %>%
  cat(file = residualsStats_tex_file)

# Q-Q plot with residuals
custom_ggqq <- function(data, add.estdist=FALSE, ...){
  col <- colnames(data)[1]
  if (add.estdist){
    qgpd <- function(p) gpd.2q(p, residuals_dist[[col]])

    quantiles <- stats::ppoints(nrow(data))
    theoretical <- qgpd(p = quantiles)

    y_coords <- quantile(pull(data), probs=c(0.25, 0.75))
    x_coords  <- qgpd(c(0.25, 0.75))
    slope <- diff(y_coords)/diff(x_coords)
    intercept <- y_coords[1L] - slope * x_coords[1L]

    x <- range(theoretical)
    add.estdist.data <- data_frame(x = x, y = slope * x + intercept)
  }

  ggplot(data, aes(sample=!!sym(col))) +
    ggtitle(col) +
    stat_qq() +
    {if (!add.estdist) stat_qq_line()} +
    {if (add.estdist) stat_qq(distribution = qgpd, colour="blue")} +
    {if (add.estdist) geom_path(data=add.estdist.data, mapping=aes(x=x,y=y), inherit.aes = F, colour="blue")} +
    xlab("") + ylab("") +
    {if (!add.estdist) coord_cartesian(ylim=c(-6,6))} +
    {if (add.estdist) coord_cartesian(ylim=c(-6,6), xlim=x)} +
    theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

qq_plot <- lapply(colnames(selected_models_residuals_tib), function(col) custom_ggqq(dplyr::select(selected_models_residuals_tib, {{col}})))

pdf(residsQQ1_pdf)
grid.arrange(grobs=qq_plot, ncol=3)
dev.off()

## Fit semi-parametric distribution with GPD tails on residuals.
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


# Q-Q plot 2
qq_plot2 <- lapply(colnames(selected_models_residuals_tib), function(col) custom_ggqq(dplyr::select(selected_models_residuals_tib, col), add.estdist = T))

pdf(residsQQ2_pdf)
grid.arrange(grobs=qq_plot2, ncol=3)
dev.off()

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

pdf(spearmanRho_corrplot_pdf)
U_hat %>%
  as_tibble() %>%
  cor(method="spearman") %>%
  corrplot(method = "color",  col=col(200),
           type = 'upper', diag = F, order='hclust',
           tl.col ='black', tl.srt = 45, tl.cex = 0.85,
           addCoef.col = "black")
dev.off()

pdf(kendallTau_corrplot_pdf)
U_hat %>%
  as_tibble() %>%
  cor(method="kendall") %>%
  corrplot(method = "color",  col=col(200),
           type = 'upper', diag = F, order='hclust',
           tl.col ='black', tl.srt = 45, tl.cex = 0.85,
           addCoef.col = "black")
dev.off()
## Tail dependence
tdc.u <- tdc(U_hat, method = "EmpTC", lower = FALSE)
tdc.l <- tdc(U_hat, method = "EmpTC", lower = TRUE)

empirical_tail_dependece <- tdc.u
empirical_tail_dependece[lower.tri(empirical_tail_dependece)] <- tdc.l[lower.tri(tdc.l)]

pdf(tailsCoeff_pdf)
empirical_tail_dependece %>%
  corrplot(method = "color",  col=col(200),
           type = 'full', diag = F,
           tl.col ='black', tl.srt = 45, tl.cex = 0.7,
           cl.pos = 'n', number.cex = 0.85,
           addCoef.col = "black",
           mar = c(3.5, 0, 3, 0)) # Values that print a pretty plot when opened in pdf
mtext(expression(hat(lambda)[u]), side=4, line=0, las=1)
mtext(expression(hat(lambda)[l]), side=1, line=2)
lines(x = c(1,9), y = c(9,1), lwd=5)
title("Estimated tail-dependence coefficients")
dev.off()

td.diff <- tdc.u - tdc.l
#td.diff[lower.tri(td.diff)] <- (tdc.l - tdc.u)[lower.tri(td.diff)]
colnames(td.diff) <- rownames(td.diff) <- colnames(empirical_tail_dependece)

pdf(tailsCoeffDiff_pdf)
td.diff  %>%
  corrplot(method = "color",  col=col(200),
           type = 'upper', diag = F,
           tl.col ='black', tl.srt = 45, tl.cex = 0.7,
           cl.pos = 'n', number.cex = 0.85,
           addCoef.col = "black",
           mar = c(0, 0, 3, 0))
title(expression(""~bold("Tail-dependence coefficients difference")~ (hat(bold(lambda))[u]-hat(lambda)[l])))
dev.off()


## Fit Student-t Copula
fitted_copula <- fitCopula(tCopula(dim=length(tickers), dispstr = "un"), U_hat)



# Save Script Output
## -----------------------------------------------------------------------------
write_rds(selected_dynamic_models, 'data/selected_dynamic_models.rds')
write_rds(residuals_dist, 'data/residuals_dist.rds')
write_rds(fitted_copula, 'data/fitted_copula.rds')
