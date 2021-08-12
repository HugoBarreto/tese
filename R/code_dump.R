###### 04-Estimate_Models #######
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



## Format and print a table in html - Reporting code

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')

# reformat models for texreg
l_models_tr <- map(l_models, extract.rugarch, include.rsquared = FALSE)

# write custom row
custom_row <- list('Variance Model' = df_grid$models_to_estimate,
                   'Distribution' = df_grid$distribution_to_estimate)
custom_names <- paste0('Model ', 1:length(l_models))


# save to html
htmlreg(l_models,
        file = my_html_file,
        custom.gof.rows = custom_row,
        custom.model.names = custom_names,
        digits = 3)

# print to screen
screenreg(l_models,
          custom.gof.rows = custom_row,
          custom.model.names = custom_names,
          digits = 3)


