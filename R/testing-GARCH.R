# Import libraries
library(cowplot)
library(tidyverse)
library(forecast)
library(rugarch)

set.seed(20200315)
# Common parameters
N <- 1000

graphics.off()
# White Noise
## simulate white noise
X <- ts(rnorm(N))

## plot
plot(X)

## acf X_t
acf(X)
Acf(X)

## acf X_t^2
Acf(X^2)

# AR(2)
## Simulate
X.ar <- arima.sim(n=N, list(ar=c(0.7, 0.2)))

## Investigate
plot(X.ar)
Acf(X.ar)
Acf(X.ar, type = "partial")

Acf(X.ar^2)

## Model
X.ar.fitted <- ar(X.ar)
X.ar.fitted

plot(X.ar.fitted$aic)
Acf(X.ar.fitted$resid)
Acf(X.ar.fitted$resid^2)


# MA(3)
## Simulate
X.ma <- arima.sim(n=N, list(ma=c(1, -0.5, 0.2)))

## Investigate
plot(X.ma)
Acf(X.ma)
Acf(X.ma, type = "partial")

Acf(X.ma^2)

## Model
X.ma.fitted <- arima(X.ma, order=c(0,0,3))
X.ma.fitted

Acf(X.ma.fitted$residuals)
Acf(X.ma.fitted$residuals^2)

# ARMA(2,2)
## Simulate
X.arma <- arima.sim(n=N, list(ar=c(0.6, .2), ma=c(1, -0.5)))

## Investigate
plot(X.arma)
Acf(X.arma)
Acf(X.arma, type = "partial")

Acf(X.arma^2)

## Model
X.arma.fitted <- arima(X.arma, order=c(2,0,2))
X.arma.fitted

Acf(X.arma.fitted$residuals)
Acf(X.arma.fitted$residuals^2)

# ARCH(1)
spec.arch.fixed <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,1)),
                              mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                              distribution.model="std",
                              fixed.pars=list(mu=0.001,omega=0.00001, alpha1=0.00, beta1=0.90, shape=4))

X.arch <- ugarchpath(spec.arch.fixed)
X.arch
plot(X.arch)

spec.arch <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(0,1)),
                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        distribution.model="std")

X.arch.fitted <- ugarchfit(spec.arch, X.arch@path$seriesSim) # error

# GARCH(1,1)
spec.sgarch.fixed <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                          distribution.model="std",
                          fixed.pars=list(mu=0.001,omega=0.00001, alpha1=0.05, beta1=0.90,
                                          shape=4))

X.sgarch <- ugarchpath(spec.sgarch.fixed)
X.sgarch
plot(X.sgarch)

spec.sgarch <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                distribution.model="std")

X.sgarch.fitted <- ugarchfit(spec.sgarch, X.sgarch@path$seriesSim)
X.sgarch.fitted
plot(X.sgarch.fitted)

# gjrGARCH(1,1)
spec.gjrgarch.fixed <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                                distribution.model="std",
                                fixed.pars=list(mu=0.001,omega=0.00001, alpha1=0.1, beta1=0.80,
                                                gamma1=0.1, shape=4))

X.gjrgarch <- ugarchpath(spec.gjrgarch.fixed)
X.gjrgarch
plot(X.gjrgarch)

spec.gjrgarch <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                          distribution.model="std")

X.gjrgarch.fitted <- ugarchfit(spec.gjrgarch, X.gjrgarch@path$seriesSim)
X.gjrgarch.fitted
plot(X.gjrgarch.fitted)


# ARMA(1,1)-GARCH(1,1)
spec.armagjrgarch.fixed <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(1,1), include.mean=TRUE),
                                distribution.model="std",
                                fixed.pars=list(mu=0.001,omega=0.00001, ar1=0.4, ma1=0.7, gamma1=0.3,
                                                alpha1=0.12, beta1=0.70, shape=4))

X.armagjrgarch <- ugarchpath(spec.armagjrgarch.fixed)
X.armagjrgarch
plot(X.armagjrgarch)

spec.armagjrgarch <- ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(1,1), include.mean=TRUE),
                          distribution.model="std")

X.armagjrgarch.fitted <- ugarchfit(spec.armagjrgarch, X.armagjrgarch@path$seriesSim)
X.armagjrgarch.fitted
plot(X.armagjrgarch.fitted)

ggAcf(residuals(X.armagjrgarch.fitted, standardize=FALSE)^2)

# Using data from package
data(sp500ret)
sp500ret.fit <- ugarchfit(spec.armagjrgarch, sp500ret)

show(sp500ret.fit)
plot(sp500ret.fit)

# Using data from paper
ibov <- read_rds('data/RAC-GARCH-Data.rds')
ibov.fit <- ugarchfit(spec.armagjrgarch, ibov$log_ret)

plot(ibov.fit)

model.residuals <- residuals(ibov.fit)
ggAcf(model.residuals^2)
ggAcf(model.residuals)

# My data
df_logret <- read_rds('data/logret.rds')

### Understranding rugarch library
library(rugarch)

## Not run:
data(sp500ret)
ctrl = list(RHO = 1,DELTA = 1e-8,MAJIT = 100,MINIT = 650,TOL = 1e-6)
spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  distribution.model = "std")
egarch.fit = ugarchfit(data = sp500ret[,1,drop=FALSE], spec = spec,
                       solver = "solnp", solver.control = ctrl)

spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
                  distribution.model = "std", fixed.pars = as.list(coef(egarch.fit)))
egarch.filter = ugarchfilter(data = sp500ret[,1,drop=FALSE], spec = spec)


data(dmbp)
spec = ugarchspec()
fit = ugarchfit(data = dmbp[,1], spec = spec)
forc = ugarchforecast(fit, n.ahead=20)
forc

spec = ugarchspec(fixed.pars = as.list(coef(fit)))
forc = ugarchforecast(fitORspec = spec, data = dmbp[,1], n.ahead=5, out.sample = 5, n.roll = 2)
forc
fitted(forc)



spec = ugarchspec()
fit = ugarchfit(data = dmbp[,1], spec = spec)
sim = ugarchsim(fit,n.sim=1000, n.start=1, m.sim=1, startMethod="sample")
sim
head(sigma(sim))
