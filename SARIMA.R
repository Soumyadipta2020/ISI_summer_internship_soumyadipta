library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(deseasonalize)
library(fGarch)
library(FinTS)

data = read.csv(file.choose())
data

y = ts(data$IIP,frequency=12,start=c(1994,4))
ynew = ts(log(data$IIP),frequency=12,start=c(1994,4))
par(mfrow=c(2,1))
plot.ts(y,col="red")
plot.ts(ynew,col="red")

ndiffs(y)
nsdiffs(y)

y1=diff(y,lag=12,differences=1)
ynew1=diff(ynew,lag=12,differences=1)

par(mfrow=c(2,2))
acf(x,lag.max=48)
pacf(x,lag.max=48)
acf(xnew,lag.max=48)
pacf(xnew,lag.max=48)

auto.arima(y,ic="aic",trace=TRUE)
auto.arima(ynew,ic="aic",trace=TRUE)
fit1=arima(y,order=c(2,0,2),seasonal=c(2,1,1))
fit2=arima(ynew,order=c(3,1,0),seasonal=c(2,1,1))

par(mfrow=c(2,1))
plot.ts(resid(fit1))
plot.ts(resid(fit2))

ArchTest(resid(fit1),lags=1)
ArchTest(resid(fit1),lags=1)
