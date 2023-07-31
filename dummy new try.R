library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(deseasonalize)
library(fGarch)
library(FinTS)

data=read.csv(file.choose())
data

y=data$IIP
ylog=log(y)
par(mfrow=c(2,1))
plot.ts(y)
plot.ts(ylog)

x=read.csv(file.choose())
X=data.matrix(x)
X
t=c(1:300)
fit1=lm(y~X+t-1)
fit2=lm(ylog~X+t-1)

y1=resid(fit1)
ylog1=resid(fit2)
par(mfrow=c(2,1))
plot.ts(y1)
plot.ts(ylog1)

adf.test(y1)
adf.test(ylog1)

par(mfrow=c(2,2))
acf(y1)
pacf(y1)
acf(ylog1)
pacf(ylog1)

fit3=arma(y1,c(3,0))
fit4=arma(ylog1,c(3,0))

y11=resid(fit3)
ylog11=resid(fit4)
par(mfrow=c(2,1))
plot.ts(y11)
plot.ts(ylog11)
