library(Matrix)
library(aTSA)
library(forecast)
library(FinTS)
library(fGarch)

data=read.csv(file.choose())
data

y=data$IIP
ynew=log(data$IIP)
x=read.csv(file.choose())
X=data.matrix(x)
X

fit1=lm(y~X-1)
fit2=lm(ynew~X-1)

y1=resid(fit1)
ynew1=resid(fit2)

par(mfrow=c(2,1))
plot.ts(y1,ylab="IIP deseasonalized")
plot.ts(ynew1,ylab="log(IIP) deseasonalized")

ndiffs(y1)
y11=diff(y1,differences=1)
ndiffs(ynew1)
ynew11=diff(ynew1,differences=1)

par(mfrow=c(2,2))
acf(y11)
pacf(y11)
acf(ynew11)
pacf(ynew11)

fit3=arima(y1,c(2,1,1))
fit4=arima(ynew1,c(2,1,1))

par(mfrow=c(2,1))
plot.ts(resid(fit3),ylab="residuals of y1 after ARIMA")
plot.ts(resid(fit4),ylab="residuals of ynew1 after ARIMA")

x1=resid(fit3)
x2=resid(fit4)

Box.test(x1,type="Ljung-Box")
Box.test(x2,type="Ljung-Box")


par(mfrow=c(2,1))
pacf(x1^2)
pacf(x2^2)

acf(x1^2)

ArchTest(x1,lags=1)
fit5=garchFit(~garch(1,1),data=x1)

predict(fit5,4)
predict(fit3,4)
 