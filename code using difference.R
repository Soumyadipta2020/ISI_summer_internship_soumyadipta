library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(deseasonalize)
library(fGarch)

data = read.csv(file.choose())  #----reading data CSV file
data

par(mfrow=c(2,1))
plot.ts(data$IIP)   #----plotting IIP
plot.ts(log(data$IIP))   #----plotting after log transformation

y = data$IIP
ynew = log(data$IIP)   #----data after log transformation
t=c(1:300)

ndiffs(y)
ndiffs(ynew)

y1=diff(y,differences=1)
ynew1=diff(ynew,differences=1)
par(mfrow=c(2,1))
plot.ts(y1)
plot.ts(ynew1)

l=getds(y1,s=12,Fm=0,ic=c("AIC"),standardizeQ=FALSE)
m=getds(ynew1,s=12,Fm=0,ic=c("AIC"),standardizeQ=FALSE)
x1=l$z
xnew1=m$z
par(mfrow=c(2,1))
plot.ts(x1,col="red")
plot.ts(xnew1,col="red")

#------Test for stationarity
adf.test(x1)
adf.test(xnew1)

par(mfrow=c(2,2))
acf(x1)
acf(xnew1)
pacf(x1)
pacf(xnew1)

fit3=arima(x1,order=c(3,0,5))   #----fittinng MA(1)
fit4 = arima(xnew1,order=c(3,0,3))    #-----fitting ARMA(2,3)
fit3
fit4

#----testing for diaognistic checking and for volatility
arch.test(fit3) 
arch.test(fit4)
Box.test(resid(fit3),type="Ljung-Box")
Box.test(resid(fit4),type="Ljung-Box")

par(mfrow=c(2,1))
plot.ts(resid(fit3),col="red")
plot.ts(resid(fit4),col="blue")

x11=resid(fit3)
xnew11=resid(fit4)
par(mfrow=c(2,2))
acf(x11^2)
acf(xnew11^2)
pacf(x11^2)
pacf(xnew11^2)

fit11=garchFit(~garch(2,4),data=x11)
fit12=garchFit(~garch(1,1),data=xnew11)
x111=residuals(fit11)
xnew111=residuals(fit12)
par(mfrow=c(2,1))
plot.ts(x111,ylab="residual of x11",col="red")
plot.ts(xnew111,ylab="residual of xnew11",col="red")