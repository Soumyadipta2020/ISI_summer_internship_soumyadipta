library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(deseasonalize)
library(fGarch)
library(FinTS)

data = read.csv(file.choose())  #----reading data CSV file
data

par(mfrow=c(2,1))
plot.ts(data$IIP)   #----plotting IIP
plot.ts(log(data$IIP))   #----plotting after log transformation

y = data$IIP
ynew = log(data$IIP)   #----data after log transformation
t=c(1:300)

season = c(0)
seasonnew = c(0)
for(i in 1:12)  #----loop for calculating seasonal index
{
	s = 0
	s1 = 0
	for(j in 1:25)
	{
		s=s+y[i+(j-1)*12]
		s1=s1+ynew[i+(j-1)*12]
	}
	season[i] = s/25
	seasonnew[i] = s1/25
}

y1 = rep(0,times=300)
ynew1 = rep(0,times=300)
for(i in 1:12)   #----loop for deseasonalization
{
	for(j in 1:25)
	{
		y1[i+(j-1)*12] = y[i+(j-1)*12] - season[i]
		ynew1[i+(j-1)*12] = ynew[i+(j-1)*12] - seasonnew[i]
	}
}

par(mfrow=c(2,1))
plot.ts(y1)
plot.ts(ynew1)

ndiffs(y1)
ndiffs(ynew1)

x1=diff(y1,differences=1)
xnew1=diff(ynew1,differences=1)
par(mfrow=c(2,1))
plot.ts(x1)
plot.ts(xnew1)

#------Test for stationarity
adf.test(x1)
adf.test(xnew1)

par(mfrow=c(2,2))
acf(x1)
acf(xnew1)
pacf(x1)
pacf(xnew1)

fit3 = arima(y1,order=c(2,1,2))   #----fittinng ARIMA(2,1,2)
fit4 = arima(ynew1,order=c(2,1,2))    #-----fitting ARIMA(2,1,2)
fit3
fit4

#----testing for diaognistic checking and for volatility
arch.test(fit3) 
arch.test(fit4)
Box.test(resid(fit3),type="Ljung-Box")
Box.test(resid(fit4),type="Ljung-Box")
ArchTest(resid(fit3),lags=1,demean=TRUE)   #-----lagrange multiplier test
ArchTest(resid(fit4),lags=1,demean=TRUE)

par(mfrow=c(2,1))
plot.ts(resid(fit3),col="red")
plot.ts(resid(fit4),col="red")

x11=resid(fit3)
xnew11=resid(fit4)
par(mfrow=c(2,2))
acf(x11^2)
acf(xnew11^2)
pacf(x11^2)
pacf(xnew11^2)

fit11=garchFit(~garch(1,1),data=x11)
fit12=garchFit(~garch(1,2),data=xnew11)  #------ not correct
x111=residuals(fit11)
xnew111=residuals(fit12)    #----- not correct
par(mfrow=c(2,1))   #---- no need
plot.ts(x111,ylab="residual of x11",col="red")
plot.ts(xnew111,ylab="residual of xnew11",col="red")  #---- no need