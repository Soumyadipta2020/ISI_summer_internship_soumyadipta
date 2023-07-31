library(forecast)
library(tseries)
library(aTSA)
library(x12)
library(fGarch)

data = read.csv(file.choose())  #----reading data CSV file
data

par(mfrow=c(2,1))
plot.ts(data$IIP,col="red")   #----plotting IIP
plot.ts(log(data$IIP),col="red")   #----plotting after log transformation

y = data$IIP
ynew = log(data$IIP)   #----data after log transformation
t=c(1:300)

fit1 = lm(y~t)
fit2 = lm(ynew~t)

new=data.frame(t=c(301,302,303,304))
predict(lm(ynew~t),newdata=new,se.fit=TRUE)

par(mfrow=c(2,1))
plot.ts(data$IIP,col="red")   #----plotting IIP
abline(fit1)
plot.ts(log(data$IIP),col="red")   #----plotting after log transformation
abline(fit2)

y1 = resid(fit1)
ynew1 = resid(fit2)

par(mfrow=c(2,1))
plot.ts(y1,col="red")
plot.ts(ynew1,col="red")

season = c(0)
seasonnew = c(0)
for(i in 1:12)  #----loop for calculating seasonal index
{
	s = 0
	s1 = 0
	for(j in 1:25)
	{
		s=s+y1[i+(j-1)*12]
		s1=s1+ynew1[i+(j-1)*12]
	}
	season[i] = s/25
	seasonnew[i] = s1/25
}

x1 = rep(0,times=300)
xnew1 = rep(0,times=300)
for(i in 1:12)   #----loop for deseasonalization
{
	for(j in 1:25)
	{
		x1[i+(j-1)*12] = y1[i+(j-1)*12] - season[i]
		xnew1[i+(j-1)*12] = ynew1[i+(j-1)*12] - seasonnew[i]
	}
}

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

fit3=arima(x1,order=c(3,0,0))   #----fittinng MA(1)
fit4 = arima(xnew1,order=c(3,0,0))    #-----fitting ARMA(2,3)
fit3
fit4

#----testing for diaognistic checking and for volatility
arch.test(fit3) 
arch.test(fit4)
Box.test(resid(fit3),type="Ljung-Box")
Box.test(resid(fit4),type="Ljung-Box")
ArchTest(resid(fit3),lags=1,demean=TRUE)
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

#----- no need of ARCH fitting since volatility is absent
fit11=garchFit(~garch(1,1),data=x11)
fit12=garchFit(~garch(1,1),data=xnew11)
x111=residuals(fit11)
xnew111=residuals(fit12)
par(mfrow=c(2,1))
plot.ts(x111,ylab="residual of x11",col="red")
plot.ts(xnew111,ylab="residual of xnew11",col="red")
