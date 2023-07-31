library("tseries")
library("strucchange")

data=read.csv(file.choose())
data

y=ts(data$IIP,frequency=12,start=c(1994,4))
ynew=ts(log(data$IIP),frequency=12,start=c(1994,4))

bp_y = breakpoints(y~1)    #-----storing the break points
bp_ynew=breakpoints(ynew~1)

summary(bp_y)
summary(bp_ynew)

ci_y=confint(bp_y)
ci_ynew=confint(bp_ynew)

par(mfrow=c(2,1))
plot(y)
lines(bp_y)
lines(ci_y)
plot(ynew)
lines(bp_ynew)
lines(ci_ynew)
