# currency-times
Stat comp time series work with currency exchanges.

rm(list=ls()) #clears variables
dev.off() #clears all plots

#Vectorizing Data
exchangerates<-read.csv("/Users/hallehghaemi/Desktop/rates_data.csv")
attach(exchangerates)
QARR<-(exchangerates$USD.QAR)
QAR<-rev(QARR)

#Time Series Plot
ts.plot(QAR,gpars= list(col=rainbow(2)), main="QAR Weekly Exchange Rate")
legend("topleft", legend = c("QAR"), col = 1, lty = 1)

#ACF
QAR.numeric<- as.numeric(QAR); n.tp <- 1:length(QAR.numeric)
Acf(diff(QAR.numeric),main="QAR Weekly Exchange Rate ACF Plot")

#Detrend Data
par(mfrow=c(1,1))
library(quantmod)
QAR.numeric<- as.numeric(QAR); n.tp <- 1:length(QAR.numeric)
plot(QAR.numeric,type="l",labels=F,main="QAR Weekly Exchange Rate",
     xlab="Date")
tickmarks <- seq(1,1201,by=200)
axis(1,at=tickmarks,labels=(Period.Start.Date)[tickmarks]); axis(2)
lm.linear <- lm(QAR.numeric~n.tp)
lm.cubic <- lm(QAR.numeric~n.tp*I(n.tp^2)*I(n.tp^3)); summary(lm.cubic)
lines(fitted(lm.linear),col=2); lines(fitted(lm.cubic),col=3)

library(fpp)
plot(QAR.numeric,type="l",labels=F,main="QAR Weekly Exchange Rate",
     xlab="Date")
tickmarks <- seq(1,1201,by=200)
axis(1,at=tickmarks,labels=(Period.Start.Date)[tickmarks]); axis(2)
lines(ma(QAR.numeric,5),col="red",lwd=1.5)
lines(ma(QAR.numeric,121),col="blue",lwd=3)
#legend("topleft",legend=c("5-MA","121-MA"),col=c(2,4),lty=1,lwd=c(1.5,3))
par(mfrow=c(1,1))

#Dickey Fuller Stationarity Test
adf.test(diff(log(QAR.numeric),12))
#Stationary series because we reject H null, pointing to a 
#stationary series.

#Residual Plot #not original 
par(mfrow=c(1,1))
res0 <- ts(resid(lm.linear)) #figure out x 
plot.ts(res0,ylab="res (QAR)", main="Fitted Residuals");
abline(0,0)
#almost stationary

#ARIMA
QAR.dif<-diff(QAR)
auto.arima(QAR.)
QAR.arima<-auto.arima(QAR, order=c(3,1,1), seasonal = FALSE, stationary = TRUE)
QAR.arima
summary(QAR.arima)
plot(forecast(QAR.arima))

arima.param<- list(c(3,1,0), c(4,1,0), c(2,1,0), c(3,1,1))
arima.aic<- sapply(arima.param,function(x) arima(QAR,order=x)$aic)
print(arima.param[[which.min((arima.aic))]])
fit<-Arima(QAR, order=c(3,1,1))
summary(fit)
plot(forecast(fit), main="QAR Forecast")
#y't=-0.7503y't-1+0.0630e't-1-0.6263y't-3
