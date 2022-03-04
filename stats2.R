df = read.csv("E:\Stats_Practice\CA2\eComm_US.csv", header = TRUE)
#install.packages('fpp2')
library(fpp2)




Values=read.csv('E:/Stats_Practice/CA2/eComm_US.csv')
Values

plot(Values)

ts1 <- ts(Values, start= c(1999,1), frequency =4)
ts1
str(ts1)
plot(ts1)
autoplot(ts1)
start(ts1)
end(ts1)
frequency(ts1)
ggseasonplot(ts1,year.labels = TRUE,year.labels.left = TRUE)

#####trying########
cycle(ts1)
plot(aggregate(ts1,FUN=mean))
boxplot(ts1~cycle(ts1))


install.packages('TSstudio')
library('TSstudio')
ts_info(ts1)







# Smoothing the Time series using simple moving average
plot(ma(ts1,3))
plot(ma(ts1,5)) # Always odd numbers are taken

plot(ma(ts1,7))

plot(ma(ts1,15))

autoplot(ts1)+autolayer(ma(ts1,3))+autolayer(ma(ts1,5))+autolayer(ma(ts1,7))

ggtsdisplay(ts1)



plot(ts1)
monthplot(ts1)
seasonplot(ts1)

#Seasonal Decomposition using Decompose() - Additive
beerprod=ts(ts1,freq=4)
decompbeer<-decompose(beerprod,type="additive")
plot(decompbeer)
fit.decadd <- decompose(ts1, type = "additive")
fit.decadd
plot(fit.decadd)
monthplot(ts1)

fit.stl<-stl(ts1$ECOMNSA,s.window="period")
plot(fit.stl)
fit.stl$time.series
exp(fit.stl$time.series)



#Seasonal Decomposition using Decompose() - Multiplicative [not appropriate for my Time Series]
fit.decmul <- decompose(ts1, type = "multiplicative")
fit.decmul
plot(fit.decmul)

install.packages('seasonal')
library(seasonal)
elecequip %>% seas(x11="") -> ts1
autoplot(ts1) +
  ggtitle("X11 decomposition of electrical equipment index")

d<-mean(ts1$ECOMNSA)
e<-sapply(ts1, mean, 2)

fcast<-meanf(ts1
,h=3)
summary(fcast)
plot(fcast)


fcast.seasonalnaive<-snaive(ts1,h=2)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)


str(ts1)
e <- as.numeric(ts1)
ts2<- ts(Values$ECOMNSA, start= c(1999,1), frequency =12)
ggseasonplot(ts2,year.labels = TRUE, year.labels.left = TRUE)+
  ylab("$ Sale") +
  ggtitle("Seasonal plot: Industry sale")

ggsubseriesplot(ts2)+
  ylab("$ Sale") +
  ggtitle("Seasonal plot: Industry sale")

#############################################################
library(fpp2)

Values=read.csv('E:/Stats_Practice/CA2/eComm_US.csv')
Values

ts2<- ts(Values$ECOMNSA, start= c(1999,4), frequency =4)


plot(ts2)
autoplot(ts2)
start(ts2)
end(ts2)
frequency(ts2)

ggseasonplot(ts2,year.labels = TRUE, year.labels.left = TRUE)+
  ylab("$ Sale") +
  ggtitle("Seasonal plo")

ggsubseriesplot(ts2)+
  ylab("$ Sale") +
  ggtitle("Seasonal plo")



plot(ts2)
monthplot(ts2) 
seasonplot(ts2)

#Seasonal decomposition using decompose() - additive
fit.decadd<-decompose(ts2, type = "additive")
fit.decadd
plot(fit.decadd)


fit<-stl(ts2,s.window="period")
plot(fit)
fit$time.series

#mean model
#forecast for two period ahead
fcast.mean<-meanf(ts2,h=2)
summary(fcast.mean)
plot(fcast.mean)

#naive model
fcast.naive<-naive(ts1,h=4)
summary(fcast.naive)
plot(fcast.naive)

#seasonal naive model
fcast.naive<-snaive(ts2,h=4)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)

fit.stl<-stl(ts2,s.window="period")
plot(fit.stl)
fit.stl$time.series
exp(fit.stl$time.series)


ts3<-window(ts2,start=1999)
ts3

plot(ts3)
ts4<-ets(ts3,model='ZZZ')
ts4

###### holt walt #########
#checking seasonality with additive or multiplication
#whichever gives least score take that

hwFit1 <- hw(ts2,trend="additive", seasonal = "additive")
hwFit1$model
hwFit1
accuracy(hwFit1)
hwFit2 <- hw(ts2,trend="additive", seasonal = "multiplicative") # Throws error as it is inappropriate
hwFit2$model
accuracy(hwFit2)

##returning 1-> not stationary
ndiffs(ts2)

plot(ts2)
d<- diff(ts2)
plot(d)

#install.packages('tseries')
library('tseries')
#Assess stationarity of the differenced series
#p>0.1509
adf.test(d)


#choosing p and q
#
acf(d)#q
pacf(d)#p

library(fpp2)
library(forecast)
#pdq
fit<-Arima(d,order=c(4,1,16))
fit


#Evaluating model fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")
checkresiduals(fit)
accuracy(fit)


forecast(fit,3)

plot(forecast(fit,3),xlab="Year",ylab="Annual Flow")


fit2<-as.numeric(unlist(fit))

fit2<-auto.arima(d)
fit2


fit3 <- Arima(d, order=c(1,0,0), seasonal=c(1,1,0))
qqnorm(fit3$residuals)
qqline(fit3$residuals)
Box.test(fit3$residuals,type="Ljung-Box")
checkresiduals(fit3)
accuracy(fit3)


plot(forecast(fit3,3))



#SARIMA

plot(decompose(ts2))
auto.arima(ts2)
g<-Arima(ts2,order=c(1,1,0),seasonal = c(1,1,0))
qqnorm(g$residuals)
qqline(g$residuals)
Box.test(g$residuals,type="Ljung-Box")
checkresiduals(g)
accuracy(g)
g%>%forecast(h=3)%>%autoplot()
forecast(g,h=3)

########### for class #############


timeseries <- ts(Values$ECOMNSA, start= c(1999,4), frequency =4)
timeseries
str(timeseries)
plot(timeseries)
autoplot(timeseries)
start(timeseries)
end(timeseries)
frequency(timeseries)
ggseasonplot(timeseries,year.labels = TRUE,year.labels.left = TRUE)


ggseasonplot(timeseries,year.labels = TRUE, year.labels.left = TRUE)+
  ylab("$billions") +
  ggtitle("Seasonal plot")

ggsubseriesplot(timeseries)+
  ylab("$billions") +
  ggtitle("Seasonal plot")

plot(ma(timeseries,3))
plot(ma(timeseries,5)) # Always odd numbers are taken

plot(ma(timeseries,7))

plot(ma(timeseries,15))

autoplot(ts1)+autolayer(ma(timeseries,3))+autolayer(ma(timeseries,5))+autolayer(ma(timeseries,7))

ggtsdisplay(timeseries)





fit.decadd<-decompose(timeseries, type = "additive")
fit.decadd
plot(fit.decadd)


fit.stl<-stl(timeseries,s.window="period")
plot(fit.stl)
fit.stl$time.series

#mean model
#forecast for two period ahead
fcast.mean<-meanf(timeseries,h=3)
summary(fcast.mean)
plot(fcast.mean)

#naive model
fcast.naive<-naive(timeseries,h=3)
summary(fcast.naive)
plot(fcast.naive)

#seasonal naive model
fcast.naive.seasonalnaive<-snaive(timeseries,h=3)
summary(fcast.naive.seasonalnaive)
plot(fcast.naive.seasonalnaive)

hwFit1 <- hw(timeseries,seasonal = "additive")
hwFit1$model
hwFit1
accuracy(hwFit1)
hwFit2 <- hw(timeseries,seasonal = "multiplicative") # Throws error as it is inappropriate
hwFit2$model
forecast(hwFit2,h=3)
accuracy(hwFit2)
plot(hwFit2,h=3)

hwFit3 <- hw(timeseries,seasonal = "multiplicative",damped=TRUE) # Throws error as it is inappropriate
hwFit3$model
forecast(hwFit3,h=3)
accuracy(hwFit3)
plot(hwFit3,h=3)




autoplot(timeseries)+autolayer(hwFit1,series="HW Additive",PI=FALSE)+
  autolayer(hwFit2,series="HW Multiplicative",PI=FALSE)+xlab("Year")+ylab("$ Sale")+
  ggtitle("hh")+guides(color=guide_legend(title="Forecast"))


ts4<-ets(timeseries,model='ZZZ')
ts4
accuracy(ts4)
forecast(ts4,h=3)




ndiffs(timeseries)
adf.test(timeseries)
plot(timeseries)
d<- diff(timeseries)
plot(d)

#install.packages('tseries')
library('tseries')
#Assess stationarity of the differenced series
#p>0.1509
adf.test(d)
ndiffs(d)
nsdiffs(d)
d<- diff(timeseries)
#choosing p and q
#
acf(d)#q
pacf(d)#p

library(fpp2)
library(forecast)
#pdq
fit<-Arima(d,order=c(4,1,16))
fit


#Evaluating model fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")
checkresiduals(fit)
accuracy(fit)


forecast(fit,3)

plot(forecast(fit,3),xlab="Year",ylab="Annual Flow")


fit2<-as.numeric(unlist(fit))

fit2<-auto.arima(d)
fit2


fit3 <- Arima(d, order=c(1,0,0), seasonal=c(1,1,0))
qqnorm(fit3$residuals)
qqline(fit3$residuals)
Box.test(fit3$residuals,type="Ljung-Box")
checkresiduals(fit3)
accuracy(fit3)


plot(forecast(fit3,3))



#SARIMA

auto.arima(d)

acf(d)#q
pacf(d)#p
sarima<-Arima(d,order=c(1,0,0),seasonal = c(1,1,0))
summary(sarima)
qqnorm(sarima$residuals)
qqline(sarima$residuals)
Box.test(sarima$residuals,type="Ljung-Box")
checkresiduals(sarima)
accuracy(sarima)
sarima%>%forecast(h=3)%>%autoplot()
forecast(sarima,h=3)






























