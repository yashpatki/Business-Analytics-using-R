setwd("F:/New Content/Time Series")
#Kings of England
kings=read.csv(file.choose(),header = TRUE)

kings_ts=ts(kings, frequency = 12, start = c(1946,1)) #data, start point, end point, give frequencung if end point not known
kings_ts
plot.ts(kings_ts)

births=read.csv(file.choose(),header = TRUE)
births_ts=ts(births, frequency=12, start=c(1946, 1))
births_ts
plot.ts(births_ts)
birthsDecomp=decompose(births_ts)
birthsDecomp$seasonal
plot(birthsDecomp)
births_ts1=ts(births$No..of.births, frequency=12, start=c(1946, 1))
birthstl<-stl(births_ts1,s.window = "periodic")
plot(birthstl)

souvenir=read.csv(file.choose(), header = T)
souvenir_ts=ts(souvenir, frequency=12, start=c(1987,1))
souvenir_ts
logsouvenir_ts=log(souvenir_ts)
plot.ts(logsouvenir)

kings_hw<-HoltWinters(kings_ts)
births_hw<-HoltWinters(births_ts)
souvenir_hw<-HoltWinters(souvenir_ts)
logsouvenir_hw<-HoltWinters(logsouvenir_ts)
logsouvenir_stl<-stl(logsouvenir[,1], s.window = "periodic")
plot(logsouvenir_stl)

install.packages("forecast")
library(forecast)

#Comparison between the models
forecast(logsouvenir_stl,h=4)
forecast(logsouvenir_hw,h=4)
accuracy(forecast(logsouvenir_hw,h=4))
accuracy(forecast(logsouvenir_stl,h=4))

library(forecast)
souvenir_forecasts2 = forecast.HoltWinters(souvenir_forecasts, h=48)
plot.forecast(souvenir_forecasts2)

acf(souvenir_forecasts2$residuals, lag.max=20)
Box.test(souvenir_forecasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(souvenir_forecasts2$residuals) # make a time plot



par(mfrow=c(1,2))
plot.ts(souvenir_ts)
logsouvenir=log(souvenir_ts)
plot.ts(logsouvenir)

souvenir_forecasts = HoltWinters(logsouvenir)
souvenir_forecasts
souvenir_forecasts$SSE
plot(souvenir_forecasts)

#error handling
library(forecast)
souvenir_forecasts2 = forecast(souvenir_hw, h=48)
plot(souvenir_forecasts2)

acf(souvenir_forecasts2$residuals[13:length(souvenir_forecasts2$residuals)], lag=20) #auto correlation

Box.test(souvenir_forecasts2$residuals[13:length(souvenir_forecasts2$residuals)], lag=20, type="Ljung-Box") 
#Box test tells us if the residuals are bad or good
#if bad check error
#if good then good for prediction

#Bad residuals handling
souvenir_data<-ndiffs(souvenir_ts)
plot.ts(souvenir_forecasts2$residuals) # make a time plot




#practise
library(TTR)
kings_tsSMA3=SMA(kings_ts,n=3)
plot.ts(kings_tsSMA3)

kings_tsSMA8=SMA(kings_ts,n=8)
plot.ts(kings_tsSMA8)

rain=read.csv("rain.csv")
rainseries=ts(rain, start=1813)
plot.ts(rainseries)

rainseriesforecasts=HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

rainseriesforecasts$fitted
plot(rainseriesforecasts)

library(forecast)

rainseriesforecasts2 = forecast.HoltWinters(rainseriesforecasts, h=8)
rainseriesforecasts2
plot.forecast(rainseriesforecasts2)




