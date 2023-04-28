library(readr)
library(tidyverse)
library(ggpubr)
library(car)
theme_set(theme_pubr())
data = read.csv(file='C:\\Users\\Earlham College CGE\\Downloads\\cbe.csv',header=TRUE)

#load in and summarize the data
choc <- ts(data$choc, start=c(1958,1),frequency=12)
beer <- ts(data$beer, start=c(1958,1),frequency=12)
elec <- ts(data$elec, start=c(1958,1),frequency=12)
class(choc)
choc
start(choc); end(choc); frequency(choc)
summary(choc)

#create a plot of the timeseries
plot(AP,ylab='Air Passengers/1000s')
#remove seasonal effects using aggregate function
plot(aggregate(AP), ylab="Annual passengers/1000’s")
#boxplots of data by month
boxplot(AP ~ cycle(AP), names=month.abb)
#combine into single plot
layout(1:2)
plot(aggregate(AP)); boxplot(AP ~ cycle(AP))
#reset layout
dev.off()

#look at correlograms
plot(AP)
acf(AP)
#zoom in on last three years
xbeg <- ts(AP[1:36],start=1958,frequency=12)
xend <- ts(AP[109:144],start=1952,frequency=12)
plot(xend)
acf(xbeg)$acf

#decompose time series into trend and seasonal components
plot(decompose(AP))     #additive model
ap.decom <- decompose(AP, type="mult")    #multiplicative model
plot(ap.decom)
trend = ap.decom$trend; seasonal = ap.decom$seasonal
ts.plot(cbind(trend, trend * seasonal), lty=1:2)

#creating a forecast model using Holt-Winters procedure
AP.exp = HoltWinters(AP, beta=0, gamma=0)     #uses exponential smoothing
AP.exp$alpha; AP.exp$beta; AP.exp$gamma
plot(AP.exp)
# using Holt_Winters procedure with trend and seasonal effects
AP.hw = HoltWinters(AP, seasonal="mult")
AP.hw$alpha; AP.hw$beta; AP.hw$gamma
plot(AP.hw)
AP.predict = predict(AP.hw, n.ahead=4*12)
ts.plot(AP, AP.predict, lty=1:2)

