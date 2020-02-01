#RAHIJ GILLANI FINAL PROJECT SYDE 631 - Time Series Analysis

library(ggplot2)
library(forecast)
library(tseries)
library(lmtest)
library(sqldf)
library(plotly)
library(TTR)
library(xts)
library(GeneCycle)
library(lubridate)
require(smooth)
require(Mcomp)
library(TSA)
library(urca)
require(lubridate)

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ DATA PREPROCESSING ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv,comment.char = "\\")
myList = c()
TS = c()
l = length(myfiles)
check = 0
for(j in seq(1:l)){
  data = data.frame(myfiles[j])  
  for(i in seq(1, nrow(data), by = 24)){
    myList <- c(myList, max(data[i:(i+23), 8]))
  }
  if(check == 0){
    inds <- seq(as.Date(as.character(data[1,1])), as.Date( (as.character(data[nrow(data),1]))), by="day")
    check = 1  
  }
  else{
    inds <- c(inds, seq(as.Date(as.character(data[1,1])), as.Date( (as.character(data[nrow(data),1]))), by="day"))
  }
}
myList = data.frame(myList)
timeSerieszoo = zoo(myList,inds,frequency = 365)
time_index <- seq(from = as.Date("2003-05-01"), 
                  to = as.Date("2019-11-17"), by = "day")
timeSeries_1 = xts(myList[(1:nrow(myList)),1], order.by = time_index)
attr(timeSeries_1, 'frequency') <- 365.25
timeSeries = ts(myList[(1:nrow(myList)),1], start=c(2003, yday("2003-05-01")), frequency=365.25)
plot(timeSeries_1,main="Daily Data")

#days_in_month(date_decimal((time(timeSeries))[1]))

r=auto.arima(timeSeries_1)
i=1
len = length(timeSeries_1)
while(i < len){
  check = as.integer(days_in_month((time(timeSeries_1))[i]))
  if((i+check-1)>len){
    TS <- c(TS, max(timeSeries_1[i:len])) 
  }
  else{
    TS <- c(TS, max(timeSeries_1[i:(i+check-1)]))  
  }
  i = i+check
}
TS = data.frame(TS)
time_index1 <- seq(from = as.Date("2003-05-01"), 
                  to = as.Date("2019-11-17"), by = "month")
#TS = xts(TS[(1:nrow(TS)),1], order.by = time_index1)
#attr(TS, 'frequency') <- 12

TS = ts(TS, start=c(2003,5, mday("2003-05-01")), frequency=12)

plot(TS, main = "Monthly Data")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ DATA PREPROCESSING ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#

Acf(TS,lag=60, main=expression(paste("ACF of Monthly TS")))
Pacf(TS,lag=60, main=expression(paste("PACF of Monthly TS")))
Acf(diff(TS,12),lag=60, main=expression(paste("ACF for ", Delta[12], " Monthly TS")))
Pacf(diff(TS,12),lag=60, main=expression(paste("PACF for ", Delta[12], " Monthly TS")))

plot(decompose(diff(TS,12)))
plot(diff(TS,12), main = expression(paste("Time Series after ", Delta[12], "Differencing")))

adf.test(diff(TS,12))

lam = BoxCox.lambda(TS)
timeSeriesTrans = BoxCox(TS,lambda = 1)
plot(timeSeriesTrans)
plot(TS)

train_date <- NROW(timeSeriesTrans) *0.8
train <- timeSeriesTrans[1:train_date]
train = ts(train, start=c(2003,5, mday("2003-05-01")), frequency=12)
test <- timeSeriesTrans[-c(1:train_date)]
test = ts(test, start=c(year(as.Date(train)[NROW(train)]), mday(as.Date(train)[NROW(train)])), frequency=12)
ac = timeSeriesTrans[1:NROW(timeSeriesTrans)]
ac = ts(ac, start=c(2003,5, mday("2003-05-01")), frequency=12)


auto.arima(train) %>% forecast(h = length(test) ) %>% accuracy(test)
m1=Arima(train, order=c(2,0,0),seasonal=list(order=c(2,1,1), period=12))
m1=Arima(train, order=c(1,0,2),seasonal=list(order=c(2,0,1), period=12))
m1 %>% forecast(h = length(test) ) %>% accuracy(test)
tsdiag(m1, gof.lag=60, main = "ARIMA(2,0,0)(0,1,1)[12]") # box test bad with order=c(1,0,0)
checkresiduals(m1)

plot((forecast(m1,h= length(test))))
lines(fitted(forecast(m1,h= length(test)) ),col="blue")
lines(timeSeriesTrans,col="red")

plot(HoltWinters(train))
plot(forecast(HoltWinters(train),h =length(test)))
lines(fitted(forecast(HoltWinters(train),h= length(test)) ),col="blue")
lines(timeSeriesTrans,col="red")
HoltWinters(train) %>% forecast(h = length(test) ) %>% accuracy(test)
checkresiduals(HoltWinters(train))

hist(resid(m1),prob=TRUE,main="Residual")
lines(density(resid(m1)))

model_overfitt=Arima(train, order=c(2,0,0),seasonal=list(order=c(3,1,1), period=12))
lrtest(m1,model_overfitt) # Likelihood Ratio Test. As the p value is low so need to overfitt.
model_overfitt %>% forecast(h = length(test) ) %>% accuracy(test)
tsdiag(model_overfitt, gof.lag=60, main = "ARIMA(2,0,0)(4,1,2)[12]") # box test bad with order=c(1,0,0)
checkresiduals(model_overfitt)


hist(resid(model_overfitt),prob=TRUE,main="Residual")
lines(density(resid(model_overfitt)))

plot((forecast(model_overfitt,h= length(test))))
lines(fitted(forecast(model_overfitt,h= length(test)) ),col="blue")
lines(timeSeriesTrans,col="red")

tscomp = decompose(timeSeriesTrans)
plot(tscomp) #additive series

    
    
