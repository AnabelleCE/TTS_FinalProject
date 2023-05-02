library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(timeSeries)
library(xts)


#To gather data using quantmod package install 
#this pulls financial data from json
getSymbols("AMZN", from="2016-01-01", to="2020-01-01") #to get the data for a specific ticker/stock
View(AMZN) #to view the table from the created dataset
plot.ts(AMZN$AMZN.Close) #to plot the time series data graph
summary(AMZN$AMZN.Close)#to see the descriptive statistics of the graphed data

#to view seasonal and trend components
AMZN.ts=ts(AMZN$AMZN.Close, frequency = 120)
AMZN.de=decompose(AMZN.ts)
plot(AMZN.de)


#SUPERVISED >>ARIMA Model<<

#check if the data is stationary
  #not stationary based on looking at graph and running adf test
adf.test(AMZN$AMZN.Close)
sAMZN=diff(log(AMZN$AMZN.Close))#convert to stationary
plot.ts(sAMZN)#check if now stationary
adf.test(sAMZN)#Augmented Dickey Fuller Test, but got NAs
adf.test(as.numeric(na.omit(sAMZN))) #removing NAs

#plot ACF and PACF to identify potential AR and MA model
acf(na.omit(sAMZN))
pacf(na.omit(sAMZN))

#identify best fit ARIMA model
auto.arima(na.omit(sAMZN))
modelAMZN=arima(na.omit(sAMZN), order=c(0,0,0))
modelAMZN

#to observe non-stationary original data forecast
auto.arima(AMZN$AMZN.Close)
non_model=arima(AMZN$AMZN.Close, order=c(0,1,0))
autoplot(forecast(non_model, h=90))

# Diagnostic check = plot ACF and PACF for residuals of ARIMA to make sure no more information needs to be extracted
checkAMZN=residuals(modelAMZN)
acf(checkAMZN) #Make sure they are not correlated/residual are independent
plot.ts(checkAMZN) #check that they have a zero mean
gghistogram(checkAMZN)#show that they are normally distributed

#forecast using best fit ARIMA model
forecastAMZN=forecast(modelAMZN, h=90)
forecastAMZN
plot(forecastAMZN)

#check accuracy, using in sample
modelAMZN=arima(sAMZN[1:1000], order=c(0,0,0))
forecastAMZN=forecast(modelAMZN, h=5)
forecastAMZN
tail(sAMZN)
#comparing the forecast to the actual we can see that the values are very similar
#or by using accuracy function
accuracy(modelAMZN) 


