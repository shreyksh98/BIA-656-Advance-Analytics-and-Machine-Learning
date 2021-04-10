#Seasonal ARIMA Models

library(Ecdat) #It may require install package jpeg
data(IncomeUK) 
consumption = IncomeUK[,2] 
consumption
plot(consumption)

#Problem 1 Describe the behavior of consumption. What types of differencing, seasonal, nonseasonal, or both, would you recommend? Do you recommend fitting a seasonal ARIMA model to the data with or without a log transformation? Consider also using ACF plots to help answer these questions.
acf(consumption) #to confirm that time series is non-stationary
plot(diff(consumption)) #time series differentiation 
acf(diff(consumption))

logConsumption=log(consumption) #consumption is a time series
plot(logConsumption)
acf(logConsumption)

#Problem 2 Regardless of your answers to Problem 1, find an ARIMA model that provides a good fit to log(consumption). What order model did you select? (Give the orders of the nonseasonal and seasonal components.)
plot(diff(logConsumption))
acf(diff(logConsumption))
pacf(diff(logConsumption))
library(forecast)
m1=arima(logConsumption,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=4))
m1

#Problem 3 Check the ACF of the residuals from the model you selected in Problem 2. Do you see any residual autocorrelation?
acf(m1$residuals) #checking the residuals for the model selected above
Box.test(m1$residuals, lag = 16, type="Ljung")

#Problem 4 Apply auto.arima to log(consumption) using BIC. What model is selected?
m2 = auto.arima(logConsumption, ic = 'bic')
m2

#Problem 5 Forecast log(consumption) for the next eight quarters using the models you found in Problems 2 and 4. Plot the two sets of forecasts in side-by- side plots with the same limits on the x- and y-axes. Describe any differences between the two sets of forecasts. Using the backshift operator, write the models you found in problems 2 and 4.
m3 = arima(logConsumption,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
m4 = auto.arima(logConsumption,ic="bic") 

forecast_a = forecast(m3,h=8)
forecast_b = forecast(m4, h=8)

par(mfrow=c(1,2))
plot(forecast_a$mean,xlim=c(1985.5,1987.25),ylim=c(10.8,11)) #shows lower and upper limit for 95% interval of pred
plot(forecast_b$mean,xlim=c(1985.5,1987.25),ylim=c(10.8,11))

#Note: To predict an arima object (an object returned by the arima function), use the predict function. To learn how the predict function works on an arima object, use ?predict.Arima. To forecast an object returned by auto.arima, use the forecast function in the forecast package. For example, the following code will forecast eight quarters ahead using the object returned by auto.arima and then plot the forecasts.

#Problem 6 Include the variable include log(Income) as an exogenous variable to forecast log(consumption) using auto.arima.  According to the AIC, is this model better than the previous models? (Hint: use xreg to include exogenous variables in arima and auto.arima)
income = IncomeUK[,1]
logincome = log(income)
#a1 = as.numeric(logincome)
#a2 = as.numeric(logConsumption)
m5=auto.arima(logConsumption, ic = 'aic', stepwise = F, approximation = F, xreg=logincome)
m5
forecast_c = forecast(m5, h=8, xreg = logincome)
#plot is not necessary, but just for reference
autoplot(forecast_c$mean)

#Your submission should include a file with your R code and another UNCOMPRESSED file with your report where you answer the above questions including the appropriate graphs. Please do not submit a compressed file.