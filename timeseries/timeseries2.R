View(AirPassengers)
d<-AirPassengers
class(d)
start(d)
# Start year and month for the data
end(d)
# end year and month for the data

frequency(d)
summary(d)
plot(d)

# do validations with time series
library(forecast)
seasonplot(d)
# seasi=onality present in the time series.. strong seasonal effect with cycle effect
monthplot(d)
# mean is not constant

a<-log(d)
plot(a)
monthplot(a)

# make the time series stationary
library(tseries)
ndiffs(a)
a1<-diff(a)
plot(a1)

adf.test(a1)
#Augmented Dickey Fuller Test
# Ho- series is not stationary
#H1- series is stationary

# to determine the ARIMA Model
par(mfrow=c(1,2))
acf(a1)
pacf(a1)
# pacf to determine order of p 
# acf to determine order of q

auto.arima(a)
auto.arima(a,trace=TRUE,ic="aicc",approximation = FALSE,stepwise = FALSE)
fit<-arima(a,c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
accuracy(fit)
# look at the MAPE (Mean absolute percentage error) should be less than 0.5

qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
# residuals are normal distributed

# to check the residuals are independent
Box.test(fit$residuals,lag=20,type="Ljung-Box")
# residuals are independent (white noise)

predi<-predict(fit,n.ahead=10*12)
predi

predict<-2.718^predi$pred
predict

ts.plot(predict,log="y",lty=c(1,3))




