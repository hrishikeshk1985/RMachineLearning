cars<-read.csv(file.choose())
View(cars)

data3 <- ts(cars[,2], start = c(1960,1), frequency= 12  )
data3
plot(data3, xlab="Years", ylab= "Car Sales")

# do validations with time series
library(forecast)
seasonplot(data3)
# seasi=onality present in the time series.. strong seasonal effect with cycle effect
monthplot(data3)
# mean is not constant

library(forecast)
par(mfrow=c(2,2))
ylim <- c(min(data3),max(data3))
plot(ma(data3, 4), main="Simple Moving Averages(k=4)", ylim= ylim)
plot(ma(data3, 6), main="Simple Moving Averages(k=6)", ylim= ylim)
plot(ma(data3, 8), main="Simple Moving Averages(k=8)", ylim= ylim)
plot(ma(data3, 12), main="Simple Moving Averages(k=12)", ylim= ylim)
# to see the trend in the data
# trend is increasing pattern

a<-log(data3)
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
fit<-arima(a,c(0,1,3),seasonal=list(order=c(1,0,0),period=12))
accuracy(fit)


qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
# residuals are normal distributed


predi<-predict(fit,n.ahead=2*12)
predi

predict<-2.718^predi$pred
predict
