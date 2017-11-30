tractdata <- read.csv(file.choose())
tractdata
View(tractdata)

# convert the data in to time series object

data <- ts(tractdata[,2], start = c(2003,1), frequency= 12  )
plot(data, xlab="Years", ylab= "Tractor Sales")

#Step 2: Difference data to make data stationary on mean (remove trend)
#Time series decomposition of this data to understand underlying patterns for tractor sales.
#No cyclic pattern here

# check the trend
library(forecast)
par(mfrow=c(2,2))
ylim <- c(min(data),max(data))
plot(ma(data, 4), main="Simple Moving Averages(k=4)", ylim= ylim)
plot(ma(data, 6), main="Simple Moving Averages(k=6)", ylim= ylim)
plot(ma(data, 8), main="Simple Moving Averages(k=8)", ylim= ylim)
plot(ma(data, 12), main="Simple Moving Averages(k=12)", ylim= ylim)
# to see the trend in the data
# trend is increasing pattern

# to do seasonal adjustments
fit <-stl(data, s.window = "period")
plot(fit)
fit$time.series
monthplot(data, xlab="years", ylab="sales")
# mean is non constant
seasonplot(data, year.labels = TRUE, main="season plot")
# there is a seasonal variation in the data
#Each shows an increasing trend and similar seasonal pattern year to year.


#Difference data to make data stationary on mean (remove trend)
plot(diff(data),ylab="Differenced Tractor Sales")
#series is not stationary on variance
#perform log tranformation to make a series stationary on variance
plot(log10(data),ylab="Log (Tractor Sales)")


#this series is not stationary on mean since we are using the original data without differencing.

plot(diff(log10(data)),ylab="Differenced Log (Tractor Sales)")

#trend less. Stationary. 

#Plot ACF and PACF to identify potential AR and MA model
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main="ACF Tractor Sales")
pacf(ts(diff(log10(data))),main="PACF Tractor Sales")


# fit an arima model
ARIMAfit <- auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#predict
pred <- predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Tractor Sales")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+2*pred$se),col="orange") # se denotes the standard error
lines(10^(pred$pred-2*pred$se),col="orange") # se denoted the standard error

par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main="ACF Residual")
pacf(ts(ARIMAfit$residuals),main="PACF Residual")
#residuals are random.arima MODEL IS FINE.






