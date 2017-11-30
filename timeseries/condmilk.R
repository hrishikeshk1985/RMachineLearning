condmilk<-read.csv(file.choose())
View(condmilk)

datac<- ts(condmilk[,2], start = c(1971,1), frequency= 12  )
plot(datac,xlab="Years",ylab="stocks")


library(forecast)
par(mfrow=c(2,2))
ylim <- c(min(data),max(data))
plot(ma(datac, 4), main="Simple Moving Averages(k=4)", ylim= ylim)
plot(ma(datac, 6), main="Simple Moving Averages(k=6)", ylim= ylim)
plot(ma(datac, 8), main="Simple Moving Averages(k=8)", ylim= ylim)
plot(ma(datac, 12), main="Simple Moving Averages(k=12)", ylim= ylim)

fit <-stl(datac, s.window = "period")
plot(fit)
fit$time.series

monthplot(datac, xlab="years", ylab="sales")

seasonplot(datac, year.labels = TRUE, main="season plot")

a<-log(datac)
plot(a)
ndiffs(a)
library(tseries)
adf.test(a)


par(mfrow=c(1,2))
acf(a)
pacf(a)
auto.arima(datac)
auto.arima(datac,trace=TRUE,ic="aicc",approximation = FALSE,stepwise = FALSE)
fit<-arima(a,c(2,0,2))
accuracy(fit)


qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))
