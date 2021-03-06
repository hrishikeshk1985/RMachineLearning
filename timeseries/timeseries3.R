d<-read.table(file.choose(),sep=".")
View(d)
data <- ts(d[,1], start = c(1866), frequency= 1  )
View(data)
start(data)
end(data)
plot(data)
library(tseries)
ndiffs(data)
data1 <- diff(data, differences=1)
plot(data1)
data2<-diff(data,difference=2)
plot(data2)

par(mfrow=c(1,2))
acf(data2)
pacf(data2)
auto.arima(data)
auto.arima(data,trace=TRUE,ic="aicc",approximation = FALSE,stepwise = FALSE)
fit<-arima(data,c(1,2,0))
accuracy(fit)
qqnorm(residuals(fit))
qqline(residuals(fit))
shapiro.test(residuals(fit))

Box.test(fit$residuals,lag=20,type="Ljung-Box")

predi<-predict(fit,n.ahead=10)
predi
