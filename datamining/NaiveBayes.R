flights<-read.csv(file.choose())
View(flights)
factor(flights$delay)
flights$schedtime<-factor(floor(flights$schedtime/100))
View(flights)
factor(flights$schedtime)
library(plyr)
flights$delay<-mapvalues(flights$delay,from=c("delayed","ontime"),to=c(1,0))
View(flights)
factor(flights$origin)
flights$origin<-mapvalues(flights$origin,from=c("BWI","DCA","IAD"),to=c(1,2,3))
View(flights)
factor(flights$dest)
flights$dest<-mapvalues(flights$dest,from=c("EWR","JFK","LGA"),to=c(1,2,3))
View(flights)
factor(flights$carrier)
flights$carrier<-mapvalues(flights$carrier,from=c("CO","DH","DL","MQ","OH","RU","UA","US"),to=c(1,2,3,4,5,6,7,8))
View(flights)

delay<-flights[,c(-3,-5,-6,-7,-11,-12)]
View(delay)
set.seed(1234)
random<-sample(x=c("Training3","Testing3"),size=nrow(delay),replace=T,prob = c(0.6,0.4))
random
Training3<-delay[random=="Training3",]
View(Training3)
Testing3<-delay[random=="Testing3",]
View(Testing3)
library(e1071)
delayclassifier<-naiveBayes(Training3,Training3$delay,laplace=1)
prediction<-predict(delayclassifier,Testing3)
table(prediction,Testing3$delay)
56/(56+690)
  