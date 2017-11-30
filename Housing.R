housing<-read.csv(file.choose())
View(housing)
missing<-is.na(housing$Builtup)
sum(missing)
housing1<-na.omit(housing)
nrow(housing1)
View(housing1)
housing2<-housing1[,-1]
View(housing2)

factor(housing2$Parking)
table(housing2$Parking)
factor(housing2$City_Category)
table(housing2$City_Category)
levels(housing2$Parking)<-c("Covered","No Parking","No Parking","Open")
levels(housing2$Parking)
table(housing2$Parking)
View(housing2)
library(plyr)
housing2$Parking<-mapvalues(housing2$Parking,from=c("Covered","No Parking","Open"),to=c(1,2,3))
View(housing2)
housing2$City_Category<-mapvalues(housing2$City_Category,from=c("CAT A","CAT B","CAT C"),to=c(1,2,3))
View(housing2)

set.seed(1234)
random<-sample(x=c("Training2","Testing2"),size=nrow(housing2),replace=T,prob = c(0.7,0.3))
random
Training2<-housing2[random=="Training2",]
View(Training1)
Testing2<-housing2[random=="Testing2",]
View(Testing1)
fit=lm(House_Price~.,data=Training2)
summary(fit)
Training_model_1<-step(object=fit,direction="both")
summary(Training_model_1)

#validation
library(car)
vif(Training_model_1) # no presence of multicollinerity
durbin.watson(Training_model_1) # no autocorrelation (error terms are independent rho=0)
#null hypothesis : Ho correlation between the error terms
qqnorm(Training_model_1$residuals)
qqline(Training_model_1$residuals)
shapiro.test(Training_model_1$residuals) # errors are not normal 

par(mfrow=c(2,2))
plot(Training_model_1)

Test_predict<-predict.lm(object=Training_model_1,newdata=Testing2)
summary(Test_predict)

cor(x=Test_predict,y=Testing2$House_Price)
