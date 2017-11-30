library(sas7bdat)
#walmart sas data set in the collection
customer<-read.sas7bdat(file.choose())
View(customer)
# The data consists of all numeric features so no need to worry!!!
# We are trying to check how customer satisfaction is related with all other features
# linear and additive
#no correlation between error terms (errors terms) no autocorrelation
# independent variables should no be correlated (absence of multicollinearity) check VIF
#Error terms must have constant variance (homocedasticity is present) (log transform independent variable makes variance constant)
#Error terms is normally distributed (transform dependent varible)


set.seed(123)
random<-sample(x=c("Training","Testing"),size=nrow(data),replace=T,prob = c(0.7,0.3))
random
Training<-customer[random=="Training",]
View(Training)
Testing<-customer[random=="Testing",]


# Training a model
Training_model<-lm(Customer_Satisfaction~.,data=Training)
summary(Training_model)

#choosing variables for Regression
Training_model_1<-step(object=Training_model,direction="both")
summary(Training_model_1)
# choose the model with the lower value of AIC (Aikake Information Criteria)

#Model Validation
library(car)
vif(Training_model_1) # no presence of multicollinerity
durbin.watson(Training_model_1) # no autocorrelation (error terms are independent rho=0)
#null hypothesis : Ho correlation between the error terms
qqnorm(Training_model_1$residuals)
qqline(Training_model_1$residuals)
shapiro.test(Training_model_1$residuals) # errors are not normal 

par(mfrow=c(2,2))
plot(Training_model_1)
#check the fitted vs residual graph no funnel shape obtained 

# Test the obtained model
Test_predict<-predict.lm(object=Training_model_1,newdata=Testing)
summary(Test_predict)

# how close are the evaluated values obtained by the model and original in test data set
cor(x=Test_predict,y=Testing$Customer_Satisfaction)
# model performance is good
plot(Testing$Customer_Satisfaction,y=Test_predict)
# Almost all the values lie on the straight line 


