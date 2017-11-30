retail<-read.csv(file.choose())
View(retail)

# First observation is that there are lots of missing entries in the data

# Let us try getting the data ready before we do any kind of analysis
table(retail$Outlet_Size,retail$Outlet_Type)
table(retail$Outlet_Size,retail$Outlet_Location_Type)
# compare the variables to fill up with the missing entries

# create levels
levels(retail$Outlet_Type)<-c(1,2,3,4)

retail[which(retail$Outlet_Type==1 & retail$Outlet_Size==""),"Outlet_Size"]<-"Small"
retail[which(retail$Outlet_Type==2 & retail$Outlet_Size==""),"Outlet_Size"]<-"Small"
# look at the data and replace with the attribute with the maximum frequencies
View(retail)
# Now that data is filled up with all the missing entries


levels(retail$Outlet_Size)
table(retail$Outlet_Type)
factor(retail$Outlet_Size)

# now map the categorical variables to ordinal variables
library(plyr)
retail$Outlet_Size<-mapvalues(retail$Outlet_Size,from=c("Small","Medium","High"),to=c(1,2,3))
View(retail)
# now replace the nominal variable outlet location type by 
levels(retail$Outlet_Location_Type)<-c(1,2,3)
View(retail)

# there is an existence of an abnormal variable so check the levels. replace it by 1 and 2
levels(retail$Item_Fat_Content)
levels(retail$Item_Fat_Content)<-c("Low Fat","Low Fat","Low Fat","Regular","Regular")
View(retail)
retail$Item_Fat_Content<-mapvalues(retail$Item_Fat_Content,from=c("Low Fat","Regular"),to=c(1,2))
View(retail)

# create a new variable called as the item establishment year
retail$Outlet_Establishment_Year<-2017-retail$Outlet_Establishment_Year
View(retail)

retail_1<-retail[,-c(1,5,7)]
View(retail_1)

# now start with analysis to do regression
set.seed(123)
random<-sample(x=c("Train","Test"),size=nrow(retail_1),replace=T,prob = c(0.7,0.3))
random
Train<-retail_1[random=="Train",]
View(Train)
Test<-retail_1[random=="Test",]
View(Test)

# train the model
Train_model<-lm(Item_Outlet_Sales~.,data=Train)
summary(Train_model)

#variable selection for regression
Train_model_1<-step(object=Train_model,direction="both")
summary(Train_model_1)

# do model validation
library(car)
vif(Train_model_1)
durbin.watson(Train_model_1)
qqnorm(Train_model_1$residuals)
qqline(Train_model_1$residuals)
shapiro.test(Train_model_1$residuals) # errors are not normal

par(mfrow=c(2,2))
plot(Train_model_1) #heteroelasdicity present we need to apply log transform to make variance of errors constant

# funnel shape present in residual vs fitted

Train_model<-lm(log(Item_Outlet_Sales)~.,data=Train)
summary(Train_model)

Train_model_1<-step(object=Train_model,direction="both")
summary(Train_model_1)

# test the model on the test data set
Test_predict<-predict.lm(object=Train_model_1,newdata=Test)
summary(Test_predict)
cor(x=Test_predict,y=Test$Item_Outlet_Sales)

# model performace is reasonably good

plot(log(Test$Item_Outlet_Sales),y=Test_predict)
# you can see a linear trend towards the graph


