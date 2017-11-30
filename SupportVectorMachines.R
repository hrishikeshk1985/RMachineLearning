library(e1071)
library(caret)
library(MASS)
library(kernlab)
library(pROC)
data("Pima.tr")
str(Pima.tr)

data("Pima.te")
str(Pima.te)
pima=rbind(Pima.tr,Pima.te)
str(pima)

pima.scale=as.data.frame(scale(pima[,-8]))
str(pima.scale)

pima.scale$type=pima$type

View(pima.scale)  

cor(pima.scale[-8])

table(pima.scale$type)
set.seed(502)
ind=sample(2,nrow(pima.scale),replace=TRUE,prob=c(0.7,0.3))
train=pima.scale[ind==1,]
test=pima.scale[ind==2,]
str(train)
str(test)

# support vector machine with linear kernel
linear.tune=tune.svm(type~.,data=train,kernel="linear",cost=c(0.001,0.01,0.1,1,5,10))
summary(linear.tune)
best.linear=linear.tune$best.model
best.linear
tune.test=predict(best.linear,newdata = test)
table(tune.test,test$type)

#support vector machine with polynomial kernel
set.seed(123)
poly.tune=tune.svm(type~.,data=train,kernel="polynomial",degree=c(3,4,5),coef0=c(0.1,0.5,1,2,3,4))
summary(poly.tune)
best.poly=poly.tune$best.model
poly.test=predict(best.poly,newdata=test)
table(poly.test,test$type)

#support vector machines with a radial basis kernel
set.seed(123)
rbf.tune=tune.svm(type~.,data=train,kernel="radial",gamma=c(0.1,0.5,1,2,3,4))
summary(rbf.tune)
best.rbf=rbf.tune$best.model
rbf.test=predict(best.rbf,newdata=test)
table(rbf.test,test$type)

#support vector machines using a sigmoidal kernel
set.seed(123)
sigmoid.tune=tune.svm(type~.,data=train,kernel="sigmoid",gamma=c(0.1,0.5,1,2,3,4),coef0=c(0.1,0.5,1,2,3,4))
summary(sigmoid.tune)
best.sigmoid=sigmoid.tune$best.model
sigmoid.test=predict(best.sigmoid,data=test)
sigmoid.test
table(sigmoid.test,test$type)
test$type
