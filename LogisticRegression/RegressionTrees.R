#german bank data set
bank<-read.csv(file.choose())
View(bank)
str(bank)
bank_1<-bank[,2:32]
View(bank_1)
str(bank_1)
summary(bank_1)

# sort the data

for(i in c(1,3:9,11:21,23:31)){
  bank_1[,i]<-factor(x=bank_1[,i],levels=sort(unique(bank_1[,i])))
}
View(bank_1)
bank_1$CHK_ACCT

# create training testing data sets
set.seed(123)
random<-sample(x=c("Training1","Testing1"),size=nrow(bank_1),replace=T,prob = c(0.7,0.3))
random
Training1<-bank_1[random=="Training1",]
View(Training1)
Testing1<-bank_1[random=="Testing1",]
View(Testing1)
library(rpart)
library(partykit)
treereg=rpart(RESPONSE~.,data=Training1)
print(treereg$cptable)
plotcp(treereg)
cp=min(treereg$cptable[5,])
plot(as.party(treereg))
prune.treereg=prune(treereg,cp=cp)
plot(as.party(prune.treereg))
Testing1$RESPONSE
rparty.test=predict(prune.treereg,newdata=Testing1,type="class")

table(rparty.test,Testing1$RESPONSE)
(41+180)/295
#Regression tree performs better than Logistic Regression

library(randomForest)
set.seed(123)
rfreg=randomForest(RESPONSE~.,data=Training1)
print(rfreg)
plot(rfreg)
which.min(rfreg$err.rate[,1])
rfreg2=randomForest(RESPONSE~.,data=Training1,ntree=353)
print(rfreg2)
rfregtest=predict(rfreg,newdata=Testing1,type="response")
table(rfregtest,Testing$RESPONSE)
(37+191)/(37+11+56+191)
# Random forests work much better than Regression Trees


