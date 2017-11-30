
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

# create a logistic model

Training_model1<-glm(RESPONSE~.,data=Training1,family="binomial")
summary(Training_model1)

library(MASS)
Training_Model_11<-stepAIC(object=Training_model1)
summary(Training_Model_11)

#categoriacal: having CHK_Accnt with with respect to 0 changes the log odds of the customer being good by 0.198
OddsRatio<-exp(coef(Training_Model_11))
OddsRatio

library(car)
vif(Training_Model_11)# all the Vifs are less than 5 hence no problem of multicollinearity exists 
#chance of person having Check account 1 being a good customer is 1.21 times greater than having check account 0 
#chance of customer being good (vs being bad ) by 0.5679
# odds ratio <1 there is a negative relation ship between dependent and independent

library(MKmisc)
#Hosmer & Lemshow goodness of fit
# Null Hypothesis Model fits the data well
HLgof.test(fit=Training_Model_11$fitted.values,obs=Training_Model_11$y,verbose=T)
# Accept null hypothesis ...Model fits the data well
Training_Model_11$fitted.values

# ROC Curve : Receiver Operationg charecterstic
library(pROC)
troc<-roc(response=Training_Model_11$y,predictor =Training_Model_11$fitted.values,plot=T)
troc$auc
#sensitivity - true positive rate
# specificity - false positive rate
# acceptable if c values lies between 0.7 to 0.8
# excellent if c values lies between 0.8 to 0.9
# above that it may be overfit
#less than 0.5 random model
#if equal to 0.5 the model has no discrimination

trpred<-ifelse(test=Training_Model_11$fitted.values<0.5,0,1)
table(Training_Model_11$y,trpred)
(108+449)/(108+99+49+449)

tspred<-predict.glm(object=Training_Model_11,newdata=Testing1,type="response")
troc1<-roc(response=Testing1$RESPONSE,predictor=tspred,plot=T)
troc1$auc
tspred1<-ifelse(test=tspred<0.5,0,1)
table(Testing1$RESPONSE,tspred1)
(46+171)/(46+47+31+171)
#good level of accuracy

