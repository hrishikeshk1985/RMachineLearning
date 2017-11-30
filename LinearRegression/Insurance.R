insurance<-read.csv(file.choose(),stringsAsFactors = TRUE)
View(insurance)
str(insurance)
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])
ins_model<-lm(charges~.,data=insurance)
ins_model
summary(ins_model)
insurance$age2<-insurance$age^2
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

#improved model
ins_model12<-lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data=insurance)
summary(ins_model12)
