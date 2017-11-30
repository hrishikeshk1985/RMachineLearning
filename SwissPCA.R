library(arules)
data("Groceries")
str(Groceries)
rules=apriori(Groceries)
rules = apriori(Groceries, parameter =list(support = 0.05, confidence = .1))
inspect(rules)
library(vcdExtra)
data(ICU)
str(ICU)
summary(ICU)
View(ICU)
ICU2=ICU[-4]
ICU2
ICU2$age = cut(ICU2$age,breaks =4)
ICU2$age
ICU2$systolic = cut(ICU2$systolic,breaks=4)
ICU2$systolic
ICU2$hrtrate = cut(ICU2$hrtrate, breaks = 4)
ICU2$hrtrate

ICU_tr = as(ICU2, "transactions")
ICU_tr
rules = apriori (ICU_tr,parameter = list(support = .85, confidence = .95))
inspect(rules)
IM=interestMeasure(rules,"FishersExactTest",ICU_tr)
round(IM,2)
rulesDeath=apriori(ICU_tr,parameter = list(confidence = 0.3,support=.1),appearance = list(rhs = c("died=Yes"), default="lhs"))
inspect(rulesDeath)

rulesDeath.df = as(rulesDeath,"data.frame")
rulesDeath.df.sorted =rulesDeath.df[order(rulesDeath.df$lift,decreasing = T),]
head(rulesDeath.df.sorted)
library(arulesViz)
plot(rulesDeath,method="paracoord", control=list(reorder=TRUE))
