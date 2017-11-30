
# case study of toothpaste feedback in factor analysis


factor<-read.table(file.choose(),header=TRUE)
View(factor)
factor_1<-factor[,-1]
View(factor_1)

# find correlation

r<-round(cor(factor_1),2)
r

# Test the correlation terms
test.cor<-bartlett.test(factor_1)
test.cor
#H0: none of the variable is corelated
#H1 : some of these variables are correlated (it is not a identity matrix)

# adequacy of sample size
library(psych)
KMO(factor_1)
# Kaiser Meyer olkin test 
#  overall MSA > 0.5 then sample size is adequate (measure of sample adequacy)

# principal component Analysis to do factoring ahead
prin.comp<-prcomp(factor_1)
summary(prin.comp)

# scree plot to identify the number of components that it can factored in to
screeplot(prin.comp,type="line")

# Number of factors to identified using Bartlett test
fact.analysis_2<-factanal(x=factor_1,factors=2,scores="Bartlett",rotation="varimax")
fact.analysis_2
# look at the pvalue accept null hypothesis. Two factors are sufficient
fact.analysis_3<-factanal(x=factor_1,factors=3,scores="Bartlett",rotation="varimax")
fact.analysis_3
# Is more than sufficient

# conclusion:- v1,v3 and v5 go in factor 1
# conclusion:- v2,v4 and v6 go in factor 2



