library(sas7bdat)
emp<-read.sas7bdat(file.choose())
View(emp)
emp1<-emp[,-1]
View(emp1)

# do principal component analysis
prin.comp1<-prcomp(emp1)
summary(prin.comp1)

# Scree plot
screeplot(prin.comp1,type="line")
library(psych)
# identify the number of factors to be working with 
fact.analysis_4i<-factanal(x=emp1,factors=4,scores="Bartlett",rotation="varimax")
fact.analysis_4i
# Reject null hypothesis
fact.analysis_5i<-factanal(x=emp1,factors=5,scores="Bartlett",rotation="varimax")
fact.analysis_5i
# FOur are sufficient to work with

# Do regression with the components
reg<-lm(emp$Employee_Performance~fact.analysis_4i$scores)
summary(reg)
