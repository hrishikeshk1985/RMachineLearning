#if p value is less than alpha reject null hypothesis
# is p value is greater than aplha accept null hypothesis

library(sas7bdat)
d<-read.sas7bdat(file.choose())
View(d)
#One sample t test
#Ho : There is no significant difference between the population mean and sample mean
#vs H1: There is a significant difference
shapiro.test(x=d$BaselineBP)
#Data is drwan from a normal population
t.test(d$BaselineBP,mu=96)
# confidence interval concepts ...for any other sample

# Two sample test
f<-read.sas7bdat(file.choose())
View(f)
shapiro.test(f$IQ_Level) # normal distributed accept null hypothesis..
library(lawstat)
levene.test(f$IQ_Level,group=as.factor(f$Gender)) # to check equality of two variances
sample1<-f[f$Gender=="F",2]
sample1
sample2<-f[f$Gender=="M",2]
sample2
t.test(sample1,sample2,var.equal = TRUE)
# accept H1 by assuming that mean of females is neq to mean of males  

# Paired t test
g<-read.sas7bdat(file.choose())
View(g)
# Paired t test
#Ho: no significant difference mud=0, mu1=mu2
# assumption that diffence is normally distributed
t<-g$Before_The__Test-g$After_The_Test
mean(t)
shapiro.test(t)
t.test(g$Before_The__Test,g$After_The_Test,paired = T)
# accept null hypothesis: There is no significant difference before and after the test


#Chisquare test
h<-read.sas7bdat(file.choose())
View(h)
#chi square data
table(h$Gender,h$Voting_preferences)
#H_o: Gender and voting preferences are independent
chisq.test(h$Gender,h$Voting_preferences)
#look at the p value to reject null hypothesis
# association is there (samples are dependent)

# correlation
p<-read.sas7bdat(file.choose())
View(p)
#Correlation data set
r<-cor(p$Education,p$Experience)
r
library(psych)
v<-corr.test(p)
v
library(ppcor)
pcor.test(x=p$Education,y=p$Experience,z=p$Age)


## ANNOVA
v<-read.sas7bdat(file.choose())
View(v)
#student scores are normally distributed
shapiro.test(v$Students_Score)
set<-aov(formula=v$Students_Score~v$Age_Group)
summary(set)
# Null hyothesis : All the age group have equal student score on an average

x<-read.sas7bdat(file.choose())
View(x)
shapiro.test(x$Students_Score)
#Ho1:- All the age group have equal student score on an average
#H02:- Male and female have equal student score on an average
#Ho3:- effect is not present (age and gender are independent)
x$Age_Group<-factor(x=x$Age_Group,levels=sort(unique(x$Age_Group)))
levels(x$Gender)<-c(1,2)
View(x)
x$Gender<-factor(x=x$Gender,levels=sort(unique(x$Gender)))
set1<-aov(formula=x$Students_Score~x$Age_Group+x$Gender+x$Age_Group*x$Gender)
summary(set1)

x<-read.sas7bdat(file.choose())
View(n)
shapiro.test(n$Weight_Gain)
n$Diet_Amount<-factor(x=n$Diet_Amount,levels=sort(unique(n$Diet_Amount)))
n$Diet_Type<-factor(x=n$Diet_Type,levels=sort(unique(n$Diet_Type)))
set2<-aov(formula=n$Weight_Gain~n$Diet_Amount+n$Diet_Type+n$Diet_Amount*n$Diet_Type)
summary(set2)
tuk<-TukeyHSD(x=set2)
tuk






