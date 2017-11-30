library(lattice)
don<-read.csv(file.choose())
View(don)
don[1:5,]
table(don$Class.Year)
barchart(table(don$Class.Year),horizontal = FALSE)


don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+don$FY03Giving+don$FY04Giving
mean(don$TGiving)
sd(don$TGiving)

#  max of the people are donating between 0 and 1000 dollars
quantile(don$TGiving,probs=seq(0,1,0.05))
quantile(don$TGiving,probs=seq(0.95,1,0.01))
hist(don$TGiving)
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])

#Boxplots using given data for the donors
boxplot(don$TGiving,horizontal=TRUE,xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,horizontal=TRUE,xlab="Total Contribution")

#donors giving the maximum donations
ddd=don[don$TGiving>=30000,]
ddd
ddd1=ddd[,c(1:5,12)]
ddd1
ddd1[order(ddd1$TGiving,decreasing=TRUE),]

#various boxplots from the given data
boxplot(TGiving~Class.Year,data=don,outline=FALSE)
boxplot(TGiving~Gender,data=don,outline=FALSE)
boxplot(TGiving~Marital.Status,data=don,outline=FALSE)
boxplot(TGiving~AttendenceEvent,data=don,outline=FALSE)

t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
t4
t5=table(don$Major)
t5
t6=cbind(t4,t5)
t6
t7=t6[t6[,2]>10,]
t7
t7[order(t7[,1],decreasing=TRUE),]
barchart(t7[,1],col="black")

t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
t4
t5=table(don$Next.Degree)
t5
t6=cbind(t4,t5)
t6
t7=t6[t6[,2]>10,]
t7[order(t7[,1],decreasing=TRUE),]
barchart(t7[,1],col="black")

#density plot
densityplot(~TGiving|factor(Class.Year),data=don[don$TGiving<=1000,][don[don$TGiving<=1000,]$TGiving>0,],plot.points=FALSE,col="black")

t11=tapply(don$TGiving,don$Class.Year,FUN=sum,na.rm=TRUE)
t11
barplot(t11,ylab="Average Donation")

barchart(tapply(don$FY04Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY03Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY02Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY01Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY00Giving,don$Class.Year,FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")


don$TGivingIND=cut(don$TGiving,c(-1,0.5,10000000),labels=FALSE)-1
don$TGivingIND
mean(don$TGivingIND)
t5=table(don$TGivingIND,don$Class.Year)
t5
barplot(t5,beside=TRUE)


mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))

t50=tapply(don$TGivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE,col="black")
mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))
t50=tapply(don$TGivingIND,don$Class.Year,FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE)

Data=data.frame(don$FY04Giving,don$FY03Giving,don$FY02Giving,don$FY01Giving,don$FY00Giving)
correlation=cor(Data)
correlation
plot(Data)
library(ellipse)
plotcorr(correlation)
mosaicplot(factor(don$Gender)~factor(don$TGivingIND))
mosaicplot(factor(don$Marital.Status)~factor(don$TGivingIND))
t2=table(factor(don$Marital.Status),factor(don$TGivingIND))
mosaicplot(t2)
mosaicplot(factor(don$AttendenceEvent)~factor(don$TGivingIND))

