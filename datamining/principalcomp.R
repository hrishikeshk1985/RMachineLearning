raw<-read.csv(file.choose())
View(raw)
states=c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
library(cluster)
raw[1:3,]
dim(raw)
rawt=t(raw)
dim(rawt)
rawt[1:3,]
pcaunemp <- prcomp(rawt,scale=FALSE)
pcaunemp
plot(pcaunemp, main="")
mtext(side=1,"Unemployment: 50 states",line=1,font=2)
pcaunemp$rotation[,1]
pcaunemp$rotation[1:10,1]
ave=dim(416)
for (j in 1:416) {
  ave[j]=mean(rawt[,j])
}
par(mfrow=c(1,2))
## plot negative loadings for first principal component
plot(-pcaunemp$rotation[,1])
## plot monthly averages of unemployment rates
plot(ave,type="l",ylim=c(3,10),xlab="month",ylab="ave
+ unemployment rate")
abs(cor(ave,pcaunemp$rotation[,1]))
pcaunemp$rotation[,2]
pcaunemp$rotation[,3]
unemppc <- predict(pcaunemp)
unemppc
set.seed(1)
grpunemp3 <- kmeans(rawt,centers=3,nstart=10)
grpunemp3
plot(unemppc[,1:2],type="n")
text(x=unemppc[,1],y=unemppc[,2],labels=states,col=rainbow(7)[grpunemp3$cluster])
