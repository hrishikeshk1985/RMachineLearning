country<-read.csv(file.choose())
View(country)

# remove country names
country1<-country[,-4]
View(country1)

# scale values to 0 and 1
scale(country1)

# check whether the data is clusterable
library(clustertend)
hopkins(country1,n=nrow(country1)-1)
#H value should be less than 0.5 highly clusterable
# Conclusion data is clusterable

# apply kmeans algorithm
set.seed(123)
k<-kmeans(country1,centers=3)
k
O=order(k$cluster)
data.frame(country$Country[O],k$cluster[O])
table(country$Country[O],k$cluster[O])
# clusters are created

# plot clusters
par(mfrow=c(1,3))
pie(colSums(country1[k$cluster==1,]),cex=1)
pie(colSums(country1[k$cluster==2,]),cex=1)
pie(colSums(country1[k$cluster==3,]),cex=1)

#clustering for all


library(NbClust)
nc<-NbClust(country1,min.nc=2,max.nc=15,method="kmeans")
nc
d<-dist(country1,method="euclidean")
H.fit<-hclust(d,method="ward.D")
H.fit
plot(H.fit)
groups<-cutree(H.fit,k=3)
rect.hclust(H.fit,k=3,border="red")




