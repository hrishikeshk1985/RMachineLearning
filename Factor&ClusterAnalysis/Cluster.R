library(HDclassif)
data("wine")
str(wine)
table(wine$class)
df<-as.data.frame(scale(wine[,-1]))
df

# use kmeans clustering algorithm

library(clustertend)
hopkins(df,n=nrow(df)-1)
#H value should be less than 0.5 highly clusterable
# Conclusion data is clusterable

# Find the number of clusters the data needs to designed in with
library(NbClust)
NbClust(df,min.nc=2,max.nc=15,method="kmeans")
set.seed(1234)
km=kmeans(df,3,nstart = 25)
table(km$cluster)
km$centers

table(km$cluster,wine$class)

# using gower metric
df$class=as.factor(wine$class)
df$class
df$v1=as.factor(ifelse(df$V1>0,"high","low"))
str(df)
