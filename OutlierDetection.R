set.seed(3147)
x<-rnorm(100)
summary(x)
boxplot.stats(x)$out
boxplot(x)

#outliers in a bivriate data
y<-rnorm(100)
df<-data.frame(x,y)
df
rm(x,y)
head(df)
attach(df)
(a<-which(x%in%boxplot.stats(x)$out))

(b<-which(y%in%boxplot.stats(y)$out))
detach(df)
(outlier.list<-intersect(a,b))
plot(df)
points(df[outlier.list,],col="red",pch="+",cex=2.5)
#outliers in the union with respect to  x and y
(outlier.list2<-union(a,b))
points(df[outlier.list2,],col="blue",pch="*",cex=2.5)

# Dectection of outlier using local Outlier factor
library(DMwR)
View(iris)
iris2<-iris[,1:4]
iris2
outlier.scores<-lofactor(iris2,k=5)
outlier.scores
plot(density(outlier.scores))
outliers<-order(outlier.scores,decreasing = TRUE)[1:5]
print(outliers)
print(iris2[outliers,])


