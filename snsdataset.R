teens<-read.table(file.choose(),sep=",",header=TRUE)
View(teens)
str(teens)

# find the missing entries in any
table(teens$gender,useNA="ifany")
summary(teens$age)
max(teens$age)

#clean the data
teens$age<-ifelse(teens$age>=13 & teens$age<20,teens$age,NA)
summary(teens$age)

#assignment of the value for a dummy variable
teens$female<-ifelse(teens$gender == "F"&!is.na(teens$gender),1,0)
teens$no_gender<-ifelse(is.na(teens$gender), 1, 0)
View(teens)
table(teens$gender,useNA="ifany")
table(teens$female,useNA="ifany")
table(teens$no_gender,useNA="ifany")

# data prepartion
mean(teens$age)
mean(teens$age,na.rm=TRUE)
#imputation technique for the means
#calculates the average age for graduations for the levels of the grad year after removing NA values
aggregate(data=teens,age~gradyear,mean,na.rm=TRUE)
ave_age <- ave(teens$age, teens$gradyear, FUN =function(x) mean(x, na.rm = TRUE))
ave_age
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
teens$age
summary(teens$age)


library(stats)
interests<-teens[5:40]
interests
interests_z <- as.data.frame(lapply(interests, scale))
teen_clusters <- kmeans(interests_z,5)
teen_clusters$size
teen_clusters$centers
teens$cluster<-teen_clusters$cluster
teens[1:5,c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)
