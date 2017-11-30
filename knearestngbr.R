wbcd<-read.table(file.choose(),sep=",",header=TRUE)
View(wbcd)
str(wbcd)
wbcd<-wbcd[-1]
str(wbcd)
table(wbcd$diagnosis)
wbcd$diagnosis<-factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
str(wbcd)
table(wbcd$diagnosis)

#normalize
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
wbcd_n
summary(wbcd_n)


#test and train data sets
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[469:569,]


wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[469:569, 1]

library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
table(wbcd_test_labels,wbcd_test_pred)

#using z Scores
d.
wbcd_z<-as.data.frame(scale(wbcd[-1]))
wbcd_train1<-wbcd_z[1:469, ]
wbcd_test1<-wbcd_z[470:569, ]
wbcd_train_labels1<-wbcd[1:469, 1]
wbcd_test_labels1<-wbcd[470:569, 1]
wbcd_test_pred1<-knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels1, k=21)
table(wbcd_test_labels1,wbcd_test_pred1)
