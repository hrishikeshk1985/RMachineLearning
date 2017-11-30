credit<-read.csv(file.choose())
View(credit)
credit1<-credit[,c(3,6,9)]
View(credit1)
new<-scale(credit1)
View(new)
