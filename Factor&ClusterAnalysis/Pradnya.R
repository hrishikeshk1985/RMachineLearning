water<-read.csv(file.choose())
View(water)
water1<-water[,-1]
View(water1)

#do principal component analysis
prin.comp2<-prcomp(water1,scale. = T)
summary(prin.comp2)

# Scree plot
screeplot(prin.comp2,type="line")


library(psych)
# identify the number of factors to be working with 
fact.analysis_2i<-factanal(x=water1,factors=2,scores="Bartlett",rotation="varimax")
fact.analysis_2i
