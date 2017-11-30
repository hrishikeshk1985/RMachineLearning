library(ElemStatLearn)
data("prostate")
View(prostate)
d<-data.frame(prostate)
View(d)
# replace the variable gleason by a 0 and 1 value
d$gleason=ifelse(d$gleason==6,0,1)
View(d)
d.train=subset(d,train==TRUE)[,1:9]
d.test=subset(d,train==FALSE)[,1:9]
View(d.train)
View(d.test)

library(rpart)
tree.pros=rpart(lpsa~.,data=d.train)
print(tree.pros$cptable)
plotcp(tree.pros)

# create the regression tree
cp=min(tree.pros$cptable[5,])
cp
prune.tree.pros=prune(tree.pros,cp=cp)
# plot the regression trees

library(partykit)
plot(as.party(tree.pros))
plot(as.party(prune.tree.pros))


# test the regression tree model on the test data set
party.pros.test=predict(prune.tree.pros,newdata=d.test)
party.pros.test

rpart.resid=party.pros.test-d.test$lpsa
rpart.resid

mean(rpart.resid^2)
