library(ElemStatLearn)
library(leaps)
library(caret)
library(corrplot)
data("prostate")
str(prostate)
plot(prostate)
plot(prostate$gleason)
table(prostate$gleason)
boxplot(prostate$lpsa~prostate$gleason,xlab="Gleason Score",ylab="Log PSA")
prostate$gleason=ifelse(prostate$gleason==6,0,1)
table(prostate$gleason)
p.cor=cor(prostate)
corrplot.mixed(p.cor)
train=subset(prostate,train==TRUE)[,1:9]
str(train)
test=subset(prostate,train==FALSE)[,1:9]
str(test)

subfit=regsubsets(lpsa~.,data=train)
b.sum=summary(subfit)
which.min(b.sum$bic)
plot(subfit,scale="bic",main="Best Subset Features")
ols=lm(lpsa~lcavol+lweight+gleason,data=train)
plot(ols$fitted.values,train$lpsa)


# work with the test dataset
pred.subfit=predict(ols,newdata=test)
plot(pred.subfit,test$lpsa)
resid.subfit=test$lpsa-pred.subfit
mean(resid.subfit^2)


# ridge regression

x=as.matrix(train[,1:8])
y=train[,9]
x
y
library(glmnet)
ridge=glmnet(x,y,family="gaussian",alpha=0)
print(ridge)
plot(ridge,label=TRUE)
plot(ridge,xvar="lambda",label="TRUE")

# create ridge coeffeicients
ridge.coef=coef(ridge,s=0.1,exact=TRUE)
ridge.coef
plot(ridge,xvar="dev",label=TRUE)
newx=as.matrix(test[,1:8])
ridge.y=predict(ridge,newx=newx,type="response",s=0.1)
plot(ridge.y,test$lpsa)

ridge.resid=ridge.y-test$lpsa
mean(ridge.resid^2)

# Use of LASSO

lasso=glmnet(x,y,family = "gaussian",alpha=1)
print(lasso)

plot(lasso,xvar="lambda",label="TRUE")
lasso.coef=coef(lasso,s=0.045,exact=TRUE)
lasso.coef
lasso.y=predict(lasso,newx=newx,type="response",s=0.045)
plot(lasso.y,test$lpsa)
lasso.resid=lasso.y-test$lpsa
mean(lasso.resid^2)


# fit an elastic net
library(caret)
grid1=expand.grid(.aplha=seq(0,1,by=0.2), .lambda=seq(0.00,0.2,by=0.02))
table(grid1)
control=trainControl(method="LOOCV")
enet.train=train(lpsa~.,data=train,method="glmnet",trcontrol=control,tuneGrid=grid1)

enet=glmnet(x,y,family = "gaussian",alpha=0,lambda=0.08)
enet.coef=coef(enet,s=.08,exact="TRUE")

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("statmod","RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/1/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()
