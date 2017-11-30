adm<-read.csv(file.choose())
View(adm)
plot(adm$GPA,adm$GMAT,col=adm$De)
library(MASS)
m1=lda(De~.,adm)
plot(m1)
predict(m1,newdata=data.frame(GPA=2.89,GMAT=358))
predict(m1,newdata=data.frame(GPA=3.17,GMAT=310))
predict(m1,newdata=data.frame(GPA=2.58,GMAT=358))
predict(m1,newdata=data.frame(GPA=3.89,GMAT=358))
predict(m2,newdata=data.frame(GPA=3.21,GMAT=497))
library(MASS)
m2=qda(De~.,adm)
m2
n=85
nt=60
neval=n-nt
rep=100
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(De~.,adm[train,])
  predict(m1,adm[-train,])$class
  tablin=table(adm$De[-train],predict(m1,adm[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin
predict(m2,newdata=data.frame(GPA=3.21,GMAT=497))


library(MASS)
set.seed(1)
data(fgl)
str(fgl)
glass=data.frame(fgl)
glass
factor(glass$type)

m1=lda(type~.,glass)
m1
predict(m1,newdata=data.frame(RI=3.0,Na=13,Mg=4,Al=1,Si=70,K=0.06,Ca=9,Ba=0,Fe=0))
predict(m1,newdata=data.frame(RI=3.0,Na=13,Mg=4,Al=1,Si=70,K=0.06,Ca=9,Ba=0,Fe=0))$class


n=length(fgl$type)
nt=200
neval=n-nt
rep=100
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  glass[train,]
  ## linear discriminant analysis
  m1=lda(type~.,glass[train,])
  predict(m1,glass[-train,])$class
  tablin=table(glass$type[-train],predict(m1,
                                          + glass[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin
n=214
neval=1
errlin=dim(n)
errqua=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## linear discriminant analysis
  m1=lda(type~.,glass[train,])
  predict(m1,glass[-train,])$class
  tablin=table(glass$type[-train],predict(m1,glass[-train,])$class)
  print(tablin)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin
