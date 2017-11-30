concrete<-read.csv(file.choose())
str(concrete)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))
str(concrete_norm)
  

summary(concrete_norm$strength)
summary(concrete$strength)


# create test and train data sets
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

# predict the model
library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag +ash + water + superplastic +coarseagg + fineagg + age,data = concrete_train)
plot(concrete_model)

# Evaluate the model on the test data set
model_results<-compute(concrete_model,concrete_test[1:8])
predict_strength<-model_results$net.result
predict_strength

cor(predict_strength,concrete_test$strength)
# depicting high correlations

#improving model performance
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +coarseagg + fineagg + age,data = concrete_train, hidden = 5)
plot(concrete_model2)

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
