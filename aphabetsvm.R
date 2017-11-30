letters<-read.csv(file.choose())
str(letters)

#create test and train data sets
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000, ]

library(kernlab)
letter_classifier<-ksvm(letter ~ .,data =letters_train,kernel="vanilladot")
letter_classifier

#evaluating model performance
letter_predictions<-predict(letter_classifier,letters_test)
head(letter_predictions)


table(letter_predictions, letters_test$letter)
agreement<-letter_predictions==letters_test$letter
table(agreement)
prop.table(table(agreement))


library(kernlab)
letter_classifier1<-ksvm(letter~.,data =letters_train,kernel="polydot")
letter_classifier1

#evaluating model performance
letter_predictions1<-predict(letter_classifier1,letters_test)
head(letter_predictions1)


table(letter_predictions1, letters_test$letter)
agreement1<-letter_predictions1==letters_test$letter
table(agreement1)
prop.table(table(agreement1))

