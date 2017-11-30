Clean_data<-read.csv(file.choose())
numeric<-c("Dist_Taxi", "Dist_Market", "Dist_Hospital", "Carpet","Builtup", "Rainfall")
categoric <- c("Parking", "City_Category")
Target<-c("House_Price")

# Prepare train and test data for regression models

set.seed(42)
train <-sample(nrow(Clean_data), 0.7*nrow(Clean_data))
test<-setdiff(seq_len(nrow(Clean_data)), train)
require(FactoMineR)
Data_for_PCA<-Clean_data[,numeric]
pca1<-PCA(Data_for_PCA)
PCA_data<-as.data.frame(cbind(Clean_data[train,c(Target,categoric)],pca1$ind$coord[train,]))
Step_PCA_Reg<-step(lm(House_Price~.,data = PCA_data))
summary(Step_PCA_Reg)
PCA_Estimate <- predict(Step_PCA_Reg, type="response", newdata=cbind(Clean_data[test,c(Target,categoric)],pca1$ind$coord[test,]))
Observed <- subset(Clean_data[test,c(numeric,categoric,Target)],select = Target)
format(cor(PCA_Estimate, Observed$House_Price)^2, digits=4)



Org_Reg<-lm(House_Price~.,data = Clean_data[train,c(Target,numeric,categoric)])
summary(Org_Reg)
Estimate <- predict(Org_Reg, type="response", newdata=Clean_data[test, c(numeric,categoric,Target)])
Observed <- subset(Clean_data[test,c(numeric,categoric,Target)],select = Target)
format(cor(Estimate, Observed$House_Price)^2, digits=4)
