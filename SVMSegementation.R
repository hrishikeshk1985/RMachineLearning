library(caret)
library(kernlab)
library(ROCR)
data("segmentationData")
View(segmentationData)

dim(segmentationData)
summary(segmentationData)

table(segmentationData$Class)
table(segmentationData$Class) / length(segmentationData$Class)

# split the data into training and validation
Index <- createDataPartition(segmentationData$Class,p=.7,list=FALSE)
svm.train <- segmentationData[Index,]
svm.validate  <- segmentationData[-Index,]
trainX <-svm.train[,4:61] 


set.seed(123)
ctrl <- trainControl(method="cv",
                     number = 2,
                     summaryFunction=twoClassSummary,
                     classProbs=TRUE)

# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train SVM
svm.tune <- train(x=trainX,
                  y= svm.train$Class,
                  method = "svmRadial",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune

# Predict Target Label
valX <-svm.validate[,4:61]
pred <- predict(svm.tune, valX, type="prob")[2]

# Model Performance Statistics
pred_val <-prediction(pred[,2], svm.validate$Class)

# Calculating Area under Curve
perf_val <- performance(pred_val,"auc")
perf_val

# Calculating True Positive and False Positive Rate
perf_val <- performance(pred_val, "tpr", "fpr")

# Plot the ROC curve
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))

