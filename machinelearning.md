---
output:
  pdf_document: default
  html_document: default
---
library(AppliedPredictiveModeling)
library(caret)
library(dplyr)
library(randomForest)
training <- read.csv("C:/Users/NIKHIL/OneDrive/Desktop/Peer/pml-training.csv", na.strings=c("", "NA"))
testing <- read.csv("C:/Users/NIKHIL/OneDrive/Desktop/Peer/pml-testing.csv", na.strings=c("", "NA"))
unique(training$classe)
ncol(training)
str(training[,1:10])
str(training[,149:160])
table(training$classe,training$user_name)
ggplot(training, aes(classe)) + geom_bar(fill = "steelblue") + ggtitle("Counts per classe")
#Data pre-processing
training$classe <- as.factor(training$classe) # classe is converted into a factor variable.

trainingPrep <- training %>% select(8:160) # Non-predictors are removed.

trainingPrep <- trainingPrep %>% select_if(colSums(is.na(trainingPrep)) < 19000) # Only the columns with LESS than 19000 NAs are left (total nr. of obs. is 19622)

ncol(trainingPrep) # The resulting amount of columns in the dataset is 53.
#Create Data Partition
#This dataset is further divided into train (75%) and test (25%) parts for cross-validation:
inTrain = createDataPartition(trainingPrep$classe, p = 3/4)[[1]]
trainPart = trainingPrep[ inTrain,]
testPart = trainingPrep[-inTrain,]
#Model training
set.seed(1234)
modfitrf <- randomForest(classe~., method = "class", data = trainPart)
predrf <- predict(modfitrf, newdata = testPart, type = "class")
confusionMatrix(predrf, testPart$classe)
set.seed(1234)
modfitlda <- train(classe ~ ., method = "lda", data = trainPart)
predlda <- predict(modfitlda, newdata = testPart)
confusionMatrix(predlda, testPart$classe)

#Model selection
#The accuracy level of the random forest model (higher than 99%) is clearly higher than that of the LDA model (close to 70%). Therefore, the random forest model is selected.
#Cross validation and expected out of sample error
#The out of sample error (calculated as 1 - Accuracy Level) is below 1%, therefore very low.
#Prediction on 20 test cases
predrf20 <- predict(modfitrf, newdata = testing, type = "class")
print(predrf20)








