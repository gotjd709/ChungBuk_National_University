################## 과제 5: 3가지 ########################



####### 과제 1번 문제 ##########
library(caret)
set.seed(123)
iris_RS <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[iris_RS, ]
testData <- iris[-iris_RS, ]
nrow(trainData) / (nrow(testData) + nrow(trainData))
## [1] 0.7
svm_model <- svm(Species ~ ., data = trainData)
summary(svm_model)
## Call:
## svm(formula = Species ~ ., data = trainData)
## Parameters:
##   SVM-Type:  C-classification 
## SVM-Kernel:  radial 
## cost:  1 
## Number of Support Vectors:  43
## ( 7 19 17 )
## Number of Classes:  3 
## Levels: 
##   setosa versicolor virginica
pred <- predict(svm_model, testData)
table(pred, testData$Species)
## pred         setosa versicolor virginica
## setosa         15          0         0
## versicolor      0         14         1
## virginica       0          1        14
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE)
svmModel <- train(Species~., data = trainData,
                  method = "svmRadial",
                  trControl = fitControl,
                  tuneLength = 8,
                  metric = "Accuracy") # metric in multi-class : Accuracy #
svmModel
## Support Vector Machines with Radial Basis Function Kernel 
## 105 samples
## 4 predictor
## 3 classes: 'setosa', 'versicolor', 'virginica' 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 94, 95, 95, 95, 94, 94, ... 
## Resampling results across tuning parameters:
##   C      Accuracy   Kappa    
## 0.25  0.9500505  0.9247008
## 0.50  0.9509949  0.9260422
## 1.00  0.9566465  0.9345017
## 2.00  0.9548283  0.9317687
## 4.00  0.9499747  0.9244746
## 8.00  0.9537374  0.9302025
## 16.00  0.9549242  0.9321537
## 32.00  0.9495303  0.9239158
## Tuning parameter 'sigma' was held constant at a value of 0.5012783
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.5012783 and C = 1.
svm_model_after_tune <- svm(Species ~ ., data = trainData, kernel = "radial", cost = 1)
summary(svm_model_after_tune)
## Call:
## svm(formula = Species ~ ., data = trainData, kernel = "radial", cost = 1)
## Parameters:
##   SVM-Type:  C-classification 
## SVM-Kernel:  radial 
## cost:  1 
## Number of Support Vectors:  43
## ( 7 19 17 )
## Number of Classes:  3 
## Levels: 
##   setosa versicolor virginica
pred_after_tune <- predict(svm_model_after_tune, testData)
table(pred_after_tune, testData$Species)
## pred_after_tune setosa versicolor virginica
## setosa         15          0         0
## versicolor      0         14         1
## virginica       0          1        14



####### 과제 2번 문제 ############
library(e1071)
heartdf <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heartdf) <- c( "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
heartdf$num <- ifelse(heartdf$num > 0, "Disease", "noDisease")
heartdf$num <- as.factor(heartdf$num)
feature.names = names(heartdf)

for( f in feature.names) {
  if (class(heartdf[[f]]) == "factor") { #level : factor, numeric, character, integer#
    levels <- unique(c(heartdf[[f]]))
    heartdf[[f]] <- factor(heartdf[[f]],
                           labels=make.names(levels))
  }
}

levels(heartdf$num) <- c("Notdisease", "Disease")
str(heartdf)
303*0.7
## [1] 212.1
set.seed(1234)
trainData <- heartdf[sample(nrow(heartdf), 212), ]
set.seed(1234)
testData <- heartdf[-sample(nrow(heartdf), 212), ]
nrow(trainData)/(nrow(testData) + nrow(trainData))
## [1] 0.69967

train_x <- trainData$num
train_y <- subset(trainData, select = -num)
test_x <- testData$num
test_y <- subset(testData, select = -num)

svm_tune <- tune(svm, train.x = train_x, train.y = train_y, validation.x = test_x, validation.y = test_y,
                 kernel = "radial", ranges = list(cost = 10^(-1:2), gamma = c(.5, 1, 2)))
?tune


svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))


fitControl <- trainControl(method = "repeatedcv",
                           number = 10, 
                           repeats = 10, 
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
svmModel <- train(num ~ ., data = na.omit(trainData),
                  method = "svmRadial",
                  trControl = fitControl, 
                  preProcess = c("center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")
svmModel
## Support Vector Machines with Radial Basis Function Kernel 
## 208 samples
## 13 predictor
## 2 classes: 'Notdisease', 'Disease' 
## Pre-processing: centered (13), scaled (13) 
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 187, 188, 186, 187, 188, 186, ... 
## Resampling results across tuning parameters:
##   C      ROC        Sens       Spec     
## 0.25  0.8975968  0.8091111  0.8571212
## 0.50  0.8946692  0.8017778  0.8653788
## 1.00  0.8881827  0.7892222  0.8507576
## 2.00  0.8845741  0.7840000  0.8328788
## 4.00  0.8754116  0.7671111  0.8174242
## 8.00  0.8552180  0.7570000  0.8075000
## 16.00  0.8463914  0.7383333  0.7976515
## 32.00  0.8372079  0.7392222  0.8000758
## Tuning parameter 'sigma' was held constant at a value of 0.05894282
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.05894282 and C = 0.25.
svmPrediction <- predict(svmModel, testData)
svmPredictionprob <- predict(svmModel, testData, type = 'prob')[2]
ConfMatirxPrediction <- confusionMatrix(svmPrediction, na.omit(testData)$num)
ConfMatirxPrediction$table
##             Reference
## Prediction   Notdisease Disease
## Notdisease         30       4
## Disease            11      44



####### 과제 3번 문제 #########
#install.packages("ztable")
#install.packages("moonBook")
require(Epi)
require(pROC)
require(ztable)
require(moonBook)
source('ROC_sub.R')
heartdf <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
names(heartdf) <- c( "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")
heartdf$num <- ifelse(heartdf$num > 0, 0, 1)
as.numeric(unlist(heartdf$num))
##   [1] 1 0 0 1 1 1 0 1 0 0 1 1 0 1 1 1 0 1 1 1 1 1 0 0 0 1 1 1 1 0 1 0 0 1 1 1 0 0 0 1 0 1 1 1 0 0 1 0 1 1 1 1 0 1 0 0 0 0 1 1
##  [61] 0 1 0 1 0 0 0 1 0 0 1 0 0 0 0 1 0 1 1 0 1 1 1 0 1 1 1 1 1 1 1 0 1 1 1 0 0 0 1 1 1 1 1 1 0 1 0 0 0 0 0 0 1 0 0 1 1 1 0 0
## [121] 0 0 1 0 0 1 0 0 1 1 1 1 1 1 1 1 0 0 0 1 1 0 1 0 1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 1 1 0 1 1 1 1 1 1 0 1 0 1 0 1 0 0 1 0 1 1
## [181] 0 0 1 1 0 1 1 0 0 0 1 0 0 0 1 0 1 1 1 0 1 1 1 1 1 0 0 0 1 0 1 0 1 0 0 1 1 1 1 1 1 1 1 0 0 1 1 1 0 0 1 0 0 1 1 0 0 0 1 1
## [241] 1 1 1 0 1 0 0 0 0 1 1 0 1 1 1 1 1 1 1 0 1 0 1 1 0 0 0 0 0 1 0 1 0 1 0 1 1 1 0 1 0 1 0 1 0 0 0 1 1 1 0 1 0 0 0 1 0 0 0 0
## [301] 0 0 1
f_age <- ROC(form=num~age,data=heartdf,plot="ROC")
f_trestbps <- ROC(form=num~trestbps,data=heartdf,plot="ROC")
f_chol <- ROC(form=num~chol,data=heartdf,plot="ROC")
f_thalach <- ROC(form=num~thalach,data=heartdf,plot="ROC")
f_oldpeak <- ROC(form=num~oldpeak,data=heartdf,plot="ROC")

result = step_ROC(num ~ age + trestbps + chol + thalach + oldpeak, data = heartdf, plot = FALSE)
## Start:  AIC=354.94
## num ~ age + trestbps + chol + thalach + oldpeak
## Df Deviance    AIC
## - age       1   54.660 352.96
## - chol      1   54.905 354.31
## - trestbps  1   54.979 354.72
## <none>          54.656 354.94
## - thalach   1   60.014 381.27
## - oldpeak   1   60.477 383.60
## Step:  AIC=352.96
## num ~ trestbps + chol + thalach + oldpeak
## Df Deviance    AIC
## - chol      1   54.933 352.47
## <none>          54.660 352.96
## - trestbps  1   55.028 352.99
## - oldpeak   1   60.491 381.67
## - thalach   1   61.044 384.43
##  Step:  AIC=352.47
## num ~ trestbps + thalach + oldpeak
## Df Deviance    AIC
## <none>          54.933 352.47
## - trestbps  1   55.390 352.98
## - oldpeak   1   60.830 381.36
## - thalach   1   61.288 383.64
plot_ROC(result$initial,result$final,show.lr.eta=FALSE,show.sens=FALSE,type=1)
print(result$table)

