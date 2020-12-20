## 9주차 과제

# 패키지 장착
#install.packages("dplyr")
#install.packages("descr")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("ISLR")
#install.packages("MASS")
#install.packages("glmnet")
#install.packages("randomForest")
#install.packages("rpart")
#install.packages("ROCR")
#install.packages("abdiv")
library(dplyr)
library(descr)
library(DT)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(rpart)
library(ROCR)
library(abdiv)

# 데이터 불러오기
rm(list = ls())

mushrooms <- read.csv("C:/Users/ay190130/Desktop/data/mushrooms.csv", header = T)
str(mushrooms)

# character 변수 factor 변수로 변경
mushrooms_var <- names(mushrooms)
for(f in mushrooms_var){
  mushrooms[[f]] <- as.factor(mushrooms[[f]])
}

# 데이터 구조 확인
head(mushrooms)
str(mushrooms)

DT::datatable(mushrooms)
CrossTable(mushrooms$class)

mushrooms %>%
  ggplot(aes(class)) +  
  geom_bar()  

levels(mushrooms$class)
descr::CrossTable(mushrooms$stalk.root)



# 데이터 전처리
mushrooms <- mushrooms[, -17]
mushrooms$class <- factor(mushrooms$class, levels = c("p", "e"))
summary(mushrooms)

A <- model.matrix(~. -class, mushrooms)
dim(A)

# 탐색적 데이터 분석(EDA)

# Bar Plot
mushrooms %>%
  group_by(class) %>%
  ggplot(aes(cap.color, fill = class)) +
  geom_bar(position = "dodge")

mushrooms %>%
  group_by(class) %>%
  ggplot(aes(gill.color, fill = class)) +
  geom_bar(position = "dodge")

mushrooms %>%
  group_by(class) %>%
  ggplot(aes(odor, fill = class)) + 
  geom_bar(position = "dodge")

mushrooms %>%
  group_by(class) %>%
  ggplot(aes(spore.print.color, fill = class)) +
  geom_bar(position = "dodge")

# Mosaicplot
mosaicplot( ~ cap.color + class,
            data = mushrooms,
            color = T,
            cex = 1.2)
mosaicplot( ~ gill.color + class,
            data = mushrooms,
            color = T,
            cex = 1.2)
mosaicplot( ~ odor + class,
            data = mushrooms,
            color = T,
            cex = 1.2)
mosaicplot( ~ spore.print.color + class,
            data = mushrooms,
            color = T,
            cex = 1.2)

# model 생성
set.seed(0124)
n <- nrow(mushrooms)
idx <- 1:n

training.idx <- sample(idx, n*.60)
idx <- setdiff(idx, training.idx)

validation.idx <- sample(idx, n*.20)
test.idx <- setdiff(idx, validation.idx)

length(training.idx)
length(validation.idx)
length(test.idx)

training <- mushrooms[training.idx, ]
validation <- mushrooms[validation.idx, ]
test <- mushrooms[test.idx, ]

# RandomForest
set.seed(0124)
mushrooms_rf <- randomForest(class~., training)
mushrooms_rf

# 중요도 알아보기
importance(mushrooms_rf)
varImpPlot(mushrooms_rf)

# validation set 을 이용한 예측
predict(mushrooms_rf, newdata = validation[1:10,])
predict(mushrooms_rf, newdata = validation[1:10,], type = "prob")

# Radnom Forest 모형평가
y_obs <- ifelse(validation$class == "e", 1, 0)
yhat_rf <- predict(mushrooms_rf, newdata = validation, type = "prob")[, 'e']

library(abdiv)
binomial_deviance(y_obs, yhat_rf)

pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure = "tpr",
                       x.measure = "fpr")
plot(perf_rf, col = "red", main = "ROC Curve")
abline(0,1)

performance(pred_rf, "auc")@y.values[[1]]

#LASSO
xx <- model.matrix(class ~ .-1, mushrooms)
x <- xx[training.idx,]
y <- ifelse(training$class == "e", 1, 0)
dim(x)

mushrooms_glmnet_fit <- glmnet(x,y)
plot(mushrooms_glmnet_fit)
mushrooms_glmnet_fit

coef(mushrooms_glmnet_fit, s = .2236)

mushrooms_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(mushrooms_cvfit)
log(mushrooms_cvfit$lambda.min)
log(mushrooms_cvfit$lambda.1se)

coef(mushrooms_cvfit, s = mushrooms_cvfit$lambda.1se)

#LASSO 모형을 이용한 예측
predict(mushrooms_cvfit,
        s = "lambda.1se",
        newx = x[1:5, ],
        type = "response")

#LASSO 모형 평가
y_obs <- ifelse(validation$class == "e", 1, 0)
yhat_glmnet <- predict(mushrooms_cvfit, s = "lambda.1se",
                       newx = xx[validation.idx, ],
                       type = "response")
yhat_glmnet <- yhat_glmnet[, 1]

binomial_deviance(y_obs, yhat_glmnet)

pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet,
                           measure = "tpr",
                          x.mesure = "fpr") 
plot(perf_rf,
     col = "red",
     main = "ROC Curve")

plot(perf_glmnet,
     add = T,
     col = "blue")

abline(0, 1)

legend("bottomright",
       inset = .1,
       legend = c("Random Forest", "LASSO"),
       col = c("red", "blue"),
       lty = 1, lwd = 2)

performance(pred_rf, "auc")@y.values[[1]]

# Test set을 이용한 분류 분석
pre1 <- predict(mushrooms_rf,
                newdata = test,
                type = "prob")[, 'e']
pre2 <- predict(mushrooms_cvfit,
                s = "lambda.1se",
                newx = xx[test.idx, ],
                type = "response")
head(pre1, 10)
head(pre2, 10)
