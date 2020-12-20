# install.packages("forecastML")
# install.packages("glmnet")
# install.packages("randomForest")
install.packages("DT")
library(forecastML)
library(dplyr)
library(DT)
library(ggplot2)
library(glmnet)
library(randomForest)


# 데이터 불러오기
data("data_seatbelts", package = "forecastML")
data <- data_seatbelts

date_frequency <- "1 month"  # Time step frequency.

# 날짜 sequence 생성
dates <- seq(as.Date("1969-01-01"), as.Date("1984-12-01"), by = date_frequency)


data$PetrolPrice <- round(data$PetrolPrice, 3) # 소수점 3자리까지 표현
data <- data[, c("DriversKilled", "kms", "PetrolPrice", "law")]  # 사용 할 변수 선택
library(DT)
datatable(head(data, 5)) # 데이터 예시  


# 마지막 12달 : test data
# 12달 제외 : train data
data_train <- data[1:(nrow(data) - 12), ]
data_test <- data[(nrow(data) - 12 + 1):nrow(data), ]


# 날짜별 Driverskilled 시각화
p <- ggplot(data, aes(x = dates, y = DriversKilled)) # plot
p <- p + geom_line() # 선으로 표현
p <- p + geom_vline(xintercept = dates[nrow(data_train)], color = "red", size = 1.1) # train test 분리선
p <- p + theme_bw() + xlab("Dataset index") # 배경 밑 x축 label 변경
p

outcome_col <- 1  # 반응변수 열의 번호

# 예측 기간 종류( 1-step-ahead, 3-step-ahead , ...)
horizons <- c(1, 3, 6, 12)

# 변수로 사용할 과거 시점들
lookback <- c(1:6, 9, 12, 15)

dynamic_features <- "law" # 현재 시점에만 사용되는 변수


# horizons와 lookback의 조합으로 data_list 생성
# horizons의 개수만큼 list 생성
data_list <- forecastML::create_lagged_df(data_train,
                                          outcome_col = outcome_col,
                                          type = "train",
                                          horizons = horizons,
                                          lookback = lookback,
                                          date = dates[1:nrow(data_train)],
                                          frequency = date_frequency,
                                          dynamic_features = dynamic_features
)

# horizons = 6 일 때의 예시
DT::datatable(head(data_list$horizon_6, 10), options = list(scrollX = TRUE))

plot(data_list) # 각 horizons 별 사용할 시점과 예측할 미래 시점 시각화


# cross vaidation을 위한 기간 분할
# window_length = 24  ==> 한 block 당 데이터 수 = 24 
windows <- forecastML::create_windows(lagged_df = data_list, window_length = 24, skip = 0,
                                      window_start = NULL, window_stop = NULL,
                                      include_partial_window = TRUE)

# 시각화
plot(windows, data_list, show_labels = TRUE)



### LASSO

model_function <- function(data) {
  
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  
  if (length(constant_features) > 1) {
    data <- data[, -c(constant_features + 1)]  # +1 because we're skipping over the outcome column.
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y, nfolds = 3)
  return(list("model" = model, "constant_features" = constant_features))
}


### Ridge

model_function_5 <- function(data){
  
  constant_features <- which(unlist(lapply(data[, -1], function(x) {!(length(unique(x)) > 1)})))
  if (length(constant_features) > 1){
    data <- data[, -c(constant_features + 1)] 
  }
  
  x <- data[, -(1), drop = FALSE]
  y <- data[, 1, drop = FALSE]
  x <- as.matrix(x, ncol = ncol(x))
  y <- as.matrix(y, ncol = ncol(y))
  
  model <- glmnet::cv.glmnet(x, y, nfolds = 3, alpha = 0)
  return(list("model" = model, "constant_features" = constant_features))
}


### Random Forest
model_function_2 <- function(data) {
  
  outcome_names <- names(data)[1]
  model_formula <- formula(paste0(outcome_names,  "~ ."))
  
  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200)
  return(model)
}


### Bagging
model_function_3 <- function(data){
  
  outcome_names <- names(data)[1]
  num_predictor <- (length(names(data)) - 1)
  model_formula <- formula(paste0(outcome_names, "~ ."))
  
  model <- randomForest::randomForest(formula = model_formula, data = data, ntree = 200, mtry = num_predictor)
  return(model)
}


# LASSO 모형 cross_validation을 이용하여 훈련
model_results <- forecastML::train_model(data_list, windows, model_name = "LASSO",
                                         model_function, use_future = FALSE)

# Ridge 모형 cross_validation을 이용하여 훈련
model_results_5 <- forecastML::train_model(data_list, windows, model_name = "Ridge",
                                           model_function_5, use_future = FALSE)


# RandomForest 모형 cross_validation을 이용하여 훈련
model_results_2 <- forecastML::train_model(data_list, windows, model_name = "RF", 
                                           model_function_2, use_future = FALSE)

# Bagging 모형 cross_validation을 이용하여 훈련
model_results_3 <- forecastML::train_model(data_list, windows, model_name = "Bagging",
                                           model_function_3, use_future = FALSE)



# LASSO predict
prediction_function <- function(model, data_features) {
  
  if (length(model$constant_features) > 1) {  # 'model' was passed as a list.
    data <- data[, -c(model$constant_features )]
  }
  
  x <- as.matrix(data_features, ncol = ncol(data_features))
  
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Ridge predict
prediction_function_5 <- function(model, data_features){
  
  if (length(model$constant_features) > 1){ 
    data <- data[, -c(model$constant_features_5)]
  }
  x <- as.matrix(data_features, ncol = ncol(data_features))
  data_pred <- data.frame("y_pred" = predict(model$model, x, s = "lambda.min"))
  return(data_pred)
}

# Random Forest predict
prediction_function_2 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# Bagging predict
prediction_function_3 <- function(model, data_features) {
  
  data_pred <- data.frame("y_pred" = predict(model, data_features))
  return(data_pred)
}

# 각 모델과 horizons별 예측 결과 
# windows 별 결과도 보여줌
data_results <- predict(model_results, model_results_2, model_results_3, model_results_5,
                        prediction_function = list(prediction_function, prediction_function_2, prediction_function_3, prediction_function_5), 
                        data = data_list)


# 예측값 정수로 만듦
data_results$DriversKilled_pred <- round(data_results$DriversKilled_pred, 0)

# 예측값 보기
DT::datatable(head(data_results, 30), options = list(scrollX = TRUE))
head(data_results, 30)

# horizons 별 결과 시각화 ( 예측값, 잔차, 안정성 )
plot(data_results, type = "prediction", horizons = c(1, 6, 12))
plot(data_results, type = "residual", horizons = c(1, 6, 12), windows = 5:7)
plot(data_results, type = "forecast_stability", windows = max(data_results$window_number))


