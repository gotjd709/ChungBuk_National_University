# 과제 8주차 #

library(mlbench)
library(h2o)
library(RCurl)
library(data.table)
library(bit64)

# BreastCancer 데이터 불러오고 결측치 제거 #
data("BreastCancer")
BreastCancer = BreastCancer[which(complete.cases(BreastCancer)==TRUE),]


# H20패키지 사용준비 #
localH2O = h2o.init(ip="localhost", port = 54321, 
                    startH2O = TRUE, nthreads=-1)

#  Connection successful!
# R is connected to the H2O cluster: 
# H2O cluster uptime:         10 hours 36 minutes 
# H2O cluster timezone:       Asia/Seoul 
# H2O data parsing timezone:  UTC 
# H2O cluster version:        3.30.0.1 
# H2O cluster version age:    1 month and 10 days  
# H2O cluster name:           H2O_started_from_R_ay190130_gbt183 
# H2O cluster total nodes:    1 
# H2O cluster total memory:   1.62 GB 
# H2O cluster total cores:    4 
# H2O cluster allowed cores:  4 
# H2O cluster healthy:        TRUE 
# H2O Connection ip:          localhost 
# H2O Connection port:        54321 
# H2O Connection proxy:       NA 
# H2O Internal Security:      FALSE 
# H2O API Extensions:         Amazon S3, Algos, AutoML, Core V3, TargetEncoder, Core V4 
# R Version:                  R version 4.0.0 (2020-04-24) 


# 변수들의 class변환 일괄적으로 하기 #
var.names <- names(BreastCancer)
for( f in var.names) {
  if (class(BreastCancer[[f]]) != "factor") 
   BreastCancer[[f]] <- factor(BreastCancer[[f]], ordered = FALSE)
}
BreastCancer$Id <- as.numeric(BreastCancer$Id)
str(BreastCancer)

# 'data.frame':	683 obs. of  11 variables:
# $ Id             : num  1 4 5 6 8 9 10 11 15 15 ...
# $ Cl.thickness   : Factor w/ 10 levels "1","2","3","4",..: 5 5 3 6 4 8 1 2 2 4 ...
# $ Cell.size      : Factor w/ 10 levels "1","2","3","4",..: 1 4 1 8 1 10 1 1 1 2 ...
# $ Cell.shape     : Factor w/ 10 levels "1","2","3","4",..: 1 4 1 8 1 10 1 2 1 1 ...
# $ Marg.adhesion  : Factor w/ 10 levels "1","2","3","4",..: 1 5 1 1 3 8 1 1 1 1 ...
# $ Epith.c.size   : Factor w/ 10 levels "1","2","3","4",..: 2 7 2 3 2 7 2 2 2 2 ...
# $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
# $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
# $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
# $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
# $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...


# 변수변환 일일히 하는 방법 #
#BreastCancer$Id <- as.numeric(BreastCancer$Id)
#BreastCancer$Cl.thickness <- factor(BreastCancer$Cl.thickness, ordered = FALSE)
#BreastCancer$Cell.size <- factor(BreastCancer$Cell.size, ordered = FALSE)
#BreastCancer$Cell.shape <- factor(BreastCancer$Cell.shape, ordered = FALSE)
#BreastCancer$Marg.adhesion <- factor(BreastCancer$Marg.adhesion, ordered = FALSE)
#BreastCancer$Epith.c.size <- factor(BreastCancer$Epith.c.size, ordered = FALSE)


# Hold-out기법을 이용하여 train, test 7:3으로 나누기 #
train.num <- as.numeric(sample(nrow(BreastCancer),round(nrow(BreastCancer)*0.7)))
train <- as.h2o(BreastCancer[-train.num,], destination_frame = "train")
# |====================================================================================================================| 100%
test <- as.h2o(BreastCancer[-train.num,], destination_frame = "test")
# |====================================================================================================================| 100%


# 종속변수와 독립변수 구분 #
y = names(train)[11]
x = names(train)[1:10]

train[,y] = as.factor(train[,y])
test[,y] = as.factor(train[,y])


# modeling 하기 #
model = h2o.deeplearning(x=x, 
                         y=y, 
                         training_frame=train, 
                         validation_frame=test, 
                         distribution = "multinomial",
                         activation = "RectifierWithDropout",
                         hidden = c(10,10,10,10),
                         input_dropout_ratio = 0.2,
                         l1 = 1e-5,
                         epochs = 50)
# |====================================================================================================================| 100%


# 결과 도출 #
print(model)

# Model Details:
# ==============
#   H2OBinomialModel: deeplearning
# Model ID:  DeepLearning_model_R_1589338568222_22 
# Status of Neuron Layers: predicting Class, 2-class classification, multinomial distribution, CrossEntropy loss, 1,332 weights/biases, 23.4 KB, 10,250 training samples, mini-batch size 1
# layer units             type dropout       l1       l2 mean_rate rate_rms momentum mean_weight weight_rms mean_bias
# 1     1    97            Input 20.00 %       NA       NA        NA       NA       NA          NA         NA        NA
# 2     2    10 RectifierDropout 50.00 % 0.000010 0.000000  0.096943 0.286153 0.000000    0.009673   0.142027  0.514883
# 3     3    10 RectifierDropout 50.00 % 0.000010 0.000000  0.003250 0.001780 0.000000   -0.000707   0.339831  0.869615
# 4     4    10 RectifierDropout 50.00 % 0.000010 0.000000  0.002079 0.001279 0.000000    0.000138   0.341891  0.932087
# 5     5    10 RectifierDropout 50.00 % 0.000010 0.000000  0.011428 0.024070 0.000000   -0.013906   0.380589  0.839412
# 6     6     2          Softmax      NA 0.000010 0.000000  0.001507 0.000754 0.000000    0.148293   1.235458 -0.003044
# bias_rms
# 1       NA
# 2 0.260932
# 3 0.148669
# 4 0.104090
# 5 0.141697
# 6 0.061762
# H2OBinomialMetrics: deeplearning
# ** Reported on training data. **
#   ** Metrics reported on full training frame **
#   MSE:  0.0485942
# RMSE:  0.2204409
# LogLoss:  0.2115898
# Mean Per-Class Error:  0.02560112
# AUC:  0.9948476
# AUCPR:  0.9889753
# Gini:  0.9896951
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   benign malignant    Error    Rate
# benign       132         5 0.036496  =5/137
# malignant      1        67 0.014706   =1/68
# Totals       133        72 0.029268  =6/205
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold      value idx
# 1                       max f1  0.302021   0.957143  71
# 2                       max f2  0.233992   0.974212  76
# 3                 max f0point5  0.589403   0.957792  59
# 4                  max accuracy  0.425512   0.970732  69
# 5                max precision  0.825687   1.000000   0
# 6                   max recall  0.233992   1.000000  76
# 7              max specificity  0.825687   1.000000   0
# 8             max absolute_mcc  0.302021   0.935829  71
# 9   max min_per_class_accuracy  0.425512   0.970588  69
# 10 max mean_per_class_accuracy  0.302021   0.974399  71
# 11                     max tns  0.825687 137.000000   0
# 12                     max fns  0.825687  67.000000   0
# 13                     max fps  0.066047 137.000000 203
# 14                     max tps  0.233992  68.000000  76
# 15                     max tnr  0.825687   1.000000   0
# 16                     max fnr  0.825687   0.985294   0
# 17                     max fpr  0.066047   1.000000 203
# 18                     max tpr  0.233992   1.000000  76
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
# H2OBinomialMetrics: deeplearning
# ** Reported on validation data. **
#   ** Metrics reported on full validation frame **
#   MSE:  0.0485942
# RMSE:  0.2204409
# LogLoss:  0.2115898
# Mean Per-Class Error:  0.02560112
# AUC:  0.9948476
# AUCPR:  0.9889753
# Gini:  0.9896951
# Confusion Matrix (vertical: actual; across: predicted) for F1-optimal threshold:
#   benign malignant    Error    Rate
# benign       132         5 0.036496  =5/137
# malignant      1        67 0.014706   =1/68
# Totals       133        72 0.029268  =6/205
# Maximum Metrics: Maximum metrics at their respective thresholds
# metric threshold      value idx
# 1                       max f1  0.302021   0.957143  71
# 2                       max f2  0.233992   0.974212  76
# 3                 max f0point5  0.589403   0.957792  59
# 4                 max accuracy  0.425512   0.970732  69
# 5                max precision  0.825687   1.000000   0
# 6                   max recall  0.233992   1.000000  76
# 7              max specificity  0.825687   1.000000   0
# 8             max absolute_mcc  0.302021   0.935829  71
# 9   max min_per_class_accuracy  0.425512   0.970588  69
# 10 max mean_per_class_accuracy  0.302021   0.974399  71
# 11                     max tns  0.825687 137.000000   0
# 12                     max fns  0.825687  67.000000   0
# 13                     max fps  0.066047 137.000000 203
# 14                     max tps  0.233992  68.000000  76
# 15                     max tnr  0.825687   1.000000   0
# 16                     max fnr  0.825687   0.985294   0
# 17                     max fpr  0.066047   1.000000 203
# 18                     max tpr  0.233992   1.000000  76
# Gains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`
