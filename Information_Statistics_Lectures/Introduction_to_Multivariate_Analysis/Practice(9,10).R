#### 9,10장 연습문제 ####


### 조교님 설명 부분
log.ir<-log(iris[,1:4])
ir.species<-iris[,5]
ir.pca<-prcomp(log.ir, center=TRUE, scale=TRUE)
print(ir.pca)
summary(ir.pca)
names(ir.pca)

## 2번 문제 풀때 쓸것
sum(ir.pca$sdev[1:2]^2)/sum(ir.pca$sdev^2)

## 공분산행렬이 주어졌을 때 고유값분해로 구한것과
## 원자료가 주어졌을 때 prcomp를 사용
cor.ir<-cor(log.ir)
eig.ir<-eigen(cor.ir)
ir.pca$sdev^2
eig.ir$values



### 9장 

## 1번
#(a)
setwd("C:/Users/ay190130/Desktop/data")
exer9101<-read.csv("exer9101.csv", header=T)
exer9101
#평균벡터
colMeans(exer9101[,2:6])
#공분산
cov(exer9101[,2:6])
#상관행렬
cor(exer9101[,2:6])
#(b) 처음변수 4개로 공분산행렬을 이용한 주성분 분석
exer.pca<-prcomp(exer9101[,2:5], center = TRUE, scale = FALSE)
print(exer.pca)
summary(exer.pca)
#(c) 처음변수 4개로 상관행렬을 이용한 주성분 분석
exer.pca<-prcomp(exer9101[,2:5], center = TRUE, scale = TRUE)
print(exer.pca)
summary(exer.pca)

## 2번
s<-matrix(c(302.3, 125.8, 100.4, 105.1, 116.1,
            125.8, 170.9, 84.2,  93.6,  97.9,
            100.4, 84.2,  111.6, 110.8, 120.5,
            105.1, 93.6,  110.8, 217.9, 153.8,
            116.1, 97.9,  120.5, 153.8, 294.4),5,5, byrow=TRUE)
diag(s)
eigen(s)
print(sum(eigen(s)$values[1:1])/sum(eigen(s)$values[1:5]))
print(sum(eigen(s)$values[1:2])/sum(eigen(s)$values[1:5]))
print(sum(eigen(s)$values[1:3])/sum(eigen(s)$values[1:5]))
print(sum(eigen(s)$values[1:4])/sum(eigen(s)$values[1:5]))
print(sum(eigen(s)$values[1:5])/sum(eigen(s)$values[1:5]))
#공분산행렬의 고유값이 상관행렬의 대각원소의 제곱과 같다.



### 10장
 
## 1번
s<-matrix(c(302.3, 125.8, 100.4, 105.1, 116.1,
            125.8, 170.9,  84.2,  93.6,  97.9,
            100.4,  84.2, 111.6, 110.8, 120.5,
            105.1,  93.6, 110.8, 217.9, 153.8,
            116.1,  97.9, 120.5, 153.8, 294.4),5,5, byrow=TRUE)
cov2cor(s)
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)
subject<-fa(r=cov2cor(s), nfactors=2, rotate="oblimin", fm="pa")
subject$loadings
load<-subject$loadings[,1:2]
plot(load, type="n")
text(load,cex=.7)

##2번
s<-matrix(c( 1.00, 0.24,  0.04, 0.31, 0.09, 0.23, -0.11, 0.27,  0.55, 0.35, 0.28, 0.34, 0.37,  0.47, 0.59,
             0.24, 1.00,  0.12, 0.38, 0.43, 0.37,  0.35, 0.48,  0.14, 0.34, 0.55, 0.51, 0.51,  0.28, 0.38,
             0.04, 0.12,  1.00, 0.00, 0.00, 0.08, -0.03, 0.05,  0.27, 0.09, 0.04, 0.20, 0.29, -0.32, 0.14,
             0.31, 0.38,  0.00, 1.00, 0.30, 0.48,  0.65, 0.35,  0.14, 0.39, 0.35, 0.50, 0.61,  0.69, 0.33,
             0.09, 0.43,  0.00, 0.30, 1.00, 0.81,  0.41, 0.82,  0.02, 0.70, 0.84, 0.72, 0.67,  0.48, 0.25,
             0.23, 0.37,  0.08, 0.48, 0.81, 1.00,  0.36, 0.83,  0.15, 0.70, 0.76, 0.88, 0.78,  0.53, 0.42,
            -0.11, 0.35, -0.03, 0.65, 0.41, 0.36,  1.00, 0.23, -0.16, 0.28, 0.21, 0.39, 0.42,  0.45, 0.00,
             0.27, 0.48,  0.05, 0.35, 0.82, 0.83,  0.23, 1.00,  0.23, 0.81, 0.86, 0.77, 0.73,  0.55, 0.55,
             0.55, 0.14,  0.27, 0.14, 0.02, 0.15, -0.16, 0.23,  1.00, 0.34, 0.20, 0.30, 0.35,  0.21, 0.69,
             0.35, 0.34,  0.09, 0.39, 0.70, 0.70,  0.28, 0.81,  0.34, 1.00, 0.78, 0.71, 0.79,  0.61, 0.62,
             0.28, 0.55,  0.04, 0.35, 0.84, 0.76,  0.21, 0.86,  0.20, 0.78, 1.00, 0.78, 0.77,  0.55, 0.43,
             0.34, 0.51,  0.20, 0.50, 0.72, 0.88,  0.39, 0.77,  0.30, 0.71, 0.78, 1.00, 0.88,  0.55, 0.53,
             0.37, 0.51,  0.29, 0.61, 0.67, 0.78,  0.42, 0.73,  0.35, 0.79, 0.77, 0.88, 1.00,  0.54, 0.57,
             0.47, 0.28, -0.32, 0.69, 0.48, 0.53,  0.45, 0.55,  0.21, 0.61, 0.55, 0.55, 0.54,  1.00, 0.40,
             0.59, 0.38,  0.14, 0.33, 0.25, 0.42,  0.00, 0.55,  0.69, 0.62, 0.43, 0.53, 0.57,  0.40, 1.00), 15, 15, byrow=TRUE)
install.packages("nFactors")
library(nFactors)
covs<-cov2cor(s)
ev<-eigen(covs)
ap<-parallel(subject=48, var=15)
nS<-nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
subject<-fa(r=s, nfactors=4, rotate="oblimin", fm="pa")
subject
#PA1: 5,6,8,10,11,12,13 업무에 필요한 역량
#PA2: 1,9,15 채용시 필요한 능력
#PA3: 4,7 일을 맡길 때 신뢰도
#PA4: 3,14 성장가능성

