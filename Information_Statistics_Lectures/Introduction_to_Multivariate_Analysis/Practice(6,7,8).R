#### 6,7,8장 연습문제 ####



### 6장


## 1번 
# 모평균 검정
install.packages("ICSNP")
library(ICSNP)
setwd("C:/Users/ay190130/Desktop/data")
exer68<-read.csv("exer68.csv", header=T)
exer68
asdf<-exer68[,c(1,3)]
asdf
HotellingsT2(asdf, mu=c(182,182), test="chi")

## 2번 
# iris 상자그림
#상자그림
#which(iris$Species=="setosa")
#iris[-index,]
#subset(iris, Species=='verxicolor'
#            Species=='virginica')
data("iris")
X<-subset(iris, Species=='versicolor' | Species=='virginica')
#install.packages("gdata")
library(gdata)
par(mfrow=c(1,2))
boxplot(X$Sepal.Length~X$Species)
boxplot(X$Petal.Length~X$Species)
par(mfrow=c(1,1))
#Hotelling T2
library(ICSNP)
a<-subset(iris, Species=='versicolor')
b<-subset(iris, Species=='virginica')
X1<-a[,c(1,3)]
X2<-b[,c(1,3)]
HotellingsT2(X1,X2)

## 3번 
#모평균 벡터가 동일한지 검정
#(A) 독립표본 간주
library(ICSNP)
setwd("C:/Users/ay190130/Desktop/data")
exer68<-read.csv("exer68.csv", header=T)
a<-exer68[,c(1,2)]
b<-exer68[,c(3,4)]
HotellingsT2(a,b)
#(B) 대응표본 간주
setwd("C:/Users/ay190130/Desktop/data")
exer68<-read.csv("exer68.csv", header=T)
c<- a-b
HotellingsT2(c)
#(C) 바람직한 검정?
#독립표본으로 간주하는 것이 더 좋다.
#그 이유는 독립표본으로 할 때 자유도 값이 더욱 커져
#P-value값이 높아지기 때문이다

## 4번
n1<-15
n2<-10
x1<-c(2.4, 9.4)
x2<-c(4.5, 6.2)
s1<-matrix(c(4.0, 1.0, 1.0, 6.3),2,2, byrow=TRUE)
s2<-matrix(c(5.2, 2.1, 2.1, 4.0),2,2, byrow=TRUE)
t2<-t(x1-x2)%*%solve(s1/n1+s2/n2)%*%(x1-x2)
#t2>chi(2,0.05)=5.99



### 7장


## 1번 iris
#(a) 상자그림
data(iris)
par(mfrow=c(1,2))
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Petal.Length~iris$Species)
par(mfrow=c(1,1))
#(b-1) 다변량 정규성검정
library(mvnormtest)
mshapiro.test(t(as.matrix(iris[,c(1:4)])))
#(b-2)
Y<-as.matrix(iris[,c(1,3)])
spec<-iris[,c(5)]
fit<-manova(Y~spec)
summary.aov(fit)

## 2번 
#(a) 지역에 따라 모평균벡터가 동일한지에 대한 검정
setwd("C:/Users/ay190130/Desktop/data")
chap7<-read.csv("chap7.csv", header=T)
Y<-as.matrix(chap7[,c(1:3)])
X<-chap7[,c(4)]
fit<-manova(Y~X)
summary.aov(fit)



### 8장

## 1번
#setwd("C:/Users/ay190130/Desktop/data")
#chap8<-read.csv("chap8.csv", header=T)
trt<-c(rep("placebo",9),rep("small",8),rep("large",13))
participate<-c(3,2,5,2,2,2,7,2,4,7,5,3,4,4,7,5,4,9,2,6,3,4,4,4,6,4,6,2,8,5)
partner<-c(4,1,5,1,2,2,7,4,5,5,3,1,2,2,6,4,2,1,3,5,4,3,3,2,0,1,3,0,1,0)
boxplot(participate~trt, xlab="treatment", ylab="participate")
boxplot(partner~trt, xlab="treatment", ylab="partner")
summary(aov(participate~trt))
summary(aov(partner~trt))
#반응변수의 평균과 공변량의평균 차이가 모두 없다.
#공변량의 효과를 제거한 뒤 반응변수의 평균에 차이가 나는지 알아보기 위해
#ANCOVA 분석을 실시한다.
lm.1<-lm(participate~partner+trt)
lm.2<-lm(participate~partner*trt)
anova(lm.1)
anova(lm.1,lm.2)
