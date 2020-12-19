#### 2,3 장 연습문제 ####



### 2장 


## 10번

setwd("C:/Users/ay190130/Desktop/data")
practice<-read.table("practice.txt", header=T)
names(practice)<-c("x","y")
practice
summary(practice)
apply(practice, 2, mean)
apply(practice, 2, sd)
attach(practice)
dy<-y-mean(y)
dx<-x-mean(x)
dy2<-dy^2
dx2<-dx^2
dxy=dx*dy
cbind(x,y,dx,dy,dx2,dy2)
sum((x-mean(x))*(y-mean(y)))/
  sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))
cov(x,y)
cov(dx,dy)
cor(x,y)
cor(dx,dy)
cor(dx2,dy2)

fit<-lm(y~x, data=practice)
summary(fit)



### 3장

## 3번
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
prc3<-read.table("prc3.txt", header=T)
attach(prc3)
names(prc3)<-c("y","x1","x2")
attach(prc3)
fitx1<-lm(x1~x2)
summary(fitx1)
fitx2<-lm(x2~x1)
summary(fitx2)
anova(fitx2)
x1x2<-sum((x1-mean(x1))*(x2-mean(x2)))
x1x2
X<-cbind(rep(1,nrow(prc3)),x1,x2)
beta<-solve(t(X)%*%X)%*%t(X)%*%y
y_hat<-X%*%beta
SSE<-sum((y-y_hat)^2)
SST<-sum((y-mean(y))^2)
1-SSE/SST
SST
#MSE*solve(t(X)%*%X)
#85.2971451^0.5

#a<-(sum((y-y_hat)^2)/20)^0.5
#b<-sum((x2-mean(x2))^2)
#a*((1/22+(mean(x2))^2/b)^0.5)

fit2<-lm(y~x2)
summary(fit2)
sum((x1-mean(x1))*(y-mean(y)))
(sum((x1-mean(x1))^2))^0.5
sum((x2-mean(x2))*(y-mean(y)))
(sum((x2-mean(x2))^2))^0.5
(sum((y-mean(y))^2))^0.5

(cor(x1,y))^2
(cor(x2,y))^2
fit1<-lm(y~x1)
summary(fit1)
fit1<-lm(y~x1)
summary(fit1)
X<-cbind(rep(1,nrow(prc3)),x1,x2)
beta<-solve(t(X)%*%X)%*%t(X)%*%y
y_hat<-X%*%beta


## 10번 
# (a)
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
a<-matrix(c(1,2,3,4), nrow=2, byrow=T)
b<-c(1,2)
a%*%b
solve(a)
data<-read.table("P060.txt", header=T)
attach(data)
fit<-lm(Y~X1+X2)
fit$fitted
summary(fit)
anova(fit)
X<-cbind(rep(1, nrow(data)), X1, X2)
my_beta<-solve(t(X)%*%X)%*%t(X)%*%Y
y_hat<-X%*%my_beta
SSE<-sum((Y-y_hat)^2)
SST<-sum((Y-mean(Y))^2)
SSR<-SST-SSE
MSE<-SSE/(nrow(data)-2-1)
cov_beta<-solve(t(X)%*%X)*MSE
diag(cov_beta)^0.5

#(b)
setwd("C:/Users/ay190130/Desktop/데이터")
p060<-read.table("p060.txt", header=T)
attach(p060)
names(p060)<-c("y","x1","x2","x3","x4","x5","x6")
attach(p060)
w<-x1+x3
fitR<-lm(y~w)
summary(fitR)
fitF<-lm(y~x1+x3)
summary(fitF)






