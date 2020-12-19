#### 6장, 7장 연습문제 ####



### 6장

## 3번
#(a)
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
a<-read.csv("chap6-1.csv", header=T)
attach(a)
plot(R~P)
fit<-lm(R~log(P,100))
summary(fit)
anova(fit)
plot(R/P~1/P)
fitt<-lm(R/P~1/P)
summary(fitt)
anova(fitt)

plot(log(R)~P+I(P^2))
fity<-lm(log(R)~P+I(P^2))
summary(fity)
anova(fity)

#(b)
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
a<-read.csv("chap6-1.csv", header=T)
library(mvoutlier)
aa<-a[,2:3]
outliers<-aq.plot(aa)
outliers

#(c)
aaa<-aaa[9:39,]
attach(aaa)
fita<-lm(R~P)
summary(fita)
anova(fita)



## 4번

rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
b<-read.table("chap6-2.txt", header=T)
attach(b)
fit<-lm(W~V+T)
fit2<-lm(W~T+V+I(V^0.5))
fitV<-lm(W~V)
summary(fitV)
fitT<-lm(W~T)
summary(fitT)
summary(fit2)
anova(fit2)
plot(V,fit2$residuals)
plot(T,fit2$residuals)
fitf<-lm(W~T*I(V^0.5)+I(V^0.5)+T+V*T+V)
summary(fitf)
0.0817*(3.71*(V^0.5)+5.81-0.25*V)*(T-91.4)+91.4



# 7번
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
c<-read.table("chap6-3.txt", header=T)
attach(c)
plot(Barrels~Year)
plot(log(Barrels)~Year)
fitc<-lm(log(Barrels)~Year)
summary(fitc)
anova(fitc)
library(car)
influenceIndexPlot(fitc)




### 7장

## 1번
setwd("C:/Users/ay190130/Desktop/data")
a<-read.table("ch5-12.txt", header=T)
fit1<-lm(Y~X1+X2+X3, data=a)
summary(fit1)
anova(fit1)
plot(predict(fit1),rstandard(fit1))
plot(a$Region,rstandard(fit1))
(wh1<-which(cooks.distance(fit1)==max(cooks.distance(fit1))))
name_infulence1<-a[30,"State"]
predict(fit1)[30]
rstandard(fit1)[30]
text(80,-2,name_infulence1)
par(mfrow=c(2,2))
plot(a$Region,rstandard(fit1))
plot(a$X1,rstandard(fit1))
plot(a$X2,rstandard(fit1))
plot(a$X3,rstandard(fit1))

a_1<-a[-30,]
fit11<-lm(Y~X1+X2+X3, data=a_1)
plot(predict(fit11), rstandard(fit11))
plot(a_1$Region, rstandard(fit11))
(SSE_1<-sum(residuals(fit11)^2))
sigma<-SSE_1/(nrow(a_1))
ss<-tapply(residuals(fit11)^2,a_1$Region,sum)
nn<-tapply(rep(1,nrow(a_1)), a_1$Region,sum)
sigma_region<-ss/(nn-1)
influ<-sqrt(sigma_region/sigma)
round(influ, digits=3)

a_1$wgt<-NULL
for(i in 1:49){
  if (a_1$Region[i] == 1) {
    a_1$wgt[i] <- 1/(1.156*1.156)
  }else if (a_1$Region[i] == 2){
    a_1$wgt[i] <- 1/(0.774*0.774)
  }else if (a_1$Region[i] == 3){
    a_1$wgt[i] <- 1/(0.762*0.762)
  }else if (a_1$Region[i] == 4){
    a_1$wgt[i] <- 1/(1.402*1.402)
  }
}
fit12<-lm(Y~X1+X2+X3, weights= wgt, data=a_1)
summary(fit12)
anova(fit12)
plot(predict(fit12),rstandard(fit12))
plot(a_1$Region, rstandard(fit12))



## 2번
b<-read.table("ch5-13.txt", header=T)
fit2<-lm(Y~X1+X2+X3, data=b)
summary(fit2)
anova(fit2)
plot(predict(fit2),rstandard(fit2))
plot(b$Region,rstandard(fit2))
(wh2<-which(cooks.distance(fit2)==max(cooks.distance(fit2))))
name_infulence2<-b[49,"State"]
predict(fit2)[49]
rstandard(fit2)[49]
text(450,3,name_infulence2)
plot(b$Region,rstandard(fit2))
plot(b$X1,rstandard(fit2))
plot(b$X2,rstandard(fit2))
plot(b$X3,rstandard(fit2))

b_1<-b[-49,]
fit21<-lm(Y~X1+X2+X3, data=b_1)
plot(predict(fit21), rstandard(fit21))
plot(b_1$Region, rstandard(fit21))
(SSE_1<-sum(residuals(fit21)^2))
sigma<-SSE_1/(nrow(b_1))
ss<-tapply(residuals(fit21)^2,b_1$Region,sum)
nn<-tapply(rep(1,nrow(b_1)), b_1$Region,sum)
sigma_region<-ss/(nn-1)
influ<-sqrt(sigma_region/sigma)
round(influ, digits=3)

b_1$wgt<-NULL
for(i in 1:49){
  if (b_1$Region[i] == 1) {
    b_1$wgt[i] <- 1/(1.239*1.239)
  }else if (b_1$Region[i] == 2){
    b_1$wgt[i] <- 1/(1.155*1.155)
  }else if (b_1$Region[i] == 3){
    b_1$wgt[i] <- 1/(0.631*0.631)
  }else if (b_1$Region[i] == 4){
    b_1$wgt[i] <- 1/(1.208*1.208)
  }
}
fit22<-lm(Y~X1+X2+X3, weights= wgt, data=b_1)
summary(fit22)
anova(fit22)
plot(predict(fit22),rstandard(fit22))
plot(b_1$Region, rstandard(fit22))




## 3번
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
a<-read.table("ch5-12.txt", header=T)
a<-a[,-1]
b<-read.table("ch5-13.txt", header=T)
b<-b[,-1]
c<-read.table("ch7-2.txt", header=T)
c<-c[,-1]
fit3<-lm(Y~X1+X2+X2, data=c)
fit3$residuals
round(influence(fit3)$hat,3)
round(rstandard(fit3),3)
round(cooks.distance(fit3),3)
round(dffits(fit3),3)
library(olsrr)
par(mfrow=c(1,1))
plot(influence(fit3)$hat, ylab="hat", main="Leverage")
lines(influence(fit3)$hat, type="h")
plot(rstandard(fit3),main="rstandard")
lines(rstandard(fit3), type="h")
abline(0,0)
ols_plot_cooksd_bar(fit3)
ols_plot_dffits(fit3)
