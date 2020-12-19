#### 5장 연습문제 ####


### 7번
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
a<-read.csv("exer57.csv", header=T)
fita<-lm(Y~F, data=a)
summary(fita)
anova(fita)
a$F1<-ifelse(a$F==2,1,0)
a$F2<-ifelse(a$F==3,1,0)
a$F3<-ifelse(a$F==4,1,0)
a$F4<-a$F1+a$F2+a$F3
fit4<-lm(Y~F4, data=a)
summary(fit4)
anova(fit4)
fit<-lm(Y~F1+F2+F3, data=a)
summary(fit)
anova(fit)

### 8번
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
b<-read.csv("exer59.csv", header=T)
fit<-lm(V~I+D+W+G*I+P+N, data=b)
summary(fit)
fitt<-lm(V~D+G*I+I*N, data=b)
summary(fitt)
b$D1<-ifelse(b$D==1,1,0)
b$D2<-ifelse(b$D==-1,1,0)
fitn<-lm(V~I+D1+D2+W+G*I+P+N, data=b)
summary(fitn)


### 추가
rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
c<-read.table("P130.txt", header=T)
c$E1<-ifelse(c$E==2,1,-1)
c$E2<-ifelse(c$E==3,1,-1)
fitas<-lm(S~E1+E2, data=c)
summary(fitas)



rm(list=ls())
setwd("C:/Users/ay190130/Desktop/data")
c<-read.table("P130.txt", header=T)
c$E1<-ifelse(c$E==2,2,-2)
c$E2<-ifelse(c$E==3,2,-2)
fitasdf<-lm(S~E1+E2, data=c)
summary(fitasdf)
