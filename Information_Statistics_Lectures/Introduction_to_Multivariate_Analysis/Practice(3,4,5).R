####3,4,5장 연습문제####


###1번###
# household{HSAUR2}에서 성별에 따라 가계 지출 패턴에 차이가 있는지?

##(A)##
# 평행좌표그림
install.packages("lattice")
install.packages("HSAUR2")
library(lattice)
library(HSAUR2)
data("household")
str(household)
parallelplot(~household[1:4] | gender, household)


##(B)##
# 상자그림
library(HSAUR2)
data("household")
str(household)
plot(housing ~ gender, data = household)
plot(food ~ gender, data = household)
plot(goods ~ gender, data = household)
plot(service ~ gender, data = household)


##(C)##
# 별그림
library(HSAUR2)
##par(mfrow=c(1,1))
##stars(subset(household, gender=='male')[1:4], draw.segments=TRUE, col.lines=NA, key.loc=c(12,7), main="male")
##stars(subset(household, gender=='female')[1:4], draw.segments=TRUE, col.lines=NA, main="female")
stars(household[1:4], draw.segments = FALSE, location=c(0,0), col.lines=household$gender, key.loc=c(0.0))
library(HSAUR2)
stars(household[1:4], location=c(0,0), col.lines=household$gender, key.loc=c(0,0))


##(D)##
# 버블차트
data("household")
op<-palette(rainbow(nrow(household), end=0.9))
symbols(household$housing, household$food, circles= household$goods/16, inches=FALSE, bg=1:nrow(household), fg="gray30", col=op)



###2번###
# heptathlon{HSAUR2}

##(A)##
# 시각화
library(HSAUR2)
str(heptathlon)
stars(heptathlon[1:7], draw.segments=TRUE, col.lines=NA, key.loc=c(18,8))


##(B)##
# 상관분석
install.packages("psych")
library(psych)
str(heptathlon)
pairs.panels(heptathlon)


##(C)##
# 단변량 이상치 식별
install.packages("mvoutlier")
library(mvoutlier)
library(HSAUR2)
data(heptathlon)
outliers<-aq.plot(heptathlon)
outliers


##(D)##
# 다변량 정규성 검토
install.packages("mvnormtest")
library(mvnormtest)
library(HSAUR2)
mshapiro.test(t(as.matrix(heptathlon)))



###3번###
# CHFLS{HSAUR2}

##(A)##
# 지역별로 여성과 배우자 소득의 자료를 시각화를 통해 비교
library(HSAUR2)
data("CHFLS")
plot(R_income ~ R_region, data=CHFLS)
plot(A_income ~ R_region, data=CHFLS)
par(mfrow=c(1,2))


##(B)##
# 지역별로 여성과 배우자 소득의 자료의 산표가 동일한지 검정
bartlett.test(R_income ~ R_region, data=CHFLS)
bartlett.test(A_income ~ R_region, data=CHFLS)

bartlett.test(log(R_income+1) ~ R_region, data=CHFLS)
bartlett.test(log(A_income+1) ~ R_region, data=CHFLS)


##(C)##
# 소득과 행복의 정도
library(HSAUR2)
data("CHFLS")
par(mfrow=c(1,2))
plot(R_income ~ R_happy, data=CHFLS)
plot(A_income ~ R_happy, data=CHFLS)
##추가적##
bartlett.test(R_income ~ R_happy, data=CHFLS)
bartlett.test(A_income ~ R_happy, data=CHFLS)

bartlett.test(log(R_income+1) ~ R_region, data=CHFLS)
bartlett.test(log(A_income+1) ~ R_region, data=CHFLS)




###4번###
# heptathlon{HSAUR2}
##적합하라는 뜻
##두 변량의 모수를 알아야 적합식을 만들 수 있으므로
##결국에는 공분산의 값을 찾으라는 뜻

##(A)##
# 두 변수간의 산점도, 이변량 정규분포 적합
plot(heptathlon$longjump, heptathlon$hurdles)
mu1<-mean(heptathlon$longjump)
mu2<-mean(heptathlon$hurdles)

s11<-var(heptathlon$longjump)
s12<-cov(heptathlon$longjump, heptathlon$hurdles)
s22<-var(heptathlon$hurdles)

rho12<- s12/ (sqrt(s11)*sqrt(s22))

x1<-seq(min(heptathlon$longjump), max(heptathlon$longjump), length(50))
x2<-seq(min(heptathlon$hurdles), max(heptathlon$hurdles), length(50))


gaussian_func <- function(x1,x2){
  
  term1<- 1/(2*pi*sqrt(s11*s22*(1-rho12^2)))
  term2<- -1/(2*(1-rho12^2))
  term3<- (x1-mu1)^2/s11
  term4<- (x2-mu2)^2/s22
  term5<- 2*rho12*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3+term4-term5))
}
library(mvtnorm)
f<- function(x,y) { dmvnorm(cbind(x,y), mean=c(mu1, mu2),
                            sigma=matrix(c(s11,s12,s12,s22), ncol=2))}
z_score <- outer(x1, x2, gaussian_func)
head(z_score)


##(B)##
# 적합된 이변량 정규분포를 산점도 위에 추가
contour(head(z_score))


##(C)##
# 이변량 상자그림을 통해 이상치에 속하는 선수를 판별
install.packages("asbio")
install.packages("MVA")
library(asbio)
library(MVA)
bvbox(heptathlon[, c("longjump","hurdles")])
subset(heptathlon, longjump<5.7 | longjump>7 )


##(D)##
# 적합된 이변량 정규분포의 밀도함수
persp(head(z_score))
