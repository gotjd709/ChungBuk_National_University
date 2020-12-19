#### 13,14장 연습문제 ####



### 13장

##1번
#(A)
require(MASS)
dat<-subset(iris,
            Species=="setosa" |
              Species=="versicolor")[,c("Sepal.Length", "Sepal.Width", "Species")]
str(dat)
dat$Species
dat$Species<-dat$Species[drop=T]
#없는 level 삭제하기
dat$Species

ilda<-lda(formula = Species ~., data= dat)
#. 모든 데이터 다 불러오는 기능
(mu1<-ilda$means[1,])
(mu2<-ilda$means[2,])
#()출력기능도 포함되있음

fac<-levels(dat$Species)
s1<-var(dat[dat$Species==fac[1],-3])
s2<-var(dat[dat$Species==fac[2],-3])
(sp<-((50-1)*s1+(50-1)*s2)/(50+50-2))
(mu1-mu2)%*%solve(sp)
-0.5%*%(mu1-mu2)%*%solve(sp)%*%(mu1+mu2)

-11.436*dat$Sepal.Length+14.143*dat$Sepal.Width+18.739
dat$Species

par(mfrow=c(1,1))
ld1<-dat[dat$Species==fac[1],-3]
ld2<-dat[dat$Species==fac[2],-3]
contour(x = ld1$Sepal.Length, y = ld1$Sepal.Width)
plot(ld1$Sepal.Length, ld1$Sepal.Width, type="p", col="green", xlim=c(3.5,7.5), ylim=c(1.5,4.5))
lines(ld2$Sepal.Length, ld2$Sepal.Width, type="p", col="red")
legend("topleft", legend=c("setosa","vesicolor"), col=c("green","red"), lty=1:1,cex=1.0)
abline(a=-18.739/14.143,b=11.436/14.143)
persp(ld1$Sepal.Length, ld1$Sepal.Width)
install.packages("ggplot2")
library(ggplot2)
ggplot(dat, aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species)) + geom_point() + geom_abline(intercept = -18.739/14.143, slope= 11.436/14.143) + geom_density_2d()

ilda<-lda(formula = Species ~., data= dat, prior=c(1,1)/2)
?plot(ilda, dimen=2)

lines
legend(4.2, 4.5, legend=c("setosa","vesicolor"), col=c("green","red"), lty=1:2,cex=1.0)

#어떤 종인지 판별해서 표시해야함
#points()

x1<-c(191,185,200,173,171,160,188,186,174,163,190,174,201,190,182,184,177,178,
      186,211,201,242,184,211,217,223,208,199,211,218,203,192,195,211,187,192)
x2<-c(131,134,137,127,118,118,134,129,131,115,143,131,130,133,130,131,127,126,
      107,122,114,131,108,118,122,127,125,124,129,126,122,116,123,122,123,109)
species<-factor(rep(c("A","B"), c(18,18)))

require(MASS)
exer2<-data.frame(x1,x2,species)
exer22<-lda(formula = species ~., data= exer2)
(mu1<-exer22$means[1,])
(mu2<-exer22$means[2,])
s1<-var(exer2[exer2$species=="A",-3])
s2<-var(exer2[exer2$species=="B",-3])
(sp<-((36-1)*s1+(36-1)*s2)/(36+36-2))
(mu1-mu2)%*%solve(sp)
-0.5%*%(mu1-mu2)%*%solve(sp)%*%(mu1+mu2)

ggplot(exer2, aes(x = x1, y = x2, color = species, shape = species)) + geom_point() + geom_abline(intercept = 14.890/0.835, slope= 0.462/0.835) + geom_density_2d() + geom_point (aes(x=190, y=125, colour="new point"))
-0.462*190+0.835*125-14.89



### 14장

##1번

#(a) 최단연결법
mhv<-c(0,1.61,0,1.97,2.02,0,1.97,2.51,2.88,0,1.40,1.70,1.35,2.21,0,
       2.45,3.49,3.34,3.83,3.19,0,
       2.83,3.94,3.64,2.89,3.01,3.00,0,
       9.58,9.59,10.05,8.78,9.30,9.74,9.23,0,
       7.79,7.82,8.43,7.08,7.76,7.86,7.76,2.64,0,
       7.86,7.92,8.36,7.44,7.79,7.90,8.26,3.38,2.56,0)
ltm<-matrix(rep(0, 10*10), nrow=10)

k<-1
for(i in 1:10) {
  for(j in 1:i) {
    ltm[i,j]<-mhv[k]
    k<-k+1
  }
}
mhd<-as.dist(ltm)
fit1<-hclust(mhd, method="single")
plot(fit1)

#(b) 최장연결법
fit2<-hclust(mhd, method="complete")
plot(fit2)


##2번

#(a)
library(datasets)
str(attitude)
at34<-attitude[,3:4]

wssplot<-function(data, nc=15, seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

df<-scale(at34[-1])
wssplot(df)
#군집 수 3개 임을 결정
install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc<-NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$best.n[1,])
barplot(table(nc$Best.n[1,]))
set.seed(1234)
fit.km<-kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
plot(df, col=fit.km$cluster)
points(fit.km$centers, col=1:3, pch=8, cex=1.5)

#(b)
dfw<-scale(attitude[-1])
wssplot(dfw)
#군집 수 2개 임을 결정
set.seed(1234)
ncw<-NbClust(dfw, min.nc=2, max.nc=15, method="kmeans")
table(ncw$best.n[1,])
barplot(table(ncw$Best.n[1,]))
set.seed(1234)
fit.kmw<-kmeans(dfw, 2, nstart=25)
fit.kmw$size
fit.kmw$centers
plot(dfw, col=fit.kmw$cluster)
points(fit.kmw$centers, col=1:2, pch=8, cex=1.5)

library(datasets)
str(USArrests)
dfw<-scale(USArrests[-1])

wssplot<-function(data, nc=15, seed=1234){
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dfw)
#군집의 수가 4개임을 결정
set.seed(1234)
data(USArrests)
install.packages("cluster")
library(cluster)
result<-pam(USArrests,4,stand=FALSE,metric="euclidean")
summary(result)
clusplot(result)
