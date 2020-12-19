##### 연습문제 2장 풀이 #####



### 1번 직접해보기

ex1_a <- c(48.2, 54.6, 58.3, 47.8, 51.4,
           52.0, 55.2, 49.1, 49.9, 52.6)
ex1_b <- c(52.3, 57.4, 55.6, 53.2, 61.3,
           58.0, 59.8, 54.8, 51.2, 46.2)
ex1_a_sd2 <- (sd(ex1_a))^2
ex1_b_sd2 <- (sd(ex1_b))^2

ex1_F <- ex1_a_sd2/ex1_b_sd2

ex1_F_0025 <- qf(p = 0.025, df1 = 9, df2 = 9, lower.tail = TRUE)
ex1_F_0975 <- qf(p = 0.975, df1 = 9, df2 = 9, lower.tail = TRUE)

### 1번 package 돌려서 해보기

# 가장 간단한 방법
var.test(ex1_a, ex1_b)

# 좀 더 심화된 방법
install.packages("lawstat")
library(lawstat)
ex1 <- c(ex1_a, ex1_b)
ex1_f <- c(rep(1, 10),rep(2, 10))

# modified robust Brown-Forsythe Levene-type test
levene.test(ex1, ex1_f)

# Classical Levene's test
levene.test(ex1, ex1_f, location = "mean")



### 2번 

ex2_a <- c(9.0, 9.4, 4.7, 4.8, 8.9, 4.9, 8.4, 5.9, 6.3, 5.7,
            5.0, 3.5, 7.8, 10.4, 8.0, 8.0, 8.6, 7.0, 6.8, 7.1,
            5.7, 7.6, 6.2, 7.1, 7.4, 8.7, 4.9, 7.4, 6.4, 7.1, 
            6.3, 8.8, 8.8, 5.2, 7.1, 5.3, 4.7, 8.4, 6.4, 8.3)
ex2_b <- c(12.6, 14.6, 16.2, 23.9, 23.3, 17.1, 20.0, 21.0, 19.1, 19.4,
            16.7, 15.9, 15.8, 16.0, 17.9, 13.4, 19.1, 16.6, 18.9, 18.7,
            20.0, 17.8, 13.9, 22.1, 13.9, 18.3, 22.8, 13.0, 17.9, 15.2,
            17.1, 15.1, 16.9, 16.4, 22.8, 19.4, 19.6, 18.4, 18.2, 20.7)
var.test(ex2_a, ex2_b)

# 2-1번 직접해보기

ex2_a_mean <- mean(ex2_a)
ex2_b_mean <- mean(ex2_b)
ex2_a_sd2 <- (sd(ex2_a))^2
ex2_b_sd2 <- (sd(ex2_b))^2

ex2_t <- (ex2_b_mean - ex2_a_mean) / sqrt(ex2_b_sd2/40 + ex2_a_sd2/40)


# 2-1번 package를 통해 해보기

t.test(ex2_b, ex2_a, alternative = "greater", paired = FALSE, var.equal = FALSE, conf.level = 0.95)



### 3번 직접해보기

ex3_d1 <- c(0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5, 0.6,
             0.7, 0.7, 0.8, 0.9, 0.9, 1.0, 1.0, 2.0)
ex3_d2 <- c(0.4, 0.5, 0.5, 0.9, 0.5, 0.5, 0.5, 0.5, 0.5, 0.6,
             1.1, 1.2, 0.8, 1.2, 1.9, 0.9, 2.0, 3.7)
ex3_d <- ex3_d2 - ex3_d1

ex3_mean <- mean(ex3_d)
ex3_sd <- sd(ex3_d)

ex3_mean/(ex3_sd/sqrt(18))

qt(p = 0.025, df = 17)
qt(p = 0.975, df = 17)

### 3번 package를 통해 해보기

t.test(ex3_d2, ex3_d1, paired = TRUE, conf.level = 0.95)



### 4번 직접해보기

# 4-1 번

ex4_d1 <- c(104, 116, 84, 77, 61, 84, 81, 72, 61, 97, 84)
ex4_d2 <- c(108, 118, 89, 71, 66, 83, 88, 76, 68, 96, 81)
ex4_d <- ex4_d2 - ex4_d1

ex4_mean <- mean(ex4_d)
ex4_sd <- sd(ex4_d)

ex4_mean/(ex4_sd/sqrt(11))

ex4_t1 <- qt(p = 0.025, df = 10)
ex4_t2 <- qt(p = 0.975, df = 10)

# 4-2 번

ex4_cl <- c(ex4_mean + ex4_t1*(ex4_sd/sqrt(11)), ex4_mean + ex4_t2*(ex4_sd/sqrt(11)))

### 4번 package를 통해 해보기

t.test(ex4_d2, ex4_d1, paired = TRUE, conf.level = 0.95)



### 5번 직접해보기

ex5_1 <- c(22.2, 97.8, 29.1, 37.0, 35.8, 44.2,
           82.0, 56.0, 9.3, 19.9, 39.5, 12.8)
ex5_2 <- c(15.1, 23.2, 10.5, 13.9, 9.7, 19.0,
           19.8, 9.1, 30.1, 15.5, 10.3, 11.0)
ex5_3 <- c(10.2, 11.3, 11.4, 5.3, 14.5, 11.0,
           13.6, 33.4, 25.0, 27.0, 36.3, 17.7)

ex5_1_mean <- mean(ex5_1)
ex5_2_mean <- mean(ex5_2)
ex5_3_mean <- mean(ex5_3)
ex5_mean <- mean(c(ex5_1, ex5_2, ex5_3))
ex5_sst <- 12*((ex5_1_mean - ex5_mean)^2) + 12*((ex5_2_mean - ex5_mean)^2) + 12*((ex5_3_mean - ex5_mean)^2)
ex5_mst <- ex5_sst/2

ex5_se1 <- sum((ex5_1 - rep(ex5_1_mean, 12))^2)
ex5_se2 <- sum((ex5_2 - rep(ex5_2_mean, 12))^2)
ex5_se3 <- sum((ex5_3 - rep(ex5_3_mean, 12))^2)
ex5_sse <- sum(ex5_se1, ex5_se2, ex5_se3)
ex5_mse <- ex5_sse/33

ex5_sse + ex5_sst
ex5_ssT <- sum((c(ex5_1, ex5_2, ex5_3) - rep(ex5_mean, 36))^2)

ex5_f <- ex5_mst/ex5_mse

ex5_fa <- qf(p=0.95, df1 = 2, df2 = 33)


### 5번 package 통해 해보기

ex5 <- c(ex5_1, ex5_2, ex5_3)

n <- rep(12, 3)
group <- rep(1:3, n)

ex5_t <- data.frame(ex5, group)
sapply(ex5_t, class)
ex5_t <- transform(ex5_t, group = factor(group))
sapply(ex5_t, class)

aov_model5 <- aov(ex5 ~ group, data = ex5_t)
summary(aov_model5)



### 6번 직접해보기

ex6_1 <- c(35, 120, 90, 109, 82, 40, 68, 84, 124, 77, 140)
ex6_2 <- c(62, 73, 60, 77, 52, 115, 82, 52, 105, 143, 80)
ex6_3 <- c(96, 107, 63, 134, 140, 103, 158, 131, 76, 69, 69)

ex6_1_mean <- mean(ex6_1)
ex6_2_mean <- mean(ex6_2)
ex6_3_mean <- mean(ex6_3)
ex6_mean <- mean(c(ex6_1, ex6_2, ex6_3))
ex6_sst <- 11*((ex6_1_mean - ex6_mean)^2) + 11*((ex6_2_mean - ex6_mean)^2) + 11*((ex6_3_mean - ex6_mean)^2)
ex6_mst <- ex6_sst/2

ex6_se1 <- sum((ex6_1 - rep(ex6_1_mean, 11))^2)
ex6_se2 <- sum((ex6_2 - rep(ex6_2_mean, 11))^2)
ex6_se3 <- sum((ex6_3 - rep(ex6_3_mean, 11))^2)
ex6_sse <- sum(ex6_se1, ex6_se2, ex6_se3)
ex6_mse <- ex6_sse/3

ex6_sse + ex6_sst
ex6_ssT <- sum((c(ex6_1, ex6_2, ex6_3) - rep(ex6_mean, 33))^2)

ex6_f <- ex6_mst/ex6_mse

ex6_fa <- qf(p=0.95, df1 = 2, df2 = 30)

### 6번 package를 통해 해보기

ex6 <- c(ex6_1, ex6_2, ex6_3)

n <- rep(11, 3)
group <- rep(1:3, n)

ex6_t <- data.frame(ex6, group)
sapply(ex6_t, class)
ex6_t <- transform(ex6, group = factor(group))
sapply(ex6_t, class)

aov_model6 <- aov(ex6 ~ group, data = ex6_t)
summary(aov_model6)



### 9번 직접해보기

ex9_11 <- c(25, 28, 22)
ex9_21 <- c(28, 32, 30)
ex9_31 <- c(25, 35, 30)
ex9_12 <- c(18, 23, 19)
ex9_22 <- c(16, 24, 20)
ex9_32 <- c(14, 16, 15)
ex9_13 <- c(17, 24, 19)
ex9_23 <- c(18, 22, 20)
ex9_33 <- c(10, 8, 12)

ex9_1_ <- c(ex9_11, ex9_12, ex9_13)
ex9_2_ <- c(ex9_21, ex9_22, ex9_23)
ex9_3_ <- c(ex9_31, ex9_32, ex9_33)
ex9__1 <- c(ex9_11, ex9_21, ex9_31)
ex9__2 <- c(ex9_12, ex9_22, ex9_32)
ex9__3 <- c(ex9_13, ex9_23, ex9_33)
ex9 <- c(ex9_1_, ex9_2_, ex9_3_)

ex9___ <- c(mean(ex9_11), mean(ex9_12), mean(ex9_13),
            mean(ex9_21), mean(ex9_22), mean(ex9_23),
            mean(ex9_31), mean(ex9_32), mean(ex9_33))

ex9_1_mean <- mean(ex9_1_)
ex9_2_mean <- mean(ex9_2_)
ex9_3_mean <- mean(ex9_3_)
ex9__1mean <- mean(ex9__1)
ex9__2mean <- mean(ex9__2)
ex9__3mean <- mean(ex9__3)
ex9_mean <- mean(ex9)

ex9_ssa <- 9*sum(((ex9_1_mean - ex9_mean)^2), ((ex9_2_mean - ex9_mean)^2), ((ex9_3_mean-ex9_mean)^2))
ex9_ssb <- 9*sum(((ex9__1mean - ex9_mean)^2), ((ex9__2mean - ex9_mean)^2), ((ex9__3mean-ex9_mean)^2))

ex9_ssab <- 3*sum(((mean(ex9_11) - ex9_1_mean - ex9__1mean + ex9_mean)^2),
            ((mean(ex9_12) - ex9_1_mean - ex9__2mean + ex9_mean)^2),
            ((mean(ex9_13) - ex9_1_mean - ex9__3mean + ex9_mean)^2),
            ((mean(ex9_21) - ex9_2_mean - ex9__1mean + ex9_mean)^2),
            ((mean(ex9_22) - ex9_2_mean - ex9__2mean + ex9_mean)^2),
            ((mean(ex9_23) - ex9_2_mean - ex9__3mean + ex9_mean)^2),
            ((mean(ex9_31) - ex9_3_mean - ex9__1mean + ex9_mean)^2),
            ((mean(ex9_32) - ex9_3_mean - ex9__2mean + ex9_mean)^2),
            ((mean(ex9_33) - ex9_3_mean - ex9__3mean + ex9_mean)^2))

ex9_sse <- sum(((rep(mean(ex9_11), 3) - ex9_11))^2,
           ((rep(mean(ex9_12), 3) - ex9_12))^2,
           ((rep(mean(ex9_13), 3) - ex9_13))^2,
           ((rep(mean(ex9_21), 3) - ex9_21))^2,
           ((rep(mean(ex9_22), 3) - ex9_22))^2,
           ((rep(mean(ex9_23), 3) - ex9_23))^2,
           ((rep(mean(ex9_31), 3) - ex9_31))^2,
           ((rep(mean(ex9_32), 3) - ex9_32))^2,
           ((rep(mean(ex9_33), 3) - ex9_33))^2)
ex_sst <- sum((ex9 - rep(ex9_mean, 27))^2)
sum(ex9_ssa, ex9_ssb, ex9_ssab, ex9_sse)

ex9_esa <- ex9_ssa/2
ex9_esb <- ex9_ssb/2
ex9_esab <- ex9_ssab/4
ex9_ese <- ex9_sse/18

qf(p=0.95, df1=2, df2=18)
qf(p=0.95, df1=4, df2=18)
ex9_fsa <- ex9_esa/ex9_ese
ex9_fsb <- ex9_esb/ex9_ese
ex9_fsab <- ex9_esab/ex9_ese

### 9번 package를 통해 해보기

n91 <- rep(9, 3)
age <- rep(c('15-19','20-24','25-29'), n91)

n921 <- rep(3, 3)
n922 <- rep(c('none', 'sometimes', 'everyday'), n92)
frequency <- rep(n922, 3)

ex9_t <- data.frame(age, frequency, ex9)
sapply(ex9_t, class)
ex9_t <- transform(ex9_t, age = factor(age), frequency = factor(frequency))
sapply(ex9_t, class)

aov_model9 <- aov(ex9 ~ age + frequency + age:frequency)
summary(aov_model9)  
  


##### 10번 직접해보기

ex10_11 <- c(21.5, 19.6, 20.9, 22.8)
ex10_21 <- c(14.8, 15.6, 13.5, 16.4)
ex10_12 <- c(14.5, 17.4, 15.0, 17.8)
ex10_22 <- c(12.1, 11.4, 12.7, 14.5)
ex10_13 <- c(16.0, 20.3, 18.5, 19.3)
ex10_23 <- c(14.4, 14.7, 13.8, 12.0)

ex10_1_ <- c(ex10_11, ex10_12, ex10_13)
ex10_2_ <- c(ex10_21, ex10_22, ex10_23)
ex10__1 <- c(ex10_11, ex10_21)
ex10__2 <- c(ex10_12, ex10_22)
ex10__3 <- c(ex10_13, ex10_23)
ex10 <- c(ex10_1_, ex10_2_)

ex10_ssa <- 12*sum(((mean(ex10_1_) - mean(ex10))^2), ((mean(ex10_2_) - mean(ex10))^2))
ex10_ssb <- 8*sum(((mean(ex10__1) - mean(ex10))^2), ((mean(ex10__2) - mean(ex10))^2), ((mean(ex10__3) - mean(ex10))^2))

ex10_ssab <- 4*sum(((mean(ex10_11) - mean(ex10_1_) - mean(ex10__1) + mean(ex10))^2),
                   ((mean(ex10_12) - mean(ex10_1_) - mean(ex10__2) + mean(ex10))^2),
                   ((mean(ex10_13) - mean(ex10_1_) - mean(ex10__3) + mean(ex10))^2),
                   ((mean(ex10_21) - mean(ex10_2_) - mean(ex10__1) + mean(ex10))^2),
                   ((mean(ex10_22) - mean(ex10_2_) - mean(ex10__2) + mean(ex10))^2),
                   ((mean(ex10_23) - mean(ex10_2_) - mean(ex10__3) + mean(ex10))^2))

ex10_sse <- sum(((rep(mean(ex10_11), 4) - ex10_11))^2,
                ((rep(mean(ex10_12), 4) - ex10_12))^2,
                ((rep(mean(ex10_13), 4) - ex10_13))^2,
                ((rep(mean(ex10_21), 4) - ex10_21))^2,
                ((rep(mean(ex10_22), 4) - ex10_22))^2,
                ((rep(mean(ex10_23), 4) - ex10_23))^2)

ex_sst <- sum((ex10 - rep(mean(ex10), 24))^2)
sum(ex10_ssa, ex10_ssb, ex10_ssab, ex10_sse)

ex10_esa  <- ex10_ssa/1
ex10_esb  <- ex10_ssb/2
ex10_esab <- ex10_ssab/2
ex10_ese  <- ex10_sse/18


qf(p=0.95, df1=1, df2=18)
qf(p=0.95, df1=2, df2=18)
ex10_fsa <- ex10_esa/ex10_ese
ex10_fsb <- ex10_esb/ex10_ese
ex10_fsab <- ex10_esab/ex10_ese

### 10번 package를 통해 해보기

n101 <- rep(12, 2)
sex <- rep(c('M','F'), n101)
n102 <- rep(4, 3)
s102 <- rep(c(1, 2, 3), n102)
species <- rep(s102, 2)

ex10_t <- data.frame(sex, species, ex10)
ex10_t <- transform(ex10_t, sex = factor(sex), species = factor(species))
sapply(ex10_t, class)

aov_model10 <- aov(ex10 ~ sex + species + sex:species, data = ex10_t)
summary(aov_model10)


### 민찬이 실험결과 분석

Exp <- c(4.67, 5.39, 4.00, 3.34, 3.56, 4.00,
         3.50, 3.67, 3.20, 2.50, 2.57, 2.57,
         2.67, 3.12, 2.67, 2.50, 2.50, 2.50,
         2.67, 2.67, 2.50, 2.50, 2.50, 2.50,
         2.50, 2.50, 2.50, 2.50, 2.50, 2.50,##P
         3.67, 4.67, 3.57, 2.50, 3.50, 3.50,
         2.50, 2.66, 2.50, 2.50, 3.50, 3.50,
         2.50, 2.57, 2.50, )

MaterialLarge <- rep(30, 3)
MaterialName <- rep(c('Plastic', 'Metal', 'Rubber'), MaterialLarge)

DaySmall <- rep(6, 5)
DayLarge <- rep(c('1 Day','2 Day', '3 Day', '4 Day', '5 Day'), DaySmall)
DayName <- rep(DayLarge, 3)

Exp_t <- data.frame(MaterialName, DayName, Exp)
ex10_t <- transform(ex10_t, sex = factor(sex), species = factor(species))
sapply(ex10_t, class)

aov_model10 <- aov(ex10 ~ sex + species + sex:species, data = ex10_t)
summary(aov_model10)



### 5번 package 통해 해보기
ex5_1 <- c(22.2, 97.8, 29.1, 37.0, 35.8, 44.2,
           82.0, 56.0, 9.3, 19.9, 39.5, 12.8)
ex5_2 <- c(15.1, 23.2, 10.5, 13.9, 9.7, 19.0,
           19.8, 9.1, 30.1, 15.5, 10.3, 11.0)
ex5_3 <- c(10.2, 11.3, 11.4, 5.3, 14.5, 11.0,
           13.6, 33.4, 25.0, 27.0, 36.3, 17.7)

ex5 <- c(ex5_1, ex5_2, ex5_3)

n <- rep(12, 3)
group <- rep(1:3, n)

ex5_t <- data.frame(ex5, group)
sapply(ex5_t, class)
ex5_t <- transform(ex5_t, group = factor(group))
sapply(ex5_t, class)

aov_model5 <- aov(ex5 ~ group, data = ex5_t)
summary(aov_model5)

################################






N1 <- c(4.12, 4.71, 4.75)
N2 <- c(3.71, 4.12, 3.75)
N <- N1-N2
P1 <- c(4.67, 5.39, 4.00, 3.34, 3.56, 4.00)
P2 <- c(3.50, 3.67, 3.20, 2.50, 2.57, 2.57)
P <- P1-P2
M1 <- c(3.67, 4.67, 3.57, 2.50, 3.50, 3.50)
M2 <- c(2.50, 2.66, 2.50, 2.50, 3.50, 3.50)
M <- M1-M2
R1 <- c(4.20, 3.75, 5.00, 3.80, 4.00, 3.80)
R2 <- c(3.50, 3.50, 2.50, 2.57, 2.50, 2.50)
R <- R1-R2
E1 <- c(5.34, 3.75, 2.50, 2.50)
E2 <- c(3.00, 2.67, 2.50, 2.50)
E <- E1-E2
Data <- c(N, P, M, R, E)

group <- c(rep(1,3), rep(2,6), rep(3,6), rep(4,6), rep(5,4))

DataTot <- data.frame(Data, group)
DataTot <- transform(DataTot, group = factor(group))

aov_model_Data <- aov(Data ~ group, data = DataTot)
summary(aov_model_Data)
