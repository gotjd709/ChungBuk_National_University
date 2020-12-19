#### 3장 문제를 위한 코드


#상대위험도
a = (5/26)/(8/90)
a*exp(1.96*sqrt((1-5/26)/5+(1-8/90)/8))
a*exp(-1.96*sqrt((1-5/26)/5+(1-8/90)/8))


#오즈비
b = (273*7260)/(2641*716)
b*exp(1.96*sqrt(1/273+1/2641+1/716+1/7260))
b*exp(-1.96*sqrt(1/273+1/2641+1/716+1/7260))


# 동질성 & 독립성 검정
row_1 <- c(49, 12)
row_2 <- c(24, 9)
row_3 <- c(2, 29)
row <- c(row_1, row_2, row_3)
sum(row_1)/sum(row)
sum(row_2)/sum(row)
sum(row_3)/sum(row)

data_rbind <- rbind(row_1, row_2, row_3)
data_rbind

dimnames(data_rbind) <- list("요인" = c("1", "2", "3"), "질병" = c("1", "2"))
addmargins(data_rbind)

chisq.test(data_rbind)

chisq.test_output_2 <- chisq.test(data_rbind)
chisq.test_output_2$expected#기댓값

qchisq(p=0.95, df=2, lower.tail = TRUE)


#Fisher'Exact Test

group<-c("A","A","B","B")
cancer<-c("1.Yes","2.No","1.Yes","2.No")

count<-c(1,5,8,2)


dat<-data.frame(group,cancer,count)
tab<-xtabs(count~group+cancer,data=dat)
tab

chisq.test(tab)$expected

fisher.test(tab)


