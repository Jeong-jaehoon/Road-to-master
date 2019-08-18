rm(list=ls())
setwd("F:/공모전/빅콘테스트2018/")
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(dplyr)
library(nnet)
install.packages("tidyr")
library(tidyr)
library(reshape2)

#########################################################################full make
a<-full_party23 %>% arrange(acc_id,party_start_week,party_start_day)
day<-a %>% select(acc_id,party_start_week,party_start_day) %>% 
  mutate(date1=(party_start_week-1)*7+party_start_day) %>% plyr::count(.) 

head(a)
head(day)
#party_daynum
day1<-day %>% group_by(acc_id) %>% summarise(party_daynum=length(date1)) %>% as.data.frame()
#######최근 누적 party num
total_party<-day %>% group_by(acc_id) %>% summarise(total_party_num=sum(freq)) %>% as.data.frame()
party_10day<-day %>% filter(date1 > 46) %>% group_by(acc_id) %>% summarise(party_10day_num=sum(freq)) %>% as.data.frame()
party_7day<-day %>% filter(date1 > 49) %>% group_by(acc_id) %>% summarise(party_7day_num=sum(freq)) %>% as.data.frame()
party_14day<-day %>% filter(date1 > 42) %>% group_by(acc_id) %>% summarise(party_14day_num=sum(freq)) %>% as.data.frame()
party_21day<-day %>% filter(date1 > 35) %>% group_by(acc_id) %>% summarise(party_21day_num=sum(freq)) %>% as.data.frame()

##########
min<-full_party23 %>% select(acc_id,fulll_min) %>% group_by(acc_id) %>% 
  summarise(mean_min=mean(fulll_min),
            trim_min=mean(fulll_min,trim=0.1),
            median_min=median(fulll_min),
            sum_min=sum(fulll_min),) %>% as.data.frame()

full_party<-label %>% select(acc_id) %>% left_join(.,day1,by="acc_id") %>% left_join(.,total_party,by="acc_id") %>%
  left_join(.,party_10day,by="acc_id") %>% left_join(.,party_7day,by="acc_id") %>% left_join(.,party_14day,by="acc_id") %>%
  left_join(.,party_21day,by="acc_id") %>% left_join(.,min,by="acc_id") %>% as.data.frame()

full_party[is.na(full_party)]<-0
head(full_party)

#########
full_train <- full_train %>% left_join(.,full_party,by="acc_id") %>% as.data.frame()
full_train$guild_cnt[is.na(full_train$guild_cnt)]<-0
head(full_train)
summary(full_train)

################################################################분ㅅ
a<-colnames(full_train)[c(3:26,56,59)]
full_train1<-full_train %>% select(-a)
head(full_train1)
summary(full_train1)

install.packages("cvTools")

library(cvTools)
library(VGAM)

cross1<-cvFolds(25000,K=10)
cross2<-cvFolds(25000,K=10)
cross3<-cvFolds(25000,K=10)
cross4<-cvFolds(25000,K=10)


###########################################logistic
K=1:10     # 10 fold
cnt = 1
acc <- numeric()  # 분류정확도가 여기에 담긴다
i=1
for(i in K){
  week_index <- cross1$subsets[cross1$which==i, 1] 
  month_index <- cross2$subsets[cross2$which==i, 1]+25000
  month2_index <- cross3$subsets[cross3$which==i, 1]+50000
  retained_index <- cross4$subsets[cross4$which==i, 1]+75000
  full_index<-c(week_index,month_index,month2_index,retained_index)
  
  test <- full_train1[full_index, ][,-1] # 검정데이터 생성
  train <- full_train1[-full_index, ][,-1]   # 훈련데이터 생성
  cat('K=',i,'검정데이터 차원 : ',dim(test),' \n') 
  
  model <- multinom(label.x ~ ., data=train)
  pred=predict(model,newdata=test)
  pred=as.factor(pred)
  t=table(test$label.x,pred)
  print(t)
  acc[cnt] <- 8/(sum(t[,1])/t[1,1] + sum(t[1,])/t[1,1] + sum(t[,2])/t[2,2] + sum(t[2,])/t[2,2]+sum(t[,3])/t[3,3] + sum(t[3,])/t[3,3]+ sum(t[,4])/t[4,4] + sum(t[4,])/t[4,4])
  cnt <- cnt + 1 
}
mean(acc)
