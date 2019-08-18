install.packages("dplyr")
install.packages("randomForest")

library(lubridate)
library(ggplot2)
library(dplyr)
library(nnet)
library(tidyr)
library(reshape2)
library(xgboost)
library(caret)
library(cvTools)
library(randomForest)

activity_1<-read.csv(file.choose)
train_activity<-read.csv(file.choose())
test_activity<-read.csv(file.choose())

activity<-rbind(train_activity,test_activity)

activ_1<-activity %>% select(acc_id,wk) %>% acast(.,acc_id~wk)%>% as.data.frame
activ_1$acc_id<-rownames(activ_1)
colnames(activ_1)<-c(paste0("week",c(1:8)),"acc_id")
rownames(activ_1)<-NULL
head(activ_1)

new_id <- activ_1 %>% filter(is.na(week1)) %>% filter(is.na(week2)) %>% filter(is.na(week3)) %>% 
  filter(is.na(week4)) %>% filter(is.na(week5)) %>% filter(is.na(week6)) %>% 
  select(acc_id) %>% as.data.frame()
head(new_id)

heaby_id<-activ_1 %>% filter(!is.na(week1)) %>% filter(!is.na(week2))%>% filter(!is.na(week3)) %>% 
  filter(!is.na(week4)) %>% filter(!is.na(week5)) %>% filter(!is.na(week6)) %>% 
  filter(!is.na(week7)) %>% filter(!is.na(week8)) %>% select(acc_id) %>%  as.data.frame()
head(heaby_id)

no_ligit<-rbind(new_id,heaby_id) %>% as.vector()
no_ligit$dd<-1
light_id<-activ_1 %>% select(acc_id) %>% left_join(.,no_ligit,by="acc_id") %>% filter(is.na(dd)) %>% select(acc_id) %>%
  as.data.frame()

length((as.character(activ_1$acc_id)  %in%  no_ligit)==FALSE)
length((activ_1$acc_id !=no_ligit)==TRUE)

33696+58750+47554

dim(no_ligit)
dim(light_id)

write.csv(new_id,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/new_id.csv")
write.csv(heaby_id,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/heavy_id.csv")
write.csv(light_id,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/light_id.csv")
