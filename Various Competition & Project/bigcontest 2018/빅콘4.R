attach(party23_trtrade)
head(party23_trtrade)
summary(party23_trtrade)
head(full_party23)
party23_trtrade
head(label)
label[,2==retain]
tail(label)
dim(full_party23)
plot(full_party23$ful)
names(full_party23)
attach(full_train)
names(full_train)
full_train
head(full_train)

##################party_daynum: 8주동안 몇일간 파티를 구성해서 게임을 진행했는가
install.packages("dplyr")
library(dplyr)
head(full_party23)

a<-full_party23 %>% arrange(acc_id,party_start_week,party_start_day)
head(a,10)

day<-a %>% select(acc_id,party_start_week,party_start_day) %>% 
  mutate(date1=(party_start_week-1)*7+party_start_day) %>% plyr::count(.) 

day1<-day %>% group_by(acc_id) %>% summarise(party_daynum=length(date1)) %>% as.data.frame()
head(day1)
summary(day1)
head(day)
full_active<-NULL
a<-NULL
aa<-NULL
activity<-NULL
day<-NULL
full_party23<-NULL
trtrade<-NULL
