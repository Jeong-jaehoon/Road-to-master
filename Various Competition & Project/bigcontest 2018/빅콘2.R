
#load data



full_train_trade_payment3<-read.csv(file.choose())
head(new_id[,])
new<-read.csv(file.choose())[,-1]
heavy<-read.csv(file.choose())[,-1]
light<-read.csv(file.choose())[,-1]

colnames(full_train_trade_payment3)

full_train_trade_payment3 = full_train_trade_payment3[,-1]

K=1     # 10 fold
cnt = 1
acc <- numeric()  # 분류정확도가 여기에 담긴다
new_train<-new_train[,-1]

neww_train<-full_train_trade_payment3 %>% filter(acc_id %in% new) %>% arrange(label.x) %>% as.data.frame()
heavy_train<-full_train_trade_payment3 %>% filter(acc_id %in% heavy) %>% arrange(label.x) %>% as.data.frame()
light_train<-full_train_trade_payment3 %>% filter(acc_id %in% light) %>% arrange(label.x) %>% as.data.frame()

write.csv(neww_train,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/neww_train.csv")
write.csv(heavy_train,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/heavy_train.csv")
write.csv(light_train,"C:/Users/정재훈/Desktop/빅콘2018/데이터/데이터나누기/light_train.csv")
read.csv(file.choose())

for(i in K){
  
  new1<-new_train %>% filter(label.x == "week") %>% dim()
  new2<-new_train %>% filter(label.x == "month") %>% dim()
  new3<-new_train %>% filter(label.x == "2month") %>% dim()
  new4<-new_train %>% filter(label.x == "retained") %>% dim()
  
  set.seed(180901)
  cross1<-cvFolds(new1[1],K=5)
  cross2<-cvFolds(new2[1],K=5)
  cross3<-cvFolds(new3[1],K=5)
  cross4<-cvFolds(new4[1],K=5)
  
  month2_index <- cross1$subsets[cross1$which==i, 1] 
  month_index <- cross2$subsets[cross2$which==i, 1]+new3[1]
  retained_index <- cross3$subsets[cross3$which==i, 1]+new3[1]++new2[1]
  week_index <- cross4$subsets[cross4$which==i, 1]+new3[1]++new2[1]+new4[1]
  full_index<-c(week_index,month_index,month2_index,retained_index)
  
  test <- new_train[full_index, ][,-1] # 검정데이터 생성
  train <- new_train[-full_index, ][,-1]   # 훈련데이터 생성
  cat('K=',i,'검정데이터 차원 : ',dim(test),' \n') 
  
  random_model=randomForest(label.x ~ ., data=train ,mtry=9, importance=TRUE)
  pred=predict(random_model,newdata=test)
  pred=as.factor(pred)
  t1=table(test$label.x,pred)
  print(t1)
  
}
t<-t1
acc <- 8/(sum(t[,1])/t[1,1] + sum(t[1,])/t[1,1] + sum(t[,2])/t[2,2] + sum(t[2,])/t[2,2] +
            sum(t[,3])/t[3,3] + sum(t[3,])/t[3,3]+ sum(t[,4])/t[4,4] + sum(t[4,])/t[4,4])

acc

  #heavy
  heavy_train
  new1<-heavy_train %>% filter(label.x == "week") %>% dim()
  new2<-heavy_train %>% filter(label.x == "month") %>% dim()
  new3<-heavy_train %>% filter(label.x == "2month") %>% dim()
  new4<-heavy_train %>% filter(label.x == "retained") %>% dim()
  
  cross1<-cvFolds(new1[1],K=5)
  cross2<-cvFolds(new2[1],K=5)
  cross3<-cvFolds(new3[1],K=5)
  cross4<-cvFolds(new4[1],K=5)
  
  month2_index <- cross1$subsets[cross1$which==i, 1] 
  month_index <- cross2$subsets[cross2$which==i, 1]+new3[1]
  retained_index <- cross3$subsets[cross3$which==i, 1]+new3[1]++new2[1]
  week_index <- cross4$subsets[cross4$which==i, 1]+new3[1]++new2[1]+new4[1]
  full_index<-c(week_index,month_index,month2_index,retained_index)
  
  test <- new_train[full_index, ][,-1] # 검정데이터 생성
  train <- new_train[-full_index, ][,-1]   # 훈련데이터 생성
  cat('K=',i,'검정데이터 차원 : ',dim(test),' \n') 
  
  random_model=randomForest(label.x ~ ., data=train ,mtry=9, importance=TRUE)
  pred=predict(random_model,newdata=test)
  pred=as.factor(pred)
  t2=table(test$label.x,pred)
  print(t2)
  
  
  #light
  new_train<-full_train_trade_payment3 %>% filter(acc_id %in% light) %>% arrange(label.x) %>% as.data.frame()
  new1<-new_train %>% filter(label.x == "week") %>% dim()
  new2<-new_train %>% filter(label.x == "month") %>% dim()
  new3<-new_train %>% filter(label.x == "2month") %>% dim()
  new4<-new_train %>% filter(label.x == "retained") %>% dim()
  
  
  cross1<-cvFolds(new1[1],K=5)
  cross2<-cvFolds(new2[1],K=5)
  cross3<-cvFolds(new3[1],K=5)
  cross4<-cvFolds(new4[1],K=5)
  
  month2_index <- cross1$subsets[cross1$which==i, 1] 
  month_index <- cross2$subsets[cross2$which==i, 1]+new3[1]
  retained_index <- cross3$subsets[cross3$which==i, 1]+new3[1]++new2[1]
  week_index <- cross4$subsets[cross4$which==i, 1]+new3[1]++new2[1]+new4[1]
  full_index<-c(week_index,month_index,month2_index,retained_index)
  
  test <- new_train[full_index, ][,-1] # 검정데이터 생성
  train <- new_train[-full_index, ][,-1]   # 훈련데이터 생성
  cat('K=',i,'검정데이터 차원 : ',dim(test),' \n') 
  
  random_model=randomForest(label.x ~ ., data=train ,mtry=9, importance=TRUE)
  pred=predict(random_model,newdata=test)
  pred=as.factor(pred)
  t3=table(test$label.x,pred)
  print(t3)
  
  t=t1+t2+t3
  acc[cnt] <- 8/(sum(t[,1])/t[1,1] + sum(t[1,])/t[1,1] + sum(t[,2])/t[2,2] + sum(t[2,])/t[2,2]+sum(t[,3])/t[3,3] + sum(t[3,])/t[3,3]+ sum(t[,4])/t[4,4] + sum(t[4,])/t[4,4])
  print(acc)
  cnt <- cnt + 1 
  
}
