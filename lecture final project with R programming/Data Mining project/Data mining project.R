##커널연습

library(stringr) #assist with text manipulation
library(dplyr) # data manipulation
library(readr) # data input
library(caret) #select tuning parameters
library(MASS) # contains the data
library(nnet) # used for Multinomial Classification 
library(readr) #assist with text manipulation
library(kernlab) #assist with SVM feature selection
library(class) # used for an object-oriented style of programmin
library(KernelKnn) # used for K- Nearest-Neighbors method
library(nnet) # Used for Neural Net
library(e1071) 
library(gbm)
library(xgboost) # Used for xgbTree
library(glmnet)
library(data.table)
library(ModelMetrics)
library(MLmetrics)
library(caret)
rm(list=ls())
testdf<- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/데이터마이닝/Final Project/breastcancerproteomes/BCGENES.csv", header=T)



##K-FOLD CV
trControl <- trainControl(method  = "cv", number  = 5)
testdf$PAM50.mRNA
fit <- train(PAM50.mRNA ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:15),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = testdf)
plot(fit$results[,])

knn.cross <- tune.knn(x = testdf, y = PAM50.mRNA, k =1:15, tune)
fit$method
fit$modelInfo
fit$results
fit$times
fit$bestTune
library(ggplot2)

gg <- ggplot(data=fit$results, aes(x = fit$results$k, y = fit$result$Accuracy))+ geom_line(color="blue")
gg2 <- gg + geom_line(aes(x = fit$result$k, y = fit$result$Kappa), color = "red") +geom_line(aes(x = fit$result$k, y=fit$result$AccuracySD), color = "orange")
gg2 + geom_line(aes(x = fit$result$k, y=fit$result$KappaSD), color = "green")

names(getModelInfo()　　
fit$bestTune
fit$
AccuraciesMultinomial<- c(0,0,0)


for(i in seq(1000)){
  
  train<- createDataPartition(testdf$PAM50.mRNA, p = .70, list = FALSE)
  trainDF<-testdf[train,]
  testDF<-testdf[-train,]
  
  net<-multinom(PAM50.mRNA~.,
                data=trainDF,trace=FALSE)
  AccuraciesMultinomial[i] <- caret::confusionMatrix(predict(net, newdata=testDF, "class"),testDF$PAM50.mRNA)$overall["Accuracy"]
}


?confusionMatrix
caret::confusionMatrix(predict(net, newdata=testDF, "class"),testDF$PAM50.mRNA)

plot(density(AccuraciesMultinomial))
summary(AccuraciesMultinomial)

?list

library(caret)
confusionMatrix(predict(net, newdata=testDF, "class"),testDF$PAM50.mRNA)$overall
summary(net)

###랜덤 포레스트 ##

train<- createDataPartition(testdf$PAM50.mRNA, p = .70, list = FALSE)
trainDF<-testdf[train,]
testDF<-testdf[-train,]

rf.fit <- randomForest(PAM50.mRNA ~., data= trainDF, mtry=57, importance = TRUE, subset = TRUE)
rf.predcit <- predict(rf.fit, testDF)

plot(rf.predcit)

rf.fit
install.packages("randomForest")

library(randomForest)
set.seed(3402)
dim(trainDF)

rf.fit <- randomForest(PAM50.mRNA ~., data = trainDF, mtry = 57, importance = TRUE, subset = TRUE)


rf.predict <- predict(rf.fit, trainDF)
plot(rf.predict, trainDF)

caret::confusionMatrix(rf.predict, testDF$PAM50.mRNA)

randomForest::predict.randomForest(rf.fit, trainDF$PAM50.mRNA)

test <- testDF$PAM50.mRNA
trainDF<-sample(1:nrow(Boston),nrow(Boston)/2) #회귀트리와 같아 1/2만 훈련셋
Boston.test<-Boston[-train,"medv"]
bag<-randomForest(PAM50.mRNA~.,data = testdf ,subset=train ,mtry=57)
yhat.bag<-predict(bag, testdf[-train,])
mean((yhat.bag-test)^2)

install.packages("klaR")
library("klaR")


##나이브 베이즈
fix(testdf)

class(x)
which(colnames(testdf)== "PAM50.mRNA")
x <- testdf$PAM50.mRNA
y <- testdf[,-2]
y <- testdf[,-1]
model = train(y, x,'nb', trControl=trainControl(method='cv',number=10))
model

predict(model$finalModel, y)$class

table(predict(model$finalModel, y)$class, x)

dim()

##아다 부스트
install.packages("adabag")
library(fastAdaboost)
library(adabag)
test_adaboost <- boosting.cv(PAM50.mRNA~., v= 10, data=testdf, mfinal=10, control = rpart.control(maxdepth =30))
test_adaboost$importance
test_adaboost$error

install.packages("pROC")
library(pROC)

multiclass.roc(predict(model$finalModel, y)$class, x)
