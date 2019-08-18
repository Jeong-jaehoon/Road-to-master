library(tseries)
library(dplyr)
library(ggplot2)
library(forecast)
library(TSA)
library(dynlm)
library(vars)
library(dynlm)

install.packages("dynlm")
install.packages("TSA")

rm(list=ls())
rawdata <- read.table("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/pformos5/pformos5.txt", header=T)
data_1 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data1.csv")
data_2 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data2.csv")
data_3 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data3.csv")

head(data_1)

##결측치, 이상값 처리

#방법 1
for(i in which(data_1$Temp==0)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}
for(i in which(data_1$Temp==21)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}
for(i in which(data_1$Temp==35)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}
for(i in which(data_1$Temp==79)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}

for(i in which(data_1$Temp==80)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}

for(i in which(data_1$Temp==993)){
  data_1$Temp[i] <- (data_1$Temp[(i-288)] + data_1$Temp[(i+288)])/2
}
par(mfrow=c(2,1))
data_1
data_1$Temp[1207]
data_1$Temp[1410]

data_1 <- as.ts(data_1)
ts.plot(data_1)

which(data_1$Temp==0)

data_1$Temp[1207]


##방법 2 
tsclean(data_1$Temp)

ts.plot(data_1$Temp)
ts.plot(tsclean(data_1$Temp), col='red')

cdata_1 <- tsclean(data_1$Temp)
cdata_1

write.csv(cdata_1,"C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/cdata1.csv")

cdata_1[5605]
cdata_1[5834]
par(mfrow=c(1,1))
for(i in 5605:5834){
  cdata_1[i] <- (cdata_1[(i-288)] + cdata_1[(i+288)])/2
}

for(i in 6840:6998){
  cdata_1[i] <- (cdata_1[(i-288)] + cdata_1[(i+288)])/2
}

for(i in 7715:8505){
  cdata_1[i] <- (cdata_1[i-288*3] + cdata_1[(i+288*3)])/2
}

cdata_1
log(cdata_1)
ts.plot(cdata_1)

ts.plot(cdata_1)
ts.plot(log(cdata_1))

cdata_11 <- tsclean(cdata_1)
ts.plot(cdata_11, col="red")

ts.plot(data_2$Temp)
ts.plot(tsclean(data_2$Temp), col='blue')

ts.plot(data_3$Temp)
ts.plot(tsclean(data_3$Temp), col='green')

acf(cdata_1, lag.max=600)
pacf(cdata_1, lag.max=600)

plot(diff(cdata_1))
acf(diff(diff(cdata_1)), lag.max=100)
pacf(diff(cdata_1), lag.max=100)

ddcdata_1 <- diff(diff(cdata_1))
plot(ddcdata_1); acf(ddcdata_1); pacf(ddcdata_1)
##acf 는 3시차에서 유의 pacf는 서서히감소 ma3
fit1 <- arima(ddcdata_1, order=c(0,2,1), seasonal = list(order=c(0,0,1),288))
summary(fit1)
fit1$res
plot(fit1$res)

acf(fit1$res, lag.max= 300)

pacf(fit1$res)

help(arima)

get.AIC(ddcdata_1, 2,2,2,2)
fit_1 <- arima(ddcdata_1, c(1,0,1),seasonal=list(order=c(2,0,1)))
fit_1
library(forecast)

diff(cdata_1)
acf(diff(cdata_1))
pacf(diff(cdata_1))

a <- diff(diff(diff(cdata_1)), 288)
acf(a,lag.max = 600)
pacf(a, lag.max = 600)

get.AIC(ddcdata_1, 3,3,3,3)

fit4 <- arima(ddcdata_1, c(1,0,1),seasonal=list(order=c(2,0,1)))
auto.arima(cdata_1)
fit_3 <- arima(cdata_1, c(2,1,1))
plot(fit_3$res)
acf(fit_3$res,2000)
pacf(fit_3$res)
plot(fit_2$residuals)
acf(fit_2$residuals, lag.max=300)
pacf(fit_2$residuals)

####################################################################################################################





count(which(rawdata$Temp>400&&rawdata$Temp<600))
which(rawdata$Temp<378)==TRUE
install.packages("imputeTS")
library(imputeTS)
tsoutliers(data_1, lambda=0)
?tsoutliers
which(rawdata$Temp ==0)





mean(rawdata$Temp)
sd(rawdata$Temp)

483+1.96*98.93903
483-1.06*98.93909

#########################################
data_11 <- data_1 %>% filter(Light==1)
mean(data_11[,"Temp"])
data_12 <- data_1 %>% filter(Light==0)
mean(data_12[,"Temp"])

plot(data_1[1:2880],ylim = c(300,700))
data_1 <- data_1[,-1]
data_1[1:2]
plot(data_1)

adf.test(data_1)

?acf
acf(data_1,lag.max=5000)
pacf(data_1, lag.max=300)

##
acf(data_1)
pacf(data_1)
head(data_1)
ts.plot(data_1)
plot(diff(data_1))
data_1[1]
mean(data_1)

for(i in 1:length(data_1)){
  if (data_1[i]>600){
    data_1[i] <- 500
  }
  else if (data_1[i]<400){
    data_1[i] <- 500
  }
  else data_1[i] = data_1[i]
}

plot(data_1)
plot(log(data_1))
acf(data_1)

pacf(data_1)

plot(diff(data_1))
acf(diff(data_1))
pacf(diff(data_1))

get.AIC<-function(X,p.max,q.max,P.max,Q.max){
  
  aic<-100000
  for(p in 0:p.max) for(q in 0:q.max) 
    for(P in 0:P.max) for(Q in 0:Q.max){
      
      try(
        aic1<-arima(X, order=c(p,0,q),
                    sea=list(order=c(P,0,Q),12))$aic
        ,TRUE)
      
      if(aic1<aic){
        cat("p=",p,"q=",q,"P=",P,"Q=",Q,"AIC=",aic1,"\n")
        aic<-aic1    
      }  
    }
}

get.AIC(diff(data_1), 1,1,1,1)
library(forecast)
auto.arima(data_1)

fit1 <- arima(data_1, order = c(1,1,3))
fit1
summary(fit1)

r <- fit1$res
plot(r)
acf(r)
pacf(r)
##계절차분도 고려해보자

data_my <- data_my%>%mutate( time = 1:28440)
data_my <- data_my[-1]
data_my <- as.ts(data_my)
data_my
plot(data_my)

data_my_1 <- 
str(data_my)










