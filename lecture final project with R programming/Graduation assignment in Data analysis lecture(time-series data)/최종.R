library(tseries)
library(dplyr)
library(ggplot2)
library(forecast)
library(TSA)
library(dynlm)
library(vars)
library(dynlm)

rm(get.AIC)
data_1 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data1.csv")
data_2 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data2.csv")
data_3 <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/mouse data/data3.csv")


##data_1 에대해서...
get.AIC1<-function(X,p.max,q.max,P.max,Q.max){
  
  aic<-100000
  for(p in 0:p.max) for(q in 0:q.max) 
    for(P in 0:P.max) for(Q in 0:Q.max){
      
      try(
        aic1<-arima(X, order=c(p,0,q),
                    sea=list(order=c(P,0,Q),288))$aic
        ,TRUE)
      
      if(aic1<aic){
        cat("p=",p,"q=",q,"P=",P,"Q=",Q,"AIC=",aic1,"\n")
        aic<-aic1    
      }  
    }
}
get.AIC2<-function(X,p.max,q.max,P.max,Q.max){
  
  aic<-100000
  for(p in 0:p.max) for(q in 0:q.max) 
    for(P in 0:P.max) for(Q in 0:Q.max){
      
      try(
        aic1<-arima(X, order=c(p,0,q),
                    sea=list(order=c(P,0,Q),294))$aic
        ,TRUE)
      
      if(aic1<aic){
        cat("p=",p,"q=",q,"P=",P,"Q=",Q,"AIC=",aic1,"\n")
        aic<-aic1    
      }  
    }
}

#data1 전처리
cdata_1 <- tsclean(data_1$Temp)
for(i in 5605:5834){
  cdata_1[i] <- (cdata_1[(i-288)] + cdata_1[(i+288)])/2
}

for(i in 6840:6998){
  cdata_1[i] <- (cdata_1[(i-288)] + cdata_1[(i+288)])/2
}

for(i in 7715:8505){
  cdata_1[i] <- (cdata_1[i-288*3] + cdata_1[(i+288*3)])/2
}
par(mfrow=c(1,1))
ts.plot(data_1)
lines(cdata_1, col="red")
fit1
ts.plot(diff(cdata_1))
acf(cdata_1)
pacf(cdata_1, lag.max=2000)
diff(cdata_1)
get.AIC1(diff(cdata_1),2,2,2,2)
adf.test(cdata_1)
Box.test(fit2$residuals)
adf.test(cdata_1, k=1)
?adf.test
fit1 <- arima(diff(cdata_1), order=c(0,0,1), sea=list(order=c(2,1,0), 288))
acf(fit1$residuals, lag.max=800)
pacf(fit1$residuals, lag.max=800)

ts.plot(fitted(fit1))
lines(diff(cdata_1), col="red")
ts.plot(fit1$residuals)

fiterr1 <- garch(fit1$residuals, order=c(1,1), trace=F)
fiterr2 <- garch(fit1$residuals, order=c(2,2), trace=F)
acf(fiterr1$residuals)

acf(fit1$residuals)
pacf(fit1$residuals)

acf(fit1$residuals^2)
pacf(fit1$residuals^2)

acf(fitres1$residuals, lag.max)
pacf(fitres1$residuals)
ts.plot(fit1$residuals)
AIC(fiterr1)
AIC(fiterr2)
summary(fiterr1)
r.err <- fiterr1$residuals
r.err <- r.err[-1]
ts.plot(r.err)
pacf(fit1$residuals)
layout(matrix(1:4,2,2))
ts.plot(r.err)
acf(r.err, lag.max=1000)
pacf(r.err, lag.max=1000)
acf(r.err^2, lag.max=1000)
pacf(r.err^2, lag.max=1000)


acf(fit1$residuals, lag=1000)
pacf(fit1$residuals, lag=1000)

fitres1 <- arima(fit1$residuals, order=c(0,0,4), sea=list(order=c(0,0,0)))
ts.plot(fitres1$residual)
acf(fitres1$residuals)                 
pacf(fitres1$residuals, lag.max=1000)
