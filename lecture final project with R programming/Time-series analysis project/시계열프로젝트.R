TSA1<-read.csv("C:/Users/정재훈/Desktop/2018학년도 2학기/시계열/TSA1.csv")
TSA1.ts<-ts(TSA1,st=c(2011,1),fr=12)

local<-TSA1.ts[,1]
foreigner<-TSA1.ts[,2]
total<-TSA1.ts[,3]

plot(local)
plot(foreigner)
plot(total)

##경복궁외국인 모형화

foreigner
plot(foreigner)
##유의사항1 2015년 6월 7월 8월 메르스효과로 전년도대비 심각한 하향
##유의사항2 2016년 초 지카바이러스 + 일본지진 으로 한국에 외국인 관광객 많아짐.
##유의사항3 2017년 말 무료개방 행사로 외국인 관광객 잠시 튐.
diff(foreigner)
plot(diff(foreigner))
library(forecast)
auto.arima(diff(foreigner))
diff_foreigner<-diff(foreigner)
diff_foreigner_seasonal<-diff(diff(foreigner),lag=12)
plot(diff_foreigner_seasonal)
acf(diff_foreigner_seasonal,lag.max=53)
pacf(diff_foreigner_seasonal, lag.max=53)

auto.arima(diff_foreigner_seasonal)

fit1<-arima(diff_foreigner_seasonal, seasonal = list(order=c(0,0,1),12))
summary(fit1)

plot(fitted(fit1))
points(diff_foreigner_seasonal, col = "red", type="l")
##개판인듯.

acf(fit1$residuals, lag.max = 53)
pacf(fit1$residuals)


fit1<-arima(X, order=c(0,1,1), seasonal=list(order=c(0,1,1),12))


arima_foreigner<-arima(diff(foreigner),)

acf(diff(foreigner),lag.max=53)
pacf(diff(foreigner), lag.max=53)
acf(foreigner, lag.max=53)
pacf(foreigner, lag.max=53)



##############################################################################################################3
TSA1.ts[,1]
local


plot(local)
points(total, col="red", type="l")
points(foreigner, col="blue", type="l")

plot(foreigner)
## path가 local과 total 이 거의 일치한다 .. 
## 왜냐하면 foreigner 의 효과가 local에 비해서 미미하기 때문.....!!! 하지만 아예 무시할 만한 영향은 아님
## 그래서 total을 토대로 미리 해보자

plot(total)
acf(total, lag.max = 53)
pacf(total, lag.max = 53)
acf(local)
##가까운것과도 종속성을가지고 주기를 가진다.
##계절성 arima 와 그냥 arima 둘다 적용해야할듯..!!


log_fore<-log(foreigner)

plot(diff(log_fore))

##forecast local jun,july /2015  : MERS

local_for_mers<-window(local, end=2015.4)


plot(local_for_mers)
plot(diff(local_for_mers))
acf(local_for_mers)
pacf(local_for_mers)

##by sin cos

t.local<-1:length(local_for_mers)
SIN<-COS<-matrix(nr= length(local_for_mers),nc=6)
for(i in 1:6){  
  SIN[,i]<-sin(2*pi*i*t.local/12) 
  COS[,i]<-cos(2*pi*i*t.local/12) 
}
## fit1
fit1<-lm(log(local_for_mers)~t.local+SIN[,1]+SIN[,2]+SIN[,3]+SIN[,4]+SIN[,5]+SIN[,6]+COS[,1]+COS[,2]+COS[,3]+COS[,4]+COS[,5]+COS[,6])
summary(fit1)

## stepwise
step(fit1, direction="forward")
step(fit1, direction="backward")
step(fit1, direction="both")

fit1$res
##t.local..? - 삭제
fit2<-lm(log(local_for_mers)~SIN[,c(1,2,3)]+COS[,c(1,2,4,5)])
summary(fit2)

##
r<-fit2$res
plot(r, type="l")
acf(r, lag.max=53)
pacf(r, lag.max=53)

##
diff_mers<-(diff(local_for_mers))

acf(diff_mers, lag.max = 53)
pacf(diff_mers, lag.max = 53)

install.packages("forecast")
library(forecast)
auto.arima(diff(local_for_mers))








##예측하기
X.dat<-data.frame(t.local=t.local, SIN=SIN, COS=COS)
f.value<-predict(fit2,X.dat)
ts.plot(log(local_for_mers), f.value, col=c("black","red"), lty=c(1,3))


r<-fit2$res
acf(r, lag.max=53)
pacf(r, lag.max=53)

plot(diff(r), type="l")
acf(diff(r),lag.max=53)
res.fit<-arima(r,c(0,1,1))

plot(res.fit$residuals)
acf(res.fit$residuals)
pacf(res.fit$residuals)

library(nlme)
fit3<-gls(log(local_for_mers)~SIN[,c(1,2,3)]+COS[,c(1,2,4,5)],cor=corARMA(-0.6770,q=1))
fit3  

res.fit<-arima(r<-fit3$res, order=c(1,0,0))
  
X.dat<-data.frame(t.local=t.local, SIN=SIN, COS=COS)
f.value<-predict(fit3,X.dat)
ts.plot(log(local_for_mers), f.value, col=c("black","red"), lty=c(1,3))
AIC(fit3)
AIC(fit2)




