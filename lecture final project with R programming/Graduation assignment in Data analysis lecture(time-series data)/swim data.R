swim <- read.csv("C:/Users/정재훈/Desktop/2019학년도 1학기/자료분석/swim.csv")
swim <- swim[,-1]
swim
library(car)
attach(swim)
colnames(swim) =c("time","shirt","goggles","flippers","end")
swim

##full model
model0 <- aov(time ~ shirt*goggles*flippers*end)
summary(model0)
vif(model0)

modela <- aov(time ~ shirt*goggles*flippers)
summary(modela)

modelb <- aov(time ~ shirt+goggles+ flippers + shirt*flippers + goggles*flippers)
summary(modelb)
interaction.plot(shirt,flippers, time)
interaction.plot(goggles,flippers, time)
##기본적으로 불균형 자료 
##개별 주효과 항들이 유의하게 나오고 2차 교호작용 항 중 2개가 유의하게 나왔음. 
##근데 데이터 개수가 부족해서 3차교호작용항 일부와 4차 교호작용 항들이 결과가 안 나오고
##(데이터 개수가 작아서 자유도가 모자람) 해석에도 어려움이 있으므로 그냥 제거하고 분석하기로 함. 
##근데 flippers 의 sum of square 지나치게 높은걸 알수 있었다. 이를 고려하고 분석을 진행
## 사실 실험 자체가 잘못되었다고 생각됨.  (같은 실험을 여러번(2번 3번씩) 하고 어떤 실험 설계는 아예 실험이 되지 않음을 설명해야함!)
## 결측값이 여러개 존재하므로 균형자료로의 변환도 불가능.


##유의하지않은 교호작용 항들을 제거한 model
model1 <- aov(time ~ shirt + goggles + flippers + end + shirt*flippers + goggles*flippers)
summary(model1)
vif(model1)

##모든항들이 유의하게 나왔지만 다중공선성도 의심되고 (goggles*flippers 항) flippers 의 영향력이 절대적임 time 에 미치는 각각 개별 항들의 효과를 확인해보기로함

model2 <- aov(time ~ shirt)
summary(model2)
## 유의하지않음

model3 <- aov(time ~goggles)
summary(model3)
## 유의하지않음

model4 <- aov(time ~ end)
summary(model4)
##매우 유의하지않음

##모든 개별항에대해서 유의하지않은 결과가 나옴을 알 수 있음

##더불어 2개의 factor를 가지는 모델들도 설정해보았음.

model5 <- aov(time ~ shirt*goggles)
summary(model5)
##유의하지 않음

model6 <- aov(time ~ shirt*end)
summary(model6)
##유의하지 않음

model7 <- aov(time ~ goggles*end)
summary(model7)
##주효과항은 유의하지않으나 교호작용항은 유의하게 나옴.
##? 이에 대한 해석??

##아래부터는 flippers 를 포함하는 2 factor 분석

model8 <- aov(time ~ flippers * shirt)
summary(model8)

##유의함

model9 <- aov(time ~ flippers * goggles)
summary(model9)
##유의함

model10 <- aov(time ~ flippers * end)
summary(model10)
## end의 개별항을 제외하고는 유의미한 결과가 나옴.

## 위까지의 결론 : flippers(오리발)을 제외하고는 모두 기록 단축에 큰 의미가 없었다!!

model11 <- aov(time ~ goggles*shirt*end)
summary(model11)

model12 <- aov(time ~ flippers*end*goggles)
summary(model12)

vif(model12)

#####아마도 최종모델..?
model13<- aov(time ~ flippers +goggles)
model13
summary(model13)
vif(model13)
model13 <- lm(time ~ flippers + goggles)
summary(model13)
model14<- lm(time ~ flippers*goggles)

plot(model13)

##정규성 만족
shapiro.test(model13$residuals)

##등분산성 만족
ncvTest(model14)

summary(model13)
summary(model14)

##Rsquared == 0.9002

##xtab : 분할표를 구하는 식
xtabs(~ Goggles+Flippers, data=swim)
xtabs(~ Goggles+End, data=swim)
xtabs(~ Goggles+Shirt, data=swim)
xtabs(~ Shirt+End, data=swim)
xtabs(~ Shirt+Flippers, data=swim)
xtabs(~ End+Flippers, data=swim)

fisher.test()

fisher.test(xtabs(~ Goggles+Flippers, data=swim))
fisher.test(xtabs(~ Goggles+End, data=swim))
fisher.test(xtabs(~ Goggles+Shirt, data=swim))
fisher.test(xtabs(~ Shirt+End, data=swim))
fisher.test(xtabs(~ Shirt+Flippers, data=swim))
fisher.test(xtabs(~ End+Flippers, data=swim))

model_b1<-aov(time ~ goggles*end, data=swim)
model_b1

attach(swim)

anova(aov(time~factor(flippers)+factor(end)))
anova(aov(time~shirt*goggles*flippers+end))

install.packages("PBIBD")

apbibd(24, 3, 2, )
l <- c(1,1,2,2,)