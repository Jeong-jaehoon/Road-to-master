rm(list=ls(all=TRUE))
install.packages("pscl")
install.packages("MASS")
install.packages("splm")
install.packages("plm")
install.packages("car")

library(pscl)
library(MASS)
library(splm)
library(plm)
library(car)


timing <- proc.time()[3]

source("E:/석사논문/2016/이종현/pgm/spfeml.r")
source("E:/석사논문/2016/이종현/pgm/splaglm.r")
source("E:/석사논문/2016/이종현/pgm/sperrorlm.r")
source("E:/석사논문/2016/이종현/pgm/impacts.splm.r")


#######################################################
############## 데이터 reading

data0 <- read.csv("E:/석사논문/2016/data2015(fin)ver3.csv")
data0 <- data0[,-1]
data0$풍향<- as.factor(data0$풍향)


########## seasonality 추가 ##########

month <- substr(data0$관측일시,5,6)
year <- substr(data0$관측일시,1,4)

m.cos <- cos(2*pi*(as.numeric(month)+1)/12)
m.sin <- sin(2*pi*(as.numeric(month)+1)/12)


#######################################################
############## Weights matrix

a <- read.table("whole.txt")  
W <- as.matrix(a[1:25,1:25])  #1~25까지가 서울


#for(i in 1:25){
#W[i,] <- W[i,]/sum(W[i,])
#}


#######################################################
############## Renaming

#dates <- data0[,1]
dates <- rep(seq(1:360),5)
regions <- data0[,3]
PM2.5 <- data0[,9]
NO2 <- data0[,4] 
O3 <- data0[,5]
CO <- data0[,6]
SO2 <- data0[,7]
PM10 <- data0[,8]
mean.temp <- data0[,10]
mn.temp <- data0[,11]
mx.temp <- data0[,12]
mean.humid <- data0[,13]
mn.humid <- data0[,14]
mx.humid <- data0[,15]
mean.wdspeed <- data0[,16]
max.wdspeed <- data0[,17]
precips <- data0[,18]
wd.dir <- data0[,20]


new.data <- data.frame(regions,dates,PM2.5,NO2,O3,CO,SO2,PM10,mean.temp,mn.temp,mx.temp,mean.humid,mn.humid,mx.humid,mean.wdspeed,max.wdspeed,precips,wd.dir,m.cos,m.sin)


########################################
######################### Analysis 1

fm <- log(PM2.5) ~ NO2+O3+CO+SO2+PM10+mean.temp+mn.temp+mx.temp+mean.humid+mn.humid+mx.humid+mean.wdspeed+max.wdspeed+precips+wd.dir+m.cos+m.sin

fm <- log(PM2.5) ~ NO2+O3+CO+SO2+PM10+mean.humid+max.wdspeed+precips+wd.dir+m.cos+m.sin

fm <- log(PM2.5) ~ NO2+O3+CO+SO2+PM10

fm <- log(PM2.5) ~ NO2+O3+CO+SO2+PM10+mn.humid+mean.wdspeed+precips+wd.dir+m.cos+m.sin

fm <- log(PM2.5) ~ NO2+O3+SO2+mn.humid+mean.wdspeed+precips+wd.dir+m.cos+m.sin


############## Panel model 

ols <- lm(fm, data=new.data)

fixed <- plm(fm, data=new.data, index=c("regions", "dates"), model="within")

pFtest(fixed, ols)

### reject H0 means fixed is better than ols

random <- plm(fm, data=new.data, index=c("regions", "dates"), model="random")

phtest(fixed, random)

### reject H0 means fixed is better than random


######### final model

summary(random)

########################################################
#### Spatial Panel model


#### original model
model.0 <- spml(fm, data=new.data, listw=mat2listw(W), model="random", lag=FALSE, spatial.error="none")
summary(model.0)

summary(model.0)$rsqr
summary(model.0)$logLik


#### SDM model
model.1 <- spml(fm, data=new.data, listw=mat2listw(W), model="random", lag=TRUE, spatial.error="b")
summary(model.1)

summary(model.1)$rsqr
summary(model.1)$logLik


#### SEM model 
model.2 <- spml(fm, data=new.data, listw=mat2listw(W), model="random", lag=FALSE, spatial.error="b")
summary(model.2)

summary(model.2)$rsqr
summary(model.2)$logLik


#### SAR model 
model.3 <- spml(fm, data=new.data, listw=mat2listw(W), model="random", lag=TRUE, spatial.error="none")
summary(model.3)

summary(model.3)$rsqr
summary(model.3)$logLik


###################### Testing

#### test for SEM  (LM spatial error)
test.1 <- sphtest(fm,data=new.data, listw=mat2listw(W),spatial.model="error",method="GM")

#### test for SDM  (LM spatial lag)
test.2 <- sphtest(fm,data=new.data, listw=mat2listw(W),spatial.model="sarar",method="GM")

#### test for SAR  (LM spatial error)
test.3 <- sphtest(fm,data=new.data, listw=mat2listw(W),spatial.model="lag",method="GM")




############## Impacts analysis

model <- model.1

times <- length(unique(years))
s.lw <- kronecker(Diagonal(times),W)
trMatc <- trW(s.lw,type="mult")

imp <- impacts(model, tr = trMatc, R = 200)
summary(imp,zstats=T,short=T)

#imp <- impacts.splm(model.1,listw=mat2listw(W,style="W"),time=7)
#summary(imp,zstats=T,short=T)


