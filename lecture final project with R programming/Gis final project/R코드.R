 library(ggmap)
 library(ggplot2)
 library(dplyr)
 library(data.table)
 library(rgdal)
 library(maptools)
 library(sp)
 library(stringr)
 
 # setwd(경로)
 #setwd("E:/GIS 과제/새 폴더")
 
 ########################################################
 # 건물 위치정보 
 ########################################################

buildingData<- fread('C:/Users/정재훈/Desktop/GIS 논문작성/gis1/서울시 건물 위치정보 (좌표계_ WGS1984).csv',encoding='UTF-8')
names(buildingData)
 
 # 상업시설 
commerce <- subset(buildingData,
                  `건물 용도코드` == 06100 |
                    `건물 용도코드` == 06201 |
                    `건물 용도코드` == 06202 |
                    `건물 용도코드` == 06203 |
                    `건물 용도코드` == 06204 |
                    `건물 용도코드` == 06205)
commerce$type <- 'commerce'
# commerce <- commerce[!duplicated(commerce[,c('건물본번', '건물부번')])]

# 학원
institute <- subset(buildingData,
                     `건물 용도코드` == 08003 |
                     `건물 용도코드` == 04010)
institute$type <- 'institute'
institute <- institute[!duplicated(institute[,c('건물본번', '건물부번')])]
 
# (초)중고등학교
school <- subset(buildingData,
                `건물 용도코드`==08101 |
                  `건물 용도코드`==08102 |
                  `건물 용도코드`==08103)
school$type <- 'school'
school <- school[!duplicated(school$'건물군 일련번호'),]


# 대학교 (전문대 포함)
university <- subset(buildingData,
                    `건물 용도코드` == 08104 |
                      `건물 용도코드` == 08105 | 
                      `건물 용도코드` == 08106)
university$type <- 'university'
# university <- university[!duplicated(university$`건물군 일련번호`),]

# 사무실 (공공기관 + 일반 사무실)
govern <- subset(buildingData,
                `건물 용도코드` == 10101 |
                  `건물 용도코드` == 10102)
condition <- (str_sub(buildingData$`건물 용도코드`, 1, 2) == '44')
private <- buildingData[condition,]
office <- rbind(govern, private)
office$type <- 'office'
office <- office[!duplicated(office[,c('건물본번', '건물부번')])]
#한 건물에 여러 회사가 들어오는 경우 고려 -> 건물본번, 부번 모두고려


# 공공주택(빌라, 아파트 ...)
condition <- (str_sub(buildingData$`건물 용도코드`, 1, 2) == '20')
apartment <- buildingData[condition, ]
apartment$type <- 'apartment'

apartment <- filter(apartment, apartment$지상층수 >= 10)
# 10층 이상 건물만 

# 지하철
subway <- fread('C:/Users/정재훈/Desktop/GIS 논문작성/gis1/서울시 역코드로 지하철역 위치 조회.csv',encoding='UTF-8')
names(subway)
subway <- select(subway, '전철역명', 'X좌표(WGS)', 'Y좌표(WGS)')
names(subway) <- c('건물명', '위도', '경도')
subway$type <- 'subway'
# 결측 수정 
subway <- subway[!is.na(subway$위도),]
############################################################################

# 구글 api 키 
register_google(key=key)
busstop <- fread("C:/Users/정재훈/Desktop/GIS 논문작성/gis1/서울특별시 버스정류소 위치정보.csv",encoding = "UTF-8")
 
# 병합을 위한 변수 
busstop$bsst_ars_no <- busstop$정류소번호
 
 
########################################################
# 도로 자료 시각화
########################################################
# location
long <- busstop$X좌표
lat  <- busstop$Y좌표
rg.long <- range(long)
rg.lat  <- range(lat)
mylocation <- c(rg.long[1]-0.01, rg.lat[1]-0.01,rg.long[2]+0.01, rg.lat[2]+0.01)
 
# map load
mymap <- get_map(location = mylocation, maptype = "roadmap")
g <- ggmap(mymap) +
 geom_point(aes(x = X좌표, y = Y좌표), data = busstop, col = "skyblue")
g

##**
# 도로 정보 load
filepath <- '도로 위치정보/TL_SPRD_MANAGE_W.shp'
road <- readOGR(filepath, encoding = 'UTF-8')
 
# 도로 계급은 1,2,3 까지
plot(subset(road, ROA_CLS_SE== 1 | ROA_CLS_SE == 3| ROA_CLS_SE == 2)) +
 points(busstop$X좌표, busstop$Y좌표, col = "skyblue", pch = 16)
 
########################################################
# 승하차인원 수 계산 
########################################################
data <- fread('C:/Users/정재훈/Desktop/GIS 논문작성/gis1/10월 버스 이용량 자료.csv',encoding = 'UTF-8')
data$bsst_ars_no <- as.numeric(data$bsst_ars_no)
data <- group_by(data, bsst_ars_no)
summ_data <- summarise(data, 
                      sum_ride_pasgr = sum(ride_pasgr_num ), 
                      sum_alight_pasgr = sum(alight_pasgr_num),) 
temp <- inner_join(busstop, summ_data, 'bsst_ars_no')
temp %>% is.na %>% colSums()
 
########################################################
# 건물 수 계산(건물위치)
########################################################
# 상업시설 
install.packages("geosphere")
library(geosphere)
matDist <- distm(commerce[,c('경도', '위도')], temp[,c('X좌표', 'Y좌표')])
temp$numComm <- (matDist < 400) %>% colSums()
# 학원
matDist <- distm(institute[,c('경도', '위도')],
                temp[,c('X좌표', 'Y좌표')])
temp$numInst <- (matDist < 400) %>% colSums()

# 초중고등학교
matDist <- distm(school[,c('경도', '위도')],
                  temp[,c('X좌표', 'Y좌표')])
temp$numSch <- (matDist < 400) %>% colSums()
 
# 대학교
matDist <- distm(university[,c('경도', '위도')],
                temp[,c('X좌표', 'Y좌표')])
temp$numUniv <- (matDist < 400) %>% colSums()

# 사무실
matDist <- distm(office[,c('경도', '위도')],
                temp[,c('X좌표', 'Y좌표')])
temp$numOffice <- (matDist < 400) %>% colSums()

# 아파트
matDist <- distm(apartment[,c('경도', '위도')],
                temp[,c('X좌표', 'Y좌표')])
temp$numApatm <- (matDist < 400) %>% colSums()

# 지하철 
matDist <- distm(subway[,c('경도', '위도')],
                temp[,c('X좌표', 'Y좌표')])
temp$numSubw <- (matDist < 400) %>% colSums()


########################################################
#회귀
#######################################################
# write.csv(temp, "trainData.csv", row.names = F)
temp <- read.csv("C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더/trainData.csv")
facil <- read.csv(file.choose())
subway <- read.csv(file.choose())

temp


temp

##시각화 

suppressPackageStartupMessages({
  library(dplyr)
  library(reshape2)
  library(GISTools)
  library(ggmap)
  #library(proj4)
  library(rgdal)
  #library(geoR)
  #library(automap)
  library(maptools)
  library(gstat)
  library(sp)
  library(viridis)
  library(mapproj)
  library(ggthemes)
  
})

register_google(key = '***')

temp %>% summary

x1.ln <- temp$X좌표
x2.lt <- temp$Y좌표
x1r = range(x1.ln)
x2r = range(x2.lt)
mylocation=c(x1r[1]+.02,x2r[1]-.08,x1r[2]+.02,x2r[2]-.08)
myMap <- get_map(location=mylocation, source="google", maptype='roadmap', zoom=11, color="bw")


ggmap(myMap) + geom_point(aes(x=X좌표, y=Y좌표), data = temp, alpha =.5, size=2, shape=3)





model <- lm(data = temp,log(sum_ride_pasgr+1) ~ +numInst+numComm+numSch+numUniv+numOffice+numSubw+numApatm)
summary(model)
plot(model)
temp[2:14]


hist(sum_ride_pasgr, typ = 'l')

shapiro.test(rstandard(model))
hist(model$residuals)
dwtest(data=temp, log(sum_ride_pasgr+1) ~ +numInst+numComm+numSch+numUniv+numOffice+numSubw+numApatm)
str(temp)

hist(log(sum_ride_pasgr))
shapiro.test(sum_ride_pasgr)

install.packages("nortest")
library(nortest)
nortest::ad.test(sum_ride_pasgr)

qqplot(sum_ride_pasgr)
boxcoxfit(sum_ride_pasgr)

temp

library(dplyr)
str(temp)

hist(log(sum_alight_pasgr))
temp1 <- temp %>% mutate(num = ifelse(log(sum_alight_pasgr) > 10, 1, 0))
temp1


temp %>% mutate(if(sum_alight_pasgr>5000){
  num <- 1
  } else if (sum_alight_pasgr<=5000)
    num <- 0)

coordinates(testdata) <- ~ LON + LAT

temp
?mutate
plot(dvgm)
model2 <- glm(num ~ numInst+numComm+numSch+numUniv+numOffice+numSubw+numApatm, family = binomial, data = temp1)
plot(model2)
summary(model2)
model2$residuals
step(model2)

dvgm <- variogram(resid~1, data=traindata)
dfit <- fit.variogram(dvgm, model=vgm(0.01, "Mat", 0.03, 0.1)) # fit model
model2 <- krige(num ~ numInst+numComm+numSch+numUniv+numOffice+numSubw+numApatm, traindata, testdata, model=dfit, data=temp2)

model2.fit

model1 <- glm(sum_ride_pasgr ~ numInst+numComm+numSch+numUniv+numOffice+numSubw+numApatm, family = binomial)
sum_ride_pasgr[sum_ride_pasgr==0]
model1
summary(model1)
step(model1)
?glm

plot(model1$residuals)
var.test(model1$residuals)
plot(model1)
Bre

model1$residuals
model1

stepstep <- step(model)
glm.fit()
model1$residuals>10
model1

## 독립성 가정 위배..


####################################
#시각화
####################################

colnames(temp)
colnames(temp)[3:4] <- c('lon','lat')

x1.ln=temp$lon
x2.lt=temp$lat
x1r = range(x1.ln)
x2r = range(x2.lt)
mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])
myMap <- get_map(location=mylocation, source="google", maptype="roadmap",zoom=11,color="bw")

ggmap(myMap)

ggmap(myMap)+geom_point(aes(x = lon, y = lat), data = temp)

par(mfrow=c(3,1))

ggmap(myMap) + stat_density_2d(aes(fill=..level.., colour='bsst_ars_no'),geom="polygon", data = temp, alpha = .2, size = 1) +
  geom_point(aes(x=lon,y=lat),data=analysisData,alpha=0.4,color="black",size=0.5) +
  coord_equal() + scale_fill_continuous(low = "lightseagreen",high="tomato",name="Density")

ggmap(myMap) + stat_density_2d(aes(fill=..level.., colour='sum_ride_pasgr'),geom="polygon", data = temp, alpha = .2, size = 1) +
  geom_point(aes(x=lon,y=lat),data=temp,alpha=0.4,color="black",size=0.5) +
  coord_equal() + scale_fill_continuous(low = "lightseagreen",high="tomato",name="Density")

ggmap(myMap) + stat_density_2d(aes(fill=..level.., colour='sum_alight_pasgr'),geom="polygon", data = temp, alpha = .2, size = 1) +
  geom_point(aes(x=lon,y=lat),data=temp,alpha=0.4,color="black",size=0.5) +
  coord_equal() + scale_fill_continuous(low = "lightseagreen",high="tomato",name="Density")

ggmap(myMap) + geom_contour(data = temp, aes(x = lon, y = lat, z = sum_ride_pasgr)) +
  stat_density2d(data= temp, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins =16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25),guide = FALSE) + 
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

############################################################################################
colnames(temp)
temp$cut <- cut(temp$sum_ride_pasgr, breaks=seq(0,30000,5000))
temp$cut 

temp$bsst_ars_no

gg <- ggmap(myMap)
gg <- gg + stat_density2d(data=temp, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=5)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
gg <- gg + facet_wrap(~numApatm, ncol=3)
gg <- gg <- gg + labs(x=NULL, y=NULL, title="서울시버스 이용현황\n")
gg <- gg + theme_map(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="center")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg


##################################################################################
colnames(temp)
temp$cut1 <- cut(temp$subway, breaks=seq(0,16,4))
temp$sum_alight_pasgr %>% summary 
temp$bsst_ars_no %>% summary
temp$sum_ride_pasgr  %>% summary
temp$numApatm %>% summary
temp$cut 

gg <- ggmap(myMap)
gg <- gg + stat_density2d(data=temp, aes(x=lon, y=lat, fill=..level.., alpha=..level..),
                          geom="polygon", size=0.01, bins=5)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg <- gg + labs(x=NULL, y=NULL, title="서울시 하차 객수 \n")
gg <- gg + theme_map(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="center")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg

####################################################################################
temp$cut %>% summary
head(temp)

filepath = "C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더/SIG_201703/TL_SCCO_SIG.shp"
area = readOGR(filepath)

area1 = subset(area, subset = (substr(area$SIG_CD,1,2) == "11")) %>% fortify(region="SIG_KOR_NM")
area1 %>% 
  ggplot(aes(x = long, y = lat, group = id)) + geom_path() + theme_bw() 


