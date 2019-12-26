library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(rgdal)
library(maptools)
library(raster)
library(gstat)
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

convertCoordSystem <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}

setwd("D:/GIS 과제")
analysisData <- read.csv('C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더 (2)/analysisData.csv')

to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
from.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
temp_coor <- convertCoordSystem(analysisData$X좌표, analysisData$Y좌표,from.crs, to.crs )
analysisData$X좌표 <- temp_coor$long
analysisData$Y좌표 <- temp_coor$lat
coordinates(analysisData) <- ~ `X좌표` + `Y좌표`




area = readOGR('C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더 (2)/SIG_201905/TL_SCCO_CTPRVN.shp')
area <- subset(area, subset = (area$CTPRVN_CD) == "11")
plot(area)

area_Gu = readOGR('C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더/SIG_201703/TL_SCCO_SIG.shp')
area_Gu <- subset(area_Gu, subset = substr(area_Gu$SIG_CD,1,2) == "11")
plot(area_Gu)

study_area <- area %>% 
  disaggregate %>% 
  geometry
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)

size <- 800
r <- raster(study_area, resolution = size)

# r.grid <- as(r, "SpatialPolygons")
# r.grid <- r.grid[study_area,]
# row.names(r.grid) <- as.character(1:length(r.grid))
# plot(study_area, col = "grey50", bg = "light blue", axes = FALSE)
# plot(r.grid, border = "orange", add = TRUE)
nc <- rasterize(analysisData, r,
                field=c("sum_pasgr","comPop","housePop","numSubw","billaPop","aptPop","numHosp","numMsch","numHsch","numUniv"),
                fun=function(x,...){mean(x)}, background=0)
nc1 <- rasterize(analysisData, r,
                 field=c("housePop"),
                 fun=function(x,...){mean(x)}, background=0)

plot(nc)
plot(nc1)
plot(area_Gu,add = T)

temp <- as.data.frame(nc@data@values)
colnames(temp) <-nc@data@names
coordinates(temp) <- coordinates(nc)

temp2 <- temp[temp$sum_pasgr!=0,]
corrplot::corrplot(cor(temp2@data))

model <- lm(data = temp2, log(sum_pasgr)~comPop+housePop+numSubw+billaPop+numHosp+numMsch+numHsch+numUniv)
summary(model)

step(model)

model2 <- lm(data = temp2, log(sum_pasgr)~numSubw+numHosp+numHsch)
summary(model2)


model3 <- lm(data = temp2, log(sum_pasgr)~comPop+numSubw+numHosp+numHsch+numUniv)
summary(model3)

plot(model3)



temp2$resid <- model3$residuals
dvgm <- variogram(resid ~ 1, data = temp2, alpha = c(0,45,90,135))
plot(dvgm)
dvgm <- variogram(resid ~ 1, data = temp2)
plot(dvgm)
dfit <- fit.variogram(dvgm, model = vgm(0.1,'Mat', 5000, 0.3))
dfit
plot(dvgm,dfit,as.table = T)


#########################################################################
#시각화
#########################################################################

library(dplyr)

temp <- read.csv("C:/Users/정재훈/Desktop/2019학년도 2학기/GIS/GIS 프로젝트/새 폴더/trainData.csv")
colnames(temp)[3:4] <- c('lon','lat')
x1.ln <- temp$lon
x2.lt <- temp$lat
x1r = range(x1.ln)
x2r = range(x2.lt)
mylocation=c(x1r[1],x2r[1],x1r[2],x2r[2])
myMap <- get_map(location=mylocation, source="google", maptype='roadmap', zoom=11, color="bw")


par(mfrow=c(3,3))
hist(temp$sum_ride_pasgr)
hist(temp$sum_alight_pasgr)
hist(temp$numComm)
hist(temp$numApatm)
hist(temp$numInst)
hist(temp$numSch)
hist(temp$numUniv)
hist(temp$numOffice)
hist(temp$numSubw)

visualization_func <- fucntion(cut_dat, a, b){
  temp$cut <- cut(cut_dat, breaks=seq(0,a,b))
  gg <- ggmap(myMap)
  gg <- gg + stat_density2d(data=temp, aes(x=lon, y=lat, fill=..level.., alpha=..level..), 
                            geom="polygon", size=0.01, bins=5)
  gg <- gg + scale_fill_viridis()
  gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
  gg <- gg + coord_map()
  gg <- gg + facet_wrap(~cut4, ncol=3)
  gg <- gg <- gg + labs(x=NULL, y=NULL, title="Seoul bus\n")
  gg <- gg + theme_map(base_family="sans")
  gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
  gg <- gg + theme(panel.margin.x=unit(1, "cm"))
  gg <- gg + theme(panel.margin.y=unit(1, "cm"))
  gg <- gg + theme(legend.position="right")
  gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
  gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
  gg
}



gg <- ggmap(myMap)
gg <- gg + stat_density2d(data=temp, aes(x=lon, y=lat, fill=..level.., alpha=..level..), 
                          geom="polygon", size=0.01, bins=5)
gg <- gg + scale_fill_viridis()
gg <- gg + scale_alpha(range=c(0.2, 0.4), guide=FALSE)
gg <- gg + coord_map()
gg <- gg + facet_wrap(~cut, ncol=3)
gg <- gg <- gg + labs(x=NULL, y=NULL, title="Seoul bus\n")
gg <- gg + theme_map(base_family="sans")
gg <- gg + theme(plot.title=element_text(face="bold", hjust=1))
gg <- gg + theme(panel.margin.x=unit(1, "cm"))
gg <- gg + theme(panel.margin.y=unit(1, "cm"))
gg <- gg + theme(legend.position="right")
gg <- gg + theme(strip.background=element_rect(fill="white", color="white"))
gg <- gg + theme(strip.text=element_text(face="bold", hjust=0))
gg

####도시계획의 
temp %>% summary
