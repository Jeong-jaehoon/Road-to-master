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
analysisData <- read.csv('analysisData.csv')

to.crs = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
from.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
temp_coor <- convertCoordSystem(analysisData$X좌표, analysisData$Y좌표,from.crs, to.crs )
analysisData$X좌표 <- temp_coor$long
analysisData$Y좌표 <- temp_coor$lat
coordinates(analysisData) <- ~ `X좌표` + `Y좌표`








area = readOGR('CTPRVN_201905/TL_SCCO_CTPRVN.shp')
area <- subset(area, subset = (area$CTPRVN_CD) == "11")
plot(area)

area_Gu = readOGR('SIG_201703/TL_SCCO_SIG.shp')
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
plot(nc)
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

library(gstat)
temp2$resid <- model3$residuals
dvgm <- variogram(resid ~ 1, data = temp2, alpha = c(0,45,90,135))
plot(dvgm)
dvgm <- variogram(resid ~ 1, data = temp2)
plot(dvgm)
dfit <- fit.variogram(dvgm, model = vgm(0.1,'Mat', 5000, 0.3))
dfit
plot(dvgm,dfit,as.table = T)








