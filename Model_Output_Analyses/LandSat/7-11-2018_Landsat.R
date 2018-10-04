
library(raster)
library(rgdal)
library(tmap) 
library(sp)
library(maptools)
library(rgeos)






Folder<-list.files("U:/CLI/SpatialData/VAClipRaw/VA_Satellite/Landsat/2017/January/LC08_1634_01_JAN_2017_TEST/TIF/", pattern=".TIF", full.names = TRUE) 
RasterList<-lapply(Folder,function(i){
  raster(i)
})



bb<-extent(609285, 837315, 4192785, 4424715)

extent(RasterList[[10]])<-bb

RasterList[[10]]<-setExtent(RasterList[[10]],bb)

res(RasterList[[10]])<-c(30,30)

dim(RasterList[[10]])<-c(7731, 7601, 58763331)

RasterStack<-brick(RasterList)

setwd("U:/CLI/SpatialData/VAClipRaw/VA_Satellite/Landsat/2017/January/LC08_1634_01_JAN_2017_TEST/TIF/")

writeRaster(RasterStack, filename = "Jan_01_2017_1634Stack", format="GTiff")

