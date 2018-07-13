############################ 
#PURPOSE: Calculate the core area of forest for v2016
#INPUT: Forest Land cover rasters 
#OUTPUT: 
#DEVELOPED: 7-4-18
#CONTACT: halperin@si.edu
#NOTES:

# PACKAGES NEEDED 
library(raster)
library(rgdal)
library(tmap) 
library(sp)
library(maptools)
library(rgeos)

#WORKING DIRECTORY
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/")

#ForestFolder<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon"
NL01file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/NL0105_ForestPoly2.shp"
NL11file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/NL1105_ForestPoly2.shp"
Q1file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/Q105_ForestPoly.shp"
Q2file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/Q205_ForestPoly.shp"
Q3file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/Q305_ForestPoly.shp"
Q4file<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/Q405_ForestPoly.shp"
RTfile<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/Polygon/RT05_ForestPoly.shp"

#NL01
NL01<-raster::shapefile(NL01file) #read in shapefile
NL01Core<-gBuffer(NL01, width=-60) #create a negative buffer of 60m 
NL01Core2<-as(NL01Core,"SpatialPolygonsDataFrame") #turn into a spatialpolygonsdataframe
writeOGR(obj=NL01Core2, ".", layer="NL01Core3", driver="ESRI Shapefile" ) #export the file

#NL11
NL11<-raster::shapefile(NL11file)
NL11Core<-gBuffer(NL11, width=-60)
NL11Core2<-as(NL11Core,"SpatialPolygonsDataFrame")
writeOGR(obj=NL11Core2, ".", layer="NL11Core3", driver="ESRI Shapefile" )

#Q1
Q1<-raster::shapefile(Q1file)
Q1Core<-gBuffer(Q1, width=-60)
Q1Core2<-as(Q1Core,"SpatialPolygonsDataFrame")
writeOGR(obj=Q1Core2, ".", layer="Q1Core2", driver="ESRI Shapefile" )


#Q2
Q2<-raster::shapefile(Q2file)
Q2Core<-gBuffer(Q2, width=-60)
Q2Core2<-as(Q2Core,"SpatialPolygonsDataFrame")
writeOGR(obj=Q2Core2, ".", layer="Q2Core2", driver="ESRI Shapefile" )

#Q3
Q3<-raster::shapefile(Q3file)
Q3Core<-gBuffer(Q3, width=-60)
Q3Core2<-as(Q3Core,"SpatialPolygonsDataFrame")
writeOGR(obj=Q3Core2, ".", layer="Q3Core2", driver="ESRI Shapefile" )

#Q4
Q4<-raster::shapefile(Q4file)
Q4Core<-gBuffer(Q4, width=-60)
Q4Core2<-as(Q4Core,"SpatialPolygonsDataFrame")
writeOGR(obj=Q4Core2, ".", layer="Q4Core2", driver="ESRI Shapefile" )

#RT
RT<-raster::shapefile(RTfile)
RTCore<-gBuffer(RT, width=-60)
RTCore2<-as(RTCore,"SpatialPolygonsDataFrame")
writeOGR(obj=RTCore2, ".", layer="RTCore2", driver="ESRI Shapefile" )