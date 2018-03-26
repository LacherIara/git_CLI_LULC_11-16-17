############################ 
#PURPOSE:  Do Combine and Hist for **NLCD Land Cover years 2001 & 2012**. Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer. # check
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape. # check
#DEVELOPED: 
	#	V1	2/18/2016 - Valentine Herrmann 
	#	V2	5/2016 - Valentine Herrmann 
	#	V3	6/1/2016 - Iara Lacher - Created raster with persistent land use in addition to changes in land use.
	#	V4	3/30/2017 - Iara Lacher - Loop through recent trends and scenarios 50 years out

#CONTACT: LacherI@si.edu
#NOTES: 
# Time difference of 14.52866 mins

# This script would be the equivalent of:
#   - opening ArcMap, adding cblcd_92_an.img, cblcd_01_an.img and cblcd_1_an.img
#   - using "combine" on cblcd_92_an.img and cblcd_01_an.img, then on cblcd_01_an.img and  cblcd_11_an.img to create raster comb_9201 and comb_0111
#   - using "zonal histogram " on comb_9201 and cblcd_01_an.img
#   - using "zonal histogram " on comb_0111 and cblcd_11_an.img
#   - merge zonal histogram into on table 
#   - expoRT merged table to .txt files

#IMPORTANT: 
# For version 2012, use nlcd landcover data WITH protected lands 

##### NEXT STEPS #####
# add compatibility to code for creating transitions raster with multiple landscapes (second suite of code on page)


############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER

# ----------------------------------------------
################################################


# PACKAGES NEEDED
library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(raster) # read and edit rasters


# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/RTempCLEARME/")


# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
cntyRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/"

# inRasterLoc <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/FutureLandscapes/RT/test_0711/"
inRasterLoc <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/FutureLandscapes/RT/"


# Define paths for output files
Comb_output <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/BasicDataAnalyses/Tables/region2/" # Combine and final histogram outputs for each landscape


# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Land Cover Rasters:
# nl01 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd01_anC.img")
# nl06 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd06_anC.img")
# nl11 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd11_anC.img")

# COUNTY RASTERS
regions <- raster(paste(cntyRasterLoc, "region_an", ".img", sep="")) # this is for the raster.
counties_vals <- getValues(regions)

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv")


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) 

#select only the Study Area counties to run basic analyses
S20_GEOID <-  read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V:


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

LS_trans <-  list('LS01' = c("v2012_RT_Landscape01.tif"),'LS02' = c("v2012_RT_Landscape02.tif"),'LS03' = c("v2012_RT_Landscape03.tif"),'LS04' = c("v2012_RT_Landscape04.tif"),'LS05' = c("v2012_RT_Landscape05.tif"))




# ------------------------------------------------------
# ------------------------------------------------------
# ZONAL HISTOGRAM ONLY (not transitions)
# ------------------------------------------------------
# ------------------------------------------------------

# ----------------------------------------------
# Loop through transitions
# ----------------------------------------------
old <- Sys.time() # TIMING SCRIPT

for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?

Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) 
Final_hist_output <- paste0(Comb_output, gsub(".tif","_hist.txt",LS_trans[[in_to_fin]][1]))
Final_Landscape <- raster(Final_Landscape)
fin_vals <- getValues(Final_Landscape)

# ZONAL HISTOGRAM ON FINAL LANDSCAPE 
# ----------------------------------------------
# ----------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select desired transitions 
final <- ifelse(fin_vals %in%  c("3","5","6","7"), fin_vals, 0)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#add county values
final <- paste0(final, ".", counties_vals)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#summarise pixels
final_hist0 <- summary(factor(final), maxsum = length(unique(final))) # had to add mmaxsum = length(unique(final)) because otherwise doesn't show all possible values and there is a column called "others" - Val

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#remove values that we don't want
final_hist0 <- final_hist0[-grep("^(-?)0.", names(final_hist0))]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reorganize #CF- create table with county as row, transition as column
final_hist <- sa_ctyGEOID

for(i in final_hist$Din_cty){
 
 for (j in c("3","5","6","7")){ 
   if(any(names(final_hist0) == paste(j, i, sep = "."))){
     final_hist[final_hist$Din_cty == i, as.character(j)] <- final_hist0[names(final_hist0) == paste(j, i, sep = ".")]
   }}}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table #CF- create table with county as column, transition as row
final_hist <- t(final_hist[-1])
colnames(final_hist) <- paste0("GEOID_", final_hist[1,])
final_hist <- final_hist[-1,]
final_hist <- cbind(data.frame(Rowid_ = 1:nrow(final_hist),LABEL = rownames(final_hist)), final_hist)
final_hist<-arrange(final_hist, LABEL)   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs #CF- conveRT NA to 0
final_hist[is.na(final_hist)]<-0

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(final_hist, file = Final_hist_output, row.names=FALSE, col.names=TRUE, sep=",") 
}

new<-Sys.time()-old # TIMING SCRIPT
print(new) # Time difference of 6.742629 mins

