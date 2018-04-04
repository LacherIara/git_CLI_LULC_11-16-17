<<<<<<< HEAD
############################ 
#PURPOSE: TESTSTESTS  Do Combine and Hist for **NLCD Land Cover years 2001 & 2012**. Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer. # check
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape. # check
#DEVELOPED: 
	#	V1	2/18/2016 - Valentine Herrmann 
	#	V2	5/2016 - Valentine Herrmann 
	#	V3	6/1/2016 - Iara Lacher - Created raster with persistent land use in addition to changes in land use.
	#	V4	3/30/2017 - Iara Lacher - Loop through recent trends and scenarios 50 years out
  # V5 4/3/2018 - Sarah Halperin - Options for each scenario over 5 timesteps 

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


### NEXT STEPS####
# add code to loop through different scenarios and comparison plots of scenarios based on development type over the timesteps #Updated by Sarah Halperin 
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
library(rgdal)
library(reshape) #manipulation of output tables 
library(ggplot2) #graphs 

# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/RTempCLEARME/")


# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
cntyRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR RT

inRasterLoc <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/RT/" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q1
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q1/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q2
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q2/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q3
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q3/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q4
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q4/"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR ALL SCENARIOS 
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/"
#Folders <-  list('Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"))
#inRasterLoc <-paste0(inRasterLoc, Folders)

# Define paths for output files

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ADJUST FOR DESURED FOLDER 
Comb_output<-"I:/Test/RT/" #Sarah Halperin test. Can be deleted. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON RESPECTIVE OUTPUT FOLDER 
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/All/"

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Land Cover Rasters:
# nl01 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd01_anC.img")
# nl06 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd06_anC.img")
# nl11 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd11_anC.img")

# COUNTY RASTERS #currently set to regions 
regions <- raster(paste(cntyRasterLoc, "regions_StudyArea", ".tif", sep="")) # this is for the raster.
counties_vals <- getValues(regions) #defining the region 
#NOTE: Craig said the only correct region raster was region_an2. I updated to region_an2 was region_an. I changed it to counties (regions_StudyArea.tif)

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv") #Geological ID for the region. May want to change to say region in the future. 


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) #values 1-8 for the region. 

#select only the Study Area counties to run basic analyses
S20_GEOID <-  read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

#TURN ON FOR RT
LS_trans <-  list('LS01' = c("v2015_RT_Landscape01.tif"),'LS02' = c("v2015_RT_Landscape02.tif"),'LS03' = c("v2015_RT_Landscape03.tif"),'LS04' = c("v2015_RT_Landscape04.tif"),'LS05' = c("v2015_RT_Landscape05.tif")) #Each of the different future landscapes for the different timesteps. 

#TURN ON FOR Q1
#LS_trans <-  list('LS01' = c("v2015_Q1_Landscape01.tif"),'LS02' = c("v2015_Q1_Landscape02.tif"),'LS03' = c("v2015_Q1_Landscape03.tif"),'LS04' = c("v2015_Q1_Landscape04.tif"),'LS05' = c("v2015_Q1_Landscape05.tif"))


#TURN ON FOR Q2
#LS_trans <-  list('LS01' = c("v2015_Q2_Landscape01.tif"),'LS02' = c("v2015_Q2_Landscape02.tif"),'LS03' = c("v2015_Q2_Landscape03.tif"),'LS04' = c("v2015_Q2_Landscape04.tif"),'LS05' = c("v2015_Q2_Landscape05.tif"))

#TURN ON FOR Q3
#LS_trans <-  list('LS01' = c("v2015_Q3_Landscape01.tif"),'LS02' = c("v2015_Q3_Landscape02.tif"),'LS03' = c("v2015_Q3_Landscape03.tif"),'LS04' = c("v2015_Q3_Landscape04.tif"),'LS05' = c("v2015_Q3_Landscape05.tif"))


#TURN ON FOR Q4
#LS_trans <-  list('LS01' = c("v2015_Q4_Landscape01.tif"),'LS02' = c("v2015_Q4_Landscape02.tif"),'LS03' = c("v2015_Q4_Landscape03.tif"),'LS04' = c("v2015_Q4_Landscape04.tif"),'LS05' = c("v2015_Q4_Landscape05.tif"))

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

Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) #full file path using inRasterLoc as the base
Final_hist_output <- paste0(Comb_output, gsub(".tif","_hist.txt",LS_trans[[in_to_fin]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
Final_Landscape <- raster(Final_Landscape) 
fin_vals <- getValues(Final_Landscape) 
  #plot of different land use types. 

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
#Reorganize #CF- create table with county as row, landuse as column
final_hist <- sa_ctyGEOID

for(i in final_hist$Din_cty){
 
 for (j in c("3","5","6","7")){ 
   if(any(names(final_hist0) == paste(j, i, sep = "."))){
     final_hist[final_hist$Din_cty == i, as.character(j)] <- final_hist0[names(final_hist0) == paste(j, i, sep = ".")]
   }}}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table #CF- create table with county as column, landuse as row
final_hist <- t(final_hist[-1])
colnames(final_hist) <- paste0("GEOID_", final_hist[1,])
final_hist <- final_hist[-1,]
final_hist <- cbind(data.frame(Rowid_ = 1:nrow(final_hist),LABEL = rownames(final_hist)), final_hist)
final_hist<-arrange(final_hist, LABEL)   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs #CF- conveRT NA to 0
final_hist[is.na(final_hist)]<-0

#result is for first timestep
#need it to iteratate through the scenarios --> list for RT and paste that goes through that
#want all the time steps to then combine to a new table 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(final_hist, file = Final_hist_output, row.names=FALSE, col.names=TRUE, sep=",")  
}


new<-Sys.time()-old # TIMING SCRIPT
print(new) # Time difference of 6.276551 mins
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#START HERE IF TABLES PRODUCED ALREADY AND ONLY WANT TO RESHAPE AND PRODUCE GRAPHS 


#~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINED TABLES 

#Used as a TEST can be deleted 
Folder<-list.files("I:/Test/RT", pattern=".txt", full.names = TRUE) #Read in RT files 
Tables<-lapply(Folder,function(i){
  read.csv(i)
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR RT
#NOTE: I set up to link to the output folder, but this may need to be changed 
#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
  #read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#BRING IN TABLES FOR Q1
#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q2

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q3

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q4

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#cOMPARISON ACROSS LAND COVER TYPES OVER TIME

#Add TimeSteps in order to track them when reshaping the table if desired 
Tables[[1]]$TimeStep<-1
Tables[[2]]$TimeStep<-2
Tables[[3]]$TimeStep<-3
Tables[[4]]$TimeStep<-4
Tables[[5]]$TimeStep<-5

Combined<-do.call(rbind.data.frame,Tables)

#Land Cover Type #3 
LABEL3<-Combined %>%
  filter(LABEL==3)
LABEL3<-LABEL3[,3:11]
  LABEL3<-t(LABEL3)
LABEL3<-LABEL3[1:8,]
colnames(LABEL3)<-c("T1.3","T2.3","T3.3","T4.3","T5.3")
#Land Cover Type #5
LABEL5<-Combined %>%
  filter(LABEL==5)
LABEL5<-LABEL5[,3:11]
LABEL5<-t(LABEL5)
LABEL5<-LABEL5[1:8,]
colnames(LABEL5)<-c("T1.5","T2.5","T3.5","T4.5","T5.5")
#Land Cover Type #6
LABEL6<-Combined %>%
  filter(LABEL==6)
LABEL6<-LABEL6[,3:11]
LABEL6<-t(LABEL6)
LABEL6<-LABEL6[1:8,]
colnames(LABEL6)<-c("T1.6","T2.6","T3.6","T4.6","T5.6")
#Land Cover Type #7
LABEL7<-Combined %>%
  filter(LABEL==7)
LABEL7<-LABEL7[,3:11]
LABEL7<-t(LABEL7)
LABEL7<-LABEL7[1:8,]
colnames(LABEL7)<-c("T1.7","T2.7","T3.7","T4.7","T5.7")


#Save CSV
CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
write.csv(CombinedReshape, paste0(Comb_output,"CombinedReshape.csv"))

#GGplot Facet by Region
CombinedMelt<-melt(Combined, id=c("Rowid_", "LABEL", "TimeStep" ))
CombinedMelt$LABEL<-as.factor(CombinedMelt$LABEL) #turn Label to factor from integer

windows()
ggplot(CombinedMelt, aes(x=TimeStep, y=value, colour=LABEL, group=LABEL))+
  geom_line()+
  facet_grid(.~variable, scales="free")
  

#Sum by Region 
region_sum<-aggregate(value~LABEL+TimeStep,CombinedMelt, sum)

#Ggplot summed over each region 
windows()
ggplot(region_sum, aes(x=TimeStep, y=value, colour=LABEL, group=LABEL))+
  geom_line()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Save CSV needed for graphs. NEED TO CHANGE BASED ON INPUT
write.csv(CombinedMelt, paste0(Comb_output,"CombinedMelt.csv"), row.names=FALSE)
write.csv(region_sum, paste0(Comb_output,"Region_sum.csv"), row.names=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TABLES COMPARED ACROSS SCENARIOS OVER TIME 
#TURN ON FOR DESIRED OUTPUT CSV
OutputRT<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT/"
OutputQ1<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1/"
OutputQ2<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2/"
OutputQ3<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3/"
OutputQ4<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4/"


##Used as a TEST can be deleted 
Folder<-list.files("I:/Test/Old/ALL/Melt", pattern=".csv", full.names = TRUE) #Read in RT files 
CSV_Melt<-lapply(Folder,function(i){
  read.csv(i)
})

##Used as a TEST can be deleted 
Folder<-list.files("I:/Test/Old/ALL/Region", pattern=".csv", full.names = TRUE) #Read in RT files 
CSV_Region<-lapply(Folder,function(i){
  read.csv(i)
})

#read in CombinedMelt.csv
CSV_MeltRT<-read.csv(paste0(OutputRT,"CombinedMelt.csv"))
CSV_MeltQ1<-read.csv(paste0(OutputQ1,"CombinedMelt.csv"))
CSV_MeltQ2<-read.csv(paste0(OutputQ2,"CombinedMelt.csv"))
CSV_MeltQ3<-read.csv(paste0(OutputQ3,"CombinedMelt.csv"))
CSV_MeltQ4<-read.csv(paste0(OutputQ3,"CombinedMelt.csv"))


#ADD SCENARIO 
CSV_Melt[[1]]$Scenario<-"Q1"
CSV_Melt[[2]]$Scenario<-"Q2"
CSV_Melt[[3]]$Scenario<-"Q3"
CSV_Melt[[4]]$Scenario<-"Q4"
CSV_Melt[[5]]$Scenario<-"RT"

CombinedMeltLC<-do.call(rbind.data.frame,CSV_Melt)

CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegionLC<-do.call(rbind.data.frame,CSV_Region)


#GGplot Facet by Region
windows()
ggplot(CombinedMeltLC, aes(x=TimeStep, y=value, colour=Scenario, group=Scenario))+
  geom_line()+
  facet_grid(variable~LABEL, scales="free")

#Ggplot Facet by development 
windows()
ggplot(CombinedRegionLC, aes(x=TimeStep, y=value, colour=Scenario, group=Scenario))+
  geom_line()+
  facet_grid(.~LABEL, scales="free")
=======
############################ 
#PURPOSE:  Do Combine and Hist for **NLCD Land Cover years 2001 & 2012**. Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer. # check
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape. # check
#DEVELOPED: 
	#	V1	2/18/2016 - Valentine Herrmann 
	#	V2	5/2016 - Valentine Herrmann 
	#	V3	6/1/2016 - Iara Lacher - Created raster with persistent land use in addition to changes in land use.
	#	V4	3/30/2017 - Iara Lacher - Loop through recent trends and scenarios 50 years out
  # V5 4/3/2018 - Sarah Halperin - Options for each scenario over 5 timesteps 

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


### NEXT STEPS####
# add code to loop through different scenarios and comparison plots of scenarios based on development type over the timesteps #Updated by Sarah Halperin 
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
library(rgdal)
library(reshape) #manipulation of output tables 
library(ggplot2) #graphs 

# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/RTempCLEARME/")


# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
cntyRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR RT

inRasterLoc <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/RT/" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q1
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q1/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q2
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q2/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q3
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q3/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR Q4
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/Q4/"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR ALL SCENARIOS 
#inRasterLoc<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/FutureLandscapes/"
#Folders <-  list('Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"))
#inRasterLoc <-paste0(inRasterLoc, Folders)

# Define paths for output files

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ADJUST FOR DESURED FOLDER 
Comb_output<-"I:/Test/RT/" #Sarah Halperin test. Can be deleted. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON RESPECTIVE OUTPUT FOLDER 
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4/"
#Comb_output<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/All/"

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Land Cover Rasters:
# nl01 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd01_anC.img")
# nl06 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd06_anC.img")
# nl11 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd11_anC.img")

# COUNTY RASTERS #currently set to regions 
regions <- raster(paste(cntyRasterLoc, "region_an2", ".img", sep="")) # this is for the raster.
counties_vals <- getValues(regions) #defining the region 
#NOTE: Craig said the only correct region raster was region_an2. I updated it to region_an2 was region_an

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv") #Geological ID for the region. May want to change to say region in the future. 


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) #values 1-8 for the region. 

#select only the Study Area counties to run basic analyses
S20_GEOID <-  read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

#TURN ON FOR RT
LS_trans <-  list('LS01' = c("v2015_RT_Landscape01.tif"),'LS02' = c("v2015_RT_Landscape02.tif"),'LS03' = c("v2015_RT_Landscape03.tif"),'LS04' = c("v2015_RT_Landscape04.tif"),'LS05' = c("v2015_RT_Landscape05.tif")) #Each of the different future landscapes for the different timesteps. 

#TURN ON FOR Q1
#LS_trans <-  list('LS01' = c("v2015_Q1_Landscape01.tif"),'LS02' = c("v2015_Q1_Landscape02.tif"),'LS03' = c("v2015_Q1_Landscape03.tif"),'LS04' = c("v2015_Q1_Landscape04.tif"),'LS05' = c("v2015_Q1_Landscape05.tif"))


#TURN ON FOR Q2
#LS_trans <-  list('LS01' = c("v2015_Q2_Landscape01.tif"),'LS02' = c("v2015_Q2_Landscape02.tif"),'LS03' = c("v2015_Q2_Landscape03.tif"),'LS04' = c("v2015_Q2_Landscape04.tif"),'LS05' = c("v2015_Q2_Landscape05.tif"))

#TURN ON FOR Q3
#LS_trans <-  list('LS01' = c("v2015_Q3_Landscape01.tif"),'LS02' = c("v2015_Q3_Landscape02.tif"),'LS03' = c("v2015_Q3_Landscape03.tif"),'LS04' = c("v2015_Q3_Landscape04.tif"),'LS05' = c("v2015_Q3_Landscape05.tif"))


#TURN ON FOR Q4
#LS_trans <-  list('LS01' = c("v2015_Q4_Landscape01.tif"),'LS02' = c("v2015_Q4_Landscape02.tif"),'LS03' = c("v2015_Q4_Landscape03.tif"),'LS04' = c("v2015_Q4_Landscape04.tif"),'LS05' = c("v2015_Q4_Landscape05.tif"))

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

Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) #full file path using inRasterLoc as the base
Final_hist_output <- paste0(Comb_output, gsub(".tif","_hist.txt",LS_trans[[in_to_fin]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
Final_Landscape <- raster(Final_Landscape) 
fin_vals <- getValues(Final_Landscape) 
  #plot of different land use types. 

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
#Reorganize #CF- create table with county as row, landuse as column
final_hist <- sa_ctyGEOID

for(i in final_hist$Din_cty){
 
 for (j in c("3","5","6","7")){ 
   if(any(names(final_hist0) == paste(j, i, sep = "."))){
     final_hist[final_hist$Din_cty == i, as.character(j)] <- final_hist0[names(final_hist0) == paste(j, i, sep = ".")]
   }}}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table #CF- create table with county as column, landuse as row
final_hist <- t(final_hist[-1])
colnames(final_hist) <- paste0("GEOID_", final_hist[1,])
final_hist <- final_hist[-1,]
final_hist <- cbind(data.frame(Rowid_ = 1:nrow(final_hist),LABEL = rownames(final_hist)), final_hist)
final_hist<-arrange(final_hist, LABEL)   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs #CF- conveRT NA to 0
final_hist[is.na(final_hist)]<-0

#result is for first timestep
#need it to iteratate through the scenarios --> list for RT and paste that goes through that
#want all the time steps to then combine to a new table 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(final_hist, file = Final_hist_output, row.names=FALSE, col.names=TRUE, sep=",")  
}


new<-Sys.time()-old # TIMING SCRIPT
print(new) # Time difference of 6.276551 mins
#---------------------------------------------------------------------------------------------------------------------------------------------------#
#START HERE IF TABLES PRODUCED ALREADY AND ONLY WANT TO RESHAPE AND PRODUCE GRAPHS 


#~~~~~~~~~~~~~~~~~~~~~~~~
#COMBINED TABLES 

#Used as a TEST can be deleted 
Folder<-list.files("I:/Test/RT", pattern=".txt", full.names = TRUE) #Read in RT files 
Tables<-lapply(Folder,function(i){
  read.csv(i)
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR RT
#NOTE: I set up to link to the output folder, but this may need to be changed 
#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
  #read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#BRING IN TABLES FOR Q1
#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q2

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q3

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES FOR Q4

#Folder<-list.files("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4", pattern=".txt", full.names = TRUE) #Read in RT files 
#Tables<-lapply(Folder,function(i){
#read.csv(i)
#})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#cOMPARISON ACROSS LAND COVER TYPES OVER TIME

#Add TimeSteps in order to track them when reshaping the table if desired 
Tables[[1]]$TimeStep<-1
Tables[[2]]$TimeStep<-2
Tables[[3]]$TimeStep<-3
Tables[[4]]$TimeStep<-4
Tables[[5]]$TimeStep<-5

Combined<-do.call(rbind.data.frame,Tables)

#Land Cover Type #3 
LABEL3<-Combined %>%
  filter(LABEL==3)
LABEL3<-LABEL3[,3:11]
  LABEL3<-t(LABEL3)
LABEL3<-LABEL3[1:8,]
colnames(LABEL3)<-c("T1.3","T2.3","T3.3","T4.3","T5.3")
#Land Cover Type #5
LABEL5<-Combined %>%
  filter(LABEL==5)
LABEL5<-LABEL5[,3:11]
LABEL5<-t(LABEL5)
LABEL5<-LABEL5[1:8,]
colnames(LABEL5)<-c("T1.5","T2.5","T3.5","T4.5","T5.5")
#Land Cover Type #6
LABEL6<-Combined %>%
  filter(LABEL==6)
LABEL6<-LABEL6[,3:11]
LABEL6<-t(LABEL6)
LABEL6<-LABEL6[1:8,]
colnames(LABEL6)<-c("T1.6","T2.6","T3.6","T4.6","T5.6")
#Land Cover Type #7
LABEL7<-Combined %>%
  filter(LABEL==7)
LABEL7<-LABEL7[,3:11]
LABEL7<-t(LABEL7)
LABEL7<-LABEL7[1:8,]
colnames(LABEL7)<-c("T1.7","T2.7","T3.7","T4.7","T5.7")


#Save CSV
CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
write.csv(CombinedReshape, paste0(Comb_output,"CombinedReshape.csv"))

#GGplot Facet by Region
CombinedMelt<-melt(Combined, id=c("Rowid_", "LABEL", "TimeStep" ))
CombinedMelt$LABEL<-as.factor(CombinedMelt$LABEL) #turn Label to factor from integer


#ggplot per individual region 
CombinedMelt<-subset(CombinedMelt, CombinedMelt$variable == "GEOID_24001") #subset based on region. Need to change the CombinedMelt$variable == 

windows()
ggplot(CombinedMelt, aes(x=TimeStep, y=value, colour=LABEL, group=LABEL))+
  geom_line()+
  #facet_grid(.~variable, scales="free")+
  scale_x_continuous(name= "Time Step", breaks= c(1,2,3,4,5), labels=c("2021", "2031", "2041", "2051", "2061"))+
  scale_colour_discrete(name= "Land Cover Types", labels=c("3", "5", "6", "7"))+
  scale_y_continuous(limits=c(0,1500000))+
   theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))



#Sum by Region 
region_sum<-aggregate(value~LABEL+TimeStep,CombinedMelt, sum)

#Ggplot summed over each region 
windows()
ggplot(region_sum, aes(x=TimeStep, y=value, colour=LABEL, group=LABEL))+
  geom_line()+
  scale_x_continuous(name= "Time Step", breaks= c(1,2,3,4,5), labels=c("2021", "2031", "2041", "2051", "2061"))+
  scale_colour_discrete(name= "Land Cover Types", labels=c("3", "5", "6", "7"))+
  scale_y_continuous(limits=c(0,25000000))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Save CSV needed for graphs. NEED TO CHANGE BASED ON INPUT
write.csv(CombinedMelt, paste0(Comb_output,"CombinedMelt.csv"), row.names=FALSE)
write.csv(region_sum, paste0(Comb_output,"Region_sum.csv"), row.names=FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TABLES COMPARED ACROSS SCENARIOS OVER TIME 
#TURN ON FOR DESIRED OUTPUT CSV
OutputRT<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/RT/"
OutputQ1<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q1/"
OutputQ2<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q2/"
OutputQ3<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q3/"
OutputQ4<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Tables/Q4/"


##Used as a TEST can be deleted 
Folder<-list.files("I:/Test/Old/ALL/Melt", pattern=".csv", full.names = TRUE) #Read in RT files 
CSV_Melt<-lapply(Folder,function(i){
  read.csv(i)
})

##Used as a TEST can be deleted 
Folder<-list.files("I:/Test/Old/ALL/Region", pattern=".csv", full.names = TRUE) #Read in RT files 
CSV_Region<-lapply(Folder,function(i){
  read.csv(i)
})

#read in CombinedMelt.csv
CSV_MeltRT<-read.csv(paste0(OutputRT,"CombinedMelt.csv"))
CSV_MeltQ1<-read.csv(paste0(OutputQ1,"CombinedMelt.csv"))
CSV_MeltQ2<-read.csv(paste0(OutputQ2,"CombinedMelt.csv"))
CSV_MeltQ3<-read.csv(paste0(OutputQ3,"CombinedMelt.csv"))
CSV_MeltQ4<-read.csv(paste0(OutputQ3,"CombinedMelt.csv"))


#ADD SCENARIO 
CSV_Melt[[1]]$Scenario<-"Q1"
CSV_Melt[[2]]$Scenario<-"Q2"
CSV_Melt[[3]]$Scenario<-"Q3"
CSV_Melt[[4]]$Scenario<-"Q4"
CSV_Melt[[5]]$Scenario<-"RT"

CombinedMeltLC<-do.call(rbind.data.frame,CSV_Melt)

CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegionLC<-do.call(rbind.data.frame,CSV_Region)


#GGplot Facet by Region
windows()
ggplot(CombinedMeltLC, aes(x=TimeStep, y=value, colour=Scenario, group=Scenario))+
  geom_line()+
  facet_grid(variable~LABEL, scales="free")

#Ggplot Facet by development 
windows()
ggplot(CombinedRegionLC, aes(x=TimeStep, y=value, colour=Scenario, group=Scenario))+
  geom_line()+
  facet_grid(.~LABEL, scales="free")+
  scale_x_continuous(name= "Time Step", breaks= c(1,2,3,4,5), labels=c("2021", "2031", "2041", "2051", "2061"))+
  scale_colour_discrete(name= "Land Cover Types", labels=c("3", "5", "6", "7"))+
  scale_y_continuous(limits=c(0,25000000))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


