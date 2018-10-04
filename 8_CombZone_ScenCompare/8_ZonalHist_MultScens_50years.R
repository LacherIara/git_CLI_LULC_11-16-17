############################ 
##Craig edit

#PURPOSE: Do Combine and Hist for **NLCD Land Cover years 2001 & 2012**. Avoid using ArcMap and doing all the steps by hand.
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
#any where there is a "version" you will need to change based on desired inputs and outputs 

# Make sure the correct version folder is pulled in! Do a search for phrases associated with "version" (capitalized or not)

### ADDED STEPS#### 
#3/2018 - Sarah Halperin  
# added code to loop through different scenarios and comparison plots of scenarios based on development type over the timesteps #Updated by Sarah Halperin 
# added compatibility to code for creating transitions raster with multiple landscapes (second suite of code on page)

#Code is written with different options. The commenting tells you generally when to run each piece depending on what scenario you want run. 

##### NEXT STEPS #####

############################

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
library(ggpubr)


# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/RTempCLEARME/")


# ----------------------------------------------
# READ INPUT FILES:
#Set Version and version input 
version<-"/StudyArea_V201/SA_V2016"
version_input<-paste0("U:/CLI/Dinamica_Runs",version, "/FutureLandscapes/")
Scenario<-"PL_Gap/" #set scenario desired to run. If want all scenarios to run say "All" for all scenarios (NOTE: when all scenarios you must use the second set of code)


# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
cntyRasterLoc <- "U:/CLI/PreparedRasters/StudyAreaBndy/" 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#TURN ON SCENARIO INPUTS: Whatever is turned on must match the designated scenario 

# ----------------------------------------------
#TURN ON FOR NL 
inRasterLoc<-paste0(version_input, "NL/nlcd_nlcd/")

# ----------------------------------------------
#TURN ON FOR ALL INDIVIDUAL SCENARIOS 
inRasterLoc <- paste0(version_input, Scenario)

# ----------------------------------------------
#TURN ON FOR ALL SCENARIOS (not NL)
inRasterLoc<-version_input
Folders <-  list('Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"))
inRasterLoc <-paste0(inRasterLoc, Folders)

# ----------------------------------------------
#TURN ON FOR PL_GAP 
inRasterLoc <- paste0(version_input, Scenario)

# ----------------------------------------------
# COUNTY RASTERS 
#currently set to county
regions <- raster(paste(cntyRasterLoc, "cnty_an", ".img", sep="")) # this is for the raster.
counties_vals <- getValues(regions) #defining the region 
#NOTE: Craig said the only correct region raster was region_an2. I updated to region_an2 was region_an. I changed it to counties (regions_StudyArea.tif), which has now been renamed to ctny_StudyArea

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/CountyNmsGEOID_cnty.csv")#SCBI V: #Geological ID for the county. 

#sa_ctyGEOID<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/FullGEOID.csv") #Geological ID for the region. May want to change to say region in the future. Regions 


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID","NAME")
sa_ctyGEOID$NAME<-NULL
str(sa_ctyGEOID)



#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) #values 1-8 for the region. 


 #select only the Study Area counties to run basic analyses
S20_GEOID <-  read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 

# ----------------------------------------------
#OUTPUT FOLDER
version_output<-"BasicDataAnalyses/Zonal_Histogram/"
Comb_output<-gsub("FutureLandscapes/", version_output, version_input)
Comb_output<-paste0(Comb_output, Scenario)


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ---------------------------------------------- 
#Version_LS_trans Must match above 
version_LS_trans<-"v2016" #must match above 

#TURN on FOR NL
LS_trans <-  list('LS01' = c("nlcd01_anC.img"),'LS02' = c("nlcd11_anC.img"))

#TURN ON FOR RT
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif"))) #Each of the different future landscapes for the different timesteps. 

#TURN ON FOR Q1
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q1_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q1_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q1_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q1_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q1_Landscape05.tif")))


#TURN ON FOR Q2
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q2_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q2_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q2_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q2_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q2_Landscape05.tif")))

#TURN ON FOR Q3
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q3_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q3_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q3_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q3_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q3_Landscape05.tif")))

#TURN ON FOR Q4
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q4_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q4_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q4_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q4_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q4_Landscape05.tif")))


#TURN ON FOR ALL SCENARIOS 
RT <-  list('LSRT01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LSRT02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LSRT03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LSRT04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LSRT05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif"))) 

Q1 <-  list('LSQ101' = c(paste0(version_LS_trans,"_Q1_Landscape01.tif")),'LSQ102' = c(paste0(version_LS_trans, "_Q1_Landscape02.tif")),'LSQ103' = c(paste0(version_LS_trans,"_Q1_Landscape03.tif")),'LSQ104' = c(paste0(version_LS_trans,"_Q1_Landscape04.tif")),'LSQ105' = c(paste0(version_LS_trans,"_Q1_Landscape05.tif")))

Q2 <-  list('LSQ201' = c(paste0(version_LS_trans,"_Q2_Landscape01.tif")),'LSQ202' = c(paste0(version_LS_trans, "_Q2_Landscape02.tif")),'LSQ203' = c(paste0(version_LS_trans,"_Q2_Landscape03.tif")),'LSQ204' = c(paste0(version_LS_trans,"_Q2_Landscape04.tif")),'LSQ205' = c(paste0(version_LS_trans,"_Q2_Landscape05.tif")))


Q3<-  list('LSQ301' = c(paste0(version_LS_trans,"_Q3_Landscape01.tif")),'LSQ302' = c(paste0(version_LS_trans, "_Q3_Landscape02.tif")),'LSQ303' = c(paste0(version_LS_trans,"_Q3_Landscape03.tif")),'LSQ304' = c(paste0(version_LS_trans,"_Q3_Landscape04.tif")),'LSQ305' = c(paste0(version_LS_trans,"_Q3_Landscape05.tif")))


Q4 <-  list('LSQ401' = c(paste0(version_LS_trans,"_Q4_Landscape01.tif")),'LSQ402' = c(paste0(version_LS_trans, "_Q4_Landscape02.tif")),'LSQ403' = c(paste0(version_LS_trans,"_Q4_Landscape03.tif")),'LSQ404' = c(paste0(version_LS_trans,"_Q4_Landscape04.tif")),'LSQ405' = c(paste0(version_LS_trans,"_Q4_Landscape05.tif")))


LS_trans<-do.call(c,(list(Q1, Q2, Q3, Q4,RT))) #create list for ALL SCENARIOS 
#TURN ON FOR GAP 
NL<-list('LSNL01' = c("nldc01_anC.tif"),'LSNL02' = c("nldc11_anC.tif"))
RT <-  list('LSRT01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LSRT02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LSRT03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LSRT04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LSRT05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif"))) 

Q1 <-  list('LSQ101' = c(paste0(version_LS_trans,"_Q1_Landscape01.tif")),'LSQ102' = c(paste0(version_LS_trans, "_Q1_Landscape02.tif")),'LSQ103' = c(paste0(version_LS_trans,"_Q1_Landscape03.tif")),'LSQ104' = c(paste0(version_LS_trans,"_Q1_Landscape04.tif")),'LSQ105' = c(paste0(version_LS_trans,"_Q1_Landscape05.tif")))

Q2 <-  list('LSQ201' = c(paste0(version_LS_trans,"_Q2_Landscape01.tif")),'LSQ202' = c(paste0(version_LS_trans, "_Q2_Landscape02.tif")),'LSQ203' = c(paste0(version_LS_trans,"_Q2_Landscape03.tif")),'LSQ204' = c(paste0(version_LS_trans,"_Q2_Landscape04.tif")),'LSQ205' = c(paste0(version_LS_trans,"_Q2_Landscape05.tif")))


Q3<-  list('LSQ301' = c(paste0(version_LS_trans,"_Q3_Landscape01.tif")),'LSQ302' = c(paste0(version_LS_trans, "_Q3_Landscape02.tif")),'LSQ303' = c(paste0(version_LS_trans,"_Q3_Landscape03.tif")),'LSQ304' = c(paste0(version_LS_trans,"_Q3_Landscape04.tif")),'LSQ305' = c(paste0(version_LS_trans,"_Q3_Landscape05.tif")))


Q4 <-  list('LSQ401' = c(paste0(version_LS_trans,"_Q4_Landscape01.tif")),'LSQ402' = c(paste0(version_LS_trans, "_Q4_Landscape02.tif")),'LSQ403' = c(paste0(version_LS_trans,"_Q4_Landscape03.tif")),'LSQ404' = c(paste0(version_LS_trans,"_Q4_Landscape04.tif")),'LSQ405' = c(paste0(version_LS_trans,"_Q4_Landscape05.tif")))


LS_trans<-do.call(c,(list(NL,Q1, Q2, Q3, Q4,RT))) 


# ------------------------------------------------------
# ------------------------------------------------------
# ZONAL HISTOGRAM ONLY (not transitions)
# ------------------------------------------------------
# ------------------------------------------------------

#FOR ONE SCENARIO AT A TIME
#SEE CODE BELOW FOR ALL SCENARIOS 


old <- Sys.time() # TIMING SCRIPT

#-----------------------------------------------------#
# ----------------------------------------------
# ZONAL HISTOGRAM ON FINAL LANDSCAPE 
# ----------------------------------------------
#USE FOR NLCD AND NDIVIDUAL SCENARIOS
for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?

Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) #full file path using inRasterLoc as the base
#Final_hist_output <- paste0(Comb_output, gsub(".img","_hist.txt",LS_trans[[in_to_fin]][1])) #For NCLD 
Final_hist_output <- paste0(Comb_output, gsub(".tif","_hist.txt",LS_trans[[in_to_fin]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
Final_Landscape <- raster(Final_Landscape) 
fin_vals <- getValues(Final_Landscape) 
  #plot of different land use types. 


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
#-----------------------------------------------------------------------------------------------------------------#



#CODE REPEATED FOR ALL SCENARIOS and GAP
# ----------------------------------------------
# ----------------------------------------------
# ALL SCENARIOS: LOOP THROUGH
# ----------------------------------------------
# ----------------------------------------------

old <- Sys.time() # TIMING SCRIPT

InRasterLoc_LsTrans<-paste0(inRasterLoc, LS_trans)


for(i in 1:length(InRasterLoc_LsTrans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?#full file path using inRasterLoc as the base
  Final_hist_output <- paste0(Comb_output,gsub(".tif","_hist.txt",LS_trans[[i]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
  Final_Landscape <- raster(InRasterLoc_LsTrans[[i]][1]) 
  fin_vals <- getValues(Final_Landscape) 
  
  #plot of different land use types. 
  
  # ----------------------------------------------
  #ZONAL HISTOGRAM ON FINAL LANDSCAPE 


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
print(new)





