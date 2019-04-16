
############################ 
#PURPOSE: Once tables are produced for timesteps 1-5 in 8_ZonalHist_MultScens_50years.R, melt together by scenario and across all scenarios to increase efficiency for use in ggplot and analysis.
#INPUT: all hist tables produced in 8_ZonalHist_MultScens_50years.R (Q1, Q2, Q3, Q4, RT folders)
#OUTPUT: "Tables" folder
#DEVELOPED: 
# 9-13-18 Sarah Halperin
#CONTACT: halperins@si.edu
#UPDATED:
# 1-24-19 Erin Carroll
#CONTACT: carrolle@si.edu
#NOTES:
# Reshaping section is only necessary for "exploratory analysis" section of Graphs script

#IMPORTANT: 
# Make sure the correct version folder is pulled in! Do a search for phrases associated with "version" (capitalized or not)

##### NEXT STEPS #####

############################

#------------------------------------------------#
###########################################

# PACKAGES NEEDED
library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(raster) # read and edit rasters
library(rgdal)
library(reshape) #manipulation of output tables 
library(ggplot2) #graphs 
library(ggpubr)


# ----------------------------------------------
# FILE LOCATIONS: 
#INPUT FILE LOCATION: 
version <- "/StudyArea_V201/SA_V2016"
version_table <- paste0("U:/CLI/Dinamica_Runs",version,"/BasicDataAnalyses/Zonal_Histogram/NoPL/")
tables <- paste0("Tables/", "v2016_") #make sure to change version
#--------------------------------------------------------------------#

#OUTPUT FILES LOCATION: 
Comb_outputCounty<-paste0(version_table, tables, "County/")
Comb_outputRegion<-paste0(version_table, tables, "Region/")
Comb_outputBuffer<-paste0(version_table,tables, "Buffer/")
Comb_outputSA<-paste0(version_table, tables,"StudyArea/")
Comb_outputCountySA<-paste0(version_table, tables, "CountySA/")
Comb_outputRegionSA<-paste0(version_table, tables, "RegionSA/")
Comb_outputReshape<-paste0(version_table, tables, "County/v2016_Reshape/") #manually change for this one

# ----------------------------------------------
# READ INPUT FILES:

#Region Names 
Region_SA<-read.csv("U:CLI/Dinamica_Runs/StudyArea_v201/CountyNmsGEOIDRegion_cnty.csv")
Region_SA$variable<-paste0(Region_SA$GEOID_, Region_SA$GEOID)
Region_SA<-Region_SA[,5:7]


#Read in files for NLCD
Folder<-list.files(paste0(version_table, "NL"), pattern=".txt", full.names = TRUE) #Read in NLCD files 
NLCD<-lapply(Folder,function(i){
  read.csv(i)
})


NLCD[[1]]$TimeStep<-1
NLCD[[2]]$TimeStep<-2


NLCD<-do.call(rbind.data.frame,NLCD)

#Read in All scenarios 
RTFolder<-list.files(paste0(version_table,"RT"), pattern=".txt", full.names = TRUE) 
TablesRT<-lapply(RTFolder,function(i){
  read.csv(i)
})

TablesRT[[1]]$TimeStep<-3
TablesRT[[2]]$TimeStep<-4
TablesRT[[3]]$TimeStep<-5
TablesRT[[4]]$TimeStep<-6
TablesRT[[5]]$TimeStep<-7



Q1Folder<-list.files(paste0(version_table,"Q1"), pattern=".txt", full.names = TRUE) 
TablesQ1<-lapply(Q1Folder,function(i){
  read.csv(i)
})

TablesQ1[[1]]$TimeStep<-3
TablesQ1[[2]]$TimeStep<-4
TablesQ1[[3]]$TimeStep<-5
TablesQ1[[4]]$TimeStep<-6
TablesQ1[[5]]$TimeStep<-7


Q2Folder<-list.files(paste0(version_table,"Q2"), pattern=".txt", full.names = TRUE) 
TablesQ2<-lapply(Q2Folder,function(i){
  read.csv(i)
})

TablesQ2[[1]]$TimeStep<-3
TablesQ2[[2]]$TimeStep<-4
TablesQ2[[3]]$TimeStep<-5
TablesQ2[[4]]$TimeStep<-6
TablesQ2[[5]]$TimeStep<-7



Q3Folder<-list.files(paste0(version_table,"Q3"), pattern=".txt", full.names = TRUE) 
TablesQ3<-lapply(Q3Folder,function(i){
  read.csv(i)
})

TablesQ3[[1]]$TimeStep<-3
TablesQ3[[2]]$TimeStep<-4
TablesQ3[[3]]$TimeStep<-5
TablesQ3[[4]]$TimeStep<-6
TablesQ3[[5]]$TimeStep<-7



Q4Folder<-list.files(paste0(version_table,"Q4"), pattern=".txt", full.names = TRUE) 
TablesQ4<-lapply(Q4Folder,function(i){
  read.csv(i)
})

TablesQ4[[1]]$TimeStep<-3
TablesQ4[[2]]$TimeStep<-4
TablesQ4[[3]]$TimeStep<-5
TablesQ4[[4]]$TimeStep<-6
TablesQ4[[5]]$TimeStep<-7


#------------------------------------------------------------------------

###########################################
# ~~~ CODE BEGINS ~~~ #
###########################################

#Combined timesteps 
CombinedRT<-do.call(rbind.data.frame,TablesRT)
CombinedQ1<-do.call(rbind.data.frame,TablesQ1)
CombinedQ2<-do.call(rbind.data.frame,TablesQ2)
CombinedQ3<-do.call(rbind.data.frame,TablesQ3)
CombinedQ4<-do.call(rbind.data.frame,TablesQ4)


CombinedRT<-rbind(NLCD,CombinedRT)
CombinedQ1<-rbind(NLCD,CombinedQ1)
CombinedQ2<-rbind(NLCD,CombinedQ2)
CombinedQ3<-rbind(NLCD,CombinedQ3)
CombinedQ4<-rbind(NLCD,CombinedQ4)

CombinedList<-list(CombinedRT, CombinedQ1, CombinedQ2, CombinedQ3, CombinedQ4) #combine scenarios


# ----------------------------------------------
# ----------------------------------------------
# MELT AND PRINT TABLES 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# MELT ALL FILES
CombinedMelt<-lapply(CombinedList, function(x){
  melt(x,id=c("Rowid_", "LABEL", "TimeStep"))
})

# ----------------------------------------------
# CREATE LIST OF FILE NAMES TO RUN LOOP THROUGH 
Version_Name<-"v2016_"

Scenario_County<-list(paste0(Version_Name, "ZonalHistogram_RT_cnty.csv"), paste0(Version_Name,"ZonalHistogram_Q1_cnty.csv"), paste0(Version_Name, "ZonalHistogram_Q2_cnty.csv"), paste0(Version_Name, "ZonalHistogram_Q3_cnty.csv"), paste0(Version_Name, "ZonalHistogram_Q4_cnty.csv"))
Scenario_Region<-list(paste0(Version_Name, "ZonalHistogram_RT_rgn.csv"), paste0(Version_Name, "ZonalHistogram_Q1_rgn.csv"), paste0(Version_Name, "ZonalHistogram_Q2_rgn.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_rgn.csv"),paste0(Version_Name,"ZonalHistogram_Q4_rgn.csv"))
Scenario_Buffer<-list(paste0(Version_Name, "ZonalHistogram_RT_buffer.csv"), paste0(Version_Name, "ZonalHistogram_Q1_buffer.csv"), paste0(Version_Name, "ZonalHistogram_Q2_buffer.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_buffer.csv"),paste0(Version_Name,"ZonalHistogram_Q4_buffer.csv"))
Scenario_SA<-list(paste0(Version_Name, "ZonalHistogram_RT_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_SA.csv"))
Scenario_RegionSA<-list(paste0(Version_Name, "ZonalHistogram_RT_rgn_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_rgn_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_rgn_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_rgn_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_rgn_SA.csv"))
Scenario_CountySA<-list(paste0(Version_Name, "ZonalHistogram_RT_cnty_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_cnty_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_cnty_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_cnty_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_cnty_SA.csv"))

# ----------------------------------------------
# GENERATE TABLES 

for(i in 1:length(CombinedMelt)){
  CombinedMelt[[i]]$valuekm<-CombinedMelt[[i]]$value*(900/1000000) #conversion from pixels to sq. km
  CombinedMelt[[i]]<-merge(CombinedMelt[[i]], Region_SA, by="variable") #add region and SA y/n by county GEOID
  write.csv(CombinedMelt[[i]], paste0(Comb_outputCounty,(Scenario_County[[i]])), row.names=FALSE) #print cnty tables
  Region<-aggregate(valuekm ~ LABEL+TimeStep+Region, CombinedMelt[[i]], FUN=sum) #rgn - combine counties into regions
  write.csv(Region, paste0(Comb_outputRegion,(Scenario_Region[[i]])), row.names=FALSE) #print rgn tables
  Buffer<-aggregate(valuekm~LABEL+TimeStep, CombinedMelt[[i]], FUN=sum) #buffer - combine over entire area
  write.csv(Buffer,paste0(Comb_outputBuffer,(Scenario_Buffer[[i]])), row.names=FALSE ) #print buffer tables
  CountySA<-subset(CombinedMelt[[i]], CombinedMelt[[i]]$SA==1) #cnty_SA - only counties within study area
  write.csv(CountySA, paste0(Comb_outputCountySA,(Scenario_CountySA[[i]])), row.names=FALSE) #print cnty_SA tables
  RegionSA<-aggregate(valuekm~LABEL+TimeStep+Region, CountySA, FUN=sum) #rgn_SA - only regions within study area
  write.csv(RegionSA, paste0(Comb_outputRegionSA,(Scenario_RegionSA[[i]])), row.names=FALSE) #print rgn_SA tables
  SA<-aggregate(valuekm ~LABEL+TimeStep,CountySA, FUN=sum) #study area - combine over entire study area
  write.csv(SA,paste0(Comb_outputSA,(Scenario_SA[[i]])), row.names=FALSE ) #print SA tables
}


# ----------------------------------------------
# ----------------------------------------------
# RESHAPE TABLES
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# GENERATE NAMES
ScenarioReshape<-list(paste0(Version_Name, "ZonalHistogram_RT_Reshape.csv"), paste0(Version_Name, "ZonalHistogram_Q1_Reshape.csv"), paste0(Version_Name, "ZonalHistogram_Q2_Reshape.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_Reshape.csv"),paste0(Version_Name,"ZonalHistogram_Q4_Reshape.csv"))

# ----------------------------------------------
# REFORMAT 
for( i in 1:length(CombinedList)){
  Combined<-CombinedList[[i]]
  LABEL3<-Combined %>%
    filter(LABEL==3)
  LABEL3<-LABEL3[,2:58] #change if number of rows changes (aka if want regions)
  LABEL3<-t(LABEL3)
  LABEL3<-LABEL3[1:57,] #change if number of rows changes (aka if want regions)
  colnames(LABEL3)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
  LABEL3<-as.data.frame(LABEL3)
  LABEL3$Change3<-LABEL3$`2061`-LABEL3$`2011`
  #Land Cover Type #5
  LABEL5<-Combined %>%
    filter(LABEL==5)
  LABEL5<-LABEL5[,2:58]
  LABEL5<-t(LABEL5)
  LABEL5<-LABEL5[1:57,]
  colnames(LABEL5)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
  LABEL5<-as.data.frame(LABEL5)
  LABEL5$Change5<-LABEL5$`2061`-LABEL5$`2011`
  #Land Cover Type #6
  LABEL6<-Combined %>%
    filter(LABEL==6)
  LABEL6<-LABEL6[,2:58]
  LABEL6<-t(LABEL6)
  LABEL6<-LABEL6[1:57,]
  colnames(LABEL6)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
  LABEL6<-as.data.frame(LABEL6)
  LABEL6$Change6<-LABEL6$`2061`-LABEL6$`2011`
  #Land Cover Type #7
  LABEL7<-Combined %>%
    filter(LABEL==7)
  LABEL7<-LABEL7[,2:58]
  LABEL7<-t(LABEL7)
  LABEL7<-LABEL7[1:57,]
  colnames(LABEL7)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
  LABEL7<-as.data.frame(LABEL7)
  LABEL7$Change7<-LABEL7$`2061`-LABEL7$`2011`
  
  CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
  write.csv(CombinedReshape, paste0(Comb_outputReshape,(ScenarioReshape[[i]]))) 
  
}

# ---------------------------------------------------------
# ---------------------------------------------------------
#TABLES COMPARED ACROSS SCENARIOS OVER TIME ("All")
# ---------------------------------------------------------
# ---------------------------------------------------------


# ----------------------------------------------
# COUNTIES (INCL BUFFER)
# ----------------------------------------------

Folder_County <-list.files(Comb_outputCounty, pattern="cnty.csv", full.names = TRUE) #Bring in merged scenario tables and combine all scenarios. Scenario tables are merged by timestep and for this example contains all counties. 
CSV_County<-lapply(Folder_County,function(i){
  read.csv(i)
})

#ADD SCENARIO 
CSV_County[[1]]$Scenario<-"Q1"
CSV_County[[2]]$Scenario<-"Q2"
CSV_County[[3]]$Scenario<-"Q3"
CSV_County[[4]]$Scenario<-"Q4"
CSV_County[[5]]$Scenario<-"RT"

CombinedCounty_LC<-do.call(rbind.data.frame,CSV_County)
CombinedCounty_LC<-subset(CombinedCounty_LC, CombinedCounty_LC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedCounty_LC, paste0(Comb_outputCounty,"All/","v2016_ZonalHistogram_AllScenarios_CNTY", ".csv"), row.names=FALSE)

#-----------------------------------------------
# COUNTIES WITHIN STUDY AREA
# ----------------------------------------------

Folder_CountySA<-list.files(Comb_outputCountySA, pattern="cnty_SA", full.names = TRUE)
CSV_CountySA<-lapply(Folder_CountySA,function(i){
  read.csv(i)
})

#ADD SCENARIO
CSV_CountySA[[1]]$Scenario<-"Q1"
CSV_CountySA[[2]]$Scenario<-"Q2"
CSV_CountySA[[3]]$Scenario<-"Q3"
CSV_CountySA[[4]]$Scenario<-"Q4"
CSV_CountySA[[5]]$Scenario<-"RT"

CombinedCountySA_LC<-do.call(rbind.data.frame,CSV_CountySA)
CombinedCountySA_LC<-subset(CombinedCountySA_LC, CombinedCountySA_LC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedCountySA_LC, paste0(Comb_outputCountySA,"All/","v2016_ZonalHistogram_AllScenarios_CNTY_SA", ".csv"), row.names=FALSE)


# ----------------------------------------------
# REGIONS (INCL BUFFER)
# ----------------------------------------------

Folder_Region<-list.files(Comb_outputRegion, pattern="rgn.csv", full.names = TRUE) 
CSV_Region<-lapply(Folder_Region,function(i){
  read.csv(i)
})

#ADD SCENARIO 
CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegion_LC<-do.call(rbind.data.frame,CSV_Region)
CombinedRegion_LC<-subset(CombinedRegion_LC, CombinedRegion_LC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedRegion_LC, paste0(Comb_outputRegion,"All/","v2016_ZonalHistogram_AllScenarios_RGN", ".csv"), row.names=FALSE)

# ----------------------------------------------
# REGIONS (STUDY AREA ONLY)
# ----------------------------------------------

Folder_RegionSA<-list.files(Comb_outputRegionSA, pattern="rgn_SA.csv", full.names = TRUE) 
CSV_RegionSA<-lapply(Folder_RegionSA,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_RegionSA[[1]]$Scenario<-"Q1"
CSV_RegionSA[[2]]$Scenario<-"Q2"
CSV_RegionSA[[3]]$Scenario<-"Q3"
CSV_RegionSA[[4]]$Scenario<-"Q4"
CSV_RegionSA[[5]]$Scenario<-"RT"

CombinedRegionSA_LC<-do.call(rbind.data.frame,CSV_RegionSA)
CombinedRegionSA_LC<-subset(CombinedRegionSA_LC, CombinedRegionSA_LC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedRegionSA_LC, paste0(Comb_outputRegionSA,"All/","V21016_ZonalHistogram_AllScenarios_RGN_SA", ".csv"), row.names=FALSE)


# ----------------------------------------------
# FULL STUDY AREA (INCL BUFFER)
# ----------------------------------------------

Folder_Buffer<-list.files(Comb_outputBuffer, pattern="_buffer", full.names = TRUE) 
CSV_Buffer<-lapply(Folder_Buffer,function(i){
  read.csv(i)
})

CSV_Buffer[[1]]$Scenario<-"Q1"
CSV_Buffer[[2]]$Scenario<-"Q2"
CSV_Buffer[[3]]$Scenario<-"Q3"
CSV_Buffer[[4]]$Scenario<-"Q4"
CSV_Buffer[[5]]$Scenario<-"RT"

CombinedBuffer_LC<-do.call(rbind.data.frame,CSV_Buffer)
write.csv(CombinedBuffer_LC, paste0(Comb_outputBuffer,"All/","v2016_ZonalHistogram_AllScenarios_SA_Buffer.csv"), row.names=FALSE)

# ----------------------------------------------
# STUDY AREA ONLY (NO BUFFER)
# ----------------------------------------------

Folder_SA<-list.files(Comb_outputSA, pattern="_SA", full.names = TRUE) 
CSV_SA<-lapply(Folder_SA,function(i){
  read.csv(i)
})

CSV_SA[[1]]$Scenario<-"Q1"
CSV_SA[[2]]$Scenario<-"Q2"
CSV_SA[[3]]$Scenario<-"Q3"
CSV_SA[[4]]$Scenario<-"Q4"
CSV_SA[[5]]$Scenario<-"RT"

CombinedSA_LC<-do.call(rbind.data.frame,CSV_SA)
write.csv(CombinedSA_LC, paste0(Comb_outputSA,"All/","v2016_ZonalHistogram_AllScenarios_SA.csv"), row.names=FALSE)

