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
#any where there is a "version" you will need to change based on desired inputs and outputs 

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
library(ggpubr)


# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/RTempCLEARME/")


#Version Input: MUST CHANGE
version<-"/StudyArea_V201/SA_V2015"
version_input<-paste0("U:/CLI/Dinamica_Runs",version, "/FutureLandscapes/")
Scenario<-"Q3/" #Say "All" for all scenarios 

# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
cntyRasterLoc <- "U:/CLI/PreparedRasters/StudyAreaBndy/" 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TURN ON FOR NL
inRasterLoc<-paste0(version_input, "NL/nlcd_nlcd/")

#TURN ON FOR ALL INDIVIDUAL SCENARIOS 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inRasterLoc <- paste0(version_input, Scenario)

#TURN ON FOR ALL SCENARIOS 
inRasterLoc<-version_input
Folders <-  list('Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q1' = c("Q1/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q2' = c("Q2/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q3' = c("Q3/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'Q4' = c("Q4/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"),'RT' = c("RT/"))
inRasterLoc <-paste0(inRasterLoc, Folders)



# Define paths for output files

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ADJUST FOR DESURED FOLDER
version_output<-"BasicDataAnalyses/Zonal_Histogram/"
Comb_output<-gsub("FutureLandscapes/", version_output, version_input)
Comb_output<-paste0(Comb_output, Scenario)
# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Land Cover Rasters:
# nl01 <- raster("U:/CLI/PreparedRasters/StudyAreaBndy/nlcd01_anC.img")
# nl06 <- raster("U:/CLI/PreparedRasters/StudyAreaBndy/nlcd06_anC.img")
# nl11 <- raster("U:/CLI/PreparedRasters/StudyAreaBndy/nlcd11_anC.img")

# COUNTY RASTERS #currently set to regions 
regions <- raster(paste(cntyRasterLoc, "regions_StudyArea", ".tif", sep="")) # this is for the raster.
counties_vals <- getValues(regions) #defining the region 
#NOTE: Craig said the only correct region raster was region_an2. I updated to region_an2 was region_an. I changed it to counties (regions_StudyArea.tif)

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 
  
#sa_ctyGEOID<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/FullGEOID.csv") #Geological ID for the region. May want to change to say region in the future. Regions 


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID","NAME")
sa_ctyGEOID$NAME<-NULL
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) #values 1-8 for the region. 

#select only the Study Area counties to run basic analyses
S20_GEOID <-  read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------
version_LS_trans<-"v2015"

#TURN on FOR NL
LS_trans <-  list('LS01' = c("nlcd01_anC.img"),'LS02' = c("nlcd11_anC.img"))

#TURN ON FOR RT
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif"))) #Each of the different future landscapes for the different timesteps. 

#TURN ON FOR Q1
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q1_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q1_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q1_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q1_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q1_Landscape05.tif")))


#TURN ON FOR Q2
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_Q2_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_Q2_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_Q2_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_Q2_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_Q2_Landscape05.tif")))

#TURN ON FOR Q3
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif")))


#TURN ON FOR Q4
LS_trans <-  list('LS01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LS02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LS03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LS04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LS05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif")))


#TURN ON FOR ALL SCENARIOS 
RT <-  list('LSRT01' = c(paste0(version_LS_trans,"_RT_Landscape01.tif")),'LSRT02' = c(paste0(version_LS_trans, "_RT_Landscape02.tif")),'LSRT03' = c(paste0(version_LS_trans,"_RT_Landscape03.tif")),'LSRT04' = c(paste0(version_LS_trans,"_RT_Landscape04.tif")),'LSRT05' = c(paste0(version_LS_trans,"_RT_Landscape05.tif"))) 

Q1 <-  list('LSQ101' = c(paste0(version_LS_trans,"_Q1_Landscape01.tif")),'LSQ102' = c(paste0(version_LS_trans, "_Q1_Landscape02.tif")),'LSQ103' = c(paste0(version_LS_trans,"_Q1_Landscape03.tif")),'LSQ104' = c(paste0(version_LS_trans,"_Q1_Landscape04.tif")),'LSQ105' = c(paste0(version_LS_trans,"_Q1_Landscape05.tif")))

Q2 <-  list('LSQ201' = c(paste0(version_LS_trans,"_Q2_Landscape01.tif")),'LSQ202' = c(paste0(version_LS_trans, "_Q2_Landscape02.tif")),'LSQ203' = c(paste0(version_LS_trans,"_Q2_Landscape03.tif")),'LSQ204' = c(paste0(version_LS_trans,"_Q2_Landscape04.tif")),'LSQ205' = c(paste0(version_LS_trans,"_Q2_Landscape05.tif")))


Q3<-  list('LSQ301' = c(paste0(version_LS_trans,"_Q3_Landscape01.tif")),'LSQ302' = c(paste0(version_LS_trans, "_Q3_Landscape02.tif")),'LSQ303' = c(paste0(version_LS_trans,"_Q3_Landscape03.tif")),'LSQ304' = c(paste0(version_LS_trans,"_Q3_Landscape04.tif")),'LSQ305' = c(paste0(version_LS_trans,"_Q3_Landscape05.tif")))


Q4 <-  list('LSQ401' = c(paste0(version_LS_trans,"_Q4_Landscape01.tif")),'LSQ402' = c(paste0(version_LS_trans, "_Q4_Landscape02.tif")),'LSQ403' = c(paste0(version_LS_trans,"_Q4_Landscape03.tif")),'LSQ404' = c(paste0(version_LS_trans,"_Q4_Landscape04.tif")),'LSQ405' = c(paste0(version_LS_trans,"_Q4_Landscape05.tif")))


LS_trans<-do.call(c,(list(Q1, Q2, Q3, Q4,RT))) #create list for ALL SCENARIOS 


# ------------------------------------------------------
# ------------------------------------------------------
# ZONAL HISTOGRAM ONLY (not transitions)
# ------------------------------------------------------
# ------------------------------------------------------

#SEE CODE BELOW FOR ALL SCENARIOS. 
# ----------------------------------------------
# Loop through transitions
# ----------------------------------------------
old <- Sys.time() # TIMING SCRIPT

#-----------------------------------------------------#
#USE FOR NCLD and individual scenarios 
for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?

Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) #full file path using inRasterLoc as the base
#Final_hist_output <- paste0(Comb_output, gsub(".img","_hist.txt",LS_trans[[in_to_fin]][1])) #For NCLD 
Final_hist_output <- paste0(Comb_output, gsub(".tif","_hist.txt",LS_trans[[in_to_fin]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
Final_Landscape <- raster(Final_Landscape) 
fin_vals <- getValues(Final_Landscape) 
  #plot of different land use types. 


#----------------------------------------------------#
#SEE CODE BELOW FOR ALL SCENARIOS 
#LINE 269


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
#-----------------------------------------------------------------------------------------------------------------#

#ALL SCENARIOS 
#CODE REPEATS 
## ----------------------------------------------
# Loop through transitions
# ----------------------------------------------
old <- Sys.time() # TIMING SCRIPT

InRasterLoc_LsTrans<-paste0(inRasterLoc, LS_trans)


for(i in 1:length(InRasterLoc_LsTrans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?#full file path using inRasterLoc as the base
  Final_hist_output <- paste0(Comb_output,gsub(".tif","_hist.txt",LS_trans[[i]][1])) #naming the file output. Taking the name of the raster to make the name of the output table. Remove tif. put _hist.txt
  Final_Landscape <- raster(InRasterLoc_LsTrans[[i]][1]) 
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
print(new)




#---------------------------------------------------------------------------------------------------------------------------------------------------#
#START HERE IF TABLES PRODUCED ALREADY AND ONLY WANT TO RESHAPE AND PRODUCE GRAPHS 
version_table<-paste0("U:/CLI/Dinamica_Runs",version, "/BasicDataAnalyses/Zonal_Histogram/")

#Set Output
Comb_outputMelt<-paste0(version_table,"Tables/Melt/")
Comb_outputRegion<-paste0(version_table,"Tables/Region/")
Comb_outputReshape<-paste0(version_table,"Tables/")

#~~~~~~~~~~~~~~~~~~~~~~~~
#TABLES FOR INDIVIDUAL SCENARIOS 

#BRING IN TABLES FOR NCLD. Leave on inorder to have scenario tables starting at 2001 and go to 2061. Full time frame 


Folder<-list.files(paste0(version_table, "NLCD"), pattern=".txt", full.names = TRUE) #Read in NCLD files 
NCLD<-lapply(Folder,function(i){
  read.csv(i)
})


NCLD[[1]]$TimeStep<-1
NCLD[[2]]$TimeStep<-2

 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Change based on scenario
#BRING IN TABLES
Scenario<-"Q2/"

Folder<-list.files(paste0(version_table,Scenario), pattern=".txt", full.names = TRUE) 
Tables<-lapply(Folder,function(i){
  read.csv(i)
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#cOMPARISON ACROSS LAND COVER TYPES OVER TIME

#Add TimeSteps in order to track them when reshaping the table if desired 
Tables[[1]]$TimeStep<-3
Tables[[2]]$TimeStep<-4
Tables[[3]]$TimeStep<-5
Tables[[4]]$TimeStep<-6
Tables[[5]]$TimeStep<-7

Combined<-do.call(rbind.data.frame,Tables)
#NCLD<-do.call(rbind.data.frame,NCLD) #only do once
Combined<-rbind(NCLD,Combined)

#Land Cover Type #3 
LABEL3<-Combined %>%
  filter(LABEL==3)
LABEL3<-LABEL3[,3:23]
  LABEL3<-t(LABEL3)
LABEL3<-LABEL3[1:20,]
colnames(LABEL3)<-c("2001.3","2011.3","2021.3","2031.3","2041.3", "2051.3","2061.3")
#Land Cover Type #5
LABEL5<-Combined %>%
  filter(LABEL==5)
LABEL5<-LABEL5[,3:23]
LABEL5<-t(LABEL5)
LABEL5<-LABEL5[1:20,]
colnames(LABEL5)<-c("2001.5","2011.5","2021.5","2031.5","2041.5", "2051.5","2061.5")
#Land Cover Type #6
LABEL6<-Combined %>%
  filter(LABEL==6)
LABEL6<-LABEL6[,3:23]
LABEL6<-t(LABEL6)
LABEL6<-LABEL6[1:20,]
colnames(LABEL6)<-c("2001.6","2011.6","2021.6","2031.6","2041.6", "2051.6","2061.6")
#Land Cover Type #7
LABEL7<-Combined %>%
  filter(LABEL==7)
LABEL7<-LABEL7[,3:23]
LABEL7<-t(LABEL7)
LABEL7<-LABEL7[1:20,]
colnames(LABEL7)<-c("2001.7","2011.7","2021.7","2031.7","2041.7", "2051.7","2061.7")


#Save CSV #MUST CHANGE
Scenario<-"Q2" #delete / 


CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
write.csv(CombinedReshape, paste0(Comb_outputReshape,"CombinedReshape", Scenario,".csv")) 


#Melt for graphs 
CombinedMelt<-melt(Combined, id=c("Rowid_", "LABEL", "TimeStep" ))
CombinedMelt$LABEL<-as.factor(CombinedMelt$LABEL) #turn Label to factor from integer


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#NEED TO CHANGE NAME BASED ON INPUT
#convert pixels 
CombinedMelt$valuekm<-CombinedMelt$value*(900/1000000)
write.csv(CombinedMelt, paste0(Comb_outputMelt,"CombinedMelt", Scenario, ".csv"), row.names=FALSE)


#Remove 2001 (optional)
CombinedMelt<-subset(CombinedMelt, CombinedMelt$TimeStep > 1)

#Sum by Region 
region_sum<-aggregate(valuekm~LABEL+TimeStep,CombinedMelt, sum)
write.csv(region_sum, paste0(Comb_outputRegion,"CombinedRegion", Scenario, ".csv"), row.names=FALSE)

#-------------------------------------------------------------------------------------------#
#Currently not being used 
#Graphs for individual scenarios comparing land cover type 


windows()
ggplot(CombinedMelt, aes(x=TimeStep, y=valuekm, colour=LABEL, group=LABEL))+
  geom_line(size=2)+
  facet_grid(.~variable, scales="free")+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#5c5252", "#9fb480","#f7e68c", "#e17d63"), labels=c("Development", "Forest", "Grass/Pasture", "Tilled Cropland"))+
  scale_y_continuous(name =expression('Total Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=14), legend.title=element_blank)


#Ggplot summed over each region 
windows()
ggplot(region_sum, aes(x=TimeStep, y=valuekm, colour=LABEL, group=LABEL))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#5c5252", "#9fb480","#f7e68c", "#e17d63"), labels=c("Development", "Forest", "Grass/Pasture", "Tilled Cropland"))+
  scale_y_continuous(name =expression('Total Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"), legend.text=element_text(size=14), legend.title=element_blank())


#export graph  #must name ggplots above 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation") #set where you want it to be saved 
png("FrederickQ2Graph.png", width=1000, height=100, units="in", res=300) #can't put units and resolution
FrederickQ1Graph #name of ggplot 
dev.off()


ggsave(file="FrederickQ2Graph.png", dpi=300)
#-------------------------------------------------------------------------------------------------------------------------------------------------#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TABLES COMPARED ACROSS SCENARIOS OVER TIME 
#Combines all scenarios 

#Read in Melt csvs
FolderM<-list.files(Comb_outputMelt, pattern=".csv", full.names = TRUE) 
CSV_Melt<-lapply(FolderM,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Melt[[1]]$Scenario<-"Q1"
CSV_Melt[[2]]$Scenario<-"Q2"
CSV_Melt[[3]]$Scenario<-"Q3"
CSV_Melt[[4]]$Scenario<-"Q4"
CSV_Melt[[5]]$Scenario<-"RT"

CombinedMeltLC<-do.call(rbind.data.frame,CSV_Melt)
CombinedMeltLC$valuekm<-CombinedMeltLC$value*(900/1000000) #if valuekm exists do not need
CombinedMeltLC<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep > 1)
write.csv(CombinedMeltLC, paste0(Comb_outputMelt,"CombinedMeltLC", ".csv"), row.names=FALSE)


##read in region sum csv
FolderR<-list.files(Comb_outputRegion, pattern=".csv", full.names = TRUE) 
CSV_Region<-lapply(FolderR,function(i){
  read.csv(i)
})

CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegionLC<-do.call(rbind.data.frame,CSV_Region)
CombinedRegionLC$valuekm<-CombinedRegionLC$value*(900/1000000) # if valuekm exists do not need 
write.csv(CombinedRegionLC, paste0(Comb_outputRegion,"CombinedRegionLC", ".csv"), row.names=FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PERCENT CHANGE INDIVIDUAL COUNTIES
CombinedMeltLC$Rowid_<-NULL

CombinedMeltLCT2<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==2)
CombinedMeltLCT7<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==7)
CombinedMeltLC27<-cbind(CombinedMeltLCT2,CombinedMeltLCT7)


CombinedMeltLC27<-CombinedMeltLC27[,c(1,2,3,5,6,12)]
CombinedMeltLC27<-mutate(CombinedMeltLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedMeltLC27$PercentChange<-round(CombinedMeltLC27$PercentChange, digits = 0)
CombinedMeltLC27$PercentChange<-paste0(CombinedMeltLC27$PercentChange,"%")


CombinedMeltLC27$TimeStep<-7
PercentChangeMelt<-CombinedMeltLC27[,c(1,2,3,4,7)]

CombinedMeltPC<-merge(CombinedMeltLC,PercentChangeMelt, by=c("Scenario","TimeStep","LABEL","variable"), all.x=TRUE)


#Subset by land cover type 
DevelopmentM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "3")
ForestM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "5")
GrassM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "6")
CropM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "7")

#Subset by county examples
Loudoun<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51107")
Frederick<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51069")
Fauquier<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51061")
Shenandoah<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51171")
Albemarle<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51003")
Rockingham<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51165")


AlbemarleD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51003")
AlbemarleF<-subset(ForestM, ForestM$variable == "GEOID_51003")
AlbemarleG<-subset(GrassM, GrassM$variable == "GEOID_51003")
AlbemarleC<-subset(CropM, CropM$variable == "GEOID_51003")

#subset by country and land cover type 
FauquierD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51061")
FauquierF<-subset(ForestM, ForestM$variable == "GEOID_51061")
FauquierG<-subset(GrassM, GrassM$variable == "GEOID_51061")
FauquierC<-subset(CropM, CropM$variable == "GEOID_51061")


FrederickD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51069")
FrederickF<-subset(ForestM, ForestM$variable == "GEOID_51069")
FrederickG<-subset(GrassM, GrassM$variable == "GEOID_51069")
FrederickC<-subset(CropM, CropM$variable == "GEOID_51069")



#IF GRAPH LOOKS weird make sure LABEL is set as a factor 
#Graphs for individual counties 
#When saved as individual graph v2015_Fred_crop
#when saved for ggarrange crop,development, forest, grass 

#CHANGE TO SIZE 40 IF GRAPHS ARE NOT GOING TO BE ARRANGED
  windows()
  
  #FAUQUIER
  v2015_Fauq_development<-ggplot(FauquierD, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
    geom_line(size=2)+
    scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
    scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    #Forest#scale_y_continuous(name =expression('Total Area km'^2), limits = c(650,800), breaks=c(650,675,700,725,750,775,800))+
    #grass# scale_y_continuous(name =expression('Total Area km'^2), limits=c(550,650), breaks=c(550,575,600,625))+
    #development#
    scale_y_continuous(name =expression('Total Area km'^2),  limits=c(0,200), breaks=c(50,100,150,200))+
    #crop#scale_y_continuous(name =expression('Total Area km'^2), limits=c(100,175), breaks=c(100,125,150,175))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.text=element_text(size=20, colour="black"),
          axis.title.x=element_text(size=20), axis.title.y =element_text(size=20, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
    theme(plot.margin=unit(c(1,1,1,1), "in"))+
    geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=20, show.legend=FALSE)+
    theme(panel.border=element_blank())+
    theme(axis.line = element_line(size=1.5, colour="black")) 
  
  #FREDERICK
v2015_Fred_forest<-ggplot(FauquierF, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #Forest# scale_y_continuous(name =expression('Total Area km'^2), limits=c(525,625), breaks=c(525,550,575,600,625))+
  #grass#scale_y_continuous(name =expression('Total Area km'^2), limits=c(250,350), breaks=c(250,275,300,325,350))+
  #development#scale_y_continuous(name =expression('Total Area km'^2), limits=c(50,200), breaks=c(50,100,150,200))+
  #crop#scale_y_continuous(name =expression('Total Area km'^2), limits=c(5,20), breaks=c(5,10,15,20))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=20, colour="black"),
        axis.title.x=element_text(size=20), axis.title.y =element_text(size=20, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=10, show.legend=FALSE)+
    theme(panel.border=element_blank())+
    theme(axis.line = element_line(size=1.5, colour="black")) 


#-----------------------------------------------------------#
#Table

#Frederick County 
Fred_PC<-subset(PercentChangeMelt, variable=="GEOID_51069")
Fred_Q1<-subset(Fred_PC, Scenario =="Q1")
Fred_Q2<-subset(Fred_PC, Scenario =="Q2")
Fred_Q3<-subset(Fred_PC, Scenario =="Q3")
Fred_Q4<-subset(Fred_PC, Scenario =="Q4")
Fred_RT<-subset(Fred_PC, Scenario =="RT")

Fred_Table<-cbind(Fred_RT[,5],Fred_Q1[,5],Fred_Q2[,5],Fred_Q3[,5],Fred_Q4[,5])
Fred_Table<-as.data.frame(Fred_Table)
colnames(Fred_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
rownames(Fred_Table)<-c("Development", "Forest", "Grass", "Crop")

Fred_Table_plot<-ggtexttable(Fred_Table, theme=ttheme("mBlackWhite", base_size=15))

windows()
FrederickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

windows()
ggarrange(FrederickGraph, Fred_Table_plot, ncol=2, nrow=1, widths =c(1,.35))


#Fauquier County 
Fauq_PC<-subset(PercentChangeMelt, variable=="GEOID_51061")
Fauq_Q1<-subset(Fauq_PC, Scenario =="Q1")
Fauq_Q2<-subset(Fauq_PC, Scenario =="Q2")
Fauq_Q3<-subset(Fauq_PC, Scenario =="Q3")
Fauq_Q4<-subset(Fauq_PC, Scenario =="Q4")
Fauq_RT<-subset(Fauq_PC, Scenario =="RT")

Fauq_Table<-cbind(Fauq_RT[,5],Fauq_Q1[,5],Fauq_Q2[,5],Fauq_Q3[,5],Fauq_Q4[,5])
Fauq_Table<-as.data.frame(Fauq_Table)
colnames(Fauq_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
rownames(Fauq_Table)<-c("Development", "Forest", "Grass", "Crop")

Fauq_Table_plot<-ggtexttable(Fauq_Table, theme=ttheme("mBlackWhite", base_size=15))

windows()
FauqerickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

windows()
ggarrange(FauqerickGraph, Fauq_Table_plot, ncol=2, nrow=1, widths =c(1,.35))


#export graph 
setwd("X:/Scenario Planning/Graphics/Map Images/4_17")
png("v2015_Fauq_development.png", width=480, height=480, units="px", res=300) #can't put units and resolution
v2015_Fauq_development
dev.off()


ggsave(file="v2015_Fauq_development.png", dpi=300,width=15, height=15)




#NAME ggplot 
#Graphs for entire study region 
#-------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PERCENT CHANGE STUDY AREA
CombinedRegionLCT2<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==2)
CombinedRegionLCT7<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==7)
CombinedRegionLC27<-cbind(CombinedRegionLCT2,CombinedRegionLCT7)
CombinedRegionLC27<-CombinedRegionLC27[,c(1,2,4,5,7,10)]
CombinedRegionLC27<-mutate(CombinedRegionLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100) #calculate percent change after only haveing time step 2 and 7
CombinedRegionLC27$PercentChange<-round(CombinedRegionLC27$PercentChange, digits = 2)


CombinedRegionLC27$TimeStep<-7
PercentChange<-CombinedRegionLC27[,c(1,2,3,7)]

CombinedRegionPC<-merge(CombinedRegionLC,PercentChange, by=c("Scenario","TimeStep","LABEL"), all.x=TRUE)

#Subset By Land Cover
DevelopmentPC<-subset(CombinedRegionPC, CombinedRegionPC$LABEL == "3")
ForestPC<-subset(CombinedRegionPC, CombinedRegionPC$LABEL == "5")
GrassPC<-subset(CombinedRegionPC, CombinedRegionPC$LABEL == "6")
CropPC<-subset(CombinedRegionPC, CombinedRegionPC$LABEL == "7")

#Remove Timestep 1
DevelopmentPC<-subset(DevelopmentPC, DevelopmentPC$TimeStep > 1)
ForestPC<-subset(ForestPC, ForestPC$TimeStep > 1)
GrassPC<-subset(GrassPC, GrassPC$TimeStep >1)
CropPC<-subset(CropPC, CropPC$TimeStep>1)
#-------------------------------------------------------------------------#

windows()
ggplot(DevelopmentPC, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c( "2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Total Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), hjust=2,vjust=2, size=5, show.legend=FALSE)


#export graph 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("CropSA.png", width=480, height=480, units="px", res=300) #can't put units and resolution
CropSA
dev.off()


ggsave(file="CropSA.png", dpi=300, width=15, height=15)


#-------------------------------------------------------#
#change color of geom_text for development
windows()
ggplot(DevelopmentPC, aes(x=Scenario, y=PercentChange, fill=Scenario))+
  geom_bar(stat="identity", position = 'dodge')+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name="Percent Change", limits=c(-100,120), labels=c("-100%","-50%", "-0%","50%", "100%", "120%"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text.y =element_text(size=40),
        axis.text.x =element_blank(),
        axis.title.x=element_blank(), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  theme(panel.border=element_blank())+
  geom_hline(yintercept=0, size=1.5)+
  geom_text(aes(label=paste0(PercentChange,"%")), vjust=1.6, size=10, colour="white")+
  theme(axis.line.y =element_line(size=1.5))

