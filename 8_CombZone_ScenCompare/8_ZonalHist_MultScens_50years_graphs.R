############################ 
#PURPOSE: Combine merged tables and use them to make graphs. 
#CREATOR: Sarah Halperin 
#CONTACT: halperins@si.edu
#INPUT: U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\BasicDataAnalyses\Zonal_Histogram 
#OUTPUT: U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\BasicDataAnalyses\OutputVisuals\ZonalHistogram_ggplots
#DEVELOPED: 4-30-18


#NOTES: There are a lot of options here on how to organize the data and generate graphs. First imports all Scenario  tables and combines them. Then possible graphs. Followed by exploratory analysis for county and region. These exploratory analysis and very rough.

#IMPORTANT: 
# Make sure the correct version folder is pulled in! Do a search for phrases associated with "version" (capitalized or not)
# Watch capitalization!!! There are two files with "_cnty". one is capitalized, and the other is not.
	# v2016_ZonalHistogram_AllScenarios_CTNY
	# v2016_ZonalHistogram_AllScenarios_CTNY_SA
	# v2016_ZonalHistogram_AllScenarios_RGN

 ##### NEXT STEPS #####

############################

# ----------------------------------------------
###########################################


# PACKAGES NEEDED
library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(raster) # read and edit rasters
library(rgdal)
library(reshape) #manipulation of output tables 
library(ggplot2) #graphs 
library(ggpubr)
library(ggrepel) #changes to graphs

# ----------------------------------------------
# READ INPUT FILES:
#Set file locations
version<-"/StudyArea_V201/SA_V2016"
version_table<-paste0("U:/CLI/Dinamica_Runs",version, "/BasicDataAnalyses/Zonal_Histogram/")
tables<-"Tables/v2016_"

# ----------------------------------------------
# OUTPUT FILES:
Comb_outputCounty<-paste0(version_table, tables, "County/")
Comb_outputSA<-paste0(version_table, tables,"StudyArea/")
Comb_outputBuffer<-paste0(version_table,tables, "Buffer/")
Comb_outputRegion<-paste0(version_table, tables, "Region/")
Comb_outputReshape<-paste0(version_table, tables, "County/v2016_Reshape/")



###########################################
# ~~~ CODE BEGINS ~~~ #
###########################################
#CODE IS REPEATED FROM 8_ZonalHist_MultScens_50Years_MergeTables incase haven't before want to make graphs. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#BRING IN TABLES


# ----------------------------------------------
# ----------------------------------------------
#TABLES COMPARED ACROSS SCENARIOS OVER TIME 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# COUNTIES (INCL BUFFER)
# ----------------------------------------------

FolderM<-list.files(Comb_outputCounty, pattern="ctny.csv", full.names = TRUE) #Bring in merged scenario tables and combine all scenarios. Scenario tables are merged by timestep and for this example contains all counties. 
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
CombinedMeltLC<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedMeltLC, paste0(Comb_outputCounty,"v2016_ZonalHistogram_AllScenarios_CTNY", ".csv"), row.names=FALSE)

#--------------------------------------------------#
# COUNTIES WITHIN STUDY AREA
# ----------------------------------------------

FolderM_SA<-list.files(Comb_outputCounty, pattern="SA.csv", full.names = TRUE) 
CSV_Melt_SA<-lapply(FolderM_SA,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Melt_SA[[1]]$Scenario<-"Q1"
CSV_Melt_SA[[2]]$Scenario<-"Q2"
CSV_Melt_SA[[3]]$Scenario<-"Q3"
CSV_Melt_SA[[4]]$Scenario<-"Q4"
CSV_Melt_SA[[5]]$Scenario<-"RT"

CombinedMeltLC_SA<-do.call(rbind.data.frame,CSV_Melt_SA)
CombinedMeltLC_SA<-subset(CombinedMeltLC_SA, CombinedMeltLC_SA$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedMeltLC_SA, paste0(Comb_outputCounty,"v2016_ZonalHistogram_AllScenarios_CTNY_SA", ".csv"), row.names=FALSE)


# ----------------------------------------------
# REGIONS (INCL BUFFER)
# ----------------------------------------------

FolderMR<-list.files(Comb_outputRegion, pattern="rgn.csv", full.names = TRUE) 
CSV_Region<-lapply(FolderMR,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegionLC<-do.call(rbind.data.frame,CSV_Region)
CombinedRegionLC<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedRegionLC, paste0(Comb_outputRegion,"v2016_ZonalHistogram_AllScenarios_RGN", ".csv"), row.names=FALSE)

# ----------------------------------------------
# REGIONS (STUDY AREA ONLY)
# ----------------------------------------------

FolderMR_SA<-list.files(Comb_outputRegion, pattern="SA.csv", full.names = TRUE) 
CSV_Region_SA<-lapply(FolderMR_SA,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Region_SA[[1]]$Scenario<-"Q1"
CSV_Region_SA[[2]]$Scenario<-"Q2"
CSV_Region_SA[[3]]$Scenario<-"Q3"
CSV_Region_SA[[4]]$Scenario<-"Q4"
CSV_Region_SA[[5]]$Scenario<-"RT"

CombinedRegionLC_SA<-do.call(rbind.data.frame,CSV_Region_SA)
CombinedRegionLC_SA<-subset(CombinedRegionLC_SA, CombinedRegionLC_SA$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedRegionLC_SA, paste0(Comb_outputRegion,"V21016_ZonalHistogram_AllScenarios_RGN_SA", ".csv"), row.names=FALSE)


# ----------------------------------------------
# FULL STUDY AREA (INCL BUFFER)
# ----------------------------------------------\

FolderS<-list.files(Comb_outputBuffer, pattern="_buffer", full.names = TRUE) 
CSV_Sum<-lapply(FolderS,function(i){
  read.csv(i)
})


CSV_Sum[[1]]$Scenario<-"Q1"
CSV_Sum[[2]]$Scenario<-"Q2"
CSV_Sum[[3]]$Scenario<-"Q3"
CSV_Sum[[4]]$Scenario<-"Q4"
CSV_Sum[[5]]$Scenario<-"RT"

CombinedSumLC<-do.call(rbind.data.frame,CSV_Sum)
write.csv(CombinedSumLC, paste0(Comb_outputBuffer,"v2016_ZonalHistogram_AllScenarios_SA_Buffer.csv"), row.names=FALSE)

# ----------------------------------------------
# STUDY AREA ONLY (NO BUFFER)
# ----------------------------------------------

FolderS_SA<-list.files(Comb_outputSA, pattern="_SA", full.names = TRUE) 
CSV_Sum_SA<-lapply(FolderS_SA,function(i){
  read.csv(i)
})



CSV_Sum_SA[[1]]$Scenario<-"Q1"
CSV_Sum_SA[[2]]$Scenario<-"Q2"
CSV_Sum_SA[[3]]$Scenario<-"Q3"
CSV_Sum_SA[[4]]$Scenario<-"Q4"
CSV_Sum_SA[[5]]$Scenario<-"RT"

CombinedSumLC_SA<-do.call(rbind.data.frame,CSV_Sum_SA)
write.csv(CombinedSumLC_SA, paste0(Comb_outputSA,"v2016_ZonalHistogram_AllScenarios_SA.csv"), row.names=FALSE)

# ----------------------------------------------
# ----------------------------------------------
# MANIPULATION FOR GRAPHS
#NOTE: ALTER BASED ON WHICH INITIAL TABLES YOU WANT TO USE 
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# COUNTY
# ----------------------------------------------


# ----------------------------------------------
# USE IF TABLES ALREADY GENERATED:

# CombinedMeltPC<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/Tables/v2016_County/v2016_ZonalHistogram_AllScenarios_CTNY_SA.csv")




#PERCENT CHANGE INDIVIDUAL COUNTIES
CombinedMeltLC<-CombinedSumLC_SA #set combinedmeltlc_sa to combinedmeltlc if want just study area 
#CombinedMeltLC<-CombinedSumLC 

CombinedMeltLC$Rowid_<-NULL

CombinedMeltLCT2<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==2)
CombinedMeltLCT7<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==7)
CombinedMeltLC27<-cbind(CombinedMeltLCT2,CombinedMeltLCT7)


CombinedMeltLC27<-CombinedMeltLC27[,c(1,2,3,4,6,7)]
CombinedMeltLC27<-mutate(CombinedMeltLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedMeltLC27$PercentChange<-round(CombinedMeltLC27$PercentChange, digits = 2)
#CombinedMeltLC27$PercentChange<-paste0(CombinedMeltLC27$PercentChange,"%")


CombinedMeltLC27$TimeStep<-7
PercentChangeMelt<-CombinedMeltLC27[,c(1,2,4,7)]

CombinedMeltPC<-merge(CombinedMeltLC,PercentChangeMelt, by=c("Scenario","TimeStep","LABEL"), all.x=TRUE)

CombinedMeltPC<-subset(CombinedMeltPC, TimeStep > 1)

# ----------------------------------------------
# SUBSET BY LANDCOVER TYPE
DevelopmentM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "3")
ForestM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "5")
GrassM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "6")
CropM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "7")


# ----------------------------------------------
# SUBSET BY COUNTY EXAMPLE
Loudoun<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51107")
Frederick<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51069")
Fauquier<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51061")
Shenandoah<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51171")
Albemarle<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51003")
Rockingham<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51165")




# ----------------------------------------------
# SUBSET BY COUNTY AND LANDCOVER TYPE EXAMPLES
FauquierD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51061")
FauquierF<-subset(ForestM, ForestM$variable == "GEOID_51061")
FauquierG<-subset(GrassM, GrassM$variable == "GEOID_51061")
FauquierC<-subset(CropM, CropM$variable == "GEOID_51061")


FrederickD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51069")
FrederickF<-subset(ForestM, ForestM$variable == "GEOID_51069")
FrederickG<-subset(GrassM, GrassM$variable == "GEOID_51069")
FrederickC<-subset(CropM, CropM$variable == "GEOID_51069")

AlbemarleD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51003")
AlbemarleF<-subset(ForestM, ForestM$variable == "GEOID_51003")
AlbemarleG<-subset(GrassM, GrassM$variable == "GEOID_51003")
AlbemarleC<-subset(CropM, CropM$variable == "GEOID_51003")

# ----------------------------------------------
# REGION
# ----------------------------------------------
#---------------------------------------------------#
#PERCENT CHANGE REGION
#CombinedRegionLC<-CombinedRegionLC_SA #set study area equal if want graphs just for study area. Saves repeating a bunch of code.

CombinedRegionLC$Rowid_<-NULL

CombinedRegionLCT2<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==2)
CombinedRegionLCT7<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==7)
CombinedRegionLC27<-cbind(CombinedRegionLCT2,CombinedRegionLCT7)


CombinedRegionLC27<-CombinedRegionLC27[,c(1,2,3,5,6,7,8,13)]
CombinedRegionLC27<-mutate(CombinedRegionLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedRegionLC27$PercentChange<-round(CombinedRegionLC27$PercentChange, digits = 0)
CombinedRegionLC27$PercentChange<-paste0(CombinedRegionLC27$PercentChange,"%")


CombinedRegionLC27$TimeStep<-7
PercentChangeRegion<-CombinedRegionLC27[,c(1,2,3,7,9)]

CombinedRegionPC<-merge(CombinedRegionLC,PercentChangeRegion, by=c("Scenario","TimeStep","LABEL","variable"), all.x=TRUE)

# ----------------------------------------------
# SUM
# ----------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PERCENT CHANGE of SUM 
#CombinedSumLC<-CombinedSumLC_SA #set equal to study area if desired 
CombinedSumLC<-CombinedSumLC_SA

CombinedSumLCT2<-subset(CombinedSumLC, CombinedSumLC$TimeStep ==2)
CombinedSumLCT7<-subset(CombinedSumLC, CombinedSumLC$TimeStep ==7)
CombinedSumLC27<-cbind(CombinedSumLCT2,CombinedSumLCT7)
CombinedSumLC27<-CombinedSumLC27[,c(1,2,3,4,7,8)]
CombinedSumLC27<-mutate(CombinedSumLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100) #calculate percent change after only haveing time step 2 and 7
CombinedSumLC27$PercentChange<-round(CombinedSumLC27$PercentChange, digits = 2)


CombinedSumLC27$TimeStep<-7
PercentChange<-CombinedSumLC27[,c(1,2,4,7)]

CombinedSumPC<-merge(CombinedSumLC,PercentChange, by=c("Scenario","TimeStep","LABEL"), all.x=TRUE)


#AREADY COMBINED CAN JUST BRING IN TABLE 
CombinedSumPC<-read.csv(paste0(Comb_outputSA,"v2016_ZonalHistogram_AllScenarios_SA.csv"))



# ----------------------------------------------
#SUBSET BY LANDCOVER 
DevelopmentPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "3")
ForestPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "5")
GrassPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "6")
CropPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "7")




#Remove Timestep 1
DevelopmentPC<-subset(DevelopmentPC, DevelopmentPC$TimeStep > 1)
ForestPC<-subset(ForestPC, ForestPC$TimeStep > 1)
GrassPC<-subset(GrassPC, GrassPC$TimeStep >1)
CropPC<-subset(CropPC, CropPC$TimeStep>1)

# ----------------------------------------------
# ----------------------------------------------
#GRAPHS
# ----------------------------------------------
# ----------------------------------------------
#IF GRAPH LOOKS weird make sure LABEL is set as a factor 
#Graphs for individual counties 
#When saved as individual graph v2015_Fred_crop
#when saved for ggarrange crop,development, forest, grass 

#CHANGE TO SIZE 40 IF GRAPHS ARE NOT GOING TO BE ARRANGED


# ----------------------------------------------
# LANDCOVER TYPE: COMPARE SCENARIOS


library(ggplot2)

windows()
Grass<-ggplot(GrassPC, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c( "2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #scale_y_continuous(name =expression('Total Area km'^2))+
  #Forest#scale_y_continuous(name =expression('Total Area km'^2), limits = c(9550,9950), breaks=c(9600,9700,9800,9900))+
  #grass# 
  scale_y_continuous(name =expression('Total Area km'^2), limits=c(5250,5350), breaks=c(5275,5300,5325,5350))+
  #development#scale_y_continuous(name =expression('Total Area km'^2),  limits=c(575,1100), breaks=c(600,700,800,900,1000))+
  #crop# scale_y_continuous(name =expression('Total Area km'^2), limits=c(550,640), breaks=c(550,575,600,625))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin=margin(t=20, r=0, b=0, l =0)))+
  theme(axis.text=element_text(size=40, colour="black"),
        axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  #geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=20, show.legend=FALSE)+
  theme(axis.line = element_line(size=1.5, colour="grey69"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/ZonalHistogram_ggplots/v2016_StudyArea/")
png("v2016_SA_Grass.png", width=480, height=480, units="px", res=300) #can't put units and resolution
Grass
dev.off()


ggsave(file="v2016_SA_Grass.png", dpi=300,width=15, height=15)



# ----------------------------------------------
# iNDIVIDUAL COUNTIES

windows()

#FAUQUIER
FauqC<-ggplot(FauquierD, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=3)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #Forest#scale_y_continuous(name =expression('Total Area km'^2), limits = c(675,775), breaks=c(675,700,725,750,775))+
  #grass# scale_y_continuous(name =expression('Total Area km'^2), limits=c(550,615), breaks=c(550,565,580,595,610))+
  #development#
  scale_y_continuous(name =expression('Total Area km'^2),  limits=c(0,125), breaks=c(25,50,75,100,125))+
  #crop# scale_y_continuous(name =expression('Total Area km'^2), limits=c(115,151), breaks=c(115,125,135,145))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin=margin(t=20, r=0, b=0, l =0)))+
  theme(axis.text=element_text(size=40, colour="black"),
        axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+#+
  #geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=20, show.legend=FALSE)+
  theme(axis.line = element_line(size=1.5, colour="grey69"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

setwd("U:/CLI/Presentations/ESA-08-XX-2018")
png("v2016_Fauq_Crop.png", width=480, height=480, units="px", res=300) #can't put units and resolution
FauqC
dev.off()


ggsave(file="v2016_Fauq_Crop.png", dpi=300,width=15, height=15)



#FREDERICK
FredF<-ggplot(FrederickF, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=3)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #Forest#
  scale_y_continuous(name =expression('Total Area km'^2), limits=c(525,625), breaks=c(525,550,575,600,625))+
  #grass#scale_y_continuous(name =expression('Total Area km'^2), limits=c(300,330), breaks=c(305,310,315,320,325))+
  #development#scale_y_continuous(name =expression('Total Area km'^2), limits=c(50,150), breaks=c(50,75,100,125,150))+
  #crop#scale_y_continuous(name =expression('Total Area km'^2), limits=c(10,16), breaks=c(10,11,12,13,14,15))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin=margin(t=20, r=0, b=0, l =0)))+
  theme(axis.text=element_text(size=40, colour="black"),
        axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+#+
  #geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=20, show.legend=FALSE)+
  theme(axis.line = element_line(size=1.5, colour="grey69"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

setwd("U:/CLI/Presentations/ESA-08-XX-2018")
png("v2016_Fred_Development.png", width=480, height=480, units="px", res=300) #can't put units and resolution
FredD
dev.off()


ggsave(file="v2016_Fred_Development.png", dpi=300,width=15, height=15)



#-----------------------------------------------------------#
#Table to show percent change

#Frederick County 
#Fred_PC<-subset(PercentChangeMelt, variable=="GEOID_51069")
#Fred_Q1<-subset(Fred_PC, Scenario =="Q1")
#Fred_Q2<-subset(Fred_PC, Scenario =="Q2")
#Fred_Q3<-subset(Fred_PC, Scenario =="Q3")
#Fred_Q4<-subset(Fred_PC, Scenario =="Q4")
#Fred_RT<-subset(Fred_PC, Scenario =="RT")

#Fred_Table<-cbind(Fred_RT[,5],Fred_Q1[,5],Fred_Q2[,5],Fred_Q3[,5],Fred_Q4[,5])
#Fred_Table<-as.data.frame(Fred_Table)
#colnames(Fred_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
#rownames(Fred_Table)<-c("Development", "Forest", "Grass", "Crop")

#Fred_Table_plot<-ggtexttable(Fred_Table, theme=ttheme("mBlackWhite", base_size=15))

#windows()
#FrederickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

#windows()
#ggarrange(FrederickGraph, Fred_Table_plot, ncol=2, nrow=1, widths =c(1,.35))


#Fauquier County 
#Fauq_PC<-subset(PercentChangeMelt, variable=="GEOID_51061")
#Fauq_Q1<-subset(Fauq_PC, Scenario =="Q1")
#Fauq_Q2<-subset(Fauq_PC, Scenario =="Q2")
#Fauq_Q3<-subset(Fauq_PC, Scenario =="Q3")
#Fauq_Q4<-subset(Fauq_PC, Scenario =="Q4")
#Fauq_RT<-subset(Fauq_PC, Scenario =="RT")

#Fauq_Table<-cbind(Fauq_RT[,5],Fauq_Q1[,5],Fauq_Q2[,5],Fauq_Q3[,5],Fauq_Q4[,5])
#Fauq_Table<-as.data.frame(Fauq_Table)
#colnames(Fauq_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
#rownames(Fauq_Table)<-c("Development", "Forest", "Grass", "Crop")

#Fauq_Table_plot<-ggtexttable(Fauq_Table, theme=ttheme("mBlackWhite", base_size=15))

#windows()
#FauqerickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

#windows()
#ggarrange(FauqerickGraph, Fauq_Table_plot, ncol=2, nrow=1, widths =c(1,.35))

# ----------------------------------------------
# GRAPH PERCENT CHANGE 
#windows()
#ggplot(DevelopmentPC, aes(x=Scenario, y=PercentChange, fill=Scenario))+
#  geom_bar(stat="identity", position = 'dodge')+
 # scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
 # scale_y_continuous(name="Percent Change", limits=c(-100,120), labels=c("-100%","-50%", "-0%","50%", "100%", "120%"))+
 # theme_bw()+
 # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
 # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
 # theme(axis.text.y =element_text(size=40),
    #    axis.text.x =element_blank(),
  #      axis.title.x=element_blank(), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
 # theme(plot.margin=unit(c(1,1,1,1), "in"))+
 # theme(panel.border=element_blank())+
  #geom_hline(yintercept=0, size=1.5)+
 # geom_text(aes(label=paste0(PercentChange,"%")), vjust=1.6, size=10, colour="white")+
  #theme(axis.line.y =element_line(size=1.5))



# ----------------------------------------------
# CODE TO EXPORT
# ----------------------------------------------
setwd("X:/Scenario Planning/Graphics/Map Images/4_17")
png("v2015_Fauq_development.png", width=480, height=480, units="px", res=300) #can't put units and resolution
v2015_Fauq_development
dev.off()


ggsave(file="v2015_Fauq_development.png", dpi=300,width=15, height=15)



#Graphs for entire study region. All scenarios but one type of land cover 




# ----------------------------------------------
# ----------------------------------------------
# EXPLORATORY ANALYSIS
# ----------------------------------------------
# ----------------------------------------------


#--------------------------------------------------------------#
#Exploratory Analysis County 

FolderReshape<-list.files(Comb_outputReshape, pattern=".csv", full.names = TRUE) 
CSV_Reshape<-lapply(FolderReshape,function(i){
  read.csv(i)
})

Q1<-CSV_Reshape[[1]]
Q2<-CSV_Reshape[[2]]
Q3<-CSV_Reshape[[3]]
Q4<-CSV_Reshape[[4]]
RT<-CSV_Reshape[[5]]

#Development
Q1_Dev<-Q1[2:57,1:9]
Q1_Dev<-mutate(Q1_Dev, PercentChange3=((Change3/Q1_Dev$X2011)*100))
Q1_Dev$Scenario<-"Q1"

Q2_Dev<-Q2[2:57,1:9]
Q2_Dev<-mutate(Q2_Dev, PercentChange3=((Change3/Q2_Dev$X2011)*100))
Q2_Dev$Scenario<-"Q2"

Q3_Dev<-Q3[2:57,1:9]
Q3_Dev<-mutate(Q3_Dev, PercentChange3=((Change3/Q3_Dev$X2011)*100))
Q3_Dev$Scenario<-"Q3"

Q4_Dev<-Q4[2:57,1:9]
Q4_Dev<-mutate(Q4_Dev, PercentChange3=((Change3/Q4_Dev$X2011)*100))
Q4_Dev$Scenario<-"Q4"

RT_Dev<-RT[2:57,1:9]
RT_Dev<-mutate(RT_Dev, PercentChange3=((Change3/RT_Dev$X2011)*100))
RT_Dev$Scenario<-"RT"

Development<-rbind(Q1_Dev, Q2_Dev, Q3_Dev, Q4_Dev, RT_Dev)
colnames(Development)<-c("GEOID", "2001", "2011", "2021", "2031", "2041", "2051", "2061", "Change", "PercentChange", "Scenario")


Development<-Development %>% 
  filter(PercentChange > 44.17)

Development$Change<-NULL
Development$PercentChange<-NULL

Development<-melt(Development, id=c("GEOID", "Scenario"))
Development$valuekm<-Development$value*(900/1000000)
colnames(Development)<-c("GEOID",  "Scenario", "TimeStep", "value", "valuekm")


#counties in the study area
Development<-subset(Development, Development$GEOID %in% c( "GEOID_51069" , "GEOID_51107" , "GEOID_51171" , "GEOID_51061" , "GEOID_51157" , "GEOID_51113" , "GEOID_51137" , "GEOID_51139",  "GEOID_51015" , "GEOID_51047" , "GEOID_51043",  "GEOID_51187",  "GEOID_51079" , "GEOID_51165" , "GEOID_51003", "GEOID_51840" ,  "GEOID_51540",  "GEOID_51660" , "GEOID_51790" , "GEOID_51820"))



windows()
ggplot(Development, aes(x=TimeStep, y=valuekm, colour=GEOID, group=GEOID))+
  geom_line(size=2)+
  facet_grid(.~Scenario)

#Forest 
Q1_For<-Q1[2:57,c(1,10,11,12,13,14,15,16,17)]
Q1_For<-mutate(Q1_For, PercentChange5=((Change5/Q1_For$X2011)*100))
Q1_For$Scenario<-"Q1"

Q2_For<-Q2[2:57,c(1,10,11,12,13,14,15,16,17)]
Q2_For<-mutate(Q2_For, PercentChange5=((Change5/Q2_For$X2011)*100))
Q2_For$Scenario<-"Q2"

Q3_For<-Q3[2:57,c(1,10,11,12,13,14,15,16,17)]
Q3_For<-mutate(Q3_For, PercentChange5=((Change5/Q3_For$X2011)*100))
Q3_For$Scenario<-"Q3"

Q4_For<-Q4[2:57,c(1,10,11,12,13,14,15,16,17)]
Q4_For<-mutate(Q4_For, PercentChange5=((Change5/Q4_For$X2011)*100))
Q4_For$Scenario<-"Q4"

RT_For<-RT[2:57,c(1,10,11,12,13,14,15,16,17)]
RT_For<-mutate(RT_For, PercentChange5=((Change5/RT_For$X2011)*100))
RT_For$Scenario<-"RT"

Forest<-rbind(Q1_For, Q2_For, Q3_For, Q4_For, RT_For)
colnames(Forest)<-c("GEOID", "2001", "2011", "2021", "2031", "2041", "2051", "2061", "Change", "PercentChange", "Scenario")


Forest<-Forest %>% 
  filter(PercentChange < -4.02) # lowest for entire study area 

Forest$Change<-NULL
Forest$PercentChange<-NULL

Forest<-melt(Forest, id=c("GEOID", "Scenario"))
Forest$valuekm<-Forest$value*(900/1000000)

Forest<-subset(Forest, Forest$GEOID %in% c( "GEOID_51069" , "GEOID_51107" , "GEOID_51171" , "GEOID_51061" , "GEOID_51157" , "GEOID_51113" , "GEOID_51137" , "GEOID_51139",  "GEOID_51015" , "GEOID_51047" , "GEOID_51043",  "GEOID_51187",  "GEOID_51079" , "GEOID_51165" , "GEOID_51003", "GEOID_51840" ,  "GEOID_51540",  "GEOID_51660" , "GEOID_51790" , "GEOID_51820"))


colnames(Forest)<-c("GEOID",  "Scenario", "TimeStep", "value", "valuekm")


Forest<-Forest %>%
  filter(valuekm > 9.97) #greater than median 


windows()
ggplot(Forest, aes(x=TimeStep, y=valuekm, colour=GEOID, group=GEOID))+
  geom_line(size=2)+
  facet_grid(.~Scenario)

#Grass
Q1_Gras<-Q1[2:57,c(1,18,19,20,21,22,23,24,25)]
Q1_Gras<-mutate(Q1_Gras, PercentChange6=((Change6/Q1_For$X2011)*100))
Q1_Gras$Scenario<-"Q1"

Q2_Gras<-Q2[2:57,c(1,18,19,20,21,22,23,24,25)]
Q2_Gras<-mutate(Q2_Gras, PercentChange6=((Change6/Q2_Gras$X2011)*100))
Q2_Gras$Scenario<-"Q2"

Q3_Gras<-Q3[2:57,c(1,18,19,20,21,22,23,24,25)]
Q3_Gras<-mutate(Q3_Gras, PercentChange6=((Change6/Q3_Gras$X2011)*100))
Q3_Gras$Scenario<-"Q3"

Q4_Gras<-Q4[2:57,c(1,18,19,20,21,22,23,24,25)]
Q4_Gras<-mutate(Q4_Gras, PercentChange6=((Change6/Q4_Gras$X2011)*100))
Q4_Gras$Scenario<-"Q4"

RT_Gras<-RT[2:57,c(1,18,19,20,21,22,23,24,25)]
RT_Gras<-mutate(RT_Gras, PercentChange6=((Change6/RT_Gras$X2011)*100))
RT_Gras$Scenario<-"RT"

Grass<-rbind(Q1_Gras, Q2_Gras, Q3_Gras, Q4_Gras, RT_Gras)
colnames(Grass)<-c("GEOID", "2001", "2011", "2021", "2031", "2041", "2051", "2061", "Change", "PercentChange", "Scenario")


Grass<-Grass%>% 
  filter(PercentChange > 3.72)

Grass$Change<-NULL
Grass$PercentChange<-NULL

Grass<-melt(Grass, id=c("GEOID", "Scenario"))
Grass$valuekm<-Grass$value*(900/1000000)

Grass<-subset(Grass, Grass$GEOID %in% c( "GEOID_51069" , "GEOID_51107" , "GEOID_51171" , "GEOID_51061" , "GEOID_51157" , "GEOID_51113" , "GEOID_51137" , "GEOID_51139",  "GEOID_51015" , "GEOID_51047" , "GEOID_51043",  "GEOID_51187",  "GEOID_51079" , "GEOID_51165" , "GEOID_51003", "GEOID_51840" ,  "GEOID_51540",  "GEOID_51660" , "GEOID_51790" , "GEOID_51820"))


colnames(Grass)<-c("GEOID",  "Scenario", "TimeStep", "value", "valuekm")

Grass<-Grass%>% 
  filter(valuekm > 1)

windows()
ggplot(Grass, aes(x=TimeStep, y=valuekm, colour=GEOID, group=GEOID))+
  geom_line(size=2)+
  facet_grid(.~Scenario)

#Crop
Q1_Crop<-Q1[2:57,c(1,26,27,28,29,30,31,32,33)]
Q1_Crop<-mutate(Q1_Crop, PercentChange7=((Change7/Q1_For$X2011)*100))
Q1_Crop$Scenario<-"Q1"

Q2_Crop<-Q2[2:57,c(1,26,27,28,29,30,31,32,33)]
Q2_Crop<-mutate(Q2_Crop, PercentChange7=((Change7/Q2_Crop$X2011)*100))
Q2_Crop$Scenario<-"Q2"

Q3_Crop<-Q3[2:57,c(1,26,27,28,29,30,31,32,33)]
Q3_Crop<-mutate(Q3_Crop, PercentChange7=((Change7/Q3_Crop$X2011)*100))
Q3_Crop$Scenario<-"Q3"

Q4_Crop<-Q4[2:57,c(1,26,27,28,29,30,31,32,33)]
Q4_Crop<-mutate(Q4_Crop, PercentChange7=((Change7/Q4_Crop$X2011)*100))
Q4_Crop$Scenario<-"Q4"

RT_Crop<-RT[2:57,c(1,26,27,28,29,30,31,32,33)]
RT_Crop<-mutate(RT_Crop, PercentChange7=((Change7/RT_Crop$X2011)*100))
RT_Crop$Scenario<-"RT"

Crop<-rbind(Q1_Crop, Q2_Crop, Q3_Crop, Q4_Crop, RT_Crop)
colnames(Crop)<-c("GEOID", "2001", "2011", "2021", "2031", "2041", "2051", "2061", "Change", "PercentChange", "Scenario")


Crop<-Crop%>% 
  filter(PercentChange < -8.84)

Crop$Change<-NULL
Crop$PercentChange<-NULL

Crop<-melt(Crop, id=c("GEOID", "Scenario"))
Crop$valuekm<-Crop$value*(900/1000000)

Crop<-subset(Crop, Crop$GEOID %in% c( "GEOID_51069" , "GEOID_51107" , "GEOID_51171" , "GEOID_51061" , "GEOID_51157" , "GEOID_51113" , "GEOID_51137" , "GEOID_51139",  "GEOID_51015" , "GEOID_51047" , "GEOID_51043",  "GEOID_51187",  "GEOID_51079" , "GEOID_51165" , "GEOID_51003", "GEOID_51840" ,  "GEOID_51540",  "GEOID_51660" , "GEOID_51790" , "GEOID_51820"))


colnames(Crop)<-c("GEOID",  "Scenario", "TimeStep", "value", "valuekm")

Crop<-Crop%>% 
  filter(valuekm > 7.28) #median

windows()
ggplot(Crop, aes(x=TimeStep, y=valuekm, colour=GEOID, group=GEOID))+
  geom_line(size=2)+
  facet_grid(.~Scenario)

#County no City 
CombinedMeltC<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/Tables/County/CombinedMeltC_SA_NoCity.csv")

Dev<-CombinedMeltC %>%
  filter(LABEL ==3)
For<-CombinedMeltC %>%
  filter(LABEL ==5)
Gras<-CombinedMeltC %>%
  filter(LABEL == 6) 
Crop<-CombinedMeltC %>%
  filter(LABEL == 7)

windows()
ggplot(Crop, aes(x=TimeStep, y=valuekm, colour=variable, group=variable))+
  geom_line(size=2)+
  facet_grid(.~Scenario)
#-------------------------------------------------------#
#Region Exploratory

Region<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/Tables/Region/CombinedMeltR_SA.csv")

Region_dev<-Region %>%
  filter(LABEL==3)
Region_for<-Region %>%
  filter(LABEL == 5)
Region_gras<-Region %>%
  filter(LABEL ==6)
Region_crop<-Region %>%
  filter(LABEL == 7)



windows()
ggplot(Region_crop, aes(x=TimeStep, y=valuekm, colour=factor(Region), group=factor(Region)))+
  geom_line(size=2)+
  facet_grid(.~Scenario)

#based on regions exploratory county

County<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/Tables/County/CombinedMeltC_SA.csv")

Counties<-County %>%
  filter(Region == 5 | Region == 2)

Counties_dev<-Counties %>%
  filter(LABEL ==3)
Counties_for<-Counties %>%
  filter(LABEL ==5)
Counties_gras<-Counties %>%
  filter(LABEL == 6)
Counties_crop<-Counties %>%
  filter(LABEL == 7)

windows()
ggplot(Counties_crop, aes(x=TimeStep, y=valuekm, colour=variable, group=variable))+
  geom_line(size=2)+
  facet_grid(. ~ Scenario)
