############################ 
#PURPOSE: Combine merged tables and use them to make graphs. 
#CREATOR: Sarah Halperin
#CONTACT: halperins@si.edu
#INPUT: U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\BasicDataAnalyses\Zonal_Histogram 
#OUTPUT: U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\BasicDataAnalyses\OutputVisuals\ZonalHistogram_ggplots
#DEVELOPED: 4-30-18

#UPDATED BY: Erin Carroll
#CONTACT: carrolle@si.edu
#DATE: 1-9-18
#PURPOSE: clean figures for LULC paper

#NOTES: There are multiple options here for organizing data and generating graphs, depending on what you want to display. 
# Section 1 (lines 91-262, Sarah): imports all Scenario tables and combines them.
# Section 2 (lines 266-637 Erin, lines 639-789 Sarah): generates possible graphs, including those used in the LULC paper. IMPORTANT: IT IS NOT NECESSARY TO RUN ANY PART OF SECTION 1 IN ORDER TO RUN SECTIONS 2.1 or 2.2.
# Section 3 (lines 794-1060, Sarah) contains exploratory analysis for county and region. These exploratory analyses are very rough.


# Notes from Sarah:
# Multiple issues with script:
# Why are some things called "melt" ?? shouldn't they be counties?
# in this line of code, "variable" refers to the county name, but in this case it sould be "Region"

# CombinedRegionPC<-merge(CombinedRegionLC,PercentChangeRegion, by=c("Scenario","TimeStep","LABEL","Region"), all.x=TRUE)  # Region is the variable! But, why would you ever merge this??

# CombinedRegionLC27<-CombinedRegionLC27[,c(1,2,3,5,6,7,8,13)] # QUESTION: WHY IS THIS SAYING 13 COLUMNS? THERE ARE ONLY 10 COLUMNS

# > str(CombinedRegionLC27)
# 'data.frame':	160 obs. of  10 variables:
# $ LABEL   : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep: int  2 2 2 2 2 2 2 2 2 2 ...
# $ Region  : int  1 1 1 1 2 2 2 2 3 3 ...
# $ valuekm : num  147.7 2500.8 1242.9 65.3 678.8 ...
# $ Scenario: chr  "Q1" "Q1" "Q1" "Q1" ...
# $ LABEL   : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep: int  7 7 7 7 7 7 7 7 7 7 ...
# $ Region  : int  1 1 1 1 2 2 2 2 3 3 ...
# $ valuekm : num  245.5 2413.3 1232.8 65.1 1287.1 ...
# $ Scenario: chr  "Q1" "Q1" "Q1" "Q1" ...

#IMPORTANT: 
# Make sure the correct version folder is pulled in! Do a search for phrases associated with "version" (capitalized or not)
# there are options to select the buffer or the study area. Make sure you know which you have!!

# Watch capitalization!!! There are two files with "_cnty". one is capitalized, and the other is not.
# v2016_ZonalHistogram_AllScenarios_CNTY
# v2016_ZonalHistogram_AllScenarios_CNTY_SA
# v2016_ZonalHistogram_AllScenarios_RGN




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
library(stringr)

# ----------------------------------------------
# READ INPUT FILES:
#Set file locations
version<-"/StudyArea_V201/SA_V2016"
version_table<-paste0("U:/CLI/Dinamica_Runs",version, "/BasicDataAnalyses/Zonal_Histogram/NoPL/")
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

# ----------------------------------------------
# ----------------------------------------------
# SECTION 1 - MANIPULATION FOR GRAPHS
# ----------------------------------------------
# ----------------------------------------------


#NOTE: ALTER BASED ON WHICH INITIAL TABLES YOU WANT TO USE
# county, Region, Sum
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# COUNTY
# ----------------------------------------------


# ----------------------------------------------
# Tables already generated in 8_ZonalHist_50years_MergeTables.R
CombinedMeltPC<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/Tables/v2016_County/v2016_ZonalHistogram_AllScenarios_cnty_SA.csv")


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


CombinedMeltLC27$TimeStep<-27
PercentChangeMelt<-CombinedMeltLC27[,c(1,2,4,7)]

# I DON'T KNOW WHY YOU WOULD WANT TO DO THIS.
# CombinedMeltPC<-merge(CombinedMeltLC,PercentChangeMelt, by=c("Scenario","TimeStep","LABEL"), all.x=TRUE)

# CombinedMeltPC<-subset(CombinedMeltPC, TimeStep > 1)

# I CANNOT GET THE BELOW TO WORK:

# # ----------------------------------------------
# # SUBSET BY LANDCOVER TYPE
# DevelopmentM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "3")
# ForestM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "5")
# GrassM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "6")
# CropM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "7")


# # ----------------------------------------------
# # SUBSET BY COUNTY EXAMPLE
# Loudoun<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51107")
# Frederick<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51069")
# Fauquier<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51061")
# Shenandoah<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51171")
# Albemarle<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51003")
# Rockingham<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51165")


# # ----------------------------------------------
# # SUBSET BY COUNTY AND LANDCOVER TYPE EXAMPLES
# FauquierD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51061")
# FauquierF<-subset(ForestM, ForestM$variable == "GEOID_51061")
# FauquierG<-subset(GrassM, GrassM$variable == "GEOID_51061")
# FauquierC<-subset(CropM, CropM$variable == "GEOID_51061")


# FrederickD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51069")
# FrederickF<-subset(ForestM, ForestM$variable == "GEOID_51069")
# FrederickG<-subset(GrassM, GrassM$variable == "GEOID_51069")
# FrederickC<-subset(CropM, CropM$variable == "GEOID_51069")

# AlbemarleD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51003")
# AlbemarleF<-subset(ForestM, ForestM$variable == "GEOID_51003")
# AlbemarleG<-subset(GrassM, GrassM$variable == "GEOID_51003")
# AlbemarleC<-subset(CropM, CropM$variable == "GEOID_51003")

# ----------------------------------------------
# REGION
# ----------------------------------------------
#---------------------------------------------------#
#PERCENT CHANGE REGION

CombinedRegionLC<-CombinedRegionLC_SA #set study area equal if want graphs just for study area. Saves repeating a bunch of code.

CombinedRegionLC$Rowid_<-NULL

CombinedRegionLCT2<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==2)
CombinedRegionLCT7<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==7)
CombinedRegionLC27<-cbind(CombinedRegionLCT2,CombinedRegionLCT7)


# > str(CombinedRegionLC27)
# 'data.frame':	160 obs. of  10 variables:
# $ LABEL   : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep: int  2 2 2 2 2 2 2 2 2 2 ...
# $ Region  : int  1 1 1 1 2 2 2 2 3 3 ...
# $ valuekm : num  147.7 2500.8 1242.9 65.3 678.8 ...
# $ Scenario: chr  "Q1" "Q1" "Q1" "Q1" ...
# $ LABEL   : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep: int  7 7 7 7 7 7 7 7 7 7 ...
# $ Region  : int  1 1 1 1 2 2 2 2 3 3 ...
# $ valuekm : num  245.5 2413.3 1232.8 65.1 1287.1 ...
# $ Scenario: chr  "Q1" "Q1" "Q1" "Q1" ...

CombinedRegionLC27<-CombinedRegionLC27[,c(1,2,3,4,5,9)] # Iara changed this 10-4-18:
CombinedRegionLC27<-mutate(CombinedRegionLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedRegionLC27$PercentChange<-round(CombinedRegionLC27$PercentChange, digits = 0)
# CombinedRegionLC27$PercentChange<-paste0(CombinedRegionLC27$PercentChange,"%")# Iara changed this. We don't need a percent symbol:


# > str(CombinedRegionLC27)
# 'data.frame':	700 obs. of  7 variables:
# $ LABEL        : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep     : int  2 2 2 2 2 2 2 2 2 2 ...
# $ Region       : int  1 1 1 1 2 2 2 2 3 3 ...
# $ valuekm      : num  147.7 2500.8 1242.9 65.3 188.6 ...
# $ Scenario     : chr  "Q1" "Q1" "Q1" "Q1" ...
# $ valuekm.1    : num  245.5 2413.3 1232.8 65.1 369.2 ...
# $ PercentChange: num  66 -4 -1 0 96 -7 -1 -18 14 -2 ...


CombinedRegionLC27$TimeStep<-27
PercentChangeRegion<-CombinedRegionLC27[,c(1,2,3,5,7)]

# CombinedRegionPC<-merge(CombinedRegionLC,PercentChangeRegion, by=c("Scenario","TimeStep","LABEL","Region"), all.x=TRUE)  # Region is the variable! But, why would you ever merge this??

# > str(PercentChangeRegion)
# 'data.frame':	700 obs. of  5 variables:
# $ LABEL        : int  3 5 6 7 3 5 6 7 3 5 ...
# $ TimeStep     : num  27 27 27 27 27 27 27 27 27 27 ...
# $ Region       : int  1 1 1 1 2 2 2 2 3 3 ...
# $ Scenario     : chr  "Q1" "Q1" "Q1" "Q1" ...
# $ PercentChange: num  66 -4 -1 0 96 -7 -1 -18 14 -2 ...

# THIS IS WHERE I ENDED. NEED TO USE ABOVE AS REFERENCE TO CREATE SUMMED REGION VALUES AND MAKE SURE COUNTY VALUES ARE CORRECTLY SOURCED.


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
CombinedSumPC<-read.csv(paste0(Comb_outputSA,"All/v2016_ZonalHistogram_AllScenarios_SA.csv"))



# ----------------------------------------------
# ----------------------------------------------
# SECTION 2 - GRAPHS
# 2.1 Line Graphs (total area of each scenario by land use class, over time) -- Erin
# 2.2 Bar Graphs (...percent change?) -- Erin
# 2.3 Individual Counties  -- Sarah
# ----------------------------------------------
# ----------------------------------------------


# ---------------------------------------------------------
## 2.1 LINE GRAPHS - TOTAL AREA OF EACH SCENARIO BY LAND USE CLASS, OVER TIME
# ---------------------------------------------------------

# DATA PREP
CombinedSumPC<-read.csv(paste0(Comb_outputSA,"All/v2016_ZonalHistogram_AllScenarios_SA.csv"))

# New scenario names
levels(CombinedSumPC$Scenario)
levels(CombinedSumPC$Scenario) = c("HS", "HA", "LA", "LS", "RT")

# TimeStep as date (in case you want to create a vertical bar highlighting 2011; works either way)
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 2] = "2011"
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 3] = "2021"
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 4] = "2031"
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 5] = "2041"
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 6] = "2051"
CombinedSumPC$TimeStep[CombinedSumPC$TimeStep == 7] = "2061"

# function to set y-axis breaks
my_breaks <- function(x) {
  seq(floor( (min(x) ) / 100) * 100,
      ceiling( (max(x) ) / 100) * 100,
      50)
}


##### INDIVIDUAL LAND USE CLASS BY SCENARIO, OVER ENTIRE STUDY AREA (ex: grass)

# subset by land use class
DevelopmentPC <- subset(CombinedSumPC, CombinedSumPC$LABEL == "3")
ForestPC <- subset(CombinedSumPC, CombinedSumPC$LABEL == "5")
GrassPC <- subset(CombinedSumPC, CombinedSumPC$LABEL == "6")
CropPC <- subset(CombinedSumPC, CombinedSumPC$LABEL == "7")

# set y-axis limits
## NOTE: edit LABEL and value to correspond to land use class and max/min
blank_data_indv = data.frame(TimeStep=(NA), LABEL=rep(6, each=2),
                             valuekm=c(5200,5400))

# generate graph
Individual_LUType <- ggplot(GrassPC)+
  geom_line(aes(x=TimeStep, y=valuekm, group=Scenario, colour=Scenario, linetype=Scenario), size=0.5)+
  geom_point(aes(x=TimeStep, y=valuekm, group=Scenario, colour=Scenario, shape=Scenario), size=2)+
  scale_shape_manual(values=c(18,15,17,8,19))+
  scale_x_discrete(name = "Observed & Projected Years",
                   labels = c("2011*", "2021", "2031", "2041", "2051", "2061"))+
  scale_y_continuous(name=expression('Total Area sq. km'), breaks = my_breaks) +
  scale_colour_manual(values=c("#36003B","#004191","#297512","#FF4B45","#FFB430"))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t=0, r=0, b=0, l=0), size=10),
        axis.title.x = element_text(margin=margin(t=0, r=0, b=0, l=0), size=10),
        axis.text.x=element_text(colour="black", margin=margin(t=5,r=0,b=10,l=0), face=c("bold", "plain", "plain","plain","plain","plain"), size=c(9,9,9,9,9,9)), 
        axis.text.y=element_text(size=9, colour="black", margin=margin(t=0,r=5,b=0,l=10)),
        axis.ticks.length = unit(0.1, "cm"),
        legend.text=element_text(size=9, margin = margin(t=0,r=0,b=0,l=0)), 
        legend.title=element_blank(),
        legend.key.width = unit(0.35, "in"),
        legend.position = "bottom",
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
        axis.line = element_line(colour="grey45"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 12, hjust = 0, vjust = 3))+
  geom_blank(data=blank_data_indv, aes(TimeStep, valuekm))

Individual_LUType = annotate_figure(Individual_LUType,
                                    fig.lab = "*2011 observed, 2021-2061 projected",
                                    fig.lab.pos = "bottom.left",
                                    fig.lab.size=8)

# Print graph to file
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/ZonalHistogram_ggplots/NoPL/v2016_StudyArea")
png("v2016_SA_Individual_LUType.png", width=7.5, height=5.5, units="in", res=300) #can't put units and resolution
Individual_LUType
dev.off()
ggsave(file="v2016_SA_Individual_LUType.png", dpi=300, width=7.5, height=5.5)



##### ALL FOUR LAND USE TYPES TOGETHER, GROUPED BY FACET_WRAP:

# set facet_wrap labels
LULCclasses <- c(
  '3'="Development",
  '5'="Forest",
  '6'="Grass",
  '7'="Crop")

# set y-axis limits
## NOTE: valuekm=c(DevMin,DevMax,ForestMin,ForestMax,GrassMin,GrassMax,CropMin,CropMax)
blank_data_all = data.frame(TimeStep=(NA), LABEL=rep(c(3,5,6,7), each=2),
                            valuekm=c(600,1100,6700,7200,5200,5400,500,700))

# generate graph
All_LUType <- ggplot(CombinedSumPC)+
  facet_wrap(~LABEL, scales = "free", nrow=2, ncol=2, labeller=labeller(LABEL = LULCclasses))+
  geom_line(aes(x=TimeStep, y=valuekm, group=Scenario, colour=Scenario, linetype=Scenario), size=0.5)+
  geom_point(aes(x=TimeStep, y=valuekm, group=Scenario, colour=Scenario, shape=Scenario), size=1)+
  scale_shape_manual(values=c(18,15,17,8,19))+
  scale_x_discrete(name = "Observed & Projected Years",
                   labels = c("2011*", "2021", "2031", "2041", "2051", "2061"))+
  scale_y_continuous(name=expression('Total Area sq. km'), breaks = my_breaks) +
  scale_colour_manual(values=c("#36003B","#004191","#297512","#FF4B45","#FFB430"))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t=0, r=0, b=0, l=0), size=10),
        axis.title.x = element_text(margin=margin(t=0, r=0, b=0, l=0), size=10),
        axis.text.x=element_text(colour="black", margin=margin(t=5,r=0,b=10,l=0), face=c("bold", "plain", "plain","plain","plain","plain"), size=c(9,9,9,9,9,9)), 
        axis.text.y=element_text(size=9, colour="black", margin=margin(t=0,r=5,b=0,l=10)),
        axis.ticks.length = unit(0.1, "cm"),
        legend.text=element_text(size=9, margin = margin(t=0,r=0,b=0,l=0)), 
        legend.title=element_blank(),
        legend.key.width = unit(0.35, "in"),
        legend.position = "bottom",
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
        axis.line = element_line(colour="grey45"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size = 12, hjust = 0, vjust = 3))+
  geom_blank(data=blank_data_all, aes(TimeStep, valuekm))

All_LUType = annotate_figure(All_LUType,
                             fig.lab = "*2011 observed, 2021-2061 projected",
                             fig.lab.pos = "bottom.left",
                             fig.lab.size=8)

# Print graph to file
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/ZonalHistogram_ggplots/NoPL/v2016_StudyArea")
png("v2016_SA_All_LUType.png", width=7.5, height=5.5, units="in", res=300) #can't put units and resolution
All_LUType
dev.off()
ggsave(file="v2016_SA_All_LUType.png", dpi=300, width=7.5, height=5.5)


# ---------------------------------------------------------
# ---------------------------------------------------------
## 2.2 BAR GRAPHS - NET CHANGE 2011-2061 (SQ. KM) BY:
# COUNTY (study area only)
# REGION (study area only)
# ---------------------------------------------------------
# ---------------------------------------------------------

##### BY COUNTY

# Input data
countySA.data = read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/NoPL/Tables/v2016_CountySA/All/v2016_ZonalHistogram_AllScenarios_CNTY_SA.csv")
cntySa.GEOID = read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/CountyNmsGEOID_cnty.csv")

# New scenario names
levels(countySA.data$Scenario)
levels(countySA.data$Scenario) = c("HS", "HA", "LA", "LS", "RT")

# Calculating net change
countySA.2011 = countySA.data[countySA.data$TimeStep==2,]
countySA.2061 = countySA.data[countySA.data$TimeStep==7,]

countySA.netchange = data.frame(countySA.2011$variable,
                                countySA.2011$LABEL,
                                countySA.2011$valuekm,
                                countySA.2011$Region,
                                countySA.2011$Scenario)
names(countySA.netchange) = c("GEOID","LABEL","valuekm","Region","Scenario")

countySA.netchange$valuekm = countySA.2061$valuekm - countySA.2011$valuekm
range(countySA.netchange$valuekm)

# GEOID to County Names
countySA.netchange$GEOID = substr(countySA.netchange$GEOID,7,11)
countySA.netchange = merge(countySA.netchange, cntySa.GEOID, by="GEOID")

names(countySA.netchange) = c("GEOID","LABEL","valuekm","Region","Scenario", "Din_cty", "County")

# Incorporate independent cities into counties that surround them
countySA.netchange$County[countySA.netchange$County=="Charlottesville city"] = "Albemarle County"
countySA.netchange$County[countySA.netchange$County=="Harrisonburg city"] = "Rockingham County"
countySA.netchange$County[countySA.netchange$County=="Staunton city"] = "Augusta County"
countySA.netchange$County[countySA.netchange$County=="Waynesboro city"] = "Augusta County"
countySA.netchange$County[countySA.netchange$County=="Winchester city"] = "Frederick County"

# Remove "County" from County Names
countySA.netchange$County = str_sub(countySA.netchange$County, end=-8)

# set labels for facet_wrap
LULCclasses = c('3' = "Development",
                '5' = "Forest",
                '6' = "Grass",
                '7' = "Crop")

# set y-axis breaks
my_breaks2 <- seq(-80,120,20)

# set y-axis limits via geom_blank
## NOTE: valuekm=c(DevMin,DevMax,ForestMin,ForestMax,GrassMin,GrassMax,CropMin,CropMax)
range(countySA.netchange$valuekm[countySA.netchange$LABEL==3])
range(countySA.netchange$valuekm[countySA.netchange$LABEL==5])
range(countySA.netchange$valuekm[countySA.netchange$LABEL==6])
range(countySA.netchange$valuekm[countySA.netchange$LABEL==7])

blank_data_cty = data.frame(County=NA, LABEL=rep(c(3,5,6,7), each=2),
                            valuekm=c(0,120,-80,20,-50,50,-40,10))

# grouping county by region on x-axis
region1 <- unique(countySA.netchange$County[countySA.netchange$Region==1])
region2 <- unique(countySA.netchange$County[countySA.netchange$Region==2])
region3 <- unique(countySA.netchange$County[countySA.netchange$Region==3])
region4 <- unique(countySA.netchange$County[countySA.netchange$Region==4])
region5 <- unique(countySA.netchange$County[countySA.netchange$Region==5])

# adding region labels
region1.label2 <- data.frame(
  label = "Region 1",
  LABEL = c(3,5,6,7),
  x = 3,
  y = c(115, 15, 45, 7))
region2.label2 <- data.frame(
  label = "2",
  LABEL = c(3,5,6,7),
  x = 7,
  y = c(115, 15, 45, 7))
region3.label2 <- data.frame(
  label = "3",
  LABEL = c(3,5,6,7),
  x = 10.5,
  y = c(115, 15, 45, 7))
region4.label2 <- data.frame(
  label = "4",
  LABEL = c(3,5,6,7),
  x = 13.5,
  y = c(115, 15, 45, 7))
region5.label2 <- data.frame(
  label = "5",
  LABEL = c(3,5,6,7),
  x = 15,
  y = c(115, 15, 45, 7))

# generate graphs
County_SA = ggplot(countySA.netchange)+
  facet_wrap(~LABEL, labeller=labeller(LABEL = LULCclasses), scales = "free")+
  geom_col(aes(x=County, y=valuekm, fill=Scenario), position="dodge", width=0.8)+
  scale_fill_manual(values = c("#36003B","#004191","#297512","#FF4B45","#FFB430"))+
  scale_y_continuous(name="Net Change, 2011 - 2061 (sq. km)", breaks=my_breaks2)+
  scale_x_discrete(limits=c(region1,region2,region3, region4, region5))+
  geom_vline(xintercept = 5.5, size=0.25)+
  geom_vline(xintercept = 8.5, size=0.25)+
  geom_vline(xintercept = 12.5, size=0.25)+
  geom_vline(xintercept = 14.5, size=0.25)+
  geom_text(data=region1.label2,
            mapping=aes(x=x, y=y, label = label),
            size=2.5)+
  geom_text(data=region2.label2,
            mapping=aes(x=x, y=y, label = label),
            size=2.5)+
  geom_text(data=region3.label2,
            mapping=aes(x=x, y=y, label = label),
            size=2.5)+
  geom_text(data=region4.label2,
            mapping=aes(x=x, y=y, label = label),
            size=2.5)+
  geom_text(data=region5.label2,
            mapping=aes(x=x, y=y, label = label),
            size=2.5)+
  geom_blank(data=blank_data_cty, aes(County, valuekm))+
  theme_bw()+
  theme(panel.border = element_rect(fill=NA, size=1.5),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(margin=margin(t=0, r=7, b=0, l=0), size=11),
        axis.title.x = element_text(margin=margin(t=0, r=0, b=0, l=0), size=11),
        axis.text.x = element_text(size=9, angle=45, hjust=1, margin=margin(t=3, r=0, b=0, l=0)),
        axis.text.y = element_text(size=9, margin=margin(t=0, r=3, b=0, l=7)),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10, margin=margin(t=0, r=0, b=0, l=5)),
        legend.key.height = unit(0.2, "in"),
        legend.spacing.x = unit(0.25, "cm"),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=12, hjust=0, vjust=1))

setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/ZonalHistogram_ggplots/NoPL/v2016_County")
png("v2016_County_SA.png", width=7.5, height=5.5, units="in", res=300)
County_SA
ggsave(file="v2016_County_SA.png", dpi=300, width=7.5, height=5.5)
dev.off()


##### BY REGION

regionSA.data = read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Zonal_Histogram/NoPL/Tables/v2016_RegionSA/All/V21016_ZonalHistogram_AllScenarios_RGN_SA.csv")
regionSA.GEOID = read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/CountyNmsGEOID_sa.csv")

# New scenario names
levels(regionSA.data$Scenario)
levels(regionSA.data$Scenario) = c("HS", "HA", "LA", "LS", "RT")

# Calculating net change
regionSA.2011 = regionSA.data[regionSA.data$TimeStep==2,]
regionSA.2061 = regionSA.data[regionSA.data$TimeStep==7,]

regionSA.netchange = data.frame(regionSA.2011$Region,
                                regionSA.2011$LABEL,
                                regionSA.2011$valuekm,
                                regionSA.2011$Scenario)
names(regionSA.netchange) = c("Region","LABEL","valuekm","Scenario")

regionSA.netchange$valuekm = regionSA.2061$valuekm - regionSA.2011$valuekm
range(regionSA.netchange$valuekm)

# Add region names
names(regionSA.GEOID)[1] = "Region"
regionSA.netchange = merge(regionSA.netchange, regionSA.GEOID, by="Region")

# region as factor (in case use numbers as axis text)
regionSA.netchange$Region = as.factor(regionSA.netchange$Region)

# set labels for facet_wrap
LULCclasses = c('3' = "Development",
                '5' = "Forest",
                '6' = "Grass",
                '7' = "Crop")

# function to set y-axis breaks
my_breaks2 = seq(-140,240,20)

# set y-axis limits via geom_blank
## NOTE: valuekm=c(DevMin,DevMax,ForestMin,ForestMax,GrassMin,GrassMax,CropMin,CropMax)
range(regionSA.netchange$valuekm[regionSA.netchange$LABEL==3])
range(regionSA.netchange$valuekm[regionSA.netchange$LABEL==5])
range(regionSA.netchange$valuekm[regionSA.netchange$LABEL==6])
range(regionSA.netchange$valuekm[regionSA.netchange$LABEL==7])

blank_data_rgn = data.frame(Region=NA, LABEL=rep(c(3,5,6,7), each=2),
                            valuekm=c(0,230,-140,10,-50,30,-70,10))

# generate graphs
Region_SA = ggplot(regionSA.netchange)+
  facet_wrap(~LABEL, labeller=labeller(LABEL = LULCclasses), scales = "free")+
  geom_col(aes(x=Region, y=valuekm, fill=Scenario), position="dodge", width=0.8)+
  scale_fill_manual(values=c("#36003B","#004191","#297512","#FF4B45","#FFB430"))+
  scale_y_continuous(name="Net Change (2011 - 2061, sq. km)", breaks=my_breaks2)+
  geom_blank(data=blank_data_rgn, aes(Region, valuekm))+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(margin=margin(t=0, r=7, b=0, l=0), size=11),
        axis.title.x = element_text(margin=margin(t=0, r=0, b=0, l=0), size=11),
        axis.text.x = element_text(size=9, hjust=1, margin=margin(t=3, r=0, b=0, l=0)),
        axis.text.y = element_text(size=9, margin=margin(t=0, r=3, b=0, l=7)),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10, margin=margin(t=0, r=0, b=0, l=5)),
        legend.key.height = unit(0.2, "in"),
        legend.spacing.x = unit(0.25, "cm"),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", size=12, hjust=0, vjust=1))

setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/ZonalHistogram_ggplots/NoPL/v2016_Region/")
png("v2016_Region_SA.png", width=7.5, height=5.5, units="in", res=300)
Region_SA
ggsave(file="v2016_Region_SA.png", dpi=300, width=7.5, height=5.5)
dev.off()

# ---------------------------------------------------------
## 2.3 INDIVIDUAL COUNTIES
# ---------------------------------------------------------

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
# SECTION 3 - Exploratory Analysis
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
