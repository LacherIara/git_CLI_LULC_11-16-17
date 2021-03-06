############################ 
#PURPOSE: Calculate the landscape configuration of forest patches across the study area for different scenarios and years
#INPUT: Land cover rasters
#OUTPUT: 
#DEVELOPED: 7-18-17
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

# # What else to know?
# - Fragmentation?
# - Forest expansion, new patches?( need 2 time frames). This is also captured in a different way already by patchstats/ classstats
# - Distance to Development?
# - What county is it in? - do majority?
# - class stats? - will need to be done by county.

#---------------------------------#
#UPDATED 4/3/2018

#Updated by Sarah Halperin to allow inputs for each scenario (RT, Q1, Q2, Q3, Q4). 
#CONTACT: halperinS@si.edu


############################

# SET WORKING DIRECTORY

# ----------------------------------------------
################################################

# PACKAGES NEEDED
library(raster) # read and edit rasters
library(SDMTools) # Species Distribution Modelling Tools
library(Hmisc)  # useful functions for data analysis
library(rgdal)  # Add this package to read .tif files
library(igraph) # Run function clump().
library(dplyr) # !! Remove dplyr in order to run clump detach(name="package:dplyr", unload=TRUE)

# SET TEMP DIRECTORY 


# ----------------------------------------------
# FILE PATHS:

# Set location for the input study area rasters
cntyRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/"

# Rasters for comparison ( now includes folder with duplicate NLCD rasters for ease in scripting)
version<-"/StudyArea_v201/SA_v2015"
version_input<-paste0("V:/IaraSpatialLayers/Dinamica_Runs",version, "/FutureLandscapes/")
Scenario<-"RT"
version_output<-"BasicDataAnalyses/Forest_Stats/"
Output_Folder<-gsub("FutureLandscapes/", version_output, version_input)


RasterLoc <- version_input


# ----------------------------------------------
# READ OUTPUT FILES:

# file name:  / R label:  

# ----------------------------------------------
# READ INPUT FILES:
# ----------------------------------------------

# ----------------------------------------------
# NLCD 2001 & 2011:
nl01 <- raster(paste0(version_input, "nlcd01_anC.img"))
nl11 <- raster(paste0(version_input, "nlcd11_anC.img"))

# ----------------------------------------------
# COUNTY RASTERS:
counties<- raster(paste(cntyRasterLoc, "cnty_an", ".img", sep="")) # this is for the raster. 

# Study Area only:
#* ('1' for in, 'NoData' for out) *had to reclassify in Rbc Arc gave a value of -128 for No Data. fml. Saved as .tif just because.
# cnty_saMASK <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/cnty_anMASK.img")
# cnty_saMASK[cnty_saMASK == 128]<-NA
# # WRITE TO FILE
# writeRaster(cnty_saMASK, filename="V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/cnty_saMASK.tif", format="GTiff", overwrite=TRUE)

# STUDY AREA MASK
cnty_saMASK<-raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/cnty_saMASK.tif")

# # Mask county_analysis raster
# regions_StudyArea <- raster::mask(regions, cnty_saMASK)
# # WRITE TO FILE
# writeRaster(regions_StudyArea, filename="V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/regions_StudyArea.tif", format="GTiff", overwrite=TRUE)

# READ FROM FILE
regions_StudyArea<-raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/regions_StudyArea.tif")
regions_vals <- getValues(regions_StudyArea) #titled regions but if looked at in GIS it is Counties 


# # Individual Counties
# County_Folder <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/IndCntys/" 
# counties <- list.files(County_Folder,pattern = ".img$")

# County Tables:
S20_GEOID<- read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv", header = T, sep=",")
colnames(S20_GEOID)<-c("VALUE", "GEOID", "Name")

################################################
# ~~~ CODE BEGINS ~~~ #
################################################
# ----------------------------------------------
# LIST FILES TO READ IN
# ----------------------------------------------
version_LS_trans<-"v2015"

LS_trans <-  list(
  #'NL01' = list(
    #'xx01' = c("NL/nlcd_nlcd/NL01_5_StudyArea.tif", "NL/nlcd_nlcd/NL01_5_StudyArea.tif")),
  #'NL11' = list(
   # '0111' = c("NL/nlcd_nlcd/NL01_5_StudyArea.tif", "NL/nlcd_nlcd/NL11_5_StudyArea.tif")), 
  'RT05'= list(
    'xxRT'= c(paste0("RT/", version_LS_trans, "_Landscape05.tif") ,paste0("RT", version_LS_trans, "_Landscape05.tif"))), 
    'Q105'= list(
      'xxQ1'= c(paste0("Q1/", version_LS_trans, "_Landscape05.tif") ,paste0("Q1", version_LS_trans, "_Landscape05.tif"))),
      'Q205'= list(
       'xxQ2'= c(paste0("Q2/", version_LS_trans, "_Landscape05.tif") ,paste0("Q2", version_LS_trans, "_Landscape05.tif"))),
  'Q305'= list(
    'xxQ3'= c(paste0("Q3/", version_LS_trans, "_Landscape05.tif") ,paste0("Q3", version_LS_trans, "_Landscape05.tif"))),
 'Q405'= list(
    'xxQ4'= c(paste0("Q4/", version_LS_trans, "_Landscape05.tif") ,paste0("Q4", version_LS_trans, "_Landscape05.tif"))))

  


# ----------------------------------------------
# ----------------------------------------------
# CREATE FOREST ONLY RASTER
# ----------------------------------------------
# ----------------------------------------------

# LS_trans <- LS_trans[2]

old <- Sys.time() # TIMING SCRIPT

for(scenario in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. 
  print(scenario)
  for(in_to_fin in names(LS_trans[[scenario]])){
    print(in_to_fin)
    print(LS_trans[[scenario]][[in_to_fin]][2])
    
    LS_raster <- raster(paste0(RasterLoc, LS_trans[[scenario]][[in_to_fin]][2]))
    LS_raster[LS_raster > 8]<-NA #remove weird noData values like -128, 255...
    
    nlcd_vals <- getValues(LS_raster) 
    LS_5_vals <- ifelse(nlcd_vals == 5| is.na(nlcd_vals), nlcd_vals, 0)
    LS_5 <- setValues(LS_raster, LS_5_vals)
    LS_5[LS_5 <= 0]<-NA
    
    # MASK TO STUDY AREA 
    LS_5_StudyArea <- raster::mask(LS_5, cnty_saMASK)
    
    
    # WRITE TO FILE
    writeRaster(LS_5_StudyArea, filename=paste0(Output_Folder, scenario,in_to_fin, "_Forest_StudyArea.tif"), format="GTiff", overwrite=TRUE)
    
    # Note: For future runs, can start here by reading the above to file
    
    # ----------------------------------------------
    # RUN PATCH STATS 
    # ----------------------------------------------
    library(igraph)
    
    LS_5_clump <- clump(LS_5_StudyArea, directions = 8, gap=FALSE) # detect clumps and gives unique ID for each of them
    LS_5_pstat <- PatchStat(LS_5_clump, cellsize = 30) # calculate patch statistics
    
    # ----------------------------------------------
    # CREATE MAJORITY REGION TABLE
    # ----------------------------------------------
    
    u_patch <- unique(LS_5_clump)
    
    regions_vals <- getValues(regions_StudyArea)
    u_vals <- sort(unique(regions_vals)[-1]) # Value for each Region
    
    
    ### Load dplyr each time bc remove it below.
    library(dplyr)
    
    ras_patch <- list()
    n_p <- 1
    for(region in 1:length(u_vals)){
      print(paste0(names(regions_StudyArea),":",u_vals[region]))
      
      categ_val <- ifelse(regions_vals == u_vals[region]|is.na(regions_vals),regions_vals,NA) 
      categ_p <- setValues(regions_StudyArea, categ_val)
      ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, LS_5_clump, fun='count', na.rm=TRUE))
      ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*900/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
      ras_patch[[n_p]]$Raster <- paste0(names(regions_StudyArea))
      ras_patch[[n_p]]$Region <- paste0(u_vals[region])
      n_p <- n_p +1
    }
    
    ras_patches<- bind_rows(ras_patch)
    
    
    
    # Create matrices to fill in.
    region_maj<- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))
    
    for(p in 1:length(u_patch)){
      patchID <- u_patch[p]
      try(temp<-filter(ras_patches,ras_patches$'zone' == u_patch[p]))
      try(area<-max(temp$area.ha))
      try(maj<-ifelse(area>0.09,filter(temp, area.ha == max(temp$area.ha))$Region,0))#Make sure resolution is right here.
      try(prop<-ifelse(area>0.09,100*(max(temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
      region_maj[p,1] <- patchID
      region_maj[p,2] <- maj
      region_maj[p,3] <- area
      region_maj[p,4] <- prop
    }
    
    colnames(region_maj) <-  c("patchID", "region_maj", "region_ha", "region_prop")
    
    
    # ----------------------------------------------
    # JOIN TO PATCH STATS TABLE
    
    iStats_majJoin <- full_join(LS_5_pstat, region_maj, by="patchID") 
    
    # WRITE TO FILE 
    write.table(iStats_majJoin, file = paste0(Output_Folder, scenario, in_to_fin, "_Forest_Pstats.txt"), row.names=FALSE, sep=",")
    
    ### !! Remove dplyr in order to run clump in next round
    detach(name="package:dplyr", unload=TRUE)
    
    
    # ----------------------------------------------
    # RECLASSIFY TO region MAJ VALUES. * Then we can run ClassStats on this.
    # ----------------------------------------------
    recl <- region_maj[,1:2]
    
    LS_5_maj_region <- reclassify(LS_5_clump, recl)		
    
    # WRITE TO FILE
    writeRaster(LS_5_maj_region, filename=paste0(Output_Folder, scenario, in_to_fin, "_Forest_majregion.tif"), format='GTiff', overwrite=TRUE)
    
    LS_5_cstat <- ClassStat(LS_5_maj_region)
    
    # WRITE TO FILE **!! CHANGE FILE NAME EACH TIME !!**
    write.table(LS_5_cstat, file = paste0(Output_Folder, scenario, in_to_fin, "_Forest_Cstats.txt"), row.names=FALSE, sep=",")
    
  }
}

new<-Sys.time()-old
print(new) # Time difference of 34.11309 mins (for one raster)



# ----------------------------------------------
# ----------------------------------------------
# READ AND FORMAT TABLES
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Class Stats
# ---------------------------------------------- 
LS_Cstat_files <- list.files(Output_Folder, pattern = "Cstats.txt$")
# > LS_Cstat_files
# [1] "Q1_Forest_Cstats.txt"
# [2] "Q2_Forest_Cstats.txt"
# [3] "Q3_Forest_Cstats.txt"
# [4] "Q4_Forest_Cstats.txt"
# [5] "RT_Forest_Cstats.txt"

#### *** !!! # Select out the nlcd ones:

library(stringr)

CStat_table <- list()
f_h <- 1
for(scenario in 1:length(LS_Cstat_files)){ # Makes code flexible for use with more than 2 landscapes. 
  print(LS_Cstat_files[scenario])
  
  CStat_table[[f_h]] <- read.table(paste0(Output_Folder, LS_Cstat_files[scenario]), header=TRUE, sep=",")
  names(CStat_table[[f_h]])[1]<-"VALUE"
  # Join with county names table
  CStat_table[[f_h]] <- merge(S20_GEOID, CStat_table[[f_h]], by="VALUE")
  
  
  # Add a prefix to the values in the VALUE column.
  CStat_table[[f_h]]$VALUE <- as.character(paste0(str_sub(LS_Cstat_files[scenario],start=1,end=3),CStat_table[[f_h]]$VALUE)) 
  CStat_table[[f_h]] <- t(CStat_table[[f_h]])
  colnames(CStat_table[[f_h]] ) <- CStat_table[[f_h]][1,]
  CStat_table[[f_h]] <- as.data.frame(CStat_table[[f_h]][-1,])
  
  f_h <- f_h+1
  
}

CStat_table<-as.data.frame(CStat_table)


# ----------------------------------------------
# MORE FINAGLING - JOIN TABLES AND sort
# ----------------------------------------------

# Sort by order of land use 
CStat_table<-CStat_table[ , order(str_sub(colnames(CStat_table), -2, -1))]
# Remove class='0' (had no core cells)
# CStat_table<-CStat_table[ ,-c(1:5)]

# WRITE TO FILE #
write.csv(CStat_table, file=paste0(Output_Folder, "AllScens_Forest_CStat.csv"),row.names=TRUE, quote=FALSE)

# READ FROM FILE #
CStat_table <- read.csv(paste0(Output_Folder, "AllScens_Forest_CStat.csv"))

#-------------------------------------------#
#Exploratory Graphs 

Input_Folder<-list.files(Output_Folder, pattern="_Forest_Cstats.txt", full.names=TRUE) 
C_stats<-lapply(Input_Folder,function(i){
  read.csv(i)
})

#ADD TIME STEP 
NL01<-C_stats[[1]]
NL11<-C_stats[[2]]
Q1<-C_stats[[3]]
Q2<-C_stats[[4]]
Q3<-C_stats[[5]]
Q4<-C_stats[[6]]
RT<-C_stats[[7]]


#SELECT DESIRED VARIABLES 
NL01<-NL01[,c(1,2,3,4,10,27,28,29,34,38)]
NL11<-NL11[,c(1,2,3,4,10,27,28,29,34,38)]
Q1<-Q1[,c(1,2,3,4,10,27,28,29,34,38)]
Q2<-Q2[,c(1,2,3,4,10,27,28,29,34,38)]
Q3<-Q3[,c(1,2,3,4,10,27,28,29,34,38)]
Q4<-Q4[,c(1,2,3,4,10,27,28,29,34,38)]
RT<-RT[,c(1,2,3,4,10,27,28,29,34,38)]


write.csv(NL01, file=paste0(Output_Folder, "NL01xx01_Forest_Cstats_thin.csv"))
write.csv(NL11, file=paste0(Output_Folder, "NL110111_Forest_Cstats_thin.csv"))
write.csv(Q3, file=paste0(Output_Folder, "Q305xxQ3_Forest_Cstats_thin.csv"))
write.csv(RT, file=paste0(Output_Folder, "RT05xxRT_Forest_Cstats_thin.csv"))
write.csv(Q1, file=paste0(Output_Folder, "Q105xxRT_Forest_Cstats_thin.csv"))
write.csv(Q2, file=paste0(Output_Folder, "Q205xxRT_Forest_Cstats_thin.csv"))
write.csv(Q3, file=paste0(Output_Folder, "Q305xxRT_Forest_Cstats_thin.csv"))
write.csv(Q4, file=paste0(Output_Folder, "Q405xxRT_Forest_Cstats_thin.csv"))



#--------------------------------------------------------------------------------------------------------------------------------------------------------#
#ALSO EASILY MADE IN EXCEL. Grab excel sheet from here: file:///V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2015/BasicDataAnalyses/Forest_Stats/Forest_Cstats_graphs.xlsx and copy graphs to new spreadsheets 


#number of patches 
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$n.patches))+
         geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$n.patches))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$n.patches))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$n.patches))+
  geom_bar(stat="identity", width=0.5)

#Total Area
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$total.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$total.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$total.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$total.area))+
  geom_bar(stat="identity", width=0.5)

#proportion of landscape
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$prop.landscape))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$prop.landscape))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$prop.landscape))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$prop.landscape))+
  geom_bar(stat="identity", width=0.5)

#total core area
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$total.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$total.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$total.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$total.core.area))+
  geom_bar(stat="identity", width=0.5)

#proprotion landscape core
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$prop.landscape.core))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$prop.landscape))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$prop.landscape.core))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$prop.landscape.core))+
  geom_bar(stat="identity", width=0.5)


#mean patch core area
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$mean.patch.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$mean.patch.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$mean.patch.core.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$mean.patch.core.area))+
  geom_bar(stat="identity", width=0.5)

#mean patch area
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$mean.patch.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$mean.patch.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$mean.patch.area))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$mean.patch.area))+
  geom_bar(stat="identity", width=0.5)

#aggregation index
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$aggregation.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$aggregation.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$aggregation.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$aggregation.index))+
  geom_bar(stat="identity", width=0.5)

#patch.cohesion 
ggplot(NL01Cstats, aes(x=NL01Cstats$class, y=NL01Cstats$patch.cohesion.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(NL11Cstats, aes(x=NL11Cstats$class, y=NL11Cstats$patch.cohesion.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(Q305Cstats, aes(x=Q305Cstats$class, y=Q305Cstats$patch.cohesion.index))+
  geom_bar(stat="identity", width=0.5)
ggplot(RT05Cstats, aes(x=RT05Cstats$class, y=RT05Cstats$patch.cohesion.index))+
  geom_bar(stat="identity", width=0.5)
#------------------------------------------------------------------------------------------------------------#
#Cstats graphs 
#NOTE MAY HAVE TO CHANGE "CLASS" TO FACTOR 

#Calculate Area of SA and individual counties to add an additional column 



Folder<-list.files(Output_Folder, pattern="Cstats_thin.csv", full.names = TRUE) #Read in NCLD files 
C_stats<-lapply(Folder,function(i){
  read.csv(i)
})

C_statsQ1<-C_stats[[3]]
C_statsQ2<-C_stats[[4]]
C_statsQ3<-C_stats[[5]]
C_statsQ4<-C_stats[[6]]
C_statsRT<-C_stats[[7]]

C_statsQ1$Scenario<-"Q1"
C_statsQ2$Scenario<-"Q2"
C_statsQ3$Scenario<-"Q3"
C_statsQ4$Scenario<-"Q4"
C_statsRT$Scenario<-"RT"

C_statsCombined<-rbind(C_statsQ1, C_statsQ2, C_statsQ3, C_statsQ4, C_statsRT)

C_statsCombined$X<-NULL
C_statsCombined<-C_statsCombined[-1,]


#Subet desired county
C_statsCombinedF3<-subset(C_statsCombined, C_statsCombined$class == 3)
C_statsCombinedF10<-subset(C_statsCombined, C_statsCombined$class == 10)
C_statsCombinedFF<-rbind(C_statsCombinedF3, C_statsCombinedF10)

#convert to km2 from hectares
#MAY HAVE TO DO FOR OTHER VARIABLES TO 
C_statsCombinedFF$mean.patch.areakm<-C_statsCombinedFF$mean.patch.area*(1/100)
C_statsCombinedFF$class<-as.factor(C_statsCombinedFF$class)



#Fragstats by county 

#mean_patch 

  IALE_v2015_FF_mean_patch<-ggplot(C_statsCombinedFF, aes(x=class, y=mean.patch.areakm, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("3", "10"), labels=c("Frederick", "Fauquier"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    xlab("County")+
  scale_y_continuous(name =expression('Mean Patch Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
    theme(plot.margin=unit(c(1,1,1,1), "in"))

#number of patches 
  IALE_v2015_FF_n_patches<-ggplot(C_statsCombinedFF, aes(x=class, y=n.patches, fill=Scenario))+
    geom_bar(stat="identity", position="dodge")+
      scale_x_discrete(breaks= c("3", "10"), labels=c("Frederick", "Fauquier"))+
    scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    xlab("County")+
    scale_y_continuous(name = "Number of Patches")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.text=element_text(size=40),
          axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
    theme(plot.margin=unit(c(1,1,1,1), "in"))

#Proportion of landscape 
Raster_SA<-raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/regions_StudyArea.tif")  

sa_ctyGEOID<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")#SCBI V: #Geological ID for the county. 

colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID","NAME")
sa_ctyGEOID$NAME<-NULL


Raster_values<-getValues(Raster_SA)
Raster_final<- summary(factor(Raster_values), maxsum = length(unique(Raster_values)))

sa_ctyGEOID$Areapx<-Raster_final[1:20]
sa_ctyGEOID$Areakm<-sa_ctyGEOID$Areapx*(900/1000000)
colnames(sa_ctyGEOID)<-c("class","GEOID", "Areapx", "Areakm")
TotalArea<-sum(sa_ctyGEOID$Areakm) #17888.25

C_statsCombined<-merge(C_statsCombined,sa_ctyGEOID, by ="class")


#Alternative Method 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Raster_freq<-freq(Raster_SA)
#Raster_freq<-as.table(Raster_freq)
#Raster_freq<-as.data.frame(Raster_freq)
#County<-subset(Raster_freq, Raster_freq$Var2 == "value")
#Freq<-subset(Raster_freq, Raster_freq$Var2 == "count")
#Raster_freq<-cbind(County,Freq)
#Raster_freq<-Raster_freq[1:20,c(3,6)]
#colnames(Raster_freq)<-c("County", "Freq")
#Raster_freq$area<-Raster_freq$Freq*(900/1000000)
#Total_Area<-sum(Raster_freq$area) #1788.25
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#resubset data to add area 
C_statsCombinedF3<-subset(C_statsCombined, C_statsCombined$class == 3)


C_statsCombinedF3$prop.landscape<-C_statsCombinedF3$prop.landscape*(TotalArea/C_statsCombinedF3$Areakm)

C_statsCombinedF10<-subset(C_statsCombined, C_statsCombined$class == 10)
C_statsCombinedF10$prop.landscape<-C_statsCombinedF10$prop.landscape*(TotalArea/C_statsCombinedF10$Areakm)

C_statsCombinedFF<-rbind(C_statsCombinedF3, C_statsCombinedF10)

C_statsCombinedFF$class<-as.character(C_statsCombinedFF$class)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  
  IALE_v2015_FF_prop_landscape<-ggplot(C_statsCombinedFF, aes(x=class, y=prop.landscape, fill=Scenario))+
    geom_bar(stat="identity", position="dodge")+
    scale_x_discrete(name="County",breaks= c("3", "10"), labels=c("Frederick", "Fauquier"))+
    scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    scale_y_continuous(name = "Proportion of the Landscape %")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.text=element_text(size=40),
          axis.title.x=element_blank(), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
    theme(plot.margin=unit(c(1,1,1,1), "in"))
  
#-------------------------------------------------------------------------#
#EXPORT 
  
  #export graph  mean patch 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("IALE_v2015_FF_mean_patch.png", width=480, height=480, units="px", res=300) #can't put units and resolution
IALE_v2015_FF_mean_patch
dev.off()


ggsave(file="IALE_v2015_FF_mean_patch.png", dpi=300, width=15, height=15)

#export graph number of patches 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("IALE_v2015_FF_n_patches.png", width=480, height=480, units="px", res=300) #can't put units and resolution
IALE_v2015_FF_n_patches
dev.off()


ggsave(file="IALE_v2015_FF_n_patches.png", dpi=300, width=15, height=15)


#export graph proportion of landscape 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("IALE_v2015_FF_prop_landscape.png", width=480, height=480, units="px", res=300)
IALE_v2015_FF_prop_landscape


dev.off()


ggsave(file="IALE_v2015_FF_prop_landscape.png", dpi=300, width=15, height=15)

#Total Area and TOtal Core AREA by County graphs 

#1st county 
F3TATC<-C_statsCombinedF3[,c(3,6,11)]
F3TATC$total.areakm<-F3TATC$total.area*(1/100)
F3TATC$total.core.areakm<-F3TATC$total.core.area*(1/100)
F3TATC$total.area<-NULL
F3TATC$total.core.area<-NULL

F3TATCmelt<-melt(F3TATC)

#2nd county 
F10TATC<-C_statsCombinedF10[,c(3,6,11)]
F10TATC$total.areakm<-F10TATC$total.area*(1/100)
F10TATC$total.core.areakm<-F10TATC$total.core.area*(1/100)
F10TATC$total.area<-NULL
F10TATC$total.core.area<-NULL

F10TATCmelt<-melt(F10TATC)

#Total Area and Total Core Area Graph for individual county 
IALE_Area_Fred<-ggplot(F3TATCmelt, aes(x=Scenario, y=value, fill=variable))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("Q1", "Q2", "Q3", "Q4", "RT"))+
  scale_fill_manual(labels=c("Total Area", "Total Core Area"),values=c( "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Area km'^2), limit=c(0,8000))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=30), axis.title=element_text(size=30,face="bold"), legend.text=element_text(size=30), legend.title=element_blank(), legend.key.height= unit(1,"in"))


#export graph 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("IALE_Area_Fred.png", width=480, height=480, units="px", res=300) #can't put units and resolution
IALE_Area_Fred
dev.off()


ggsave(file="IALE_Area_Fred.png", dpi=300, width=15, height=15)





# ----------------------------------------------
# Patch Stats - come back to this.
# ---------------------------------------------- 

# test_Pstat<-read.table(paste0(Output_Folder,LS_Pstat_files[2]), sep=",", header=TRUE)


# # For Patch Stats, filter out rows with '0' in the column "region_maj"


# > str(test_Pstat)
# 'data.frame':	43570 obs. of  15 variables:
# $ patchID          : int  1 2 3 4 5 6 7 8 9 10 ...
# $ n.cell           : int  38263 139490 12 25 1 2 30 6 6 1 ...
# $ n.core.cell      : int  27986 106547 0 2 0 0 0 0 0 0 ...
# $ n.edges.perimeter: int  13322 36688 22 36 4 6 64 18 16 4 ...
# $ n.edges.internal : int  139730 521272 26 64 0 2 56 6 8 0 ...
# $ area             : int  34436700 125541000 10800 22500 900 1800 27000 5400 5400 900 ...
# $ core.area        : int  25187400 95892300 0 1800 0 0 0 0 0 0 ...
# $ perimeter        : int  399660 1100640 660 1080 120 180 1920 540 480 120 ...
# $ perim.area.ratio : num  0.01161 0.00877 0.06111 0.048 0.13333 ...
# $ shape.index      : num  16.99 24.56 1.57 1.8 1 ...
# $ frac.dim.index   : num  1.33 1.34 1.1 1.12 1 ...
# $ core.area.index  : num  0.731 0.764 0 0.08 0 ...
# $ region_maj       : int  3 3 3 3 0 3 3 3 3 0 ...
# $ region_ha        : num  3443.67 12554.1 1.08 2.25 0.09 ...
# $ region_prop      : num  100 100 100 100 0 100 100 100 100 0 ...

# str(test_Cstat) # '0' are patches that were only comprised of 1 cell.
# 'data.frame':	20 obs. of  38 variables:
# $ class                  : int  0 3 4 6 10 12 15 21 23 29 ...
# $ n.patches              : int  13093 1978 3914 1771 3366 704 32 1337 1791 844 ...
# $ total.area             : int  13094 536084 20171 643624 841708 796327 1857 1068407 446067 68143 ...
# $ prop.landscape         : num  0.00126 0.05146 0.00194 0.06179 0.0808 ...
# $ patch.density          : num  0.001257 0.00019 0.000376 0.00017 0.000323 ...
# $ total.edge             : int  52376 193496 50454 178186 332094 187480 1588 221332 193734 52198 ...
# $ edge.density           : num  0.00503 0.01857 0.00484 0.01711 0.03188 ...
# $ landscape.shape.index  : num  114.4 66 88.5 55.5 90.5 ...
# $ largest.patch.index    : num  1.92e-07 1.89e-02 7.68e-06 4.93e-02 1.22e-02 ...
# $ mean.patch.area        : num  1 271.02 5.15 363.42 250.06 ...
# $ sd.patch.area          : num  8.74e-03 5.61e+03 5.67 1.22e+04 3.45e+03 ...
# $ min.patch.area         : int  1 2 2 2 2 2 2 2 2 2 ...
# $ max.patch.area         : int  2 196487 80 513223 126984 433210 464 767953 120469 11208 ...
# $ perimeter.area.frac.dim: num  10.121 0.722 5.148 0.553 0.789 ...
# $ mean.perim.area.ratio  : num  4 2.33 2.92 2.06 1.93 ...
# $ sd.perim.area.ratio    : num  0 0.807 0.682 0.849 0.786 ...
# $ min.perim.area.ratio   : num  4 0.192 0.971 0.153 0.16 ...
# $ max.perim.area.ratio   : int  4 4 4 4 4 4 4 4 4 4 ...
# $ mean.shape.index       : num  1 1.62 1.34 1.74 1.7 ...
# $ sd.shape.index         : num  0.00291 1.25166 0.37022 1.06505 1.09939 ...
# $ min.shape.index        : int  1 1 1 1 1 1 1 1 1 1 ...
# $ max.shape.index        : num  1.33 24.56 4.13 27.37 20.77 ...
# $ mean.frac.dim.index    : num  2 1.41 1.49 1.41 1.37 ...
# $ sd.frac.dim.index      : num  NA 0.228 0.31 0.207 0.187 ...
# $ min.frac.dim.index     : num  2 1 1 1 1 ...
# $ max.frac.dim.index     : int  2 2 2 2 2 2 2 2 2 2 ...
# $ total.core.area        : int  0 383971 188 502681 572470 635600 714 879806 288903 30296 ...
# $ prop.landscape.core    : num  0 0.03686 0.000018 0.048256 0.054955 ...
# $ mean.patch.core.area   : num  0 194.121 0.048 283.84 170.074 ...
# $ sd.patch.core.area     : num  0.00 4.49e+03 4.83e-01 1.05e+04 2.76e+03 ...
# $ min.patch.core.area    : int  0 0 0 0 0 0 0 0 0 0 ...
# $ max.patch.core.area    : int  0 162603 14 443819 103707 385077 256 687482 88380 8241 ...
# $ prop.like.adjacencies  : num  0 0.834 0.231 0.871 0.82 ...
# $ aggregation.index      : num  0 91.1 37.7 93.2 90.2 ...
# $ lanscape.division.index: num  1 0.999 1 0.998 1 ...
# $ splitting.index        : num  8.29e+09 1.74e+03 4.73e+08 4.12e+02 2.69e+03 ...
# $ effective.mesh.size    : num  1.26e-03 5.99e+03 2.20e-02 2.53e+04 3.87e+03 ...
# $ patch.cohesion.index   : num  NA 9.97 7.16 9.98 9.94 ...












