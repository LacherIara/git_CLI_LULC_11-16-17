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
cntyRasterLoc <- "U:/CLI/PreparedRasters/StudyAreaBndy/"

# Rasters for comparison ( now includes folder with duplicate NLCD rasters for ease in scripting)
version<-"/StudyArea_V201/SA_V2016"
version_input<-paste0("U:/CLI/Dinamica_Runs",version, "/FutureLandscapes/")
version_output<-"BasicDataAnalyses/Forest_Stats/Region/"
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
#NL<-"NL/nlcd_nlcd/"
#nl01 <- raster(paste0(version_input,NL, "nlcd01_anC.img"))
#nl11 <- raster(paste0(version_input,NL, "nlcd11_anC.img"))

# ----------------------------------------------
# COUNTY RASTERS:
counties<- raster(paste(cntyRasterLoc, "cnty_an", ".img", sep="")) #pretty sure this isn't used but kept just in case


# STUDY AREA MASK 
cnty_saMASK<-raster("U:/CLI/PreparedRasters/StudyAreaBndy/cnty_saMASK.tif") #use for analysis performed on study area

#COUNTY AREA MASKS
county_trans<-"U:/CLI/SpatialData/VAClipRaw/VA_NRCS/CountiesEd/Indv/TIF_SA/"
county_saMaskList <-  list('Augusta' = c(paste0(county_trans,"aug_stau_wayn.img")),'Rockingham' = c(paste0(county_trans, "rock_harr.img")),'Albemarle' = c(paste0(county_trans,"albe_charl.img")),'Frederick' = c(paste0(county_trans,"fred_win.img")),'Clarke' = c(paste0(county_trans,"clarke.img")),'Fauquier' = c(paste0(county_trans,"fauquier.img")),'Page' = c(paste0(county_trans,"page.img")),'Culpeper' = c(paste0(county_trans,"culpeper.img")),'greene' = c(paste0(county_trans,"greene.img")),'loudon' = c(paste0(county_trans,"loudon.img")),'madison' = c(paste0(county_trans,"madison.img")),'orange' = c(paste0(county_trans,"orange.img")),'rappahannock' = c(paste0(county_trans,"raphanock.img")),'shenandoah' = c(paste0(county_trans,"shenandoah.img")),'warren' = c(paste0(county_trans,"warren.img"))) #use for analysis performed per county 

#REGION AREA MASKS
region_trans<-"U:/CLI/SpatialData/VAClipRaw/VA_NRCS/Regions/"
region_sa_MaskList<-list('1'=c(paste0(region_trans, "region_1.img")), '2'=c(paste0(region_trans, "region_2.img")),'3'=c(paste0(region_trans, "region_3.img")), '4'=c(paste0(region_trans, "region_4.img")), '5'=c(paste0(region_trans, "region_5.img")),'6'=c(paste0(region_trans, "region_6.img")),'7'=c(paste0(region_trans, "region_7.img")),'8'=c(paste0(region_trans, "region_8.img")))

# READ FROM FILE
regions_StudyArea<-raster("U:/CLI/PreparedRasters/StudyAreaBndy/ctny_StudyArea.tif")
regions_vals <- getValues(regions_StudyArea) #renamed in ArcGIS but haven't renamed in Rscript. the file represents counties (3-56)


# County Tables:
S20_GEOID<- read.table("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv", header = T, sep=",") #not sure if these have a purpose
colnames(S20_GEOID)<-c("VALUE", "GEOID", "Name")

################################################
# ~~~ CODE BEGINS ~~~ #
################################################
# ----------------------------------------------
# LIST FILES TO READ IN
# ----------------------------------------------
version_LS_trans<-"v2016"

# ----------------------------------------------
#RASTER FILES 
LS_trans <-  list(
 'NL01' = list(
   'xx01' = c("NL/nlcd_nlcd/nlcd01_anC.img", "NL/nlcd_nlcd/nlcd01_anC.img")), #should it be using this file? 
 'NL11' = list(
  '0111' = c("NL/nlcd_nlcd/nlcd11_anC.img", "NL/nlcd_nlcd/nlcd11_anC.img")), 
 'RT05'= list(
   'xxRT'= c(paste0("RT/", version_LS_trans,"_RT", "_Landscape05.tif") ,paste0("RT/", version_LS_trans, "_RT", "_Landscape05.tif"))), 
    'Q105'= list(
     'xxQ1'= c(paste0("Q1/", version_LS_trans, "_Q1", "_Landscape05.tif") ,paste0("Q1/", version_LS_trans, "_Q1", "_Landscape05.tif"))),
     'Q205'= list(
      'xxQ2'= c(paste0("Q2/", version_LS_trans, "_Q2", "_Landscape05.tif") ,paste0("Q2/", version_LS_trans, "_Q2", "_Landscape05.tif"))),
  'Q305'= list(
    'xxQ3'= c(paste0("Q3/", version_LS_trans, "_Q3", "_Landscape05.tif") ,paste0("Q3/", version_LS_trans, "_Q3", "_Landscape05.tif"))),
 'Q405'= list(
    'xxQ4'= c(paste0("Q4/", version_LS_trans, "_Q4", "_Landscape05.tif") ,paste0("Q4/", version_LS_trans, "_Q4", "_Landscape05.tif"))))
#test
LS_trans <- list('RT05'= list(
  'xxRT'= c(paste0("RT/", version_LS_trans,"_RT", "_Landscape05.tif") ,paste0("RT/", version_LS_trans, "_RT", "_Landscape05.tif"))))

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
    
  # ---------------------------------------------- 
    #DEPENDING ON WHETHER ANALYSIS IS PERFORMED ON STUDY AREA OR INDIVIDUAL COUNTIES 
    #CHOOSE WHAT TO MASK
    
    # MASK TO STUDY AREA 
    #LS_5_StudyArea <- raster::mask(LS_5, cnty_saMASK)
    
    #MASK TO COUNTIES 
    #for(county in 1:length(county_saMaskList)){
      #county_saMask<-raster(county_saMaskList[[county]][1])
     #LS_5_StudyArea<-raster::mask(LS_5,county_saMask)
    
    #MASK TO REGION
    for(regions in 1:length(region_sa_MaskList)){
    region_saMask<-raster(region_sa_MaskList[[regions]][1])
    LS_5_StudyArea<-raster::mask(LS_5,region_saMask)
    
     # WRITE TO FILE
    writeRaster(LS_5_StudyArea, filename=paste0(Output_Folder,names(region_sa_MaskList[regions]), scenario,in_to_fin, "_Forest_StudyArea.tif"), format="GTiff", overwrite=TRUE)
    #writeRaster(LS_5, filename=paste0(Output_Folder, scenario,in_to_fin, "_Forest_FullArea.tif"), format="GTiff", overwrite=TRUE)
     

    # Note: For future runs, can start here by reading the above to file
    
    # ----------------------------------------------
    # RUN PATCH STATS 
    # ----------------------------------------------
    library(igraph)
    
    LS_5_clump <- clump(LS_5_StudyArea, directions = 8, gap=FALSE) # detect clumps (patches) and gives unique ID for each of them. Uses the 8-neighbor rule. 
    LS_5_pstat <- PatchStat(LS_5_clump, cellsize = 30) # calculate patch statistics. Statistics based on each patch. 
    
    # ----------------------------------------------
    # CREATE MAJORITY REGION TABLE -We dont use this part anymore
    # ----------------------------------------------
    #Creates a table that assigns patches to either a region or county (dependent on what is set as regions_StudyArea). Each individual patch is assigned an ID that ID is associated with 
    #multiple regions or counties if the patch lies across region or county lines. If so, the patch is then assigned to the region or county that has the majority. 
    
    # ----------------------------------------------
    # WITHOUT MAJORITY -We dont use this part anymore
    # ----------------------------------------------
    u_patch <- unique(LS_5_clump) #turns patchIDs into numeric 
    
    #USE FOR ALL COUNTIES 
    #regions_vals <- getValues(regions_StudyArea) 
    #u_vals <- sort(unique(regions_vals)[-1]) # Value for each county
    
    #USE FOR INDIVIDUAL COUNTIES 
    #county_vals<-getValues(county_saMask)
   #u_vals<-sort(unique(county_vals)[-1])
    
   #USE FOR REGIONS
   region_vals<-getValues(region_saMask)
   u_vals<-sort(unique(region_vals)[-1])
    
    ### Load dplyr each time bc remove it below. #THIS MAy NEED TO BE CHANGED to COUNTY MASK 
    library(dplyr)
    
#NEED TO ADJUST BASED ON REGION, COUNTY,SA ---> adjust names 
       ras_patch <- list()
   n_p <- 1
   for(region in 1:length(u_vals)){
     print(paste0(names(region_saMask),":",u_vals[region])) #adjust based on whether county or region (region_vals vs. county_vals)
       categ_val <- ifelse(region_vals== u_vals[region]|is.na(region_vals),region_vals,NA) 
     categ_p <- setValues(region_saMask, categ_val)
     ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, LS_5_clump, fun='count', na.rm=TRUE))
     ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*900/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
     ras_patch[[n_p]]$Raster <- paste0(names(region_saMask))
     ras_patch[[n_p]]$Region <- paste0(u_vals[region])
     n_p <- n_p +1
   }
   
   ras_patches<- bind_rows(ras_patch)
   
   
   # ----------------------------------------------
   #OLD Version created a majority and ran statistics that way. 
   #THIS IS THE OLD CODE 
   
    #ras_patch <- list()
    #n_p <- 1
    #for(region in 1:length(u_vals)){
    # print(paste0(names(regions_StudyArea),":",u_vals[region]))
      
      #categ_val <- ifelse(regions_vals == u_vals[region]|is.na(regions_vals),regions_vals,NA) 
     #categ_p <- setValues(regions_StudyArea, categ_val)
     #ras_patch[[n_p]] <- as.data.frame(zonal(categ_p, LS_5_clump, fun='count', na.rm=TRUE))
     #ras_patch[[n_p]]$area.ha<-ras_patch[[n_p]]$count*900/10000#Makes sure resolution is right here. 30*30=900, 360*360=129600
     #ras_patch[[n_p]]$Raster <- paste0(names(regions_StudyArea))
     # ras_patch[[n_p]]$Region <- paste0(u_vals[region])
    # n_p <- n_p +1
   # }
    
   #ras_patches<- bind_rows(ras_patch) #ras_patch (for counties) is a set of 20 tables one for each county that each have 23979 different patches. For each patch there is an associated count (# of pixels within that patch). If a patch is in two counties, the count is just split and the patch ID is repeated. 
    
    
    #Create matrices to fill in.
  #  region_maj<- as.data.frame(matrix(nrow = length(u_patch), ncol = 4))
    
   # for(p in 1:length(u_patch)){
    #  patchID <- u_patch[p]
    #  try(temp<-filter(ras_patches,ras_patches$'zone' == u_patch[p]))
    #  try(area<-(temp["area.ha"]))
     # try(maj<-ifelse(area>0.09,area,0))#Make sure resolution is right here. #exclude area <0.09 (1 pixel)ne' == u_patch[p]))
     # try(prop<-ifelse(area>0.09,100*((temp$area.ha)/sum(temp$area.ha)),0))#Make sure resolution is right here.
     # region_maj[p,1] <- patchID
     # region_maj[p,2] <- maj
     # region_maj[p,3] <- area
     # region_maj[p,4] <- prop
   # }
    
    #colnames(region_maj) <-  c("patchID", "region_maj", "region_ha", "region_prop") #region_maj takes ras_patches (combined of the 20 tables) and determines which county has the majority of the patch and then assigns the whole patch to that county. 
    
  
    # ----------------------------------------------
    # JOIN TO PATCH STATS TABLE
    
  
    #iStats_majJoin <- full_join(LS_5_pstat, region_maj, by="patchID") 
    
    # WRITE TO FILE 
    write.table(LS_5_pstat, file = paste0(Output_Folder, names(region_saMaskList[regions]), scenario, in_to_fin, "_Forest_Pstats.txt"), row.names=FALSE, sep=",")
    
    ### !! Remove dplyr in order to run clump in next round
    detach(name="package:dplyr", unload=TRUE)
    
    
    # ----------------------------------------------
    # RECLASSIFY TO region MAJ VALUES. * Then we can run ClassStats on this.
    # ----------------------------------------------
   recl <- ras_patches[,c(1,5)]
    
    LS_5_maj_region <- reclassify(LS_5_clump, recl)		#using the patchID and associated counties from region_maj, it assigns those IDs to the original clump (patches) raster. This is then used to run classstats  
    
    # WRITE TO FILE
    writeRaster(LS_5_maj_region, filename=paste0(Output_Folder, names(region_saMaskList[regions]), scenario, in_to_fin, "_Forest_majregion.tif"), format='GTiff', overwrite=TRUE) #change names to either the region list or the county list 
    
    LS_5_cstat <- ClassStat(LS_5_maj_region)
    
    # WRITE TO FILE **!! CHANGE FILE NAME for REGION VS. County !!**
    write.table(LS_5_cstat, file = paste0(Output_Folder,names(region_saMaskList[regions]), scenario, in_to_fin, "_Forest_Cstats.txt"), row.names=FALSE, sep=",") #chagne names to either region list or the county list
    
    }
  }
}

new<-Sys.time()-old
print(new)
###########################################
# ~~~ MERGE Region CSTAT TABLES ~~~ #
###########################################   

version_table<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Region/"
    


Folder<-list.files(paste0(version_table, "NL01"), pattern="_Cstats.txt", full.names = TRUE) #Read in NCLD files 
NL11<-lapply(Folder,function(i){
  read.csv(i)
})

NL11[[1]]$region<-"1"
NL11[[2]]$region<-"2"
NL11[[3]]$region<-"3"
NL11[[4]]$region<-"4"
NL11[[5]]$region<-"5"
NL11[[6]]$region<-"6"
NL11[[7]]$region<-"7"
NL11[[8]]$region<-"8"




CombinedNL11<-do.call(rbind.data.frame,NL11)
CombinedNL11$Scenario<-"NL11"


Folder<-list.files(paste0(version_table, "NL11"), pattern="_Cstats.txt", full.names = TRUE) #Read in NCLD files 
NL01<-lapply(Folder,function(i){
  read.csv(i)
})

NL01[[1]]$region<-"1"
NL01[[2]]$region<-"2"
NL01[[3]]$region<-"3"
NL01[[4]]$region<-"4"
NL01[[5]]$region<-"5"
NL01[[6]]$region<-"6"
NL01[[7]]$region<-"7"
NL01[[8]]$region<-"8"




CombinedNL01<-do.call(rbind.data.frame,NL01)
CombinedNL01$Scenario<-"NL01"


Folder<-list.files(paste0(version_table, "Q1"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q1<-lapply(Folder,function(i){
  read.csv(i)
})

Q1[[1]]$region<-"1"
Q1[[2]]$region<-"2"
Q1[[3]]$region<-"3"
Q1[[4]]$region<-"4"
Q1[[5]]$region<-"5"
Q1[[6]]$region<-"6"
Q1[[7]]$region<-"7"
Q1[[8]]$region<-"8"




CombinedQ1<-do.call(rbind.data.frame,Q1)
CombinedQ1$Scenario<-"Q1"

Folder<-list.files(paste0(version_table, "Q2"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q2<-lapply(Folder,function(i){
  read.csv(i)
})

Q2[[1]]$region<-"1"
Q2[[2]]$region<-"2"
Q2[[3]]$region<-"3"
Q2[[4]]$region<-"4"
Q2[[5]]$region<-"5"
Q2[[6]]$region<-"6"
Q2[[7]]$region<-"7"
Q2[[8]]$region<-"8"



CombinedQ2<-do.call(rbind.data.frame,Q2)
CombinedQ2$Scenario<-"Q2"

Folder<-list.files(paste0(version_table, "Q3"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q3<-lapply(Folder,function(i){
  read.csv(i)
})

Q3[[1]]$region<-"1"
Q3[[2]]$region<-"2"
Q3[[3]]$region<-"3"
Q3[[4]]$region<-"4"
Q3[[5]]$region<-"5"
Q3[[6]]$region<-"6"
Q3[[7]]$region<-"7"
Q3[[8]]$region<-"8"



CombinedQ3<-do.call(rbind.data.frame,Q3)
CombinedQ3$Scenario<-"Q3"

Folder<-list.files(paste0(version_table, "Q4"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q4<-lapply(Folder,function(i){
  read.csv(i)
})

Q4[[1]]$region<-"1"
Q4[[2]]$region<-"2"
Q4[[3]]$region<-"3"
Q4[[4]]$region<-"4"
Q4[[5]]$region<-"5"
Q4[[6]]$region<-"6"
Q4[[7]]$region<-"7"
Q4[[8]]$region<-"8"



CombinedQ4<-do.call(rbind.data.frame,Q4)
CombinedQ4$Scenario<-"Q4"

Folder<-list.files(paste0(version_table, "RT"), pattern="RT05xxRT_Forest_Cstats.txt", full.names = TRUE) #Read in NCLD files 
RT<-lapply(Folder,function(i){
  read.csv(i)
})

RT[[1]]$region<-"1"
RT[[2]]$region<-"2"
RT[[3]]$region<-"3"
RT[[4]]$region<-"4"
RT[[5]]$region<-"5"
RT[[6]]$region<-"6"
RT[[7]]$region<-"7"
RT[[8]]$region<-"8"



CombinedRT<-do.call(rbind.data.frame,RT)
CombinedRT$Scenario<-"RT"


Allscenarios<-rbind(CombinedNL01, CombinedNL11, CombinedRT, CombinedQ1, CombinedQ2, CombinedQ3, CombinedQ4)

write.csv(Allscenarios, "U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Region/V2016_Fragstats_region.csv", row.names = FALSE)
###########################################
# ~~~ MERGE COUNTY CSTAT TABLES ~~~ #
###########################################  

version_table<-"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/County/"

Folder<-list.files(paste0(version_table, "NL"), pattern="110111_Forest_Cstats.txt", full.names = TRUE) #Read in NCLD files 
NL<-lapply(Folder,function(i){
  read.csv(i)
})

NL[[1]]$county<-"Albemarle"
NL[[2]]$county<-"Augusta"
NL[[3]]$county<-"Clarke"
NL[[4]]$county<-"Culpeper"
NL[[5]]$county<-"Fauquier"
NL[[6]]$county<-"Frederick"
NL[[7]]$county<-"Greene"
NL[[8]]$county<-"Loundon"
NL[[9]]$county<-"Madison"
NL[[10]]$county<-"Orange"
NL[[11]]$county<-"Page"
NL[[12]]$county<-"Rappahannock"
NL[[13]]$county<-"Rockingham"
NL[[14]]$county<-"Shenandoah"
NL[[15]]$county<-"Warren"



CombinedNL<-do.call(rbind.data.frame,NL)
CombinedNL$Scenario<-"NL11"


Folder<-list.files(paste0(version_table, "Q1"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q1<-lapply(Folder,function(i){
  read.csv(i)
})

Q1[[1]]$county<-"Albemarle"
Q1[[2]]$county<-"Augusta"
Q1[[3]]$county<-"Clarke"
Q1[[4]]$county<-"Culpeper"
Q1[[5]]$county<-"Fauquier"
Q1[[6]]$county<-"Frederick"
Q1[[7]]$county<-"Greene"
Q1[[8]]$county<-"Loundon"
Q1[[9]]$county<-"Madison"
Q1[[10]]$county<-"Orange"
Q1[[11]]$county<-"Page"
Q1[[12]]$county<-"Rappahannock"
Q1[[13]]$county<-"Rockingham"
Q1[[14]]$county<-"Shenandoah"
Q1[[15]]$county<-"Warren"



CombinedQ1<-do.call(rbind.data.frame,Q1)
CombinedQ1$Scenario<-"Q1"

Folder<-list.files(paste0(version_table, "Q2"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q2<-lapply(Folder,function(i){
  read.csv(i)
})

Q2[[1]]$county<-"Albemarle"
Q2[[2]]$county<-"Augusta"
Q2[[3]]$county<-"Clarke"
Q2[[4]]$county<-"Culpeper"
Q2[[5]]$county<-"Fauquier"
Q2[[6]]$county<-"Frederick"
Q2[[7]]$county<-"Greene"
Q2[[8]]$county<-"Loundon"
Q2[[9]]$county<-"Madison"
Q2[[10]]$county<-"Orange"
Q2[[11]]$county<-"Page"
Q2[[12]]$county<-"Rappahannock"
Q2[[13]]$county<-"Rockingham"
Q2[[14]]$county<-"Shenandoah"
Q2[[15]]$county<-"Warren"



CombinedQ2<-do.call(rbind.data.frame,Q2)
CombinedQ2$Scenario<-"Q2"

Folder<-list.files(paste0(version_table, "Q3"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q3<-lapply(Folder,function(i){
  read.csv(i)
})

Q3[[1]]$county<-"Albemarle"
Q3[[2]]$county<-"Augusta"
Q3[[3]]$county<-"Clarke"
Q3[[4]]$county<-"Culpeper"
Q3[[5]]$county<-"Fauquier"
Q3[[6]]$county<-"Frederick"
Q3[[7]]$county<-"Greene"
Q3[[8]]$county<-"Loundon"
Q3[[9]]$county<-"Madison"
Q3[[10]]$county<-"Orange"
Q3[[11]]$county<-"Page"
Q3[[12]]$county<-"Rappahannock"
Q3[[13]]$county<-"Rockingham"
Q3[[14]]$county<-"Shenandoah"
Q3[[15]]$county<-"Warren"



CombinedQ3<-do.call(rbind.data.frame,Q3)
CombinedQ3$Scenario<-"Q3"

Folder<-list.files(paste0(version_table, "Q4"), pattern="Cstats.txt", full.names = TRUE) #Read in NCLD files 
Q4<-lapply(Folder,function(i){
  read.csv(i)
})

Q4[[1]]$county<-"Albemarle"
Q4[[2]]$county<-"Augusta"
Q4[[3]]$county<-"Clarke"
Q4[[4]]$county<-"Culpeper"
Q4[[5]]$county<-"Fauquier"
Q4[[6]]$county<-"Frederick"
Q4[[7]]$county<-"Greene"
Q4[[8]]$county<-"Loundon"
Q4[[9]]$county<-"Madison"
Q4[[10]]$county<-"Orange"
Q4[[11]]$county<-"Page"
Q4[[12]]$county<-"Rappahannock"
Q4[[13]]$county<-"Rockingham"
Q4[[14]]$county<-"Shenandoah"
Q4[[15]]$county<-"Warren"



CombinedQ4<-do.call(rbind.data.frame,Q4)
CombinedQ4$Scenario<-"Q4"

Folder<-list.files(paste0(version_table, "RT"), pattern="RT05xxRT_Forest_Cstats.txt", full.names = TRUE) #Read in NCLD files 
RT<-lapply(Folder,function(i){
  read.csv(i)
})

RT[[1]]$county<-"Albemarle"
RT[[2]]$county<-"Augusta"
RT[[3]]$county<-"Clarke"
RT[[4]]$county<-"Culpeper"
RT[[5]]$county<-"Fauquier"
RT[[6]]$county<-"Frederick"
RT[[7]]$county<-"Greene"
RT[[8]]$county<-"Loundon"
RT[[9]]$county<-"Madison"
RT[[10]]$county<-"Orange"
RT[[11]]$county<-"Page"
RT[[12]]$county<-"Rappahannock"
RT[[13]]$county<-"Rockingham"
RT[[14]]$county<-"Shenandoah"
RT[[15]]$county<-"Warren"



CombinedRT<-do.call(rbind.data.frame,RT)
CombinedRT$Scenario<-"RT"


Allscenarios<-rbind(CombinedNL, CombinedRT, CombinedQ1, CombinedQ2, CombinedQ3, CombinedQ4)

write.csv(Allscenarios, "U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/County/V2016_Fragstats_Ctny.csv", row.names = FALSE)

# ----------------------------------------------
#Fragstats by county graphs

  #V2016 Updated for cstats created by county 
Cstats<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/County/V2016_Fragstats_Ctny.csv")
Cstats$mean.patch.areakm<-Cstats$mean.patch.area*(900/1000000)

CstatsFauq<-subset(Cstats, Cstats$county == "Fauquier")
CstatsFred<-subset(Cstats, Cstats$county == "Frederick")

CstatsFF<-rbind(CstatsFauq, CstatsFred)
CstatsFF<-subset(CstatsFF, CstatsFF$Scenario %in%  c("RT", "Q1", "Q2", "Q3", "Q4"))

#mean_patch 

FFMP<-ggplot(CstatsFF, aes(x=county, y=mean.patch.areakm, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("Frederick", "Fauquier"), labels=c("Frederick", "Fauquier"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    xlab("County")+
  scale_y_continuous(name =expression('Mean Patch Area km'^2))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40, colour="black"),
        axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  theme(axis.line = element_line(size=1.5, colour="grey69"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

setwd("U:/CLI/Presentations/ESA-08-XX-2018")
png("v2016_FredFauq_Forestmeanpatch.png", width=480, height=480, units="px", res=300) #can't put units and resolution
FFMP
dev.off()


ggsave(file="v2016_FredFauq_Forestmeanpatch.png", dpi=300,width=15, height=15)




#number of patches 
 v2016_FredFauq_n_patches<-ggplot(CstatsFF, aes(x=county, y=n.patches, fill=Scenario))+
    geom_bar(stat="identity", position="dodge")+
      scale_x_discrete(breaks= c("Frederick", "Fauquier"), labels=c("Frederick", "Fauquier"))+
    scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    xlab("County")+
    scale_y_continuous(name = "Number of Patches")+
    theme_bw()+
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.text=element_text(size=40, colour="black"),
          axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
    theme(plot.margin=unit(c(1,1,1,1), "in"))+
    theme(axis.line = element_line(size=1.5, colour="grey69"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
 
 setwd("U:/CLI/Presentations/ESA-08-XX-2018")
 png("v2016_FredFauq_n_patches.png", width=480, height=480, units="px", res=300) #can't put units and resolution
 v2016_FredFauq_n_patches
 dev.off()
 
 
 ggsave(file="v2016_FredFauq_n_patches.png", dpi=300,width=15, height=15)
  
 #----------------------------------------------
 #Proportion of landscape 
 #Determine area of county of entire study area to get proportion 
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

C_statsCombined<-merge(Cstats,sa_ctyGEOID, by ="class")

sa_ctyGEOIDFF<-subset(sa_ctyGEOID, sa_ctyGEOID$class %in% c(3,10))
sa_ctyGEOIDFF$county<-c("Frederick", "Fauquier")
CstatsFFprop<-merge(sa_ctyGEOIDFF,CstatsFF, by="county")

CstatsFFprop$total.areakm<-CstatsFFprop$total.area*(900/1000000) #Fragstats left in pixels contrary to what the website says 
C_statsCombined$total.areakm<-Cstats$total.area*(900/1000000)
C_statsCombined$prop.landscape<-C_statsCombined$total.areakm/C_statsCombined$Areakm

CstatsFFpropF3<-subset(CstatsFFprop, CstatsFFprop == "Frederick")


CstatsFFpropF3$prop.landscape<-(CstatsFFpropF3$total.areakm/CstatsFFpropF3$Areakm)

CstatsFFpropF10<-subset(CstatsFFprop, CstatsFFprop == "Fauquier")
CstatsFFpropF10$prop.landscape<-(CstatsFFpropF10$total.areakm/CstatsFFpropF10$Areakm)

C_statsFFpropcombined<-rbind(CstatsFFpropF3,CstatsFFpropF10)



C_statsFFpropcombined$prop.landscapePC<-C_statsFFpropcombined$prop.landscape*100

# ----------------------------------------------
#Proprotion Graph 
 FFprop<-ggplot(C_statsFFpropcombined, aes(x=county, y=prop.landscapePC, fill=Scenario))+
    geom_bar(stat="identity", position="dodge")+
    scale_x_discrete(name="County", breaks= c("Frederick", "Fauquier"))+
    scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
    scale_y_continuous(name = "Proportion of the Landscape %", limits=c(0,53), breaks=c(10,20,30,40,50))+
   theme_bw()+
   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
   theme(axis.text=element_text(size=40, colour="black"),
         axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
   theme(plot.margin=unit(c(1,1,1,1), "in"))+
   theme(axis.line = element_line(size=1.5, colour="grey69"), 
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank())
 
 setwd("U:/CLI/Presentations/ESA-08-XX-2018")
 png("v2016_FredFauq_proplandscape.png", width=480, height=480, units="px", res=300) #can't put units and resolution
FFprop
 dev.off()
 
 
 ggsave(file="v2016_FredFauq_proplandscape.png", dpi=300,width=15, height=15)
  
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

#Total Area and Total Core Area 
Folder<-list.files("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/FocalStatistics_Core/Tables/", pattern="rgn.csv", full.names = TRUE) #Read in NCLD files 
CoreArea<-lapply(Folder,function(i){
  read.csv(i)
})

CoreArea[[1]]$Scenario<-"NL01"
CoreArea[[2]]$Scenario<-"NL11"
CoreArea[[3]]$Scenario<-"Q1"
CoreArea[[4]]$Scenario<-"Q2"
CoreArea[[5]]$Scenario<-"Q3"
CoreArea[[6]]$Scenario<-"Q4"
CoreArea[[7]]$Scenario<-"RT"

Combined<-do.call(rbind.data.frame,CoreArea)
Combined$Areakm<-Combined$VALUE_65*(1/1000000) #analysis done in GIS so in meters 
Combined$Rowid_<-NULL

write.csv(Combined,"U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/Forest_Stats/Forest_FullArea/FocalStatistics_Core/Tables/v2016_AllScenarios_CoreArea_rgn.csv", row.names = FALSE)

Area<-read.csv("U:/CLI/Presentations/ESA-08-XX-2018/CoreAreaTotalArea.csv")

Area$TotalArea<-NULL

MeltArea<-melt(Area, id=c("county", "Scenario"))

MeltAreaFred<-subset(MeltArea, MeltArea$county == "Frederick")
MeltAreaFauq<-subset(MeltArea, MeltArea$county == "Fauquier")

AreaFred<-subset(Area, Area$county == "Frederick")
AreaFauq<-subset(Area, Area$county == "Fauquier")


FredTC<-ggplot(MeltAreaFred, aes(x=variable, y=value, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("CoreAreakm", "TotalAreakm"), labels=c("Core Area", "Total Area"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Area km'^2))+
  xlab("Forest Metric")+
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


CoreArea<-ggplot(Area, aes(x=county, y=CoreAreakm, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("Frederick", "Fauquier"), labels=c("Frederick", "Fauquier"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Area km'^2))+
  xlab("County")+
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
png("v2016_FredFauq_CoreArea.png", width=480, height=480, units="px", res=300) #can't put units and resolution
CoreArea
dev.off()

ggsave(file="v2016_FredFauq_CoreArea.png", dpi=300, width=15, height=15)


#------------------------------------------------------------------------------------------#
#Study Area

Percent<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/Fragstats/v2016_RstudioPercent_Fragstats.csv") 

Percent$PercentLostCore<-round(Percent$PercentLostCore, digits = 1)
Percent$PercentNpatches<-round(Percent$PercentNpatches, digits=1)

Percent$PercentNpatches<-Percent$PercentNpatches/100
Percent$PercentLostCore<-Percent$PercentLostCore/-100
Percent$PercentLostAveragePatch<-Percent$PercentLostAveragePatch/-100

Npatches<-ggplot(Percent, aes(x=Scenario, y=PercentNpatches, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  #scale_x_discrete(breaks= c("Q1", "Fauquier"), labels=c("Frederick", "Fauquier"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(limits=c(.25,.42), breaks=c(.25,.3,.35,.4,.45), oob=rescale_none, labels=percent)+
  ylab("Percent Change")+
  xlab("Scenario")+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin=margin(t=20, r=0, b=0, l =0)))+
  theme(axis.text=element_text(size=40, colour="black"),
        axis.title.x=element_text(size=40), axis.title.y =element_text(size=40, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  theme(axis.line = element_line(size=1.5, colour="grey69"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())



setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/OutputVisuals/Fragstats/")

png("v2016_PercentChange2011_Npatches_SA.png", width=480, height=480, units="px", res=300) #can't put units and resolution
Npatches
dev.off()

ggsave(file="v2016_PercentChange2011_Npatches_SA.png", dpi=300, width=15, height=15)


