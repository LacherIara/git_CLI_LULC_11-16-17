############################ 
#PURPOSE:  Do Combine and Hist for NLCD Land Cover years 2001 & 2011.Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer. # check
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape. # check
#DEVELOPED: 
#			V1	2/18/2016 - Valentine Herrmann 
#			V2	5/2016 - Valentine Herrmann 
#			V3	6/1/2016 - Iara Lacher - Created raster with persistent land use in addition to changes in land use.

#CONTACT: LacherI@si.edu
#NOTES: 
# Time difference of 13.62602 mins
# This script would be the equivalent of:
#   - opening ArcMap, adding cblcd_92_an.img, cblcd_01_an.img and cblcd_1_an.img
#   - using "combine" on cblcd_92_an.img and cblcd_01_an.img, then on cblcd_01_an.img and  cblcd_11_an.img to create raster comb_9201 and comb_0111
#   - using "zonal histogram " on comb_9201 and cblcd_01_an.img
#   - using "zonal histogram " on comb_0111 and cblcd_11_an.img
#   - merge zonal histogram into on table 
#   - export merged table to .txt files

#IMPOQ4ANT: 
# Use nlcd landcover data withOUT protected lands (PL=NA)

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
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# READ OUTPUT FILES:
# comb_hist_output <- read.table("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/SAnlCcomb_0111.txt")
# final_hist <- read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/nlcd11_anC_hist.txt")
# PChange_Raster<- raster("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Combine/Rasters/SAnlCcomb_0111.bil")


# ----------------------------------------------
# READ INPUT FILES:
# regions <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/cnty_an.img")
# sa_ctyGEOID<- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/FullGEOID.csv")
# Initial_Landscape <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/nlcd01_anC.img") # check if should put nlcd01rc instead
# Final_Landscape <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/nlcd11_anC.img") # check if should put nlcd11rc instead



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations, counties raster and transitions 
# --------------------------------------------------------
# --------------------------------------------------------


# ----------------------------------------------
# FILE LOCATIONS
# ----------------------------------------------

# Set location for the input study area rasters
inRasterLoc <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/"
scRasterLoc <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/FutureLandscapes/Q4/"
# Define paths for output files
Comb_output <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Tables/" # Combine (01-11)
Final_output<-"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/" # final (2011) histogram by county ##CF- not into a output folder?


# ----------------------------------------------
# COUNTIES
# ----------------------------------------------

regions <- raster(paste(inRasterLoc, "cnty_smsc", ".img", sep="")) # this is for the raster.
counties_vals <- getValues(regions)

str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

smsc_ctyGEOID<- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/SMSC_GEOID.csv")
colnames(smsc_ctyGEOID)<-c("Din_cty", "GEOID")
str(smsc_ctyGEOID)


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

# Use nlcd landcover "RCc" data WITH protected lands (PL=NA) # remove ?
LS_trans <-  list('11_to_Q4' = c("nlcd11c_smsc.img", "smsc_v1071_Q4_Landscape05.tif", "SMSCnlCcomb_11Q4.txt")) 



# ------------------------------------------------------
# ------------------------------------------------------
# IDENTIFY AND COLLATE CHANGES IN LAND USE BETWEEN YEARS
# ------------------------------------------------------
# ------------------------------------------------------


# ----------------------------------------------
# TIMING SCRIPT
old <- Sys.time()


# ----------------------------------------------
# Loop through transitions
# ----------------------------------------------

for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. 

Initial_Landscape <- paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) 
Final_Landscape <-paste0(scRasterLoc, LS_trans[[in_to_fin]][2]) 

comb_hist_output <- paste0(Comb_output, LS_trans[[in_to_fin]][3])
Final_hist_output <- paste0(Final_output, gsub(".img","_hist.txt",LS_trans[[in_to_fin]][2]))

Initial_Landscape <- raster(Initial_Landscape)
Final_Landscape <- raster(Final_Landscape)

init_vals <- getValues(Initial_Landscape)
fin_vals <- getValues(Final_Landscape)

  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ZONAL HISTOGRAM ON TRANSITIONS BETWEEN (2001-2011) LANDSCAPES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select desired transitions 
comb_infi <- paste0(init_vals, fin_vals)
comb_infi[comb_infi == "NANA"] <- NA
comb_infi <- ifelse(comb_infi %in% c("22","33","55","66","77","52","62","72","23","53","63","73","65","75","56","76","57","67"), comb_infi, 0)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#add county values
comb_infi <- paste0(comb_infi, ".", counties_vals)

# > unique(comb_infi)
# [1] "0.NA"  "55.18" "22.18" "66.18" "55.19" "22.19" "66.19" "33.19" "77.18" "56.18" "33.18" "0.19"  "0.18" 
# [14] "77.19" "55.17" "0.17"  "77.17" "22.17" ...


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#summarise pixels
comb_infi_hist0 <- summary(factor(comb_infi), maxsum = length(unique(comb_infi))) # had to add maxsum = length(unique(comb_infi)) because otherwise doesn't show all possible values and there is a column called "others"  - Val

# > comb_infi_hist0
 # 0.1     0.10     0.11     0.12     0.13     0.14     0.15     0.16     0.17     0.18     0.19      0.2 
 # 774    63909     1640     3256   109002...


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#remove values that we don't want
comb_infi_hist0 <- comb_infi_hist0[-grep("^(-?)0.", names(comb_infi_hist0))]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reorganize

comb_infi_hist <- smsc_ctyGEOID

for(i in comb_infi_hist$Din_cty){ 
paste(i)
for (j in c("22","33","55","66","77","52","62","72","23","53","63","73","65","75","56","76","57","67")){ 
  if(any(names(comb_infi_hist0) == paste(j, i, sep = "."))){
    comb_infi_hist[comb_infi_hist$Din_cty == i, as.character(j)] <- comb_infi_hist0[names(comb_infi_hist0) == paste(j, i, sep = ".")]
}}}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table
comb_infi_hist <- t(comb_infi_hist[-1])
colnames(comb_infi_hist) <- paste0("GEOID_", comb_infi_hist[1,])
comb_infi_hist <- comb_infi_hist[-1,]
comb_infi_hist <- cbind(data.frame(Rowid_ = 1:nrow(comb_infi_hist),LABEL = rownames(comb_infi_hist)), comb_infi_hist)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs
comb_infi_hist[is.na(comb_infi_hist)]<-0


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#save
write.table(comb_infi_hist, file = comb_hist_output, row.names=FALSE, sep=",") 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ZONAL HISTOGRAM ON FINAL LANDSCAPE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select desired transitions 
final <- ifelse(fin_vals %in%  c("2","3","5","6","7"), fin_vals, 0)


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
#Reorganize
final_hist <- smsc_ctyGEOID

for(i in final_hist$Din_cty){
 
 for (j in c("2","3","5","6","7")){ 
   if(any(names(final_hist0) == paste(j, i, sep = "."))){
     final_hist[final_hist$Din_cty == i, as.character(j)] <- final_hist0[names(final_hist0) == paste(j, i, sep = ".")]
   }}}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table
final_hist <- t(final_hist[-1])
colnames(final_hist) <- paste0("GEOID_", final_hist[1,])
final_hist <- final_hist[-1,]
final_hist <- cbind(data.frame(Rowid_ = 1:nrow(final_hist),LABEL = rownames(final_hist)), final_hist)
final_hist<-arrange(final_hist, LABEL)   


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs
final_hist[is.na(final_hist)]<-0


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(final_hist, file = Final_hist_output, row.names=FALSE, col.names=TRUE, sep=",") 
}


# ----------------------------------------------
# TIMING SCRIPT
new<-Sys.time()-old
print(new)



