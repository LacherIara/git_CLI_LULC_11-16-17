############################ 
#PURPOSE:  Do Combine and Hist for NLCD Land Cover years 2001 & 2011.Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer.
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape.
#Version:4 on 5/30/2017 by Iara Lacher & Craig Fergus
#CONTACT: LacherI@si.edu

#IMPORTANT: 
# Use nlcd landcover data with protected lands #LS_trans = Landscape Transtions



############################

# SET WORKING DIRECTORY
setwd("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/")
old1 <- Sys.time()

# ----------------------------------------------
################################################


# PACKAGES NEEDED
library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(raster) # read and edit rasters


# SET TEMP DIRECTORY
rasterOptions(tmpdir = "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/rtempCLEARME/")


# ----------------------------------------------
# READ OUTPUT FILES:


# ----------------------------------------------
# READ INPUT FILES:



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

# Define paths for output files
Comb_output <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Tables/" # Combine (01-11)
Final_output<-"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/" # final (2011) histogram by county ##CF- not into a output folder?


# ----------------------------------------------
# COUNTIES
# ----------------------------------------------

regions <- raster(paste(inRasterLoc, "cnty_smsc", ".img", sep="")) # this is for the raster. 
counties_vals <- getValues(regions) 

str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ... ##CF- int [1:15788305] NA NA NA NA NA NA NA NA NA NA ...

sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/SMSC_GEOID.csv")
colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")##CF- do we need this or could I just change the names in the csv ahead of time? Does setting the names in r change anything
str(sa_ctyGEOID)
##CF- add ouput? 'data.frame':	56 obs. of  2 variables: -- Din_cty: int  1 2 3 4 5 6 7 8 9 10 ...-- $ GEOID  : int  11001 51029 51069 51107 51109 51171 24021 24031 51017 51061 ..

# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

# Use nlcd landcover "RCc" data WITH protected lands  ##CF- LS_trans = Landscape Transtions
LS_trans <-  list('01_to_11' = c("nlcd01c_smsc.img", "nlcd11c_smsc.img", "SMSC_nlCcomb_0111.txt")) 



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

for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?

Initial_Landscape <- paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) 
Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][2]) 

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
comb_infi <- ifelse(comb_infi %in% c("33","55","66","77","53","63","73","65","75","56","76","57","67"), comb_infi, 0)


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
comb_infi_hist0 <- comb_infi_hist0[-grep("^(-?)0.", names(comb_infi_hist0))]##CF- understad task, dont understand language


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reorganize #CF- create table with county as row, transition as column

comb_infi_hist <- sa_ctyGEOID

for(i in comb_infi_hist$Din_cty){ 
paste(i)
for (j in c("33","55","66","77","53","63","73","65","75","56","76","57","67")){ 
  if(any(names(comb_infi_hist0) == paste(j, i, sep = "."))){
    comb_infi_hist[comb_infi_hist$Din_cty == i, as.character(j)] <- comb_infi_hist0[names(comb_infi_hist0) == paste(j, i, sep = ".")]
}}}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table #CF- create table with county as column, transition as row
comb_infi_hist <- t(comb_infi_hist[-1])
colnames(comb_infi_hist) <- paste0("GEOID_", comb_infi_hist[1,])
comb_infi_hist <- comb_infi_hist[-1,]
comb_infi_hist <- cbind(data.frame(Rowid_ = 1:nrow(comb_infi_hist),LABEL = rownames(comb_infi_hist)), comb_infi_hist)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs #CF- convert NA to 0
comb_infi_hist[is.na(comb_infi_hist)]<-0


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#save
write.table(comb_infi_hist, file = comb_hist_output, row.names=FALSE, sep=",") 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ZONAL HISTOGRAM ON FINAL LANDSCAPE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~


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
# Remove NAs #CF- convert NA to 0
final_hist[is.na(final_hist)]<-0


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(final_hist, file = Final_hist_output, row.names=FALSE, col.names=TRUE, sep=",") 
}


# ----------------------------------------------
# TIMING SCRIPT
new<-Sys.time()-old
print(new)



# ----------------------------------------------
# ----------------------------------------------
# CREATE TRANSITIONS RASTERS
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# TIMING SCRIPT
old <- Sys.time()


# ----------------------------------------------
# Select desired transitions  ##CF- unsure of strucure dif from line 150
Change_Values <- paste0(init_vals, fin_vals, sep="")
Change_Values[Change_Values == "NANA"] <- NA
Change_Values <- ifelse(Change_Values %in% c("33","55","66","77","53","63","73","65","75","56","76","57","67") | is.na(Change_Values), Change_Values, 0) 
Change_Raster <- Final_Landscape 
Change_Raster <- setValues(Change_Raster, as.numeric(Change_Values)) 

# ----------------------------------------------
# TIMING SCRIPT
new<-Sys.time()-old
print(new)  

# ----------------------------------------------
# Save in .bil format instead of .img because R creates a multi-band raster with .img.
writeRaster(Change_Raster, filename="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Rasters/SAnlCcomb_0111.bil", format="EHdr", overwrite=TRUE)
PChange_Raster<- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Rasters/SAnlCcomb_0111.bil")

new1<-Sys.time()-old1

