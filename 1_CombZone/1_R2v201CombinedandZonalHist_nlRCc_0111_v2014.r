############################ 
#PURPOSE:  Do Combine and Hist for **NLCD Land Cover years 2001 & 2011**. Avoid using ArcMap and doing all the steps by hand.
#INPUT: Region layer and county table with "Din_cty" and "GEOID", intial landscape layer, final landscape layer. # check
#OUTPUT: Zonal histogram of transitions between initial and final, zonal historgram of final landscape, raster of change in landscape. # check
#DEVELOPED: 
	#	V1	2/18/2016 - Valentine Herrmann 
	#	V2	5/2016 - Valentine Herrmann 
	#	V3	6/1/2016 - Iara Lacher - Created raster with persistent land use in addition to changes in land use.
	#	V4	3/30/2017 - Iara Lacher - Loop through recent trends and scenarios 50 years out

#CONTACT: LacherI@si.edu
#NOTES: 
# Time difference of 14.52866 mins

# This script would be the equivalent of:
#   - opening ArcMap, adding cblcd_92_an.img, cblcd_01_an.img and cblcd_1_an.img
#   - using "combine" on cblcd_92_an.img and cblcd_01_an.img, then on cblcd_01_an.img and  cblcd_11_an.img to create raster comb_9201 and comb_0111
#   - using "zonal histogram " on comb_9201 and cblcd_01_an.img
#   - using "zonal histogram " on comb_0111 and cblcd_11_an.img
#   - merge zonal histogram into on table 
#   - export merged table to .txt files

#IMPORTANT: 
# For version 2011, use nlcd landcover data WITH protected lands 

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
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE PATHS

# Set location for the input study area rasters
inRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/R4Bndy/"

# Define paths for output files
Comb_output <- "V:/IaraSpatialLayers/Dinamica_Runs/Regi4Area_V201/R4_2014/Parameters/Combine/Tables/" # Combine and final histogram outputs for each landscape

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Land Cover Rasters:
# nl01 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd01_anC.img")
# nl06 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd06_anC.img")
# nl11 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/nlcd11_anC.img")

# COUNTY RASTERS
regions <- raster(paste(inRasterLoc, "region_r4", ".img", sep="")) # this is for the raster.
counties_vals <- getValues(regions)

# str(counties_vals)
# int [1:64956544] NA NA NA NA NA NA NA NA NA NA ...

# COUNTY TABLES
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Regi4Area_V201/R4GEOID.csv")


colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) 



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------

# Use nlcd landcover "RCc" data WITH protected lands (PL=NA) # remove ?
LS_trans <-  list('01_to_11' = c("nlcd01_R4.img", "nlcd11_R4.img", "R4_nlCcomb_0111.txt"))

# ------------------------------------------------------
# ------------------------------------------------------
# IDENTIFY AND COLLATE CHANGES IN LAND USE BETWEEN YEARS
# ------------------------------------------------------
# ------------------------------------------------------


# ----------------------------------------------
# Loop through transitions
# ----------------------------------------------
old <- Sys.time() # TIMING SCRIPT

for(in_to_fin in names(LS_trans)){ # Makes code flexible for use with more than 2 landscapes. ##CF- so not actual loop here?

Initial_Landscape <- paste0(inRasterLoc, LS_trans[[in_to_fin]][1]) 
Final_Landscape <-paste0(inRasterLoc, LS_trans[[in_to_fin]][2]) 

comb_hist_output <- paste0(Comb_output, LS_trans[[in_to_fin]][3])
Init_hist_output <- paste0(Comb_output, gsub(".img","_hist.txt",LS_trans[[in_to_fin]][1]))
Final_hist_output <- paste0(Comb_output, gsub(".img","_hist.txt",LS_trans[[in_to_fin]][2]))

Initial_Landscape <- raster(Initial_Landscape)
Final_Landscape <- raster(Final_Landscape)

init_vals <- getValues(Initial_Landscape)
fin_vals <- getValues(Final_Landscape)

  
# ----------------------------------------------
# ----------------------------------------------
{# ZONAL HISTOGRAM ON TRANSITIONS BETWEEN (2001-2011) LANDSCAPES
# ----------------------------------------------
# ----------------------------------------------


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
}

# ----------------------------------------------
# ----------------------------------------------
{# ZONAL HISTOGRAM ON INITIAL LANDSCAPE 
# ----------------------------------------------
# ----------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select desired transitions 
initial <- ifelse(init_vals %in%  c("3","5","6","7"), init_vals, 0)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#add county values
initial <- paste0(initial, ".", counties_vals)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#summarise pixels
initial_hist0 <- summary(factor(initial), maxsum = length(unique(initial))) # had to add mmaxsum = length(unique(initial)) because otherwise doesn't show all possible values and there is a column called "others" - Val

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#remove values that we don't want
initial_hist0 <- initial_hist0[-grep("^(-?)0.", names(initial_hist0))]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reorganize #CF- create table with county as row, transition as column
initial_hist <- sa_ctyGEOID

for(i in initial_hist$Din_cty){
 
 for (j in c("3","5","6","7")){ 
   if(any(names(initial_hist0) == paste(j, i, sep = "."))){
     initial_hist[initial_hist$Din_cty == i, as.character(j)] <- initial_hist0[names(initial_hist0) == paste(j, i, sep = ".")]
   }}}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reformat Table #CF- create table with county as column, transition as row
initial_hist <- t(initial_hist[-1])
colnames(initial_hist) <- paste0("GEOID_", initial_hist[1,])
initial_hist <- initial_hist[-1,]
initial_hist <- cbind(data.frame(Rowid_ = 1:nrow(initial_hist),LABEL = rownames(initial_hist)), initial_hist)
initial_hist<-arrange(initial_hist, LABEL)   

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove NAs #CF- convert NA to 0
initial_hist[is.na(initial_hist)]<-0

# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# WRITE TO FILE
write.table(initial_hist, file = Init_hist_output, row.names=FALSE, col.names=TRUE, sep=",") 
}

# ----------------------------------------------
# ----------------------------------------------
{# ZONAL HISTOGRAM ON FINAL LANDSCAPE 
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

}

new<-Sys.time()-old # TIMING SCRIPT
print(new) # Time difference of 14.52866 mins

# # ----------------------------------------------
# # SELECT ONLY COUNTIES IN STUDY AREA
# # **IMPORTANT** DO NOT USE THIS FOR CALCULATING TRANSITION RATES!!
# # ----------------------------------------------

# # ----------------------------------------------
# # READ FROM FILE
# sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/SAnlCcomb_0111.txt", header=TRUE, sep=",")#SCBI V:
# sa_cty_transitions<-sa_cty_transitions[,-1]
# row.names(sa_cty_transitions) <- sa_cty_transitions[,1]

# # First reconfigure the table
# t_trans <- t(sa_cty_transitions)
# t_trans <- t_trans[-1,]
# t_trans <- as.data.frame(t_trans)
# t_trans$GEOID <- as.numeric(substring(row.names(t_trans),7,12)) #create GEOID column
# t_trans$Version <- "V2011_RT" #create GEOID column


# # Select only the counties in the study area
# ctytr_V2011_RT<-subset(t_trans, t_trans$GEOID %in% S20_GEOID$GEOID)

# write.table(ctytr_V2011_RT,"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/BasicDataAnalyses/ctytr_V2011_RT.txt", row.names=FALSE)



# # ----------------------------------------------
# # ----------------------------------------------
# # CREATE TRANSITIONS RASTERS
# # ----------------------------------------------
# # ----------------------------------------------

# # ----------------------------------------------
# # TIMING SCRIPT
old <- Sys.time()


# # ----------------------------------------------
# # Select desired transitions 
Change_Values <- paste0(init_vals, fin_vals, sep="")
Change_Values[Change_Values == "NANA"] <- NA
Change_Values <- ifelse(Change_Values %in% c("33","55","66","77","53","63","73","65","75","56","76","57","67") | is.na(Change_Values), Change_Values, 0) 
Change_Raster <- Final_Landscape 
Change_Raster <- setValues(Change_Raster, as.numeric(Change_Values)) 

# # ----------------------------------------------
# # TIMING SCRIPT
new<-Sys.time()-old
print(new)  

# # ----------------------------------------------
# # Save in .bil format instead of .img because R creates a multi-band raster with .img.
writeRaster(Change_Raster, filename="V:/IaraSpatialLayers/Dinamica_Runs/Regi4Area_V201/R4_2014/Parameters/Combine/Rasters/R4nlCcomb_0111.bil", format="EHdr", overwrite=TRUE)


