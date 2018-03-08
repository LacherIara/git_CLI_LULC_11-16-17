############################ 
#PURPOSE: Data preparation and analysis needed in the Model Validation step of the LULC process
#INPUT: 
#OUTPUT: 
#DEVELOPED: 5-23-17
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY


# ----------------------------------------------
################################################


# PACKAGES NEEDED
library(raster)
library(dplyr)

# Header for what grouped packages are for


# SET TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE PATHS: 

# ----------------------------------------------
# READ OUTPUT FILES:

# ----------------------------------------------
# READ INPUT FILES:

# Transition Rates
Trans_Rates <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107_save/smsc_1071/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC.csv")

# Patch Tables
Exp_Table <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107_save/smsc_1071/Parameters/Patch_Tables/smsc107_Exp_Table_FGC-DFGC.csv")
New_Table <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107_save/smsc_1071/Parameters/Patch_Tables/smsc107_New_Table_FGC-DFGC.csv")
Perc_Table <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107_save/smsc_1071/Parameters/Patch_Tables/smsc107_Percent_Table_FGC-DFGC.csv")


nl01 <- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/nlcd01c_smsc.img")
nl11 <- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/nlcd11c_smsc.img")

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# Only have tables represent one year.

Trans_Rates1  <-filter(Trans_Rates , Year. ==1)
Exp_Table1    <-filter(Exp_Table  , Year. ==1)
New_Table1    <-filter(New_Table  , Year. ==1)
Perc_Table1   <-filter(Perc_Table , Year. ==1)

# set to 3 decimal values??
# options(scipen = 999)

# Correct file names

names(Trans_Rates1) <- c("Year*","Region*","From*","To*","Rate")
Trans_Rates1[,1] <- as.numeric(Trans_Rates1[,1])
Trans_Rates1[,2] <- as.numeric(Trans_Rates1[,2])
Trans_Rates1$Rate <- as.numeric(substr(as.character(Trans_Rates1$Rate),1,10))

names(Exp_Table1) <- c("Year*","Region*","From*","To*","Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry")
Exp_Table1[,1] <- as.numeric(as.character(Exp_Table1[,1]))
Exp_Table1[,2] <- as.numeric(as.character(Exp_Table1[,2]))
Exp_Table1[,3] <- as.numeric(as.character(Exp_Table1[,3]))
Exp_Table1[,4] <- as.numeric(as.character(Exp_Table1[,4]))
Exp_Table1[,5] <- as.numeric(as.character(Exp_Table1[,5]))
Exp_Table1[,6] <- as.numeric(as.character(Exp_Table1[,6]))
Exp_Table1[,7] <- as.numeric(as.character(Exp_Table1[,7]))

names(New_Table1) <- c("Year*","Region*","From*","To*","Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry")
New_Table1[,1] <- as.numeric(as.character(New_Table1[,1]))
New_Table1[,2] <- as.numeric(as.character(New_Table1[,2]))
New_Table1[,3] <- as.numeric(as.character(New_Table1[,3]))
New_Table1[,4] <- as.numeric(as.character(New_Table1[,4]))
New_Table1[,5] <- as.numeric(as.character(New_Table1[,5]))
New_Table1[,6] <- as.numeric(as.character(New_Table1[,6]))
New_Table1[,7] <- as.numeric(as.character(New_Table1[,7]))

names(Perc_Table1)<-c("Year*","Region*","From*","To*","Percent")
Perc_Table1[,1] <- as.numeric(as.character(Perc_Table1[,1]))
Perc_Table1[,2] <- as.numeric(as.character(Perc_Table1[,2]))
Perc_Table1[,3] <- as.numeric(as.character(Perc_Table1[,3]))
Perc_Table1[,4] <- as.numeric(as.character(Perc_Table1[,4]))
Perc_Table1[,5] <- as.numeric(as.character(Perc_Table1[,5]))

#QC:
str(Trans_Rates1); str(Exp_Table1); str(New_Table1); str(Perc_Table1)


# WRITE TO FILE

write.csv(Trans_Rates1, file="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/SMSC_107_Validation_5-18-17/SMSC_V1071_Validation/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_yr1.csv",row.names=FALSE, quote=FALSE)

write.csv(Exp_Table1, file="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/SMSC_107_Validation_5-18-17/SMSC_V1071_Validation/Parameters/Patch_Tables/smsc107_Exp_Table_FGC-DFGC_yr1.csv",row.names=FALSE, quote=FALSE)

write.csv(New_Table1, file="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/SMSC_107_Validation_5-18-17/SMSC_V1071_Validation/Parameters/Patch_Tables/smsc107_New_Table_FGC-DFGC_yr1.csv",row.names=FALSE, quote=FALSE)

write.csv(Perc_Table1, file="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/SMSC_107_Validation_5-18-17/SMSC_V1071_Validation/Parameters/Patch_Tables/smsc107_Percent_Table_FGC-DFGC_yr1.csv",row.names=FALSE, quote=FALSE)





















