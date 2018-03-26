############################ 
#PURPOSE: create new tables for R2 test analysis region
#INPUT: 
#OUTPUT: 
#DEVELOPED: 3-13-18
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

############################

# PACKAGES NEEDED

library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(tidyr) # General data manipulation

# ----------------------------------------------
# FILE PATHS

Regi2Path<-"V:/IaraSpatialLayers/Dinamica_Runs/Regi2Area_V201/R2_V2013/"

###########################################
# ~~~ CODE BEGINS ~~~ #
###########################################

# ----------------------------------------------
# REGIONS
# ----------------------------------------------

sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv")

sa_ctyGEOID <- sa_ctyGEOID[1:3,]
sa_ctyGEOID[1,2] <-  51043
sa_ctyGEOID[3,2] <-  51157

  # VALUE GEOID
# 1     1 51043
# 2     2 51002
# 3     3 51157

write.csv(sa_ctyGEOID, file="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID_r2.csv",row.names=FALSE, quote=FALSE)

# sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID_r2.csv")

# ----------------------------------------------
# COMBINE TABLES
# ----------------------------------------------

# ----------------------------------------------
# WithOUT "Lumber" Modifier

# Regions version
R_sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/SA_nlCcomb_0111.txt", header=TRUE,sep=",")

# Counties version
C_sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Combine/Tables/SA_nlCcomb_0111.txt", header=TRUE,sep=",")

# Join together in correct order for R2
sa_cty_transitions <- cbind(C_sa_cty_transitions[,c("Rowid_","LABEL", "GEOID_51043")], R_sa_cty_transitions["GEOID_51002"], C_sa_cty_transitions["GEOID_51157"])

write.table(sa_cty_transitions, file = paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlCcomb_0111.txt"), row.names=FALSE, col.names=TRUE, sep=",") 

#sa_cty_transitions<-read.table(paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlCcomb_0111.txt"), header=TRUE,sep=",")

# ----------------------------------------------
# With "Lumber" Modifier

# Regions version
R_sa_cty_transitions_lum<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/SA_nlCcomb_0111_LUMBER.txt", header=TRUE,sep=",")

# Counties version
C_sa_cty_transitions_lum<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Combine/Tables/SA_nlCcomb_0111_LUMBER.txt", header=TRUE,sep=",")
C_sa_cty_transitions_lum <- rbind(C_sa_cty_transitions[c(1:2, 6, 3),-1], C_sa_cty_transitions_lum) # missing static land cover (33, 44...), so adding from C_ table above. should be the same since nothing changes. #** This table is formatted differently. Not sure why... if you have problems, check that.

# Join together in correct order for R2
sa_cty_transitions_lum <- cbind(R_sa_cty_transitions_lum["Rowid_"],C_sa_cty_transitions_lum[,c("LABEL", "GEOID_51043")], R_sa_cty_transitions_lum["GEOID_51002"], C_sa_cty_transitions_lum["GEOID_51157"])

write.table(sa_cty_transitions_lum, file = paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlCcomb_0111_LUMBER.txt"), row.names=FALSE, col.names=TRUE, sep=",") 

#sa_cty_transitions_lum<-read.table(paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlCcomb_0111_LUMBER.txt"), header=TRUE,sep=",")



# ----------------------------------------------
# Zonal histogram

# Regions version
R_sa_zonal_T0<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/nlcd11_anC_hist.txt", header=TRUE,sep=",")

# Counties version
C_sa_zonal_T0<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Combine/Tables/nlcd11_anC_hist.txt", header=TRUE,sep=",")

# Join together in correct order for R2
sa_zonal_T0 <- cbind(C_sa_zonal_T0[,c("Rowid_","LABEL", "GEOID_51043")], R_sa_zonal_T0["GEOID_51002"], C_sa_zonal_T0["GEOID_51157"])

write.table(sa_zonal_T0, file = paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlcd11_anC_hist.txt"), row.names=FALSE, col.names=TRUE, sep=",") 

#sa_zonal_T0<-read.table(paste0(Regi2Path,"Parameters/Combine/Tables/R2_nlcd11_anC_hist.txt"), header=TRUE,sep=",")

# ----------------------------------------------
# TRANSITION RATES
# ----------------------------------------------

# Regions version
R_sa_Trans_Rates <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0111_LUMBER.csv") # 2001-2011 with 2006 averages for "lumber counties", #corrected for year zero

# Counties version
C_sa_Trans_Rates<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0111_LUMBER.csv")

# Join together in correct order for R2
sa_Trans_Rates <- rbind(C_sa_Trans_Rates[c(1576:1620),], R_sa_Trans_Rates[c(46:90),], C_sa_Trans_Rates[c(496:540),])

# Rename region #s. (R2 for region has same Region # by chance.)
sa_Trans_Rates$Region.[sa_Trans_Rates$Region. == 36] <- 1
sa_Trans_Rates$Region.[sa_Trans_Rates$Region. == 12] <- 3


names(sa_Trans_Rates) <- c("Year*","Region*","From*","To*","Rate")
options(scipen = 999)
sa_Trans_Rates[,1] <- as.numeric(sa_Trans_Rates[,1])
sa_Trans_Rates[,2] <- as.numeric(sa_Trans_Rates[,2])
sa_Trans_Rates$Rate <- as.numeric(substr(as.character(sa_Trans_Rates$Rate),1,10))

write.csv(sa_Trans_Rates, file=paste0(Regi2Path,"Parameters/Transition_Rates/R2_Trans_Rates_FGC-DFGC_0111_LUMBER.csv"),row.names=FALSE, quote=FALSE)

# sa_Trans_Rates <- read.csv(paste0(Regi2Path,"Parameters/Transition_Rates/R2_Trans_Rates_FGC-DFGC_0111_LUMBER.csv")) # 2001-2011 with 2006 averages for "lumber counties", #corrected for year zero


# ----------------------------------------------
# PATCH STATS TABLES
# ----------------------------------------------

# Regions version
R_expansion_matrix <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Patch_Tables/sa_Exp_Table_FGC-DFGC.csv")
R_new_patch_matrix <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Patch_Tables/sa_New_Table_FGC-DFGC.csv")
R_exp_percent_matrix<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Patch_Tables/sa_Percent_Table_FGC-DFGC.csv")


# Counties version
C_expansion_matrix <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Patch_Tables/sa_Exp_Table_FGC-DFGC.csv")
C_new_patch_matrix <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Patch_Tables/sa_New_Table_FGC-DFGC.csv")
C_exp_percent_matrix<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/Patch_Tables/sa_Percent_Table_FGC-DFGC.csv")

# QC:
str(R_expansion_matrix); str(R_new_patch_matrix); str(R_exp_percent_matrix)
str(C_expansion_matrix); str(C_new_patch_matrix); str(C_exp_percent_matrix)

# Join together in correct order for R2 test version
expansion_matrix <- rbind(C_expansion_matrix[C_expansion_matrix$Region. == 36,], R_expansion_matrix[R_expansion_matrix$Region. == 2,], C_expansion_matrix[C_expansion_matrix$Region. == 12,])

new_patch_matrix <- rbind(C_new_patch_matrix[C_new_patch_matrix$Region. == 36,], R_new_patch_matrix[R_new_patch_matrix$Region. == 2,], C_new_patch_matrix[C_new_patch_matrix$Region. == 12,])

exp_percent_matrix <- rbind(C_exp_percent_matrix[C_exp_percent_matrix$Region. == 36,], R_exp_percent_matrix[R_exp_percent_matrix$Region. == 2,], C_exp_percent_matrix[C_exp_percent_matrix$Region. == 12,])

# Rename region #s. (R2 for region has same Region # by chance.)
expansion_matrix$Region.[expansion_matrix$Region. == 36] <- 1
expansion_matrix$Region.[expansion_matrix$Region. == 12] <- 3

new_patch_matrix$Region.[new_patch_matrix$Region. == 36] <- 1
new_patch_matrix$Region.[new_patch_matrix$Region. == 12] <- 3

exp_percent_matrix$Region.[exp_percent_matrix$Region. == 36] <- 1
exp_percent_matrix$Region.[exp_percent_matrix$Region. == 12] <- 3

##
expansion_matrix[,1] <- as.numeric(as.character(expansion_matrix[,1]))
expansion_matrix[,2] <- as.numeric(as.character(expansion_matrix[,2]))
expansion_matrix[,3] <- as.numeric(as.character(expansion_matrix[,3]))
expansion_matrix[,4] <- as.numeric(as.character(expansion_matrix[,4]))
expansion_matrix[,5] <- as.numeric(as.character(expansion_matrix[,5]))
expansion_matrix[,6] <- as.numeric(as.character(expansion_matrix[,6]))
expansion_matrix[,7] <- as.numeric(as.character(expansion_matrix[,7]))

new_patch_matrix[,1] <- as.numeric(as.character(new_patch_matrix[,1]))
new_patch_matrix[,2] <- as.numeric(as.character(new_patch_matrix[,2]))
new_patch_matrix[,3] <- as.numeric(as.character(new_patch_matrix[,3]))
new_patch_matrix[,4] <- as.numeric(as.character(new_patch_matrix[,4]))
new_patch_matrix[,5] <- as.numeric(as.character(new_patch_matrix[,5]))
new_patch_matrix[,6] <- as.numeric(as.character(new_patch_matrix[,6]))
new_patch_matrix[,7] <- as.numeric(as.character(new_patch_matrix[,7]))

exp_percent_matrix[,1] <- as.numeric(as.character(exp_percent_matrix[,1]))
exp_percent_matrix[,2] <- as.numeric(as.character(exp_percent_matrix[,2]))
exp_percent_matrix[,3] <- as.numeric(as.character(exp_percent_matrix[,3]))
exp_percent_matrix[,4] <- as.numeric(as.character(exp_percent_matrix[,4]))
exp_percent_matrix[,5] <- as.numeric(as.character(exp_percent_matrix[,5]))


colnames(expansion_matrix) <- c("Year*","Region*","From*","To*","Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry") 
colnames(new_patch_matrix) <- c("Year*","Region*","From*","To*","Mean_Patch_Size","Patch_Size_Variance","Patch_Isometry")
colnames(exp_percent_matrix)<-c("Year*","Region*","From*","To*","Percent")


write.csv(expansion_matrix,file=paste0(Regi2Path,"Parameters/Patch_Tables/R2_Exp_Table_FGC-DFGC.csv"),row.names=FALSE)
write.csv(new_patch_matrix,file=paste0(Regi2Path,"Parameters/Patch_Tables/R2_New_Table_FGC-DFGC.csv"),row.names=FALSE)
write.csv(exp_percent_matrix,file=paste0(Regi2Path,"Parameters/Patch_Tables/R2_Percent_Table_FGC-DFGC.csv"),row.names=FALSE)

# expansion_matrix <- read.csv(paste0(Regi2Path,"Parameters/Patch_Tables/R2_Exp_Table_FGC-DFGC.csv"))
# new_patch_matrix <- read.csv(paste0(Regi2Path,"Parameters/Patch_Tables/R2_New_Table_FGC-DFGC.csv"))
# exp_percent_matrix<-read.csv(paste0(Regi2Path,"Parameters/Patch_Tables/R2_Percent_Table_FGC-DFGC.csv"))

# ----------------------------------------------
# WOE TABLES
# ----------------------------------------------

# Read all WOE files in (these are the final recalculated, significant ones, with alterations made to WOE)

# Regions version
sav201WOE_LOC<-paste0("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/WOE_recalc/LPA/")
sav201WOE_files <- list.files(sav201WOE_LOC,pattern = ".*v201.*\\csv$")
sav201WOE<-lapply(paste0(sav201WOE_LOC, sav201WOE_files), read.csv)
sav201WOE_nms<-gsub(".csv", "",sav201WOE_files)
names(sav201WOE)<-sav201WOE_nms

# Counties version
sav108WOE_LOC<-paste0("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V108/SA_V1081/Parameters/WOE_recalc/LPA/")
sav108WOE_files <- list.files(sav108WOE_LOC,pattern = ".*v108.*\\csv$")
sav108WOE<-lapply(paste0(sav108WOE_LOC, sav108WOE_files), read.csv)
sav108WOE_nms<-gsub(".csv", "",sav108WOE_files)
names(sav108WOE)<-sav108WOE_nms

{# str(sav201WOE)
# List of 8
 # $ :'data.frame':	394 obs. of  6 variables...

# str(sav108WOE)
# List of 56
 # $ :'data.frame':	74 obs. of  6 variables...
 
# > names(sav201WOE)
# [1] "pgrv201_WOE01" "pgrv201_WOE02" "pgrv201_WOE03" "pgrv201_WOE04" "pgrv201_WOE05"
# [6] "pgrv201_WOE06" "pgrv201_WOE07" "pgrv201_WOE08"
# > names(sav108WOE)
 # [1] "pgrv108_WOE01" "pgrv108_WOE02" "pgrv108_WOE03" "pgrv108_WOE04" "pgrv108_WOE05"
 # [6] "pgrv108_WOE06" "pgrv108_WOE07" "pgrv108_WOE08" "pgrv108_WOE09" "pgrv108_WOE10"
# [11] "pgrv108_WOE11" "pgrv108_WOE12" "pgrv108_WOE13" "pgrv108_WOE14" "pgrv108_WOE15"
# [16] "pgrv108_WOE16" "pgrv108_WOE17" "pgrv108_WOE18" "pgrv108_WOE19" "pgrv108_WOE20"
# [21] "pgrv108_WOE21" "pgrv108_WOE22" "pgrv108_WOE23" "pgrv108_WOE24" "pgrv108_WOE25"
# [26] "pgrv108_WOE26" "pgrv108_WOE27" "pgrv108_WOE28" "pgrv108_WOE29" "pgrv108_WOE30"
# [31] "pgrv108_WOE31" "pgrv108_WOE32" "pgrv108_WOE33" "pgrv108_WOE34" "pgrv108_WOE35"
# [36] "pgrv108_WOE36" "pgrv108_WOE37" "pgrv108_WOE38" "pgrv108_WOE39" "pgrv108_WOE40"
# [41] "pgrv108_WOE41" "pgrv108_WOE42" "pgrv108_WOE43" "pgrv108_WOE44" "pgrv108_WOE45"
# [46] "pgrv108_WOE46" "pgrv108_WOE47" "pgrv108_WOE48" "pgrv108_WOE49" "pgrv108_WOE50"
# [51] "pgrv108_WOE51" "pgrv108_WOE52" "pgrv108_WOE53" "pgrv108_WOE54" "pgrv108_WOE55"
# [56] "pgrv108_WOE56"
}

R2_pgrv201_WOE01 <- sav108WOE[["pgrv108_WOE36"]]

R2_pgrv201_WOE02 <- sav201WOE[["pgrv201_WOE02"]]

R2_pgrv201_WOE03 <- sav108WOE[["pgrv108_WOE12"]]

str(R2_pgrv201_WOE02); str(R2_pgrv201_WOE01); str(R2_pgrv201_WOE03) # These have different # of variables represented. is that a problem??



names(R2_pgrv201_WOE01) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
write.csv(R2_pgrv201_WOE01,file = paste0(Regi2Path,"Parameters/WOE_recalc/LPA/R2_pgrv201_WOE01.csv"),row.names=FALSE)

names(R2_pgrv201_WOE02) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
write.csv(R2_pgrv201_WOE02,file = paste0(Regi2Path,"Parameters/WOE_recalc/LPA/R2_pgrv201_WOE02.csv"),row.names=FALSE)

names(R2_pgrv201_WOE03) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
write.csv(R2_pgrv201_WOE03,file = paste0(Regi2Path,"Parameters/WOE_recalc/LPA/R2_pgrv201_WOE03.csv"),row.names=FALSE)











