############################ 
#PURPOSE: Read all files into R
#INPUT: raw rasters and tables
#OUTPUT: 
#DEVELOPED: Iara Lacher 1/16/15 
#CONTACT: LacherI@si.edu
#LAST EDITED: 
#NOTES: 
##### NEXT STEPS #####
# * Make sure this is for study area instead of Study Area - correct

############################



# PACKAGES NEEDED
library(raster)

#new temp directory
rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/")


# ----------------------------------------------
# FILE PATHS

# Study Area Rasters
inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server
inRasterLoc<-"V:/IaraSpatialLayers/PreparedModRas/Full/" #V drive
PL_RasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/ProLands/" 

# Individual Counties
County_Folder <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/IndCntys" 

# Output Folders
Output_Folder <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/Patch_Stats"
Comb_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/Combine/Tables/" # Combine (01-11)
CombRas_output <- "Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/Combine/Rasters/"
Final_output<-"Y:/Lacher/ProtectedLandsProject/PatchesTransitions_SA/" # final (2011) histogram by county


################################################
################################################
# TABLES 
################################################
################################################


# ----------------------------------------------
{# COUNTY GEOID 
# ----------------------------------------------
sa_ctyGEOID<- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/FullGEOID.csv")
colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero<-sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty<-sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) 

#select only the Study Area counties to run basic analyses - **need to create still
S20_GEOID<- read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/SAcntyOnly.csv", header = T, sep=",")
S20_GEOID<-S20_GEOID[,c(2,3)]
colnames(S20_GEOID)<-c("GEOID", "Name")
# or 
# S20_GEOID<-S20_GEOID[,c(1,2)]
# colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")

# COUNTY AREA - CALCULATED IN PL_PLOTTING.R SCRIPT FROM NLCD RASTER 

}

# ----------------------------------------------
{# TRANSITION MATRIX 
# ----------------------------------------------

# *NLCD "RCa"*
# ----------------------------------------------
# Result of Combine Function - produces table for refereincing vales used in landcover transitions
sa_combine<-read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Combine/Tables/SAnlcomb_0111.txt", header = T, sep=",")
sa_combine<-sa_combine[,-1]

# zonal histogram
sa_zonal_T0 <- read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/SAnl_11_hist.txt", header = T, sep = ",")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NLCD difference between 2001 and 2011 
# Number of cells that change in each county (exported from ArcGIS above)
sa_cty_transitions <- read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Combine/Tables/SAnlcomb_0111_hist.txt", header = T, sep =",")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transition Rates Including Buffer (comes from R script TransitionRates.r)
sabuff_trans<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sabuff_trans.csv" )

# Transition Rates FINAL TABLE Study Area Only (**THIS IS THE ONE YOU READ IN**)
sa_trans<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sa_trans.csv" )
 

 }
 
# ----------------------------------------------
{# PATCH STATS 
# ----------------------------------------------

# Created using R script:
# Input into newest version of Dinamica:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patch Statistics

##  To read csv. Set the paths of the expansion/new stats ("For_Dev", "Gra_Dev", "Cro_Dev")
ExpNew_Folder <- "Y:/Lacher/Dinamica_Runs/PatchesTransitions/Patch_Stats"

D42_expansion <- read.csv(paste(ExpNew_Folder,"/ExpanDevelopment42.csv", sep = "")) 

str(D52_expansion)
# 'data.frame':	15 obs. of  7 variables:
 # $ County                 : int  51043 51047 51061 51069 51113 51139 51157 51171 51187 51840 ...
 # $ Number.of.Patches      : int  NA NA NA 127 NA NA NA 13 26 24 ...
 # $ MN.Patch.Area.ha.      : num  NA NA NA 4.71 NA ...
 # $ Variance.Patch.Area.ha.: num  NA NA NA 52.9 NA ...
 # $ MN.Perm.Area.Rat.m.m2. : num  NA NA NA 0.0585 NA ...
 # $ MN.Shape.Index         : num  NA NA NA 1.76 NA ...
 # $ Perc.Expansion         : num  NA NA NA 0.809 NA ...

D42_newpatch <- read.csv(paste(ExpNew_Folder, "/NewDevelopment42.csv", sep = "")) 

str(D42_newpatch)
# 'data.frame':	15 obs. of  7 variables:
 # $ County                 : int  51043 51047 51061 51069 51113 51139 51157 51171 51187 51840 ...
 # $ Number.of.Patches      : int  NA NA NA 55 NA NA NA 8 26 2 ...
 # $ MN.Patch.Area.ha.      : num  NA NA NA 0.836 NA ...
 # $ Variance.Patch.Area.ha.: num  NA NA NA 9.56 NA ...
 # $ MN.Perm.Area.Rat.m.m2. : num  NA NA NA 0.102 NA ...
 # $ MN.Shape.Index         : num  NA NA NA 1.16 NA ...
 # $ Perc.New               : num  NA NA NA 0.327 NA ...

D52_expansion <- read.csv(paste(ExpNew_Folder,"/ExpanDevelopment52.csv", sep = "")) 
D52_newpatch <- read.csv(paste(ExpNew_Folder, "/NewDevelopment52.csv", sep = "")) 

D62_expansion <- read.csv(paste(ExpNew_Folder,"/ExpanDevelopment62.csv", sep = "")) 
D62_newpatch <- read.csv(paste(ExpNew_Folder, "/NewDevelopment62.csv", sep = "")) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# New Patch Parameters:

expansion_matrix <- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions/Patch_Tables/Exp_TableFGC2D.csv" )

str(expansion_matrix)
# 'data.frame':	450 obs. of  7 variables:
 # $ Year.              : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Region.            : int  1 1 1 2 2 2 3 3 3 4 ...
 # $ From.              : int  42 52 62 42 52 62 42 52 62 42 ...
 # $ To.                : int  2 2 2 2 2 2 2 2 2 2 ...
 # $ Mean_Patch_Size    : num  4.51 4.71 4.24 5.57 2.44 ...
 # $ Patch_Size_Variance: num  93.8 52.9 14.4 44.4 10.4 ...
 # $ Patch_Isometry     : num  1.56 1.76 1.81 1.64 1.61 ...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Expansion Parameters:

new_patch_matrix <- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions/Patch_Tables/New_TableFGC2D.csv")

str(new_patch_matrix)
# 'data.frame':	450 obs. of  7 variables:
 # $ Year.              : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Region.            : int  1 1 1 2 2 2 3 3 3 4 ...
 # $ From.              : int  42 52 62 42 52 62 42 52 62 42 ...
 # $ To.                : int  2 2 2 2 2 2 2 2 2 2 ...
 # $ Mean_Patch_Size    : num  0.836 1.206 0.63 1.913 1.376 ...
 # $ Patch_Size_Variance: num  9.556 10.432 0.607 2.554 5.086 ...
 # $ Patch_Isometry     : num  1.16 1.28 1.36 1.73 1.61 ...

 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # Percent of Transition by Expansion:

exp_percent_matrix <- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions/Patch_Tables/Percent_TableFGC2D.csv")

str(exp_percent_matrix)
# 'data.frame':	450 obs. of  5 variables:
 # $ Year.  : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Region.: int  1 1 1 2 2 2 3 3 3 4 ...
 # $ From.  : int  42 52 62 42 52 62 42 52 62 42 ...
 # $ To.    : int  2 2 2 2 2 2 2 2 2 2 ...
 # $ Percent: num  0.673 0.809 0.474 0.619 0.65 ...
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Patch Stats Including Buffer (comes from R script TransitionRates.r)
 sabuff_patch<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sabuff_patch.csv" )

# Patch Stats FINAL TABLE Study Area Only (**THIS IS THE ONE YOU READ IN**)
 sa_patch<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sa_patch.csv" )

 
}

# ----------------------------------------------
{# WEIGHTS OF EVIDENCE (recalculated for significance and protected areas) *VERSION SPECIFIC*
# ----------------------------------------------
 
# Read all WOE files in (these are the final recalculated, significant ones)
sav1031WOE_LOC<-"Y:/Lacher/Dinamica_Runs/StudyArea_V1031/ModelStats/WOE_recalc/LPA/"
sav1031WOE_files <- list.files(sav1031WOE_LOC,pattern = "csv$")
sav1031WOE<-lapply(paste0(sav1031WOE_LOC, sav1031WOE_files), read.csv)
sav1031WOE_nms<-gsub(".csv", "",sav1031WOE_files)
names(sav1031WOE)<-sav1031WOE_nms

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WOE Including Buffer (comes from R script TransitionRates.r) #Figure out naming convention here.
sav1031sabuffWOE<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sav1031sabuffWOE.csv" )

# WOE Compiled Table. Study Area WITH buffer.
v104sa_WOE<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sav104sabuffWOE.csv")

# WOE FINAL TABLE Study Area Only (**THIS IS THE ONE YOU READ IN**)
sav1031sa_WOE<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/sav1031sa_WOE.csv" )
 
 
}


################################################
################################################
# RASTERS 
################################################
################################################

# files<-list.files(path="path/path/", pattern="uniquepattern+.*filetype")
# files2<-paste("path/path/", files, sep="")
# filename<-mapply(brick, files2)

# ----------------------------------------------
{# INPUT *RAW* RASTERS FOR STUDY AREA ####
# ----------------------------------------------

list.files(path="Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/", pattern=".img$", full.names=FALSE)

{# 72 files on 6-21-16. ID which to move to "old" folder

 [1] "agw_pct_00_an.img" "agw_pct_11_an.img" "cblcd_01_an.img"   "cblcd_11_an.img"  
 [5] "cblcd_92_an.img"   "cnty_an.img"       "dist_dev.img"      "dist_dev_open.img"      
 [9] "dist2dev_NLCD_an.img"    "dist2water_NLCD__an.img" "ed_pct_00_an.img"  "ed_pct_11_an.img" 
[13] "elev_an.img"       "GAP_01_an.img"     "GAP_11_an.img"     "Geol_an.img"      
[17] "h_den_00_an.img"   "h_den_11_an.img"   "h_value_00_an.img" "h_value_11_an.img"      
[21] "hc_an.img"   "med_inc_00_an.img" "med_inc_11_an.img" "mine_an.img"      
[25] "nl01CNP_an.img"    "nl01CNP_an_2.img"  "nl11CNP_an.img"    "nl11CNP_an_2.img" 
[29] "nl2001_an.img"     "nl2011_an.img"     "nlcd01_anC.img"    "nlcd01rawrcb1.img"      
[33] "nlcd11_anC.img"    "padus_gp01.img"    "padus_gp11.img"    "padus_pp01.img"   
[37] "padus_pp11.img"    "pc_inc_00_an.img"  "pc_inc_11_an.img"  "pi_an.img"  
[41] "pl_dist_an.img"    "pl_type_draft_an.img"    "pl01_dist.img"     "pl01_gap.img"     
[45] "pl01_pp.img"       "pl11_dist.img"     "pl11_gap.img"      "pl11_pp.img"      
[49] "pldist_01_an.img"  "pldist_11_an.img"  "poc_pct_00_an.img" "poc_pct_11_an.img"      
[53] "pop_den_00_an.img" "pop_den_11_an.img" "pov_pct_00_an.img" "pov_pct_11_an.img"      
[57] "ppt_an.img"  "pubpriv01_an.img"  "pubpriv11_an.img"  "pwr_plant.img"    
[61] "slope_an.img"      "tmax_an.img"       "tmin_an.img"       "tprcl_cnt.img"    
[65] "tprcl_max.img"     "tprcl_mean.img"    "tprcl_min.img"     "tprcl_sd.img"     
[69] "travelt_01_an.img" "travelt_11_an.img" "zone_an.img"       "zone2_an.img"

} 

# ***New additions:***
# "dist_dev.img"
# "dist_dev_open.img"
# "pl01_gap.img"
# "pl11_gap.img"
# "pl01_pp.img"
# "pl11_pp.img"
# "pl11_dist.img"
# "pl01_dist.img"

# same name, but new file:
# "nl01CNP_an_2.img"
# "nl11CNP_an_2.img" 


# Set location for the input study area rasters
inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server

# inRasterLoc<-"V:/IaraSpatialLayers/PreparedModRas/Full/" #V drive

### RASTERS ###

# ADMIN
regions <-raster(paste(inRasterLoc, "cnty_an", ".img", sep="")) # this is for the raster. The workd "counties" in other script refers to the .csv GEOID file or the individual counties. 

# Individual COUNTIES
#(this is how it is written in other script that requires loop)

County_Folder <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/IndCntys" 
counties <- list.files(County_Folder,pattern = ".img$") 

 for(c in 1:length(counties)){
county <- raster(paste(County_Folder,"/",counties[c],sep = ""))
}
 
 {# List of 56 individual counties
 # [1] "s_11001.img" "s_24001.img" "s_24017.img" "s_24021.img" "s_24031.img" "s_24043.img" "s_51003.img" "s_51009.img" "s_51011.img"
# [10] "s_51013.img" "s_51015.img" "s_51017.img" "s_51029.img" "s_51043.img" "s_51047.img" "s_51049.img" "s_51059.img" "s_51061.img"
# [19] "s_51065.img" "s_51069.img" "s_51075.img" "s_51079.img" "s_51091.img" "s_51107.img" "s_51109.img" "s_51113.img" "s_51125.img"
# [28] "s_51137.img" "s_51139.img" "s_51153.img" "s_51157.img" "s_51163.img" "s_51165.img" "s_51171.img" "s_51177.img" "s_51179.img"
# [37] "s_51187.img" "s_51530.img" "s_51540.img" "s_51600.img" "s_51610.img" "s_51630.img" "s_51660.img" "s_51678.img" "s_51683.img"
# [46] "s_51685.img" "s_51790.img" "s_51820.img" "s_51840.img" "s_54003.img" "s_54023.img" "s_54027.img" "s_54031.img" "s_54037.img"
# [55] "s_54065.img" "s_54071.img"
}


# ----------------------------------------------
# LAND COVER
# ----------------------------------------------

# NLCD RCc LAND COVER - *Includes BUFFER
nlcd01rc <- raster(paste(inRasterLoc, "nlcd01_anC", ".img", sep="")) 
nlcd11rc <- raster(paste(inRasterLoc, "nlcd11_anC", ".img", sep="")) 
nlcd_rcs <-list(nlcd01rc,nlcd11rc)

# Rename to nlcd_11 (for Var WOE Raster creation)
nlcd_11<-nlcd11rc

# # NLCD RCc WITHOUT Protected Lands
nlcd01rcNP <- raster(paste(inRasterLoc, "nl01CNP_an_2", ".img", sep=""))
nlcd11rcNP <- raster(paste(inRasterLoc, "nl11CNP_an_2", ".img", sep=""))

# ----------------------------------------------
# DISTANCE to landcover class
# ----------------------------------------------
# Development (nlcd 2011) *Need this.
# distance_to_2
# distance_to_3
distance_to_2<-raster(paste(inRasterLoc, "dist_dev_open", ".img", sep=""))
distance_to_3<-raster(paste(inRasterLoc, "dist_dev", ".img", sep=""))

# ----------------------------------------------
# PHYSIOGRAPHY
# ----------------------------------------------
slope <- raster(paste(inRasterLoc, "slope_an", ".img", sep="")) 
soils <- raster(paste(inRasterLoc, "pi_an", ".img", sep="")) 
geol <- raster(paste(inRasterLoc, "Geol_an", ".img", sep="")) #V105
ppt <- raster(paste(inRasterLoc, "ppt_an", ".img", sep="")) 
tmax <- raster(paste(inRasterLoc, "tmax_an", ".img", sep="")) 
tmin <- raster(paste(inRasterLoc, "tmin_an", ".img", sep="")) 

# ----------------------------------------------
# ENERGY
# ----------------------------------------------
mine <- raster(paste(inRasterLoc, "mine_an", ".img", sep="")) #V105
pwrpl  <- raster(paste(inRasterLoc, "pwr_plant", ".img", sep="")) #V105 



# ----------------------------------------------
# DEVELOPMENT
# ----------------------------------------------

zoning <- raster(paste(inRasterLoc, "zone2_an", ".img", sep="")) #V105
travelt_01 <- raster(paste(inRasterLoc, "travelt_01_an"      , ".img", sep=""))
travelt_11 <- raster(paste(inRasterLoc, "travelt_11_an"      , ".img", sep=""))
popden_00<- raster(paste(inRasterLoc, "pop_den_00_an"      , ".img", sep="")) 
popden_11<- raster(paste(inRasterLoc, "pop_den_11_an"      , ".img", sep=""))
hden_00 <- raster(paste(inRasterLoc, "h_den_00_an"        , ".img", sep="")) 
hden_11 <- raster(paste(inRasterLoc, "h_den_11_an"        , ".img", sep=""))  
  
# ---------------------------------------------- 
#  ECOLOGY
# ----------------------------------------------


pl01_gap <- raster(paste0(inRasterLoc,"gap01.img"))
pl11_gap <- raster(paste0(inRasterLoc,"gap11.img"))

# check the below prot areas rasters
gap01 <- raster(paste(inRasterLoc, "GAP_01_an", ".img", sep=""))
gap11 <- raster(paste(inRasterLoc, "GAP_11_an", ".img", sep="")) 
prot_dist01 <- raster(paste(inRasterLoc, "pldist_01_an", ".img", sep=""))
prot_dist11 <- raster(paste(inRasterLoc, "pldist_11_an", ".img", sep=""))
pubpriv01 <- raster(paste(inRasterLoc, "pubpriv01_an"       , ".img", sep="")) 
pubpriv11 <- raster(paste(inRasterLoc, "pubpriv11_an"       , ".img", sep="")) 
habcore <- raster(paste(inRasterLoc, "hc_an", ".img", sep="")) 

 
# ---------------------------------------------- 
#  ECONOMIC
# ----------------------------------------------

pcinc_00 <- raster(paste(inRasterLoc, "pc_inc_00_an"       , ".img", sep="")) 
pcinc_11 <- raster(paste(inRasterLoc, "pc_inc_11_an"       , ".img", sep="")) 
medinc_00 <- raster(paste(inRasterLoc, "med_inc_00_an", ".img", sep="")) 
medinc_11 <- raster(paste(inRasterLoc, "med_inc_11_an", ".img", sep="")) 
agprof_00 <- raster(paste(inRasterLoc, "agw_pct_00_an"      , ".img", sep="")) 
agprof_11 <- raster(paste(inRasterLoc, "agw_pct_11_an"      , ".img", sep="")) 
pov_00 <- raster(paste(inRasterLoc, "pov_pct_00_an"      , ".img", sep="")) 
pov_11 <- raster(paste(inRasterLoc, "pov_pct_11_an"      , ".img", sep="")) 
 
# hval_00 <- raster(paste(inRasterLoc, "h_value_00_an"      , ".img", sep="")) 
# hval_11 <- raster(paste(inRasterLoc, "h_value_11_an"      , ".img", sep="")) 


# ---------------------------------------------- 
#  SOCIAL
# ----------------------------------------------
edAA_00 <- raster(paste(inRasterLoc, "ed_pct_00_an"       , ".img", sep="")) 
edAA_11 <- raster(paste(inRasterLoc, "ed_pct_11_an"       , ".img", sep="")) 
poc_00 <- raster(paste(inRasterLoc, "poc_pct_00_an"      , ".img", sep="")) 
poc_11 <- raster(paste(inRasterLoc, "poc_pct_11_an"      , ".img", sep="")) 
  
 
# ----------------------------------------------
# ----------------------------------------------
# Stack rasters
# anstk<-stack(region, ... )

# # pairs plots for raster layers
# pairs(anstk, hist=TRUE, cor=TRUE)

# #correlation
# layerStats(anstk, 'pearson')
}


{ # !!!! DEFUNCT DATA !!!!


# ----------------------------------------------
# LAND COVER
# ----------------------------------------------

# nlcd01rcNP <- raster(paste(inRasterLoc, "nl01CNP_an", ".img", sep="")) 
# nlcd11rcNP <- raster(paste(inRasterLoc, "nl11CNP_an", ".img", sep="")) 
# nlcd_rcsNP <-list(nlcd01rcNP,nlcd11rcNP)

# # NLCD RCa LAND COVER - *Includes BUFFER
# nlcd_01 <- raster(paste(inRasterLoc, "nl2001_an", ".img", sep="")) 
# nlcd_11 <- raster(paste(inRasterLoc, "nl2011_an", ".img", sep="")) 
# nlcd_s<-list(nlcd_01,nlcd_11)

# # NLCD RCb WITH Protected Lands
# # NLCD RCb LAND COVER - *Includes BUFFER
# nlcd01rb<-raster(paste(inRasterLoc, "nlcd01rawrcb1", ".img", sep=""))
# nlcd11rb<-raster(paste(inRasterLoc, "nlcd11rawrcb1", ".img", sep=""))
# nlcd_rbs<-list(nlcd01rb,nlcd11rb)

# # CBP Original RC (RCb) WITH Protected Lands
# # CBP LAND COVER - *Includes BUFFER
# cblcd_92 <- raster(paste(inRasterLoc, "cblcd_92_an", ".img", sep=""))
# cblcd_01 <- raster(paste(inRasterLoc, "cblcd_01_an", ".img", sep=""))
# cblcd_11 <- raster(paste(inRasterLoc, "cblcd_11_an", ".img", sep=""))
# cblcd_s<-list(cblcd_92,cblcd_01,cblcd_11)

# # NLCD RCb WITHOUT Protected Lands
# # NLCD RCb LAND COVER - *Includes BUFFER
# nlcd01rbNP<-raster(paste(inRasterLoc, "nlcd01r_anNP", ".img", sep=""))
# nlcd11rbNP<-raster(paste(inRasterLoc, "nlcd11r_anNP", ".img", sep=""))
# nlcd_rbNPs<-list(nlcd01rbNP,nlcd11rbNP)

# # CBP RCb WITHOUT Protected Lands
# # CBP LAND COVER - *Includes BUFFER
# cblcd_92NP <- raster(paste(inRasterLoc, "cblcd92_anNP", ".img", sep=""))
# cblcd_01NP <- raster(paste(inRasterLoc, "cblcd01_anNP", ".img", sep=""))
# cblcd_11NP <- raster(paste(inRasterLoc, "cblcd11_anNP", ".img", sep=""))
# cblcd_NPs<-list(cblcd_92NP,cblcd_01NP,cblcd_11NP)

# ----------------------------------------------
# DISTANCE to landcover class
# ----------------------------------------------
# dist_wat <- raster(paste(inRasterLoc, "dist2water_an", ".img", sep=""))
# distance_to_2 <- raster(paste(inRasterLoc, "dist2dev_NLCD_an"   , ".img", sep="")) 
# ### <- raster(paste(inRasterLoc, "dist2water_NLCD__an", ".img", sep=""))

# ----------------------------------------------
# PHYSIOGRAPHY
# ----------------------------------------------
# elev <- raster(paste(inRasterLoc, "elev_an", ".img", sep="")) #V103
# karst <- raster(paste(inRasterLoc, "ka_an", ".img", sep="")) 

# ----------------------------------------------
# ENERGY
# ----------------------------------------------

# ----------------------------------------------
# DEVELOPMENT
# ----------------------------------------------
# zoning <- raster(paste(inRasterLoc, "zone_an", ".img", sep="")) #V104
# road_01 <- raster(paste(inRasterLoc, "road01_an", ".img", sep="")) 
# road_11 <- raster(paste(inRasterLoc, "road11_an", ".img", sep="")) 
# hot_01 <- raster(paste(inRasterLoc, "hot01_an", ".img", sep="")) # different extent. need to fix.
# hot_11 <- raster(paste(inRasterLoc, "hot11_an", ".img", sep="")) # different extent. need to fix.
# vuln <- raster(paste(inRasterLoc, "vuln_an", ".img", sep="")) 
# hdens_00 <- raster(paste(inRasterLoc, "h_dens00_an", ".img", sep="")) 
# hdens_10 <- raster(paste(inRasterLoc, "h_dens10_an", ".img", sep=""))
# ###<- raster(paste(inRasterLoc, "tprcl_cnt"          , ".img", sep="")) 
# ###<- raster(paste(inRasterLoc, "tprcl_max"          , ".img", sep="")) 
# ###<- raster(paste(inRasterLoc, "tprcl_mean"         , ".img", sep="")) 
# ###<- raster(paste(inRasterLoc, "tprcl_min"          , ".img", sep="")) 
# ###<- raster(paste(inRasterLoc, "tprcl_sd"           , ".img", sep=""))
# pop_den00 <- raster(paste(inRasterLoc, "pop_den00_an", ".img", sep="")) 
# pop_den11 <- raster(paste(inRasterLoc, "pop_den11_an", ".img", sep="")) 
    
# ---------------------------------------------- 
#  ECOLOGY
# ----------------------------------------------
# ### <- raster(paste(inRasterLoc, "padus_gp01"         , ".img", sep="")) 
# ### <- raster(paste(inRasterLoc, "padus_gp11"         , ".img", sep="")) 
# ### <- raster(paste(inRasterLoc, "padus_pp01"         , ".img", sep="")) 
# ### <- raster(paste(inRasterLoc, "padus_pp11"         , ".img", sep=""))
# prot_type <- raster(paste(inRasterLoc, "pl_an_nodata_an", ".img", sep="")) 
# prot_dist <- raster(paste(inRasterLoc, "pl_dist_an", ".img", sep="")) # V104
# prot_type <- raster(paste(inRasterLoc, "pl_type_draft_an", ".img", sep="")) # V104

 
# ---------------------------------------------- 
#  ECONOMIC
# ----------------------------------------------

# ---------------------------------------------- 
#  SOCIAL
# ----------------------------------------------
  
 
}



# ----------------------------------------------
# ----------------------------------------------
{# DINAMICA OUTPUT RASTERS FOR Study Area
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# OUTPUT: Landscapes ####
# ----------------------------------------------
# ----------------------------------------------
# PROJECTED LANDSCAPES

# Use if have ONE VERSION AND ONLY LANDSCAPE MAPS ONE FOLDER:
L_files<-list.files(path="Y:/Lacher/Dinamica_Runs/StudyArea_V104/Prob_Maps/", pattern=".img$")
L_files2<-paste("Y:/Lacher/Dinamica_Runs/StudyArea_V104/Prob_Maps/", L_files, sep="")
v104_Landscapes<-mapply(brick, L_files2)
L_names<-gsub(".img", "",L_files)
names(v104_Landscapes)<-L_names


# MODELED LANDSCAPES
# Landscape names
LS_names<-lapply(Versions, paste0, paste0("_Landscape", timesteps))#makes a list of 4, 10 items each

# Input path for each landscape for each version
path<-paste0("Y:/Lacher/Dinamica_Runs/Study Area_", toupper(Versions), "/Prob_Maps/")
#~
files<-list()
v_t<-1
for(v in 1:length(Versions)){
	print(paste(v_t, ':'))
	files[[v_t]]<-paste0(path[v], LS_names[[v]], ".img")#list of 4, one per version, with each of 10 landscapes 
	v_t<-v_t+1
}
names(files)<-Versions
#~

# Landscapes @ ea. version & year
Landscapes<-list()
r_t<-1
# Read rasters
for(r in 1:length(Versions)){
	print(paste(r_t, ':'))
	Landscapes[[r_t]]<-lapply(files[[r]], raster)
	names(Landscapes[[r_t]])<-LS_names[[r]] # list of 4 with 10 items each.
	r_t<-r_t+1
}
names(Landscapes)<-names(Versions)
# List of 4
 # $ v1  :List of 10
  # ..$ v1_Landscape01:Formal class 'RasterLayer'

# stack landscapes for each version
L_ver_s<-lapply(Landscapes, stack) #gives 4 stacks, one per Version



# VERSION 1
v1files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V1/Prob_Maps/",pattern = "v1_Landscape+.*img$")
v1files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V1/Prob_Maps/", v1files, sep="")
v1<-mapply(brick, v1files2)
v1f_names<-gsub(".img", "",v1files)
names(v1)<-v1f_names    

# VERSION 3
v3files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V3/Prob_Maps/",pattern = "v3_Landscape+.*img$")
v3files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V3/Prob_Maps/", v3files, sep="")
v3<-mapply(brick, v3files2)
v3f_names<-gsub(".img", "",v3files)
names(v3)<-v3f_names 

# VERSION 5
v5files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V5/Prob_Maps/",pattern = "v5_Landscape+.*img$")
v5files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V5/Prob_Maps/", v5files, sep="")
v5<-mapply(brick, v5files2)
v5f_names<-gsub(".img", "",v5files)
names(v5)<-v5f_names 

# VERSION 100
v100files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V100/Prob_Maps/",pattern = "v100_Landscape+.*img$")
v100files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V100/Prob_Maps/", v100files, sep="")
v100<-mapply(brick, v100files2)
v100f_names<-gsub(".img", "",v100files)
names(v100)<-v100f_names 


# ----------------------------------------------
# OUTPUT: Probabilities (by region) ####
# ----------------------------------------------

# PROBABILITY MAPS
# path<-lapply("Y:/Lacher/Dinamica_Runs/Study Area_", paste0, toupper(Versions), "/Prob_Maps/")


# VERSION 1
pv1files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V1/Prob_Maps/",pattern = "v1_Prob+.*img$")
pv1files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V1/Prob_Maps/", pv1files, sep="")
pv1<-mapply(brick, pv1files2)
pv1f_names<-gsub(".img", "",pv1files)
names(pv1)<-v1f_names    

# VERSION 3
pv3files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V3/Prob_Maps/",pattern = "v3_Prob+.*img$")
pv3files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V3/Prob_Maps/", pv3files, sep="")
pv3<-mapply(brick, pv3files2)
pv3f_names<-gsub(".img", "",pv3files)
names(pv3)<-v3f_names 

# VERSION 5
pv5files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V5/Prob_Maps/",pattern = "v5_Prob+.*img$")
pv5files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V5/Prob_Maps/", pv5files, sep="")
pv5<-mapply(brick, pv5files2)
pv5f_names<-gsub(".img", "",pv5files)
names(pv5)<-v5f_names 

# VERSION 100
pv100files<- list.files(path="Y:/Lacher/Dinamica_Runs/Study Area_V100/Prob_Maps/",pattern = "v100_Prob+.*img$")
pv100files2<-paste("Y:/Lacher/Dinamica_Runs/Study Area_V100/Prob_Maps/", pv100files, sep="")
pv100<-mapply(brick, pv100files2)
pv100f_names<-gsub(".img", "",pv100files)
names(pv100)<-v100f_names 


}
