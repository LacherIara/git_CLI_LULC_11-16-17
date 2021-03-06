############################ 
#PURPOSE: A) Extract patch stats for both Expansion Patches and New Patches B) To extract 3 .csv files for Expansion Patch Parameters, New Patch Parameters, and % Expansion from the New/Expanded Patch Stats.
#INPUT: 	# 1a) Initial Landscape - Filename/path for the initial landscape raster
			# 2a) Timestep1 - The Filename/path for Timestep 1 raster
			# 3a) County_Folder - Folder path that contains rasters of all counties (rasters should be in .img format)
			# 4a) ExpNew_Folder - Desired folder path/name for the output files
			# 1b) Expansion Patch Stats File, New Patch Stats File
			# 2b) County ID Crosswalk
#OUTPUT: 	2 .csv files (1 for Expansion, 1 for New)
#Version: 5/30/2017 by Iara Lacher & Craig Fergus
#CONTACT: 	Luca Morreale(lmorreale@fas.harvard.edu), Iara Lacher (LacherI@si.edu)
#NOTES:		Adjacent Patch = Expansion patch, Non-adjacent = new


# ----------------------------------------------
################################################


# PACKAGES NEEDED

library(raster) # read and edit rasters
library(SDMTools) # Species Distribution Modelling Tools
library(Hmisc)  # useful functions for data analysis
library(rgdal)  # Add this package to read .tif files
library(igraph) # Run function clump().
library(foreach) # provides looping functions for parallelization
library(doSNOW) # Foreach Parallel Adaptor for "snow" package, for parallelization


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations, county rasters and landscapes 
# --------------------------------------------------------
# --------------------------------------------------------


# ----------------------------------------------
# FILE LOCATIONS
# ----------------------------------------------

# Set location for the input study area rasters
inRasterLoc <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/"

# Set the paths of the expansion/new stats (path for output of part A and input of part B)
# ExpNew_Folder <- "Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Patch_Stats/"
# ExpNew_Folder <- "C:/Users/LacherL/Desktop/RTEST"
ExpNew_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Stats/"# SCBI V:

## Output path and file names for part B 
expansion_file <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_Exp_Table_FGC-DFGC.csv" 
new_patch_file <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_New_Table_FGC-DFGC.csv"
exp_percent_file <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_Percent_Table_FGC-DFGC.csv"


# ----------------------------------------------
# COUNTIES
# ----------------------------------------------

# ----------------------------------------------
# Region
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/SMSC_GEOID.csv")
colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)

# ----------------------------------------------
# Individual Counties
County_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/IndCntys" 
counties <- list.files(County_Folder,pattern = ".img$")


# ----------------------------------------------
# Landscapes
# ----------------------------------------------

Initial_Landscape <- raster(paste(inRasterLoc, "nlcd01c_smsc.img", sep="")) 
Final_Landscape <- raster(paste(inRasterLoc, "nlcd11c_smsc.img", sep="")) 

Change_Raster<- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Rasters/SMSC_nlCcomb_0111.bil") 

#QC:
# Initial_Landscape; Final_Landscape; Change_Raster


# ----------------------------------------------
# ----------------------------------------------
# PART A - Parallelized 
# ----------------------------------------------
# ----------------------------------------------


# ----------------------------------------------
# TIMING SCRIPT
old <- Sys.time()



# ----------------------------------------------
# Prepare transitions
# ----------------------------------------------

PersistentLU <- c(33,55,66,77)
TransitionNames <- list("3" = c("53","63","73"), "5" = c("65","75"), "6" = c("56","76"), "7" = c("57","67"))
LandUseConversions <- list("3" = c(53,63,73), "5" = c(65,75), "6" = c(56,76), "7" = c(57,67))


# ----------------------------------------------
# Loop through PersistentLU using parallelisation
# ----------------------------------------------
# ----------------------------------------------
# Run the loop (Make sure you set the temp raster folder *)

LU_Chg_Raster <- Change_Raster


### TEST ###
# counties <- counties[7]
# test_cty <- raster(paste(County_Folder, "/", counties[7], sep = ""))

# PersistentLU <- c(33)
# LandUseConversions <- list("3" = c(53,63,73))

# ----------------------------------------------
# prepare clustering for parallelisation
cl <- makeCluster(length(PersistentLU)) 
registerDoSNOW(cl)  
# #stopCluster(cl)


# for(landuse in 1:length(PersistentLU)){ # if not using clustering # remove ?
foreach (landuse =  1:length(PersistentLU), .packages = c('raster','SDMTools','Hmisc')) %dopar% {
  
  rasterOptions(tmpdir = "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/temp") 
  print(landuse)
  print(names(LandUseConversions[landuse]))
  Expansion_PatchStats <- matrix(nrow = length(counties), ncol = 7)
  New_PatchStats <- matrix(nrow = length(counties), ncol = 7)
  colnames(Expansion_PatchStats) <- c("County","Number of Patches", "MN Patch Area(ha)", "Variance Patch Area(ha)", "MN Perm Area Rat(m/m2)",  "MN Shape Index","Perc Expansion Area")
  colnames(New_PatchStats) <- c("County","Number of Patches", "MN Patch Area(ha)", "Variance Patch Area(ha)", "MN Perm Area Rat(m/m2)",  "MN Shape Index","Perc New Area")
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # finds cells in the lu change raster that are adjacent to landuse that didn't change. for expansion stats.
  adj_vals <- getValues(LU_Chg_Raster)
  Adjacent_Cells <- adjacent(LU_Chg_Raster, which(adj_vals == PersistentLU[landuse]),directions = 8, pairs = F) 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  # make values from lu raster (adj_vals) that are adjacent =1 and those that are not adjacent =0
  adj_vals[Adjacent_Cells] <- 1 ;  adj_vals[!Adjacent_Cells] <- NA
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # create a new raster that sets the LU change raster values to 1 or 0 depending on if they are adjacent to persistent lu or not.
  adj_raster <- setValues(LU_Chg_Raster,adj_vals) 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # Loop through landuse transitions and couties to make 3 tables with all the counties in it. 
 
  for(transition in 1:length(LandUseConversions[[landuse]])){
    print(paste("Processing Transition:",LandUseConversions[[landuse]][transition])) 
    
    for(c in 1:length(counties)){ 
      
      temp <- strsplit(counties[c], "_")[[1]][c(1,2)] 
      
      County_Name <- strsplit(temp[2], "\\.")[[1]][1]
      print(County_Name)
      
      county <- raster(paste(County_Folder, "/", counties[c], sep = ""))
      
      cty_vals <- getValues(county) 
      
      change_county <- raster::mask(LU_Chg_Raster, county )
      
      ch_ctyvals <- getValues(change_county)
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      # mask the binary raster of adjacent cells by county. pick out all the  cells with value of 1.
      adj_county <- raster::mask(adj_raster, county)
      
      adj_cty_vals <- getValues(adj_county)
      adj_cty_vals <- which(adj_cty_vals == 1) 
      
      boolean_vals <- ifelse(ch_ctyvals == LandUseConversions[[landuse]][transition]|is.na(ch_ctyvals), ch_ctyvals, 0) # If trans values in the cty = the transition in question or if trans vals in cty mask are NA, assign the trans values for cty vals mask, other wise = 0. This gives a raster that only represents NA and transition areas.
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
      Change_Masked <- setValues(change_county, boolean_vals) # create a new raster that sets the county change raster values to the value of the desired transition and NA, excluding other transitions or persistent landuses. 
      
      print(paste("masked raster created:",LandUseConversions[[landuse]][transition])) 
      
      if(length(unique(boolean_vals)) > 2){
        
        try(cty_clump <- clump(Change_Masked, directions = 8)) # detect clumps and gives unique ID for each of them
        try(cty_pstat <- PatchStat(cty_clump, cellsize = 30)) # calculate patch statistics
        try(adj_patches <- extract(cty_clump, Adjacent_Cells)) # extract the clump ID for the previously identified adjacent cells 
        #** Compare to: try(adj_patches <- extract(cty_clump,adj_cty_vals)) in Luca's script. # remove ?
        try(adj_Stats <- subset(cty_pstat, cty_pstat[,1]%in% unique(adj_patches))) # extract the stats of clumps that contain adjacent cells
        try(new_Stats <- subset(cty_pstat, cty_pstat[,1]%nin% adj_Stats[,1])) # extract the stats of clumps that don't contain adjacent cells --> does that makes sense to you?
      }else{
        adj_Stats <- data.frame(area = as.numeric(), perim.area.ratio = as.numeric())
        new_Stats <- data.frame(area = as.numeric(), perim.area.ratio = as.numeric())
      }
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
      #creating the desired output matrix, adds each county as a row, in 2 different matrices
      
      Expansion_PatchStats[c,1] <- County_Name
      Expansion_PatchStats[c,2] <- nrow(adj_Stats)
      Expansion_PatchStats[c,3] <- mean(adj_Stats$area / 10000, na.rm =T) 
      Expansion_PatchStats[c,4] <- var(adj_Stats$area /10000, na.rm = T) 
      Expansion_PatchStats[c,5] <- mean(adj_Stats$perim.area.ratio, na.rm = T)
      Expansion_PatchStats[c,6] <- mean(adj_Stats$shape.index, na.rm = T)
      
      New_PatchStats[c,1] <- County_Name
      New_PatchStats[c,2] <- nrow(new_Stats)
      New_PatchStats[c,3] <- mean(new_Stats$area / 10000, na.rm =T) 
      New_PatchStats[c,4] <- var(new_Stats$area / 10000, na.rm = T)
      New_PatchStats[c,5] <- mean(new_Stats$perim.area.ratio, na.rm = T)
      New_PatchStats[c,6] <- mean(new_Stats$shape.index, na.rm = T)
    
      
      #Change the below to divide by column 3 instead of 2 for future versions. This will calculate area ratio. # remove ?
      Expansion_PatchStats[c,7] <- as.numeric(Expansion_PatchStats[c,3])/(as.numeric(Expansion_PatchStats[c,3])+as.numeric(New_PatchStats[c,3]))
      New_PatchStats[c,7] <- as.numeric(New_PatchStats[c,3])/(as.numeric(Expansion_PatchStats[c,3])+as.numeric(New_PatchStats[c,3]))
      
      print(paste("patch table created:",LandUseConversions[transition])) 
      
    }
    
  
    write.csv(Expansion_PatchStats,paste(ExpNew_Folder,"/sa_Expan_FGC-DFGC",LandUseConversions[[landuse]][transition],".csv",sep=""), row.names = F)
    
    write.csv(New_PatchStats,paste(ExpNew_Folder,"/sa_New_FGC-DFGC",LandUseConversions[[landuse]][transition],".csv",sep=""), row.names = F)
    
    #note - they do have "NA" and "NaN" in the cells still. # remove ?
  }
  
  removeTmpFiles()
}


# ----------------------------------------------
# TIMING SCRIPT
new<-Sys.time()-old
print(new)
# ran in ~26 hours
stopCluster(cl)







# ----------------------------------------------
# ----------------------------------------------
# PART B
# ----------------------------------------------
# ----------------------------------------------

# ----------------------------------------------
# Prepare transitions
# ----------------------------------------------

LandUseConversions <- list("3" = c(53,63,73), "5" = c(65,75), "6" = c(56,76), "7" = c(57,67))


# ----------------------------------------------
# Read all Patch Stats files in 
# ----------------------------------------------

# ----------------------------------------------
# Expan
sav107Exp_files <- list.files(ExpNew_Folder,pattern = ".*Expan_FGC-DFGC.*\\csv$") # check isn't it ".*Exp_FGC-DDFGC.*\\csv$" ???
sav107Exp<-lapply(paste0(ExpNew_Folder, sav107Exp_files), read.csv)
# str(sav107Exp) = List of 9 data frames

# ----------------------------------------------
# New
sav107New_files <- list.files(ExpNew_Folder,pattern = ".*New_FGC-DFGC.*\\csv$")
sav107New<-lapply(paste0(ExpNew_Folder, sav107New_files), read.csv)
# str(sav107New) = List of 9 data frames

# ----------------------------------------------
# Clean up matrix by removing na and nan

sav107Exp <- lapply(sav107Exp, function(d) { d[is.na(d)] <- 0.00001; d })
sav107New <- lapply(sav107New, function(d) { d[is.na(d)] <- 0.00001; d })


# ----------------------------------------------
# Prepare elements for loop
# ----------------------------------------------

options(scipen = 999) #sets amount of decimals viewed

years <- c(1:5)   ### Years Desired

counties <- sa_ctyGEOID$Din_cty # is this the same as line 117 ?

# ----------------------------------------------
# Create function to merge csv files and add sa_ctyGEOID at same time
add.geoid <- function(Landcover){
  merge(Landcover,sa_ctyGEOID, by.x = "County",by.y = "GEOID")
}


# ----------------------------------------------
# Run add.geoid function
expan <- lapply(sav107Exp, add.geoid)
newpat <- lapply(sav107New, add.geoid)


# ----------------------------------------------
# Prepare matrix
expansion_matrix <- matrix(nrow = length(years)*length(counties)*length(unlist(LandUseConversions)),ncol=7)
new_patch_matrix <- matrix(nrow = length(years)*length(counties)*length(unlist(LandUseConversions)),ncol=7)
exp_percent_matrix <- matrix(nrow = length(years)*length(counties)*length(unlist(LandUseConversions)),ncol=5)


# ----------------------------------------------
# Loop through years and counties to fill matrix
# ----------------------------------------------

# for(landuse in 1:length(unlist(LandUseConversions))){ # remove ?

row <- 1
for(year in years){
  print(year)
  for(county in counties){
    print(county)
    for(landuse in 1:length(unlist(LandUseConversions))){
    county2 <- which(expan[[landuse]]["Din_cty"] == county) #first written file, second, etc. 
    expansion_matrix[row,1] <- year
    expansion_matrix[row,2] <- county
   	expansion_matrix[row,3] <- substring(as.character(unlist(LandUseConversions)[landuse]), 1,1)
    expansion_matrix[row,4] <- substring(as.character(unlist(LandUseConversions)[landuse]), 2,2)
    expansion_matrix[row,5] <- expan[[landuse]][county2,"MN.Patch.Area.ha."]
    expansion_matrix[row,6] <- expan[[landuse]][county2,"Variance.Patch.Area.ha."]
    expansion_matrix[row,7] <- expan[[landuse]][county2,"MN.Shape.Index"]
	 
    county2 <- which(newpat[[landuse]]["Din_cty"] == county)
    new_patch_matrix[row,1] <- year
    new_patch_matrix[row,2] <- county
   	new_patch_matrix[row,3] <- substring(as.character(unlist(LandUseConversions)[landuse]), 1,1)
    new_patch_matrix[row,4] <- substring(as.character(unlist(LandUseConversions)[landuse]), 2,2)
    new_patch_matrix[row,5] <- newpat[[landuse]][county2,"MN.Patch.Area.ha."]
    new_patch_matrix[row,6] <- newpat[[landuse]][county2,"Variance.Patch.Area.ha."]
    new_patch_matrix[row,7] <- newpat[[landuse]][county2,"MN.Shape.Index"] 
        
    county2 <- which(expan[[landuse]]["Din_cty"] == county)
    exp_percent_matrix[row,1] <- year
    exp_percent_matrix[row,2] <- county
  	exp_percent_matrix[row,3] <- substring(as.character(unlist(LandUseConversions)[landuse]), 1,1)
    exp_percent_matrix[row,4] <- substring(as.character(unlist(LandUseConversions)[landuse]), 2,2)
    exp_percent_matrix[row,5] <- expan[[landuse]][county2,"Perc.Expansion.Area"]
	     
	 row <- row + 1
		}
	}
}

expansion_matrix<- as.data.frame(expansion_matrix)
new_patch_matrix<- as.data.frame(new_patch_matrix)
exp_percent_matrix<- as.data.frame(exp_percent_matrix)

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


# ----------------------------------------------
# Write
write.csv(expansion_matrix,file=expansion_file,row.names=FALSE, quote = FALSE)
write.csv(new_patch_matrix,file=new_patch_file,row.names=FALSE, quote = FALSE)
write.csv(exp_percent_matrix,file=exp_percent_file,row.names=FALSE, quote = FALSE)


# ----------------------------------------------
# Read
expansion_matrix <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_Exp_Table_FGC-DFGC.csv" )
new_patch_matrix <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_New_Table_FGC-DFGC.csv")
exp_percent_matrix<-read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Patch_Tables/smsc_Percent_Table_FGC-DFGC.csv")



