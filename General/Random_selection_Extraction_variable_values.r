
############################ 
#PURPOSE: Randomly selct points and extract the variables values for these points
#INPUT: 1 stack raster per variable (each layer of stack is a transition for whole region)
#OUTPUT: 
#DEVELOPED: (V1) 7/13/2016 Valentine Hermann
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 

# ----------------------------------------------
################################################

# PACKAGES NEEDED

# Raster editing and SDMs
library(raster)

# NEW TEMP DIRECTORY
# rasterOptions(tmpdir = "Y:/Lacher/rtempCLEARME/") # HF

# ----------------------------------------------
# READ OUTPUT FILES:

# file name:  / R label: 

# ----------------------------------------------
# READ INPUT FILES:

# Set location for the input study area rasters
inRasterLoc <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/" #HF server
# inRasterLoc <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/" #V Drive


# Individual Counties
County_Folder <- "Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/IndCntys" 
counties <- list.files(County_Folder,pattern = ".img$")


# Variable Rasters for V105 (saved Raster File Quick Reference. xlsx as .csv and making File.name column match the actual names of the layers)
RasterFileQuickReference <- read.csv("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/Raster File Quick Reference.csv", stringsAsFactors = F)

RasterFileQuickReference <- RasterFileQuickReference[RasterFileQuickReference$V105 %in% c("x", "X", "X-din"), c("File.name", "Model.Abbrev.")]# change *V105* to version needed + remove X-den if not needed + in csv file I put 'different extent" for pwr_plant because doesn't work when stack. need to be fixed, raster needs to have same extent.


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# Load in all rasters (1 for each variable)
for(i in 1:nrow(RasterFileQuickReference)){
  file.to.read <- RasterFileQuickReference[i, 1]
  objectname <- RasterFileQuickReference[i, 2]
  print(file.to.read)
  assign(objectname, raster(paste0(inRasterLoc, file.to.read)))
}

# loop through all variables and extract 5000 random values (always the same 10000 same cells because set.seed(1))

old<-Sys.time()
for (v in RasterFileQuickReference[, 2]){
  
  print(paste("----------------", v))
  
  variable_raster <- get(v)
  
  if(v == RasterFileQuickReference[1, 2]){
    set.seed(1)
    random_cells <- sample(c(1:ncell(variable_raster))[!is.na(getValues(variable_raster))], 10000)
    Random_cells_vals <- data.frame(cell = random_cells)
  }
  
  Random_cells_vals[, v] <- variable_raster[random_cells]
}
new <- Sys.time() - old
print(new)


# load county rasters and extract values from raster to fill in the county field

County_Folder <- "V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/IndCntys" 
counties <- list.files(County_Folder,pattern = ".img$")

for (c in counties){
  
  print(c)
  
  temp <- strsplit(c, "_")[[1]][c(1,2)] 
  County_Name <- strsplit(temp[2],"\\.")[[1]][1]
  
  county_raster <- raster(paste(County_Folder,"/",c,sep = ""))
  
  if(c == counties[1]){
    Random_cells_vals$county <- extract(county_raster, Random_cells_vals[,1])
  }else{
    Random_cells_vals$county <- ifelse(is.na(Random_cells_vals$county), extract(county_raster, Random_cells_vals[,1]), Random_cells_vals$county)
  }
  
}

head(Random_cells_vals)
table(Random_cells_vals$county)

write.csv(Random_cells_vals, "Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/SpearmanRank/Random_cells_vals.csv", row.names = F)

