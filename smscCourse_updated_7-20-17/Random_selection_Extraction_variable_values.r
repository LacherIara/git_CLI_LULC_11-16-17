
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
inRasterLoc <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/"

# Individual Counties
County_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/IndCntys" 
counties <- list.files(County_Folder,pattern = ".img$")


# List files
rasterlist <- list.files("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/")

# Variable Rasters(saved Raster File Quick Reference. xlsx as .csv - column matches the actual names of the layers)
RasterFileQuickReference <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/Raster_File_Quick_Reference.csv", stringsAsFactors = F)

# > RasterFileQuickReference
            # RasterFile                   Name
# 1    agwpct11_smsc.img working in agriculture
# 2   dis2watnl_smsc.img      distance to water
# 3  disdevopen_smsc.img distance to open space
# 4    dist_urb_smsc.img      distance to urban
# 5     edpct11_smsc.img              education
# 6        elev_smsc.img              elevation
# 7        Geol_smsc.img                geology
# 8    h_den_11_smsc.img        housing density
# 9  h_value_11_smsc.img          housing value
# 10         hc_smsc.img           habitat core
# 11 med_inc_11_smsc.img          median income
# 12       mine_smsc.img                 mining
# 13  pc_inc_11_smsc.img      per capita income
# 14         pi_smsc.img      soil productivity
# 15    pl11gap_smsc.img protected lands status
# 16   pocpct11_smsc.img        people of color
# 17   popden11_smsc.img     population density
# 18   povpct11_smsc.img                poverty
# 19        ppt_smsc.img          precipitation
# 20      slope_smsc.img                  slope
# 21       tmax_smsc.img               max temp
# 22       tmin_smsc.img               min temp
# 23  travelt11_smsc.img            travel time
# 24         zone_g5.img                 zoning

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
print(new) # Time difference of 27.49266 mins


# load county rasters and extract values from raster to fill in the county field
County_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/IndCntys" 
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

write.csv(Random_cells_vals, "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/Spearman/Random_cells_vals.csv", row.names = F)

