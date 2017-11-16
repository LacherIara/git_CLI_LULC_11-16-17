############################ 
#PURPOSE: Convert Dinamica's .img rasters to tif for easier visualization in ArcMap
#INPUT: Dinamica's future landscapes
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:

#IMPORTANT: 
##### NEXT STEPS #####

############################

# PACKAGES NEEDED
library(raster)

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################


RT <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/RT/v1071_RT_Landscape05.img")
Q1 <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q1/v1071_Q1_Landscape05.img")
Q2 <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q2/v1071_Q2_Landscape05.img")
Q3 <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q3/v1071_Q3_Landscape05.img")



writeRaster(RT,filename= "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/RT/v1071_RT_Landscape05.TIF", format='GTiff', overwrite=TRUE)

writeRaster(Q1,filename= "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q1/v1071_Q1_Landscape05.TIF", format='GTiff', overwrite=TRUE)

writeRaster(Q2,filename= "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q2/v1071_Q2_Landscape05.TIF", format='GTiff', overwrite=TRUE)

writeRaster(Q3,filename= "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q3/v1071_Q3_Landscape05.TIF", format='GTiff', overwrite=TRUE)


# Lilliput:

lili <- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/Dinamica Models/lilliput_100/Output_tests/lilliput_m1_ls_05.img")

writeRaster(lili,filename= "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/Dinamica Models/lilliput_100/Output_tests/lilliput_m1_ls_modetest05.img", format='GTiff', overwrite=TRUE)

# V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V106/modifiers/mod_test_smsc.img
# V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/RoadClassMod_sa.img

lili <- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/Dinamica Models/lilliput_100/Output_tests/lilliput_m1_ls_modetest05.img")


# Modifier Rasters
mod_urban <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/UrbanRoadMod_an.img")


writeRaster(mod_urban,filename="V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/UrbanRoadMod_an.tif", format='GTiff', overwrite=TRUE)

writeRaster(mod_urban,filename="V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/UrbanRoadMod_an2.img", format='HFA', overwrite=TRUE)



L_mod_urban <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V106/modifiers/mod_test_smsc.img")
writeRaster(L_mod_urban,filename="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V106/modifiers/mod_test_smsc2.img", format='HFA', overwrite=TRUE)







Q1 <- raster("Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q1/v1071_Q1_Landscape05.img")
Q2 <- raster("Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q2/v1071_Q2_Landscape05.img")
Q3 <- raster("Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q3/v1071_Q3_Landscape05.img")



writeRaster(Q1,filename= "Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q1/v1071_Q1_Landscape05.tif", format='GTiff', overwrite=TRUE)

writeRaster(Q2,filename= "Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q2/v1071_Q2_Landscape05.tif", format='GTiff', overwrite=TRUE)

writeRaster(Q3,filename= "Y:/Lacher/Dinamica_Runs/StudyArea_V107/FutureLandscapes/Q3/v1071_Q3_Landscape05.tif", format='GTiff', overwrite=TRUE)

library(raster)



mod1 <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/RT/v1071_RT_Landscape05.img")


sa_mod2 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/UrbanRoadMod_an.img")
l_mod2 <- raster("V:/IaraSpatialLayers/PreparedRasters/StudyAreaBndy/Modifier_ras/mod_prob_smsc.img")


mod3 <- raster("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/FutureLandscapes/Q2/v1071_Q2_Landscape05.img")





