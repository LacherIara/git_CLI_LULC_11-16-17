# Creator: Erin Carroll
# Date: 8-15-19
# Purpose: Analyze the change in the amount and spatial distribution of four forest quality indices from 2011 - 2061, under each future land use scenario (v2016).
# The four indices are: Climate Resiliency, Element Occurence, Soil Productivity Index, and Watershed Conservation Priority.
# See details on these indices at EcologicalIntegrity_One > Forest Patches > v2016 Forest Patches, 2019 > Secondary Metrics

setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/SecondaryMetricAnalysis/Tables")

library(raster)
library(rgdal)
library(bnspatial)
library(reshape2)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

#################################
## read in data
#################################

# metrics
ClimateRes <- raster("U:/CLI/SpatialData/VAClipRaw/VA_Habitat/VA_Habitat_Ed/ClimateResilience/ClimateResilience_0v1.tif")
EO <- raster("V:/IaraSpatialLayers/SpatialData/VAClipRaw/VA_Species/Species_Ed/EO_count.tif")
WatershedConsPrior <- raster("U:/CLI/SpatialData/VAClipRaw/VA_HydroWatersh/VA_HydroWatersh_ed/ConsPrior_prjC.tif")
PI <- raster("U:/CLI/PreparedRasters/StudyAreaBndy/PI_16_an.img")

# forest masks
for.2011 <- raster("U:/CLI/SpatialData/VAClipRaw/VA_NLCD/NLCD_Ed/indv/for")
for.Q1 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Spatial Data/Indv/NoData/v2016_Q1_05_for_onlyND.tif")
for.Q2 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Spatial Data/Indv/NoData/v2016_Q2_05_for_onlyND.tif")
for.Q3 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Spatial Data/Indv/NoData/v2016_Q3_05_for_onlyND.tif")
for.Q4 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Spatial Data/Indv/NoData/v2016_Q4_05_for_onlyND.tif")
for.RT <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Spatial Data/Indv/NoData/v2016_RT_05_for_onlyND.tif")

# counties in SA
counties.poly <- readOGR(dsn=("U:/CLI/SpatialData/VAClipRaw/VA_Boundaries/CountiesEd/ExCntyCensus_SA_NoCities.shp"), layer="ExCntyCensus_SA_NoCities")

#################################
## extract by mask
#################################

names(ClimateRes) <- "ClimateRes"
names(EO) <- "EO"
names(WatershedConsPrior) <- "WatershedConsPrior"
names(PI) <- "PI"

metrics <- list(ClimateRes, EO, WatershedConsPrior, PI)

for (i in metrics) {
  name.2011 <- paste0(names(i), ".2011")
  assign(name.2011, extractByMask(i, for.2011, spatial=TRUE))
  
  name.2061.Q1 <- paste0(names(i), ".2061.Q1")
  assign(name.2061.Q1, extractByMask(i, for.Q1, spatial=TRUE))
  
  name.2061.Q2 <- paste0(names(i), ".2061.Q2")
  assign(name.2061.Q2, extractByMask(i, for.Q2, spatial=TRUE))
  
  name.2061.Q3 <- paste0(names(i), ".2061.Q3")
  assign(name.2061.Q3, extractByMask(i, for.Q3, spatial=TRUE))
  
  name.2061.Q4 <- paste0(names(i), ".2061.Q4")
  assign(name.2061.Q4, extractByMask(i, for.Q4, spatial=TRUE))
  
  name.2061.RT <- paste0(names(i), ".2061.RT")
  assign(name.2061.RT, extractByMask(i, for.RT, spatial=TRUE))
}


#################################
## tabulate area
#################################

TabulateArea <- function(indx, extracted, counties.poly, cntyname) {
  dat <- as.data.frame(table(extracted[[indx]]))
  dat$name <- counties.poly[[cntyname]][[indx]]
  return(dat)
}

# find a more elegant way to do this. In a loop, deparse(substitute(i)) just prints i, not object name :(
names(ClimateRes.2011) <- "ClimateRes.2011"
names(ClimateRes.2061.Q1) <- "ClimateRes.2061.Q1"
names(ClimateRes.2061.Q2) <- "ClimateRes.2061.Q2"
names(ClimateRes.2061.Q3) <- "ClimateRes.2061.Q3"
names(ClimateRes.2061.Q4) <- "ClimateRes.2061.Q4"
names(ClimateRes.2061.RT) <- "ClimateRes.2061.RT"

names(EO.2011) <- "EO.2011"
names(EO.2061.Q1) <- "EO.2061.Q1"
names(EO.2061.Q2) <- "EO.2061.Q2"
names(EO.2061.Q3) <- "EO.2061.Q3"
names(EO.2061.Q4) <- "EO.2061.Q4"
names(EO.2061.RT) <- "EO.2061.RT"

names(PI.2011) <- "PI.2011"
names(PI.2061.Q1) <- "PI.2061.Q1"
names(PI.2061.Q2) <- "PI.2061.Q2"
names(PI.2061.Q3) <- "PI.2061.Q3"
names(PI.2061.Q4) <- "PI.2061.Q4"
names(PI.2061.RT) <- "PI.2061.RT"

names(WatershedConsPrior.2011) <- "WatershedConsPrior.2011"
names(WatershedConsPrior.2061.Q1) <- "WatershedConsPrior.2061.Q1"
names(WatershedConsPrior.2061.Q2) <- "WatershedConsPrior.2061.Q2"
names(WatershedConsPrior.2061.Q3) <- "WatershedConsPrior.2061.Q3"
names(WatershedConsPrior.2061.Q4) <- "WatershedConsPrior.2061.Q4"
names(WatershedConsPrior.2061.RT) <- "WatershedConsPrior.2061.RT"


forests <- list(ClimateRes.2011, ClimateRes.2061.Q1, ClimateRes.2061.Q2, ClimateRes.2061.Q3, ClimateRes.2061.Q4, ClimateRes.2061.RT, 
                EO.2011, EO.2061.Q1, EO.2061.Q2, EO.2061.Q3, EO.2061.Q4, EO.2061.RT, 
                WatershedConsPrior.2011, WatershedConsPrior.2061.Q1, WatershedConsPrior.2061.Q2, WatershedConsPrior.2061.Q3, WatershedConsPrior.2061.Q4, WatershedConsPrior.2061.RT, 
                PI.2011, PI.2061.Q1, PI.2061.Q2, PI.2061.Q3, PI.2061.Q4, PI.2061.RT)

for (i in forests) {
  ext <- extract(i, counties.poly, method="simple")
  tabs <- lapply(seq(ext), TabulateArea, ext, counties.poly, "NAME")
  tabs <- do.call("rbind", tabs)
  tabs <- spread(tabs, key=Var1, value=Freq, fill=0)
  
  name <- names(i)
  name <- paste0("TA.", name)
  assign(name, tabs)
  
  write.csv(tabs, file=paste0(name, ".csv"))
}

###################################################################################################
###################################################################################################
## Wihtout Protected Lands
###################################################################################################
###################################################################################################

# mask forest masks by negative of public lands

mask <- raster("U:/CLI/SpatialData/VAClipRaw/VA_ProtectedLand/Not_pl_2011_x.tif")

for.2011.NoPL <- extractByMask(for.2011, mask, spatial = TRUE)
for.Q1.NoPL <- extractByMask(for.Q1, mask, spatial = TRUE)
for.Q2.NoPL <- extractByMask(for.Q2, mask, spatial = TRUE)
for.Q3.NoPL <- extractByMask(for.Q3, mask, spatial = TRUE)
for.Q4.NoPL <- extractByMask(for.Q4, mask, spatial = TRUE)
for.RT.NoPL <- extractByMask(for.RT, mask, spatial = TRUE)

#################################
## extract by mask - No PL
#################################

names(ClimateRes) <- "ClimateRes"
names(EO) <- "EO"
names(WatershedConsPrior) <- "WatershedConsPrior"
names(PI) <- "PI"

metrics <- list(ClimateRes, EO, WatershedConsPrior, PI)

for (i in metrics) {
  name.2011.NoPL <- paste0(names(i), ".2011.NoPL")
  assign(name.2011.NoPL, extractByMask(i, for.2011.NoPL, spatial=TRUE))
  
  name.2061.Q1.NoPL <- paste0(names(i), ".2061.Q1.NoPL")
  assign(name.2061.Q1.NoPL, extractByMask(i, for.Q1.NoPL, spatial=TRUE))
  
  name.2061.Q2.NoPL <- paste0(names(i), ".2061.Q2.NoPL")
  assign(name.2061.Q2.NoPL, extractByMask(i, for.Q2.NoPL, spatial=TRUE))
  
  name.2061.Q3.NoPL <- paste0(names(i), ".2061.Q3.NoPL")
  assign(name.2061.Q3.NoPL, extractByMask(i, for.Q3.NoPL, spatial=TRUE))
  
  name.2061.Q4.NoPL <- paste0(names(i), ".2061.Q4.NoPL")
  assign(name.2061.Q4.NoPL, extractByMask(i, for.Q4.NoPL, spatial=TRUE))
  
  name.2061.RT.NoPL <- paste0(names(i), ".2061.RT.NoPL")
  assign(name.2061.RT.NoPL, extractByMask(i, for.RT.NoPL, spatial=TRUE))
}

#################################
## tabulate area - No PL
#################################

TabulateArea <- function(indx, extracted, counties.poly, cntyname) {
  dat <- as.data.frame(table(extracted[[indx]]))
  dat$name <- counties.poly[[cntyname]][[indx]]
  return(dat)
}

# find a more elegant way to do this. In a loop, deparse(substitute(i)) just prints i, not object name :(
names(ClimateRes.2011.NoPL) <- "ClimateRes.2011.NoPL"
names(ClimateRes.2061.Q1.NoPL) <- "ClimateRes.2061.Q1.NoPL"
names(ClimateRes.2061.Q2.NoPL) <- "ClimateRes.2061.Q2.NoPL"
names(ClimateRes.2061.Q3.NoPL) <- "ClimateRes.2061.Q3.NoPL"
names(ClimateRes.2061.Q4.NoPL) <- "ClimateRes.2061.Q4.NoPL"
names(ClimateRes.2061.RT.NoPL) <- "ClimateRes.2061.RT.NoPL"

names(EO.2011.NoPL) <- "EO.2011.NoPL"
names(EO.2061.Q1.NoPL) <- "EO.2061.Q1.NoPL"
names(EO.2061.Q2.NoPL) <- "EO.2061.Q2.NoPL"
names(EO.2061.Q3.NoPL) <- "EO.2061.Q3.NoPL"
names(EO.2061.Q4.NoPL) <- "EO.2061.Q4.NoPL"
names(EO.2061.RT.NoPL) <- "EO.2061.RT.NoPL"

names(PI.2011.NoPL) <- "PI.2011.NoPL"
names(PI.2061.Q1.NoPL) <- "PI.2061.Q1.NoPL"
names(PI.2061.Q2.NoPL) <- "PI.2061.Q2.NoPL"
names(PI.2061.Q3.NoPL) <- "PI.2061.Q3.NoPL"
names(PI.2061.Q4.NoPL) <- "PI.2061.Q4.NoPL"
names(PI.2061.RT.NoPL) <- "PI.2061.RT.NoPL"

names(WatershedConsPrior.2011.NoPL) <- "WatershedConsPrior.2011.NoPL"
names(WatershedConsPrior.2061.Q1.NoPL) <- "WatershedConsPrior.2061.Q1.NoPL"
names(WatershedConsPrior.2061.Q2.NoPL) <- "WatershedConsPrior.2061.Q2.NoPL"
names(WatershedConsPrior.2061.Q3.NoPL) <- "WatershedConsPrior.2061.Q3.NoPL"
names(WatershedConsPrior.2061.Q4.NoPL) <- "WatershedConsPrior.2061.Q4.NoPL"
names(WatershedConsPrior.2061.RT.NoPL) <- "WatershedConsPrior.2061.RT.NoPL"


forests.NoPL <- list(ClimateRes.2011.NoPL, ClimateRes.2061.Q1.NoPL, ClimateRes.2061.Q2.NoPL, ClimateRes.2061.Q3.NoPL, ClimateRes.2061.Q4.NoPL, ClimateRes.2061.RT.NoPL, 
                EO.2011.NoPL, EO.2061.Q1.NoPL, EO.2061.Q2.NoPL, EO.2061.Q3.NoPL, EO.2061.Q4.NoPL, EO.2061.RT.NoPL, 
                WatershedConsPrior.2011.NoPL, WatershedConsPrior.2061.Q1.NoPL, WatershedConsPrior.2061.Q2.NoPL, WatershedConsPrior.2061.Q3.NoPL, WatershedConsPrior.2061.Q4.NoPL, WatershedConsPrior.2061.RT.NoPL, 
                PI.2011.NoPL, PI.2061.Q1.NoPL, PI.2061.Q2.NoPL, PI.2061.Q3.NoPL, PI.2061.Q4.NoPL, PI.2061.RT.NoPL)

for (i in forests.NoPL) {
  ext <- extract(i, counties.poly, method="simple")
  tabs <- lapply(seq(ext), TabulateArea, ext, counties.poly, "NAME")
  tabs <- do.call("rbind", tabs)
  tabs <- spread(tabs, key=Var1, value=Freq, fill=0)
  
  name <- names(i)
  name <- paste0("TA.", name)
  assign(name, tabs)
  
  write.csv(tabs, file = paste0(name, ".csv"))
}
