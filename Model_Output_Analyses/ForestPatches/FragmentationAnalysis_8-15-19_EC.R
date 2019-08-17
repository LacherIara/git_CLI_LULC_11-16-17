# Creator: Erin Carroll
# Date: 8-15-19
# Purpose: Analyze the change in the amount and spatial distribution of forest fragments from 2011 - 2061, under each future land use scenario (v2016). Conduct analyses over entire study area AND by county.
# Outputs: 
    # U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/Fragmentation_SA.csv
    # U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/Fragmentation_CNTY.csv
# See details on these indices at EcologicalIntegrity_One > Forest Patches > v2016 Forest Patches, 2019 > Fragstats Metrics

setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/R")

library(landscapemetrics)
library(raster)
library(rgdal)
library(sp)
library(bnspatial)

#################################
## read in data
#################################

SA.2011 <- raster("U:/CLI/SpatialData/VAClipRaw/VA_NLCD/NLCD_Ed/nlcd11_SA.tif")
SA.Q1 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/FutureLandscapes/Q1/v2016_Q1_Landscape05sa.tif")
SA.Q2 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/FutureLandscapes/Q2/v2016_Q2_Landscape05sa.tif")
SA.Q3 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/FutureLandscapes/Q3/v2016_Q3_Landscape05sa.tif")
SA.Q4 <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/FutureLandscapes/Q4/v2016_Q4_Landscape05sa.tif")
SA.RT <- raster("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/FutureLandscapes/RT/v2016_RT_Landscape05sa.tif")
  
county.mask <- readOGR(dsn=("U:/CLI/SpatialData/VAClipRaw/VA_Boundaries/CountiesEd/ExCntyCensus_SA_NoCities.shp"), layer="ExCntyCensus_SA_NoCities")

#################################
## Study Area
#################################

# trying to run all 6 rasters as list at once did not work --- # Error: cannot allocate vector of size 4.5 Gb

landscapes.SA <- list(SA.2011, SA.Q1, SA.Q2, SA.Q3, SA.Q4, SA.RT)

# fragmentation.SA <- calculate_lsm(landscapes.SA,
#                                   what = c("lim_c_ca", 
#                                            "lsm_c_pland",
#                                            "lsm_c_ed", 
#                                            "lsm_c_area_mn", "lsm_c_area_sd", 
#                                           "lsm_c_gyrate_mn", "lsm_c_gyrate_sd", "lsm_c_gyrate_cv",
#                                            "lsm_c_pacfrac",
#                                            "lsm_c_tca",
#                                            "lsm_c_ndca",
#                                            "lsm_c_dcad",
#                                           "lsm_c_core_mn", "lsm_c_core_sd",
#                                            "lsm_c_np",
#                                            "lsm_c_pd",
#                                            "lsm_c_division",
#                                            "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv"),
#                                   directions = 8,
#                                   edge_depth = 2, # unit = cells
#                                   neighbourhood = 8,
#                                   progress = TRUE)
# write.csv(fragmentation.SA, file = "Fragmentation_SA.csv")

for (i in landscapes.SA) {
  frag.SA <- calculate_lsm(i,
                           what = c("lim_c_ca", 
                                     "lsm_c_pland",
                                     "lsm_c_ed", 
                                     "lsm_c_area_mn", "lsm_c_area_sd", 
                                     "lsm_c_gyrate_mn", "lsm_c_gyrate_sd", "lsm_c_gyrate_cv",
                                     "lsm_c_pacfrac",
                                     "lsm_c_tca",
                                     "lsm_c_ndca",
                                     "lsm_c_dcad",
                                     "lsm_c_core_mn", "lsm_c_core_sd",
                                     "lsm_c_np",
                                     "lsm_c_pd",
                                     "lsm_c_division",
                                     "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv"),
                           directions = 8,
                           edge_depth = 2,
                           neighbourhood = 8,
                           progress = TRUE)
  
  write.csv(frag.SA, file = paste0("Frag_SA_", names(i), ".csv"))
}

#################################
## By County
#################################

for (i in landscapes.SA) {
  frag.cnty <- sample_lsm(i,
                          y = county.mask,
                          what = c("lim_c_ca", 
                                  "lsm_c_pland", 
                                  "lsm_c_ed", 
                                  "lsm_c_area_mn", "lsm_c_area_sd", 
                                  "lsm_c_gyrate_mn", "lsm_c_gyrate_sd", "lsm_c_gyrate_cv",
                                  "lsm_c_pacfrac",
                                  "lsm_c_tca",
                                  "lsm_c_ndca",
                                  "lsm_c_dcad",
                                  "lsm_c_core_mn", "lsm_c_core_sd",
                                  "lsm_c_np",
                                  "lsm_c_pd",
                                  "lsm_c_division",
                                  "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv"),
                          directions=8,
                          neighbourhood=8)
  
  write.csv(frag.cnty, file = paste0("Frag_CNTY_", names(i), ".csv"))
}

###################################################################################################
###################################################################################################
## Wihtout Protected Lands
###################################################################################################
###################################################################################################

# new base layers:

mask <- raster("U:\CLI\SpatialData\VAClipRaw\VA_ProtectedLand\Not_pl_2011_x.tif")

SA.2011.NoPL <- extractByMask(SA.2011, mask, spatial=T)
SA.Q1.NoPL <- extractByMask(SA.Q1, mask, spatial=T)
SA.Q2.NoPL <- extractByMask(SA.Q2, mask, spatial=T)
SA.Q3.NoPL <- extractByMask(SA.Q3, mask, spatial=T)
SA.Q4.NoPL <- extractByMask(SA.Q4, mask, spatial=T)
SA.RT.NoPL <- extractByMask(SA.RT, mask, spatial=T)

names(SA.2011.NoPL) <- "SA.2011.NoPL"
names(SA.Q1.NoPL) <- "SA.Q1.NoPL"
names(SA.Q2.NoPL) <- "SA.Q2.NoPL"
names(SA.Q3.NoPL) <- "SA.Q3.NoPL"
names(SA.Q4.NoPL) <- "SA.Q4.NoPL"
names(SA.RT.NoPL) <- "SA.RT.NoPL"

#################################
## Study Area - No PL
#################################

landscapes.SA.NoPL <- list(SA.2011.NoPL, SA.Q1.NoPL, SA.Q2.NoPL, SA.Q3.NoPL, SA.Q4.NoPL, SA.RT.NoPL)

for (i in landscapes.SA.NoPL) {
  frag.SA.NoPL <- calculate_lsm(i,
                                what = c("lim_c_ca", 
                                          "lsm_c_pland",
                                          "lsm_c_ed", 
                                          "lsm_c_area_mn", "lsm_c_area_sd", 
                                          "lsm_c_gyrate_mn", "lsm_c_gyrate_sd", "lsm_c_gyrate_cv",
                                          "lsm_c_pacfrac",
                                          "lsm_c_tca",
                                          "lsm_c_ndca",
                                          "lsm_c_dcad",
                                          "lsm_c_core_mn", "lsm_c_core_sd",
                                          "lsm_c_np",
                                          "lsm_c_pd",
                                          "lsm_c_division",
                                          "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv"),
                                directions = 8,
                                edge_depth = 2, # unit = cells
                                neighbourhood = 8,
                                progress = TRUE)
  
  write.csv(frag.SA.NoPL, file = paste0("Frag__SA_NoPL_", names(i), ".csv"))
}

#################################
## By County - No PL
#################################

for (i in landscapes.SA.NoPL) {
  frag.cnty.NoPL <- sample_lsm(i,
                               y = county.mask,
                               what = c("lim_c_ca", 
                                        "lsm_c_pland", 
                                        "lsm_c_ed", 
                                        "lsm_c_area_mn", "lsm_c_area_sd", 
                                        "lsm_c_gyrate_mn", "lsm_c_gyrate_sd", "lsm_c_gyrate_cv",
                                        "lsm_c_pacfrac",
                                        "lsm_c_tca",
                                        "lsm_c_ndca",
                                        "lsm_c_dcad",
                                        "lsm_c_core_mn", "lsm_c_core_sd",
                                        "lsm_c_np",
                                        "lsm_c_pd",
                                        "lsm_c_division",
                                        "lsm_c_enn_mn", "lsm_c_enn_sd", "lsm_c_enn_cv"),
                               directions=8,
                               neighbourhood=8)
  
  write.csv(frag.cnty.NoPL, file = paste0("Frag_CNTY_NoPL_", names(i), ".csv"))
  
}





















