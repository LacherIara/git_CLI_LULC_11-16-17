############################ 
#PURPOSE: Create a single boxplot showing WOE for one transition, one variable
#INPUT: WOE tables for each version  
#OUTPUT: 
#DEVELOPED: 4-5-19 
#CONTACT: CarrollE@si.edu
#NOTES:


############################

# SET WORKING DIRECTORY *Must set to write pdf in file
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/WOE_Results/plots")
# ----------------------------------------------

################################################

# PACKAGES NEEDED

library(plyr) # General data manipulation

# -----------------------------------------------------------
# SINGLE PLOT TO PDF
# -----------------------------------------------------------

Transitions_all <- data.frame("num" = c(53,63,73,65,75,56,76,57,67), "title" = c("Forest to Development","Grass to Development","Crop to Development","Grass to Forest","Crop to Forest","Forest to Grass","Crop to Grass","Forest to Crop","Grass to Crop"))

Input_Path <- "U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/BasicDataAnalyses/WOE_Results/sav2016allWOE_saBINS/"
Plot_Transition <- 73
Variable <- "distance_to_3"
      #distance_to_2
      #distance_to_3
      #geology
      #medinc_11
      #popden_11
      #pov_11
      #prot_dist_11
      #prot_gap_11
      #slope
      #soils
      #travelt_11
      #zoning
WOEfile <- paste0("sav201bins_ct",Plot_Transition,Variable)
Title <- Transitions_all$title[Transitions_all$num == Plot_Transition]

WOEplot <- read.csv(paste0(Input_Path, WOEfile, ".csv"))

names(WOEplot) = substr(names(WOEplot), 2, length(names(WOEplot)))

z=boxplot(WOEplot[,-(1:8)])

pdf(paste0("v201_t",Plot_Transition, "_",Variable,"_WOE.pdf"), paper="letter")
bxp(z, main = paste0(Title, ":"), xlab = "Bin", ylab = "Weight of Evidence", whisklty=1, medlwd=2.25)
  abline(0,0, lty=2)
  mtext(Variable, side=3, line=0.3, cex = 1.1)
  
dev.off()

