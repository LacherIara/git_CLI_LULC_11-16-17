############################ 
#PURPOSE: Conduct Spearman- Rank Correlation on Spatial Rasters representing each potential driver variable.
#INPUT: table of random cells values # check
#OUTPUT: result of the Spearman- Rank Correlation  # check
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER


# ----------------------------------------------
################################################


# PACKAGES NEEDED
library(coin) # conditional Inference Procedures in a Permutation Test Framework
library(Hmisc) # useful functions for data analysis
library(corrplot) # visualize the correlation matrix


# ----------------------------------------------
# READ OUTPUT FILES:
# SpearRank <- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/SpearmanRank/SpearRank.csv")

# READ INPUT FILES:
# rand_vals <- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/BasicDataAnalyses/SpearmanRank/Random_cells_vals.csv")



############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# SPEARMAN RANK CORRELATION
# ----------------------------------------------
# ----------------------------------------------





# ----------------------------------------------
# Read random cells values
rand_vals <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/Spearman/Random_cells_vals.csv")

# ----------------------------------------------
# Create correlation matrix for columns 2:20
Rcorr <- rcorr(as.matrix(rand_vals[,2:26]), type=c("spearman"))


# ----------------------------------------------
# Create function "flattenCorrMatrix"
# ----------------------------------------------

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
# num : matrix of the number of values

flattenCorrMatrix <- function(cormat, pmat, num) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut],
	n = num[ut]
    )
}

# ----------------------------------------------
# Run "flattenCorrMatrix"
SpearRank <- flattenCorrMatrix(Rcorr$r, Rcorr$P, Rcorr$n)

# ----------------------------------------------
# Write to file
write.csv(SpearRank, "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/Spearman/SpearRank.csv", row.names = FALSE)

SpearRank <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/Spearman/SpearRank.csv")


# ----------------------------------------------
# ----------------------------------------------
# PLOTTING CORRELATIONS MATRIX
# ----------------------------------------------
# ----------------------------------------------

corrplot(Rcorr$r, type="upper", tl.col="black", tl.srt=45) #margins are off.

