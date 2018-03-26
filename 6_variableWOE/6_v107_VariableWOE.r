############################ 
#PURPOSE: Create dataframe and scatter plots for WOE at each: transition, county, variable, bin
#INPUT: WOE for each version  
#OUTPUT: 
#DEVELOPED: (V1) 3/31/2016 
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
# * Need to create the output folder in the drive prior to running write.csv e.g. "Y:\Lacher\Dinamica_Runs\PatchesTransitions_SA\BasicDataAnalyses\
# These 2 folders--> SAv103\sav103_woeVarBins"
##### NEXT STEPS #####
# Remove leading value that resulted from +1 in first step
# Individual transition loops at bottom can be done as one loop. A later modification.

############################

# SET WORKING DIRECTORY *Must set to write pdf in file
setwd("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses") #V SCBI
# ----------------------------------------------
################################################


# PACKAGES NEEDED

library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(tidyr) # General data manipulation
library(ggplot2) # Plotting


# ----------------------------------------------
# READ OUTPUT FILES:


# ----------------------------------------------
# READ INPUT FILES:

# COUNTY TABLES
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/FullGEOID.csv")
colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")

S20_GEOID <-  read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SAcntyOnly.csv")

# Alter county tables 
#add leading zero to numbers with less than 1 digit
sa_ctyGEOIDzero <- sa_ctyGEOID
sa_ctyGEOIDzero$Din_cty <- sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# COMPILE WOE TABLES:
# ----------------------------------------------
# ----------------------------------------------

# Read all WOE files in (these are the final recalculated, significant ones, with alterations made to WOE)
sav107WOE_LOC<-"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/WOE_recalc/LPA/" 

sav107WOE_files <- list.files(sav107WOE_LOC,pattern = ".*v107.*\\csv$")

sav107WOE<-lapply(paste0(sav107WOE_LOC, sav107WOE_files), read.csv)
sav107WOE_nms<-gsub(".csv", "",sav107WOE_files)
names(sav107WOE)<-sav107WOE_nms
# MERGE INTO ONE CSV
v_v<-1
for( v in 1:length(sav107WOE)){
print(paste(v_v, ':'))
sav107WOE[[v_v]]$Din_cty<-as.integer(gsub("pgrv107_WOE", "",sav107WOE_nms[[v]]))
v_v<-v_v+1
}
View(sav107WOE)

# some data finagling
sav107allWOE_sabuff<-do.call(rbind,sav107WOE)

#select only counties in study area
sav107allWOE_sa<-subset(sav107allWOE_sabuff, sav107allWOE_sabuff$Din_cty %in% S20_GEOID$VALUE)

colnames(sav107allWOE_sa) <- c("From", "To", "Variable", "Lower", "Upper", "Weight", "Din_cty")
write.csv(sav107allWOE_sa,"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/BasicDataAnalyses/sav107allWOE_sa.csv", row.names=FALSE)

# READ FROM FILE #

#COMPILED WOE TABLES:
v107WOE <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_sa.csv")

# > str(v107WOE)
# 'data.frame':	4945 obs. of  7 variables:
 # $ From    : int  5 5 5 5 5 5 5 5 5 5 ...
 # $ To      : int  3 3 3 3 3 3 3 3 3 3 ...
 # $ Variable: Factor w/ 12 levels " distance/distance_to_2",..: 1 1 1 1 2 2 2 2 2 2 ...
 # $ Lower   : int  0 100 500 600 0 100 300 400 500 600 ...
 # $ Upper   : int  100 500 600 3300 100 300 400 500 600 6000 ...
 # $ Weight  : num  0.531 0.181 -1.762 -3.189 2.619 ...
 # $ Din_cty : int  3 3 3 3 3 3 3 3 3 3 ...






# --------------------------------------------------------
# --------------------------------------------------------
# Create dataframe and scatter plots for WOE at each: transition, county, variable, bin
# --------------------------------------------------------
# --------------------------------------------------------

# Variable Dependent
# **for each version**
# for(vars in unique(v107WOE$Variable)){print(vars)}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# VARIABLES
# These are sorted alphabetically

# VARIABLE				V103	V105	V106   V107
# "agprof_11" -			10		5              
# "distance_to_2" - 	100 	100     100    100
# "distance_to_3" -             100     100    100
# "edAA_11" - 			10		               
# "geology" - 					cat:1	cat:1  cat:1
# "hden_11"	-			5		20             
# "hval_11" -			25000	               
# "medinc_11" - 		10000 	10000   10000  10000
# "mines"						1              
# "pcinc_11" -			5000	10000          
# "poc_11" -			10		               
# "popden_11" - 		100 	100     100    100
# "pov_11" - 			10		5       5      5
# "ppt" - 				100 	50      50     
# "prot_dist" - 		100 	500     500    500
# "prot_gap" - 					cat:1	cat:1  cat:1
# "slope" - 			5 		2       2      2
# "soils" - 			cat:1 	cat:1   cat:1  cat:1
# "tmax - 				5 		1       1      
# "tmin" - 				5 		1              
# "travelt_11" - 		10 		5       5      5
# "zoning" -			cat		cat:1   cat:1  cat:1
	
	

#V107
v107inc <- c(100,100,1,10000,100,5,500,1,2,1,5,1)

# Create ID for each row 
v107WOE$ID <- as.character(1:nrow(v107WOE))

# Get the WOE for each variable
Transitions <- c(53,63,73,65,75,56,76,57,67)

# The below can be done as one loop. A later modification.

t_53 <- filter(v107WOE, v107WOE$From == 5 & v107WOE$To == 3)
t_53$Variable <- sub(".*/", "", t_53$Variable)
# t_53 <- subset(t_53,!t_53$Weight == 0) # remove zeros
uvar_53 <- sort(unique(t_53$Variable), decreasing = FALSE)#Find unique variables

t_63 <- filter(v107WOE, v107WOE$From == 6 & v107WOE$To == 3)
t_63$Variable <- sub(".*/", "", t_63$Variable)
# t_63 <- subset(t_63,!t_63$Weight == 0) # remove zeros
uvar_63 <- sort(unique(t_63$Variable), decreasing = FALSE)#Find unique variables

t_73 <- filter(v107WOE, v107WOE$From == 7 & v107WOE$To == 3)
t_73$Variable <- sub(".*/", "", t_73$Variable)
# t_73 <- subset(t_73,!t_73$Weight == 0) # remove zeros
uvar_73 <- sort(unique(t_73$Variable), decreasing = FALSE)#Find unique variables

t_65 <- filter(v107WOE, v107WOE$From == 6 & v107WOE$To == 5)
t_65$Variable <- sub(".*/", "", t_65$Variable)
# t_65 <- subset(t_65,!t_65$Weight == 0) # remove zeros
uvar_65 <- sort(unique(t_65$Variable), decreasing = FALSE)#Find unique variables

t_75 <- filter(v107WOE, v107WOE$From == 7 & v107WOE$To == 5)
t_75$Variable <- sub(".*/", "", t_75$Variable)
# t_75 <- subset(t_75,!t_75$Weight == 0) # remove zeros
uvar_75 <- sort(unique(t_75$Variable), decreasing = FALSE)#Find unique variables

t_56 <- filter(v107WOE, v107WOE$From == 5 & v107WOE$To == 6)
t_56$Variable <- sub(".*/", "", t_56$Variable)
# t_56 <- subset(t_56,!t_56$Weight == 0) # remove zeros
uvar_56 <- sort(unique(t_56$Variable), decreasing = FALSE)#Find unique variables

t_76 <- filter(v107WOE, v107WOE$From == 7 & v107WOE$To == 6)
t_76$Variable <- sub(".*/", "", t_76$Variable)
# t_76 <- subset(t_76,!t_76$Weight == 0) # remove zeros
uvar_76 <- sort(unique(t_76$Variable), decreasing = FALSE)#Find unique variables

t_57 <- filter(v107WOE, v107WOE$From == 5 & v107WOE$To == 7)
t_57$Variable <- sub(".*/", "", t_57$Variable)
# t_57 <- subset(t_57,!t_57$Weight == 0) # remove zeros
uvar_57 <- sort(unique(t_57$Variable), decreasing = FALSE)#Find unique variables

t_67 <- filter(v107WOE, v107WOE$From == 6 & v107WOE$To == 7)
t_67$Variable <- sub(".*/", "", t_67$Variable)
# t_67 <- subset(t_67,!t_67$Weight == 0) # remove zeros
uvar_67 <- sort(unique(t_67$Variable), decreasing = FALSE)#Find unique variables


# ----------------------------------------------
# ----------------------------------------------
#  LU 5->3 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_53){
	print(vars)
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_53, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- (uvar_53)

# # Error in u[[u_u]] : subscript out of bounds

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_53



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_53)){
	for(f in 1:nrow(mv[[v]])){
		# print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				# print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t53 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t53[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t53) <- uvar_53

Output_Files_53 <- paste0("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t53_", names(sav107bins_t53), ".csv", sep="")

for(f in 1:length(sav107bins_t53)){
write.csv(sav107bins_t53[[f]], Output_Files_53[[f]], row.names=FALSE)
}
 

}

# ----------------------------------------------
# READ ALL WOE FILES IN


sav107bins_t53 <- lapply(Output_Files_53, read.csv, check.names=FALSE)
sav107bins_t53_nms <- gsub(".csv", "",Output_Files_53)
names(sav107bins_t53) <- sav107bins_t53_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# ----------------------------------------------

# Make sure working directory is set to correct file


pdf("V107_Forest2Dev; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t53)){


boxplot(sav107bins_t53[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Forest2Dev","-",sav107bins_t53[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 6->3 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_63){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_63, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_63

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_63



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_63)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t63 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t63[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t63) <- uvar_63

Output_Files_63 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t63_", names(sav107bins_t63), ".csv", sep="")

for(f in 1: length(sav107bins_t63)){
write.csv(sav107bins_t63[[f]], Output_Files_63[[f]], row.names=FALSE)
}
	 

}

# ----------------------------------------------
# READ ALL WOE FILES IN

sav107bins_t63 <- lapply(Output_Files_63, read.csv, check.names=FALSE)
sav107bins_t63_nms <- gsub(".csv", "",Output_Files_63)
names(sav107bins_t63) <- sav107bins_t63_nms


# ----------------------------------------------
# PLOT TO PDF FILES
# ----------------------------------------------

pdf("V107_Grass2Dev; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t63)){

boxplot(sav107bins_t63[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Grass2Dev","-",sav107bins_t63[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()

}



# ----------------------------------------------
# ----------------------------------------------
#  LU 7->3 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_73){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_73, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_73

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_73



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_73)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t73 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t73[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t73) <- uvar_73

Output_Files_73 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t73_", names(sav107bins_t73), ".csv", sep="")

for(f in 1: length(sav107bins_t73)){
write.csv(sav107bins_t73[[f]], Output_Files_73[[f]], row.names=FALSE)
}


}

# ----------------------------------------------
# READ ALL WOE FILES IN

sav107bins_t73 <- lapply(Output_Files_73, read.csv, check.names=FALSE)
sav107bins_t73_nms <- gsub(".csv", "",Output_Files_73)
names(sav107bins_t73) <- sav107bins_t73_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Crop2Dev; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t73)){

boxplot(sav107bins_t73[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Crop2Dev","-",sav107bins_t73[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 6->5 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_65){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_65, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_65

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_65



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_65)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t65 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t65[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t65) <- uvar_65

Output_Files_65 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t65_", names(sav107bins_t65), ".csv", sep="")

for(f in 1: length(sav107bins_t65)){
write.csv(sav107bins_t65[[f]], Output_Files_65[[f]], row.names=FALSE)
}
 

}

# ----------------------------------------------

# READ ALL WOE FILES IN

sav107bins_t65 <- lapply(Output_Files_65, read.csv, check.names=FALSE)
sav107bins_t65_nms <- gsub(".csv", "",Output_Files_65)
names(sav107bins_t65) <- sav107bins_t65_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Grass2Forest; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t65)){

boxplot(sav107bins_t65[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Grass2Forest","-",sav107bins_t65[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 7->5 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_75){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_75, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_75

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_75



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_75)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t75 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t75[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t75) <- uvar_75

Output_Files_75 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t75_", names(sav107bins_t75), ".csv", sep="")

for(f in 1: length(sav107bins_t75)){
write.csv(sav107bins_t75[[f]], Output_Files_75[[f]], row.names=FALSE)
}
 

}

# ----------------------------------------------

# READ ALL WOE FILES IN


sav107bins_t75 <- lapply(Output_Files_75, read.csv, check.names=FALSE)
sav107bins_t75_nms <- gsub(".csv", "",Output_Files_75)
names(sav107bins_t75) <- sav107bins_t75_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:

pdf("V107_Crop2Forest; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t75)){

boxplot(sav107bins_t75[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Crop2Forest","-",sav107bins_t75[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 5->6 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_56){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_56, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_56

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_56



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_56)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t56 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t56[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t56) <- uvar_56

Output_Files_56 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t56_", names(sav107bins_t56), ".csv", sep="")

for(f in 1: length(sav107bins_t56)){
write.csv(sav107bins_t56[[f]], Output_Files_56[[f]], row.names=FALSE)
}
 

}

# ----------------------------------------------

# READ ALL WOE FILES IN

sav107bins_t56 <- lapply(Output_Files_56, read.csv, check.names=FALSE)
sav107bins_t56_nms <- gsub(".csv", "",Output_Files_56)
names(sav107bins_t56) <- sav107bins_t56_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Forest2Grass; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t56)){

boxplot(sav107bins_t56[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Forest2Grass","-",sav107bins_t56[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 7->6 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_76){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_76, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_76

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_76



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_76)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t76 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t76[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t76) <- uvar_76

Output_Files_76 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t76_", names(sav107bins_t76), ".csv", sep="")

for(f in 1: length(sav107bins_t76)){
write.csv(sav107bins_t76[[f]], Output_Files_76[[f]], row.names=FALSE)
}
# Read all WOE files in
 

}

# ----------------------------------------------
# READ ALL WOE FILES IN

sav107bins_t76 <- lapply(Output_Files_76, read.csv, check.names=FALSE)
sav107bins_t76_nms <- gsub(".csv", "",Output_Files_76)
names(sav107bins_t76) <- sav107bins_t76_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Crop2Grass; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t76)){

boxplot(sav107bins_t76[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Crop2Grass","-",sav107bins_t76[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 5->7 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_57){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_57, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_57

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_57



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_57)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t57 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t57[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t57) <- uvar_57

Output_Files_57 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t57_", names(sav107bins_t57), ".csv", sep="")

for(f in 1: length(sav107bins_t57)){
write.csv(sav107bins_t57[[f]], Output_Files_57[[f]], row.names=FALSE)
}
	 

}

# ----------------------------------------------

# READ ALL WOE FILES IN

sav107bins_t57 <- lapply(Output_Files_57, read.csv, check.names=FALSE)
sav107bins_t57_nms <- gsub(".csv", "",Output_Files_57)
names(sav107bins_t57) <- sav107bins_t57_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Forest2Crop; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t57)){

boxplot(sav107bins_t57[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Forest2Crop","-",sav107bins_t57[[p]]$Variable[1]), cex.main = 2)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


# ----------------------------------------------
# ----------------------------------------------
#  LU 6->7 
# ----------------------------------------------
# ----------------------------------------------
{
{# Create binned .csv files

# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_67){
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(t_67, Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- uvar_67

# merge back into one dataframe in new order
mv2 <- do.call(rbind,mv) 
# Loop the rest through the list you made for mv dataframes.

mvbins <- list()
t_t <- 1
# m_m <- 1
for(r in 1:length(mv)){
# print(r)
# print(paste(t_t, ':'))
mm <- list()
m_m <- 1
for( i in 1:nrow(mv[[r]])){
	# print(i)
	print(paste(m_m, ':'))
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v107inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
	mm[[m_m]] <- mm[[m_m]][mm[[m_m]]%in%u[[r]]]
	names(mm[[m_m]]) <- c(mm[[m_m]])
	# matrix(mm[[)
	# m_m <- m_m+1
	m_m <- m_m+1
	}
mvbins[[t_t]] <- mm
t_t <- t_t+1
}
# names(mvbins)


mvw <- list()
t_t <- 1
for(r in 1:length(mv)){
ww <- list()
w_w <- 1
for( k in 1:nrow(mv[[r]])){
	print(paste(w_w, ':'))
	ww[[w_w]] <- rep(mv[[r]]$Weight[k],times=length(mvbins[[r]][[k]]))
	ww[[w_w]] <- data.frame(ww[[w_w]])
	ww[[w_w]] <- t(ww[[w_w]])
	ww[[w_w]] <- data.frame(ww[[w_w]])
	colnames(ww[[w_w]]) <- c(mvbins[[r]][[k]])
	w_w <- w_w+1
}
mvw[[t_t]] <- ww
names(mvw[[r]]) <- mv[[r]]$ID
t_t <- t_t+1
}

# Create matrices
# max(lapply(u, length)) # to see what the longest/ most # columns would be

mat <- list()
x_x <- 1
for(r in 1:length(mv)){
	print(paste(x_x, ':'))

	mat[[x_x]] <- data.frame(matrix(nrow=nrow(mv[[r]]), ncol=length(u[[r]])))
	colnames(mat[[x_x]]) <- c(u[[r]])
	rownames(mat[[x_x]]) <- mv[[r]]$ID
	x_x <- x_x+1
}
names(mat) <- uvar_67



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_67)){
	for(f in 1:nrow(mv[[v]])){
		print(f)
			for(j in 1:length(mvw[[v]][[f]])){
				print(j)
				mat[[v]][f,c(names(mvw[[v]][[f]][j])==names(mat[[v]]))] <- mvw[[v]][[f]][j]
				mat[[v]]$ID <- mv[[v]]$ID
	}	}	}
	# Warning messages:
# 1: In `[ <- .data.frame`(`*tmp*`, f, c(names(mvw[[v]][[f]][j]) == names(mat[[v]])),  :
  # provided 1 variable to replace 0 variables

# Join to original table
sav107bins_t67 <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav107bins_t67[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav107bins_t67) <- uvar_67

Output_Files_67 <- paste("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/BasicDataAnalyses/sav107allWOE_saBINS/", "sav107bins_t67_", names(sav107bins_t67), ".csv", sep="")

for(f in 1: length(sav107bins_t67)){
write.csv(sav107bins_t67[[f]], Output_Files_67[[f]], row.names=FALSE)
}


}

# ----------------------------------------------

# READ ALL WOE FILES IN

sav107bins_t67 <- lapply(Output_Files_67, read.csv, check.names=FALSE)
sav107bins_t67_nms <- gsub(".csv", "",Output_Files_67)
names(sav107bins_t67) <- sav107bins_t67_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# Make sure working directory is set to:


pdf("V107_Grass2Crop; WOE per variable.pdf", paper="letter")
par(mfrow=c(3,4),mai=c(0.8,0.5,0.1,0.2))
for(p in 1:length(sav107bins_t67)){

boxplot(sav107bins_t67[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0("Grass2Crop","-",sav107bins_t67[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()


}


