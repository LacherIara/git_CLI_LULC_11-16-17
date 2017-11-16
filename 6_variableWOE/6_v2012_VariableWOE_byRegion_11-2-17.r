############################ 
# # TEST EDIT
#PURPOSE: Create dataframe and scatter plots for WOE at each: transition, county, variable, bin
#INPUT: WOE for each version  
#OUTPUT: 
#DEVELOPED: (V1) 3/31/2016 
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
# * Need to create the output folder in the drive prior to running write.csv e.g. "Y:\Lacher\Dinamica_Runs\PatchesTransitions_SA\BasicDataAnalyses\
# These 2 folders--> SAv103\sav103_woeVarBins"

# **!! MUST EDIT ON A COUNTY BY COUNTY BASIS. **!! SAME APPLIES FOR ALL TRANSITIONS BELOW.

##### NEXT STEPS #####
# Remove leading value that resulted from +1 in first step
# Individual transition loops at bottom can be done as one loop. A later modification.

############################

# SET WORKING DIRECTORY *Must set to write pdf in file
setwd("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/BasicDataAnalyses") #V SCBI
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
sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv")
colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")


# # # Alter county tables 
# # #add leading zero to numbers with less than 1 digit
# # sa_ctyGEOIDzero <- sa_ctyGEOID
# # sa_ctyGEOIDzero$Din_cty <- sapply(sa_ctyGEOIDzero$Din_cty, function(x){if(nchar(x)<2){paste0(0,x)}else{x}}) 


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# ----------------------------------------------
# ----------------------------------------------
# COMPILE WOE TABLES:
# ----------------------------------------------
# ----------------------------------------------

# Read all WOE files in (these are the final recalculated, significant ones, with alterations made to WOE)
sav201WOE_LOC<-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/Parameters/WOE_recalc/LPA/" 

sav201WOE_files <- list.files(sav201WOE_LOC,pattern = ".*v201.*\\csv$")

sav201WOE<-lapply(paste0(sav201WOE_LOC, sav201WOE_files), read.csv)
sav201WOE_nms<-gsub(".csv", "",sav201WOE_files)
names(sav201WOE)<-sav201WOE_nms
# MERGE INTO ONE CSV
v_v<-1
for( v in 1:length(sav201WOE)){
print(paste(v_v, ':'))
sav201WOE[[v_v]]$Din_cty<-as.integer(gsub("pgrv201_WOE", "",sav201WOE_nms[[v]]))
v_v<-v_v+1
}
View(sav201WOE)

# some data finagling
sav201allWOE_sabuff<-do.call(rbind,sav201WOE)

# # #select only counties in study area
# # sav201allWOE_sa<-subset(sav201allWOE_sabuff, sav201allWOE_sabuff$Din_cty %in% sa_ctyGEOID$VALUE)

colnames(sav201allWOE_sabuff) <- c("From", "To", "Variable", "Lower", "Upper", "Weight", "Din_cty")

write.csv(sav201allWOE_sabuff,"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/BasicDataAnalyses/sav201allWOE_sabuff.csv", row.names=FALSE)


# READ FROM FILE #

#COMPILED WOE TABLES:
v201WOE <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/BasicDataAnalyses/sav201allWOE_sabuff.csv")
# Create ID for each row 
v201WOE$ID <- as.character(1:nrow(v201WOE))

# > str(v201WOE)
# 'data.frame':	3826 obs. of  7 variables:
 # $ From    : int  5 5 5 5 5 5 5 5 5 5 ...
 # $ To      : int  3 3 3 3 3 3 3 3 3 3 ...
 # $ Variable: Factor w/ 12 levels " distance/distance_to_2",..: 1 1 1 1 1 2 2 2 2 2 ...
 # $ Lower   : int  0 100 300 400 500 0 100 300 400 500 ...
 # $ Upper   : int  100 300 400 500 3600 100 300 400 500 600 ...
 # $ Weight  : num  0.608 0.536 -0.133 -1.059 -1.704 ...
 # $ Din_cty : int  1 1 1 1 1 1 1 1 1 1 ...

# ----------------------------------------------
# VARIABLES
# ----------------------------------------------
# These are sorted alphabetically

# VARIABLE				V103	V105	V106   V107   V201
# "agprof_11" -			10		5                     
# "distance_to_2" - 	100 	100     100    100    100
# "distance_to_3" -             100     100    100    100
# "edAA_11" - 			10		                      
# "geology" - 					cat:1	cat:1  cat:1  cat:1
# "hden_11"	-			5		20                    
# "hval_11" -			25000	                      
# "medinc_11" - 		10000 	10000   10000  10000  10000
# "mines"						1                     
# "pcinc_11" -			5000	10000                 
# "poc_11" -			10		                      
# "popden_11" - 		100 	100     100    100    100
# "pov_11" - 			10		5       5      5      5
# "ppt" - 				100 	50      50            
# "prot_dist" - 		100 	500     500    500    500
# "prot_gap" - 					cat:1	cat:1  cat:1  cat:1
# "slope" - 			5 		2       2      2      2
# "soils" - 			cat:1 	cat:1   cat:1  cat:1  cat:1
# "tmax - 				5 		1       1             
# "tmin" - 				5 		1                     
# "travelt_11" - 		10 		5       5      5      5
# "zoning" -			cat		cat:1   cat:1  cat:1  cat:1
	
	
# ----------------------------------------------
#V201
v201inc <- c(100,100,1,10000,100,5,500,1,2,1,5,1)



# ----------------------------------------------
# ----------------------------------------------
# GET THE WOE FOR EACH VARIABLE
# ----------------------------------------------
# ----------------------------------------------
Transitions <- c(53,63,73,65,75,56,76,57,67)

fr <- substr(ft, 1,1)
to <- substr(ft, 2,2)


t_ft <- list()
uvar_ft <- list()
iter <- 1
for(ft in Transitions){
	print(ft)
	t_ft[[iter]] <- filter(v201WOE, v201WOE$From == substr(ft, 1,1) & v201WOE$To == substr(ft, 2,2))
	t_ft[[iter]]$Variable <- sub(".*/", "", t_ft[[iter]]$Variable)
	uvar_ft[[iter]] <- sort(unique(t_ft[[iter]]$Variable), decreasing = FALSE)#Find unique variables # These are actually the same for each region. That wasn't the case for counties since they were smaller?? Only need one for rest of analysis.

iter <- iter+1
}
names(t_ft) <- paste0("tr", Transitions)

uvar_ft <- uvar_ft[[1]]# Only need one for rest of analysis. (see notes in loop above)

cty_n <- list()
iter <- 1
for(cty in sa_ctyGEOID$Din_cty){
	print(cty)
	cty_n[[iter]] <- lapply(t_ft, filter, Din_cty==cty)
	# cty_4 <- filter(t_ft, t_ft$Din_cty == 4) 
iter <- iter+1
}
names(cty_n) <- paste0("Reg", sa_ctyGEOID$Din_cty)

# str(cty_n)
# End up with 8 lists of 2.
# List of 8 - each region
 # $ :List of 9 - each transition

cty_n <- unlist(cty_n, recursive=FALSE)
# str(cty_n)
	# List of 72
names(cty_n) <- gsub("[.]", "_", names(cty_n))

# names(cty_n)
 # [1] "Reg1_tr53" "Reg1_tr63" "Reg1_tr73" "Reg1_tr65" "Reg1_tr75" "Reg1_tr56" "Reg1_tr76" "Reg1_tr57"
 # [9] "Reg1_tr67" "Reg2_tr53" "Reg2_tr63" "Reg2_tr73" "Reg2_tr65" "Reg2_tr75" "Reg2_tr56" "Reg2_tr76"
# [17] "Reg2_tr57" "Reg2_tr67" "Reg3_tr53" "Reg3_tr63" "Reg3_tr73" "Reg3_tr65" "Reg3_tr75" "Reg3_tr56"
# [25] "Reg3_tr76" "Reg3_tr57" "Reg3_tr67" "Reg4_tr53" "Reg4_tr63" "Reg4_tr73" "Reg4_tr65" "Reg4_tr75"
# [33] "Reg4_tr56" "Reg4_tr76" "Reg4_tr57" "Reg4_tr67" "Reg5_tr53" "Reg5_tr63" "Reg5_tr73" "Reg5_tr65"
# [41] "Reg5_tr75" "Reg5_tr56" "Reg5_tr76" "Reg5_tr57" "Reg5_tr67" "Reg6_tr53" "Reg6_tr63" "Reg6_tr73"
# [49] "Reg6_tr65" "Reg6_tr75" "Reg6_tr56" "Reg6_tr76" "Reg6_tr57" "Reg6_tr67" "Reg7_tr53" "Reg7_tr63"
# [57] "Reg7_tr73" "Reg7_tr65" "Reg7_tr75" "Reg7_tr56" "Reg7_tr76" "Reg7_tr57" "Reg7_tr67" "Reg8_tr53"
# [65] "Reg8_tr63" "Reg8_tr73" "Reg8_tr65" "Reg8_tr75" "Reg8_tr56" "Reg8_tr76" "Reg8_tr57" "Reg8_tr67"

# ----------------------------------------------
# ----------------------------------------------
# CREATE BINNED .CSV FILES
# ----------------------------------------------
# ----------------------------------------------

for(Reg_tr in 1:length(cty_n)){
print(names(cty_n[Reg_tr]))
# Create matrix for each variable
mv <- list()
v_v <- 1
u <- list()
u_u <- 1
for(vars in uvar_ft){
	print(vars)
	print(paste(v_v, ':'))
	print(paste(u_u, ':'))
	mv[[v_v]] <- filter(cty_n[[Reg_tr]], Variable==vars)
	u[[u_u]] <- c(mv[[v_v]]$Lower, mv[[v_v]]$Upper)
	u[[u_u]] <- sort(unique(u[[u_u]], decreasing=FALSE))
	v_v <- v_v+1
	u_u <- u_u+1
	}
names(mv) <- (uvar_ft)

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
	mm[[m_m]] <- seq(from = mv[[r]]$Lower[i], mv[[r]]$Upper[i], length.out=((mv[[r]]$Upper[i]- mv[[r]]$Lower[i])/v201inc[[r]]+1)) #=1 for the zero #need to edit division based on number of bins for each variable. this is "u"
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
names(mat) <- uvar_ft



#select from ww, which is a list, the variables that match the ID in mat
for(v in 1:length(uvar_ft)){
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
sav201bins_tft <- list()
m_m <- 1
for(m in 1:length(mat)){
	print(paste(m_m, ':'))
	sav201bins_tft[[m_m]] <- full_join(mv[[m]], mat[[m]])
	m_m <- m_m+1
}
names(sav201bins_tft) <- uvar_ft



Output_Files_ft <- paste0("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2012/BasicDataAnalyses/sav2012allWOE_saBINS/", "WOEv2012_",names(cty_n[Reg_tr]), "_", names(sav201bins_tft), ".csv", sep="")

for(f in 1:length(sav201bins_tft)){
write.csv(sav201bins_tft[[f]], Output_Files_ft[[f]], row.names=FALSE)
}
 





# ----------------------------------------------
# READ ALL WOE FILES IN


sav201bins_tft <- lapply(Output_Files_ft, read.csv, check.names=FALSE)
sav201bins_tft_nms <- gsub(".csv", "",Output_Files_ft)
names(sav201bins_tft) <- sav201bins_tft_nms

# ----------------------------------------------
# PLOT TO PDF FILES
# ----------------------------------------------

# Make sure working directory is set to correct file


# pdf("V201_Forest2Dev; WOE per variable.pdf", paper="letter")

pdf(paste0(names(sav201bins_tft), ".pdf"), paper="letter")
par(mfrow=c(4,3),mai=c(0.5,0.5,0.1,0.2), width=8, height=10)
for(p in 1:length(sav201bins_tft)){


boxplot(sav201bins_tft[[p]][,-(1:8)], cex.axis=1)
abline(0,0, lty=2)
  title(main=paste0(names(cty_n[Reg_tr]),"-",sav201bins_tft[[p]]$Variable[1]), cex.main = 1)
  mtext("Weight of Evidence", side = 0.5, line = 1.5, cex = 0.5)
  mtext("Bin", side = 1, line = 3, cex = 0.5)
}
dev.off()
}



