############################ 
#PURPOSE: Take transition rate from seperate Dynamica steps and writes them to one .csv file
#INPUT:  Region layer and county table with "Din_cty" and "GEOID", and zonal statistics and histogram
#OUTPUT: Table of transition rates
#DEVELOPED: (HFv1) 10/7/2014 - (HFv3.1) 3/10/2016 Written by Luca L. Morreale & Josh Plisinski at Harvard Forest
#           (SIv1) 3/10/2016 - IL: Changed the timesteps so that the first projected year now represents the same rate, but an increase in area from the original nlcd 2001 v 2011 raster comparison. Changed the "TRANSITIONS" matrices to account for additions and losses in area for the same landcover class.
#			(SIv2) 5/20/2016 - Valentine: Modified code to use histogram tables created in R script. Modified to apply to a three year transition period.
#			(SIv2.1) 6/2/2016 - IL: Added additional transitions using new landcover reclassification scheme
#			(SIv2.2) 8/18/2017 - CF: Changed to iterate through regions of grouped counties
#CONTACT: LacherI@si.edu, HerrmannV@si.edu, lmorreale@fas.harvard.edu

#NOTES: 
# If you get an error saying the county was not found, restart R. There is a bug in the get() function
# This version has blocked sections that are for scenario adjustments
# Bizzarro issue - Dinamica would not read the table in correctly until I copy-pasted the same column headings from the lilliput model. NO IDEA WHY. 

#IMPORTANT: 
# This contains code (currently blocked) that does not allow sum rate>1. This is the case if one LU type -> many, where rate can not be >1. Watch for this.
# Need to change TRANSITIONS for loop call so that it calls the name, not the list #

##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER


# ----------------------------------------------
################################################

# PACKAGES NEEDED

library(plyr) # General data manipulation
# library(dplyr) # General data manipulation
# detach("package:dplyr") # library(dplyr) #cannot have dplyr bc removes the function summarise in plyr... 
library(reshape2) # Flexibly restructure and aggregate data




# ----------------------------------------------
# READ OUTPUT FILES:
#sa_Trans_Rates_FGCDFGC<-read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Transition_Rates/sa_Trans_Rates_FGC-DFGC.csv")
	# names(sa_Trans_Rates_FGCDFGC) <- c("Year*","Region*","From*","To*","Rate")


# str(sa_Trans_Rates_FGCDFGC)
# 'data.frame':	2610 obs. of  5 variables:
 # $ Year.  : int  1 1 1 1 1 1 1 1 1 2 ...
 # $ Region.: int  1 1 1 1 1 1 1 1 1 1 ...
 # $ From.  : int  5 5 5 6 6 6 7 7 7 5 ...
 # $ To.    : int  3 6 7 3 5 7 3 5 6 3 ...
 # $ Rate   : num  0.022 0 0 0 0 0 0.684 0 0 0.023 ...



# ----------------------------------------------
# INPUT FILES:
# regions <- raster("Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy/cnty_an.img")
# sa_ctyGEOID<- read.csv("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/FullGEOID.csv")
# sa_cty_transitions<-read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/Combine/Tables/SAnlCcomb_0111.txt", header=TRUE,sep=",")
# sa_zonal_T0<-read.table("Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA/nlcd11_anC_hist.txt", header=TRUE,sep=",")


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################
  

# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations, counties raster, areas and transitions 
# --------------------------------------------------------
# --------------------------------------------------------


# ----------------------------------------------
# REGIONS
# ----------------------------------------------

sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/FullGEOID.csv")

colnames(sa_ctyGEOID)<-c("Din_cty", "GEOID")
str(sa_ctyGEOID)


# ----------------------------------------------
# ----------------------------------------------
# AREAS # *WITH PROTECTED AREAS
# ----------------------------------------------
# ----------------------------------------------


# # ----------------------------------------------
# # USE FOR NLCD 2001-2011:
# # ----------------------------------------------


sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/SA_nlCcomb_0111.txt", header=TRUE,sep=",")
sa_cty_transitions<-sa_cty_transitions[,-1]
# remove persistant land use.
sa_cty_transitions<-subset(sa_cty_transitions, sa_cty_transitions$LABEL %in% c("53","63","73","65","75","56","76","57","67"))

# With "Lumber" Modifier
sa_cty_transitions_lum<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/SA_nlCcomb_0111_LUMBER.txt", header=TRUE,sep=",")##CF- from previous script
# note: unlike the tables above, don't need to remove first column bc of how this one was written... #CF as of 08/18/17 this note is no longer true so next lline of code added
sa_cty_transitions_lum<-sa_cty_transitions_lum[,-1]
# remove persistant land use.
sa_cty_transitions_lum<-subset(sa_cty_transitions_lum, sa_cty_transitions_lum$LABEL %in% c("53","63","73","65","75","56","76","57","67"))

sa_cty_transitions <- sa_cty_transitions_lum


sa_zonal_T0<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Combine/Tables/nlcd11_anC_hist.txt", header=TRUE,sep=",")
sa_zonal_T0<-sa_zonal_T0[,-1]



# > sa_zonal_T0
  # LABEL GEOID_11001 GEOID_51029 GEOID_51069 GEOID_51107 GEOID_51109 GEOID_51171 GEOID_24021
# 1     3        6680        4541       59327      165454       10189       33787       94172
# 2     5        1854      836830      676912      487519      749397      905844      259890
# 3     6           0      214341      350977      548587      182193      420202      271118
# 4     7          32        3196       17318      108809       28807       29549      158318
  

# ----------------------------------------------
# USE FOR NLCD 2001-2006:
# ----------------------------------------------

 # sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Combine/Tables/SA_nlCcomb_0106.txt", header=TRUE,sep=",")
 # sa_cty_transitions<-sa_cty_transitions[,-1] # removes rowid column
 # # remove persistant land use.
 # sa_cty_transitions<-subset(sa_cty_transitions, sa_cty_transitions$LABEL %in% c("53","63","73","65","75","56","76","57","67"))

 # Do I use 2011 as a startng point or 2006?? I'm going to say, 2006.
 # #2006
 # sa_zonal_T0<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Combine/Tables/nlcd06_anC_hist.txt", header=TRUE,sep=",")
 # sa_zonal_T0<-sa_zonal_T0[,-1]


# # ----------------------------------------------
# # USE FOR NLCD 2006-2011:
# # ----------------------------------------------

# sa_cty_transitions<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Combine/Tables/SA_nlCcomb_0611.txt", header=TRUE,sep=",")
# sa_cty_transitions<-sa_cty_transitions[,-1] # removes rowid column
# # remove persistant land use.
# sa_cty_transitions<-subset(sa_cty_transitions, sa_cty_transitions$LABEL %in% c("53","63","73","65","75","56","76","57","67"))

# sa_zonal_T0<-read.table("V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Combine/Tables/nlcd11_anC_hist.txt", header=TRUE,sep=",")
# sa_zonal_T0<-sa_zonal_T0[,-1]



# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------
# Remember to use nlcd landcover "RCc" data WITH protected lands (PL=NA)

trans_that_matter <- c(53,63,73,65,75,56,76,57,67)
tsteps <- c(5) # The number of time steps required to reach 50years.  If 2001-2011 then timesteps = 5, if 2001-2006 or 2006-2011 then timesteps = 10

cnames <- names(sa_cty_transitions[2:9]) #gets the name of each county

# For TRANSITIONS, cannot have landuse appear more than once in dec column.
TRANSITIONS <- list("3" =data.frame( inc = c(53,63,73), dec = NA),"5" = data.frame(inc = c(65,75,NA),dec = c(53,56,57)), "6" = data.frame(inc = c(56,76,NA),dec = c(63,65,67)), "7" = data.frame(inc = c(57,67,NA),dec = c(73,75,76)))


#This is a somewhat confusing input (looking for a better way) # remove ?

FromToCW <- data.frame(OBJECTID = trans_that_matter, From = c(5,6,7,6,7,5,7,5,6),To = c(3,3,3,5,5,6,6,7,7),
                       ToLU = c("Development","Development","Development","Forest","Forest","Grasses","Grasses","Crop","Crop"), Modifier = c("F.D","G.D","C.D","G.F","C.F","F.G","C.G","F.C","G.C") ) # crosswalk of landuse value to From/To

# > FromToCW
  # OBJECTID From To        ToLU Modifier
# 1       53    5  3 Development      F.D
# 2       63    6  3 Development      G.D
# 3       73    7  3 Development      C.D
# 4       65    6  5      Forest      G.F
# 5       75    7  5      Forest      C.F
# 6       56    5  6     Grasses      F.G
# 7       76    7  6     Grasses      C.G
# 8       57    5  7        Crop      F.C
# 9       67    6  7        Crop      G.C				   





# ----------------------------------------------
# ----------------------------------------------
#  RATES BY COUNTY # check 
# ----------------------------------------------
# ----------------------------------------------

# This version does not assign rate of 1 to anything
TRdf <-  data.frame()  #create empty dataframe to write to
WarningDF <- data.frame(County = NA, Timestep= NA, Landuse = NA) # create empty data frame that will save warnings when land use area is used up


# ----------------------------------------------
# loop through each county
# ----------------------------------------------



for(county in 1:8){ 
# for(county in 3){ 


  cty <- sa_cty_transitions[,c(1,county + 1)]; RT_CTY <-cty
  # print(RT_CTY)
  a0 <- sa_zonal_T0[,c(1,county + 1)]
  area <- as.data.frame(matrix(NA,dimnames = list(NULL,as.character(a0[,1])), nrow = tsteps + 1, ncol = 4, byrow =T)); area[1,] <- a0[,2] 
  trigger <- 0
  
  for(time in seq_len(tsteps)){
    # Need to change TRANSITIONS for loop call so that it calls the name, not the list # remove ?
    
    for(t in 1:length(TRANSITIONS)){
      dec <- TRANSITIONS[[t]][2]; inc <- TRANSITIONS[[t]][1]
      decdf <- cty[cty$LABEL %in% unlist(dec),]; incdf <- cty[cty$LABEL %in% unlist(inc),]
	   # modifier <- ddply(decdf, .(LABEL),summarise,  Sum = sum(get(cnames[county]))); modifier$Prop <- modifier$Sum / sum(modifier$Sum)
      Tdec <- sum(decdf[,2], na.rm = T); Tinc <- sum(incdf[,2], na.rm = T)
      tempA <- area[time,t] - Tdec 
      
      
	   if (area[time ,t] != 0){
        if(tempA >= 0) { 
          newA <- tempA + Tinc 
          area[time + 1,t] <- newA
        }else{
          area[(time+1):nrow(area),t] <- 0
	  
          # cty[which(cty$LABEL %in% modifier$LABEL), cnames[county]]  <- modifier$Prop * area[time, t]
          #area[(time+1):11,which(names(area) %in% modifier$ToLU)] <- mapply('-',area[(time + 1):11,which(names(area) %in% modifier$ToLU)], modifier$Sum)
          #lapply(as.character(modifier$ToLU), function(LU)  area[time+1,LU] - modifier[which(modifier$ToLU == LU),"Sum"])
		   trigger <- decdf$LABEL
          WarningDF <- rbind(WarningDF,c(cnames[county],time,names(TRANSITIONS[t]))) ## writes to a csv the year/timestep/county if a landuse is used up
        }
      }else if(area[time,t] == 0 & time == 1){
        newA <- tempA + Tinc
        if(newA == 0){
          area[(time+1):nrow(area),t]  <- 0  
        }else{
          area[time+1,t] <- newA
        }
      }
	   
      
      }
	      if(trigger[1] != 0) cty[which(cty$LABEL %in% trigger), cnames[county]] <- 0

      }
    
  area$Total <- rowSums(area)
  TR <- ldply(TRANSITIONS)
  TR <- merge(TR,RT_CTY, by.x = 'dec',by.y = 'LABEL'); names(TR) <- c("TR", "LandUse","OID","CTY") # I remember that I have come across this question before. Why just the decreasing column??
		   # TR <- full_join(TR,RT_CTY, by = 'LABEL'); names(TR) <- c("TR", "LandUse","OID","CTY") # try full_join instead?
  TR <- ddply(TR, c("LandUse"),summarise, TR  = TR ,TransArea = CTY,prop = CTY/sum(CTY)) # must unload dlpyr to use summarise function
    
  rates <- ldply(mapply(y = TR$TransArea, id = TR$LandUse, prop = TR$prop, function(y,id,prop) {
    lapply(area[id],function(x) ifelse(x == 0, 0,ifelse(sum(TR[TR$LandUse == id,"TransArea"]) < x, y/x, (x * prop) / x )))   }))
  
  rates$OBJECTID <- TR$TR; rates$County <- cnames[county]; rates <- join(rates,FromToCW,by = "OBJECTID") 
  
  out <- cbind(rates[,3:(tsteps+2)], rates[c("County","From","To")])# the 2nd col of rates is year0 (current time). the third is year1.
  colnames(out)<-c("V1", "V2", "V3", "V4", "V5", "County", "From", "To") #this sets year 1 to the rate that represents an incremental increase from the year before. a more conservative approach would be to leave the rate for the first projected year as equal to the difference between nlcd 2001-2011.
  
   write.csv(area, file=paste0("V:/IaraSpatialLayers/Dinamica_Runs/", county, time, t, "test.csv"),row.names=FALSE, quote=FALSE)

  TRdf <- rbind(TRdf,melt(out,id = c("From","To","County")))
   
# write... insert county name.. in output...    
	
}

 

levels(TRdf$variable) <- c(1:tsteps)
TRdf$GEOID <- as.numeric(substring(TRdf$County,7,12)) #create GEOID column
TRdf <- join(TRdf,sa_ctyGEOID, by = "GEOID")
names(TRdf) <- c("From*", "To*","County","Year*","Rate","GEOID","Region*")

FINAL <- TRdf[c("Year*","Region*","From*","To*","Rate")]
options(scipen = 999)
FINAL[,1] <- as.numeric(FINAL[,1])
FINAL[,2] <- as.numeric(FINAL[,2])
FINAL$Rate <- as.numeric(substr(as.character(FINAL$Rate),1,10))


# 2001-2011 with 2006 averages for "lumber counties"
write.csv(FINAL, file="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0111_LUMBER.csv",row.names=FALSE, quote=FALSE) #corrected for year zero

# 2001-2011:
#write.csv(FINAL, file="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0111.csv",row.names=FALSE, quote=FALSE) #corrected for year zero

# # 2001-2006:
# write.csv(FINAL, file="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0106.csv",row.names=FALSE, quote=FALSE) #corrected for year zero

# # 2006-2011:
# write.csv(FINAL, file="V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V107/SA_V1071/Parameters/Transition_Rates/sa_Trans_Rates_FGC-DFGC_0611.csv",row.names=FALSE, quote=FALSE) #corrected for year zero