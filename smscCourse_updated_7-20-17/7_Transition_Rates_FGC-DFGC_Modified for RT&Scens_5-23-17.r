############################ 
#PURPOSE: Take transition rate from seperate Dynamica steps and writes them to one .csv file
#INPUT:  Region layer and county table with "Din_cty" and "GEOID", and zonal statistics and histogram
#Version: 5/30/2017 by Iara Lacher & Craig Fergus
#CONTACT: LacherI@si.edu

#NOTES: 
# If you get an error saying the county was not found, restart R. There is a bug in the get() function

#IMPORTANT: 
# This contains code (currently blocked) that does not allow sum rate>1. This is the case if one LU type -> many, where rate can not be >1. Watch for this.
# Need to change TRANSITIONS for loop call so that it calls the name, not the list #


############################

# SET WORKING DIRECTORY
setwd("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/")
old2 <- Sys.time()

# ----------------------------------------------
################################################

# PACKAGES NEEDED

library(plyr) # General data manipulation
# detach("package:dplyr") # library(dplyr) #cannot have dplyr bc removes the function summarise in plyr... 
library(reshape2) # Flexibly restructure and aggregate data


# ----------------------------------------------
# READ OUTPUT FILES:
# sa_Trans_Rates_FGCDDFGC<-read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/PatchesTransitions_SA/Transition_Rates/sa_Trans_Rates_FGCDDFGC.csv")



# ----------------------------------------------
# READ INPUT FILES:
# regions <- raster("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/cnty_smsc.img")
# sa_ctyGEOID<- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/PatchesTransitions_SA/FullGEOID.csv")
# sa_cty_transitions<-read.table("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/PatchesTransitions_SA/Combine/Tables/SAnlCcomb_0111.txt", header=TRUE,sep=",")
# sa_zonal_T0<-read.table("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/PatchesTransitions_SA/nlcd11_smscC_hist.txt", header=TRUE,sep=",")


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################
  

# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations, counties raster, areas and transitions 
# --------------------------------------------------------
# --------------------------------------------------------


# ----------------------------------------------
# COUNTIES
# ----------------------------------------------


smsc_ctyGEOID<- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/SMSC_GEOID.csv")
colnames(smsc_ctyGEOID)<-c("Din_cty", "GEOID")
str(smsc_ctyGEOID)


# ----------------------------------------------
# AREAS
# ----------------------------------------------

smsc_cty_transitions<-read.table("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Combine/Tables/SMSC_nlCcomb_0111.txt", header=TRUE,sep=",")##CF- from previous script
smsc_cty_transitions<-smsc_cty_transitions[,-1] ##CF - removes rowid column

# remove persistant land use.
ssmsc_cty_transitions<-subset(smsc_cty_transitions, smsc_cty_transitions$LABEL %in% c("53","63","73","65","75","56","76","57","67"))


smsc_zonal_T0<-read.table("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/nlcd11c_smsc_hist.txt", header=TRUE,sep=",")
smsc_zonal_T0<-smsc_zonal_T0[,-1]

# ----------------------------------------------
# TRANSITIONS
# ----------------------------------------------
  
trans_that_matter <- c(53,63,73,65,75,56,76,57,67)
tsteps <- c(5) # The number of time steps required to reach 50years.  If 2001-2011 then timesteps = 5, if 2001-2006 or 2006-2011 then timesteps = 10

cnames <- names(smsc_cty_transitions[2:25]) #gets the name of each county

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


Modifier_Q1 <- read.csv("V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Transition_Rates/Rate_Modifier_Q1_CtysEqual.csv")



################MODIFY RATES BY STATE (for different scenarios)################# # remove ?
cnames <- names(smsc_cty_transitions[2:25]) #gets the name of each county
regioncols <- grep(x = names(smsc_cty_transitions),pattern = "GEOID_")
# > cnames
# [1] "GEOID_11001" "GEOID_51029"
# > regioncols
# [1] 2 3

modify.rate <- function(Transitions,Regions,ModifiersDF,TransitionDF){
  outdf <- data.frame(cbind(Transitions,sapply(Regions,function(x) unlist(lapply(Transitions, function(y) TransitionDF[which(ModifiersDF[["Transition_Number"]] == y),x] * ModifiersDF[which(ModifiersDF[["Transition_Number"]] == y),x])))))
  names(outdf) <- c("LABEL",names(smsc_cty_transitions)[Regions])
  return(outdf)
}

# Run these one at a time. 
new_rates <- modify.rate(Modifier_Q1$Transition_Number, regioncols, Modifier_Q1,smsc_cty_transitions)



# ----------------------------------------------
# ----------------------------------------------
# MODIFY RATES BY COUNTY # check 
# ----------------------------------------------
# ----------------------------------------------

tsteps <- 5

# This version does not assign rate of 1 to anything
TRdf <-  data.frame()  #create empty dataframe to write to
WarningDF <- data.frame(County = NA, Timestep= NA, Landuse = NA) # create empty data frame that will save warnings when land use area is used up

for(county in 1:24){ 
# for(county in 3){ 


  cty <- new_rates[,c(1,county + 1)]; RT_CTY <-cty
  # print(RT_CTY)
  a0 <- smsc_zonal_T0[,c(1,county + 1)]
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
  
  
  TRdf <- rbind(TRdf,melt(out,id = c("From","To","County")))
  
  
}



levels(TRdf$variable) <- c(1:tsteps)
TRdf$GEOID <- as.numeric(substring(TRdf$County,7,12)) #create GEOID column
TRdf <- join(TRdf,smsc_ctyGEOID, by = "GEOID")
names(TRdf) <- c("From*", "To*","County","Year*","Rate","GEOID","Region*")


FINAL <- TRdf[c("Year*","Region*","From*","To*","Rate")]
options(scipen = 999)
FINAL[,1] <- as.numeric(FINAL[,1])
FINAL[,2] <- as.numeric(FINAL[,2])
FINAL$Rate <- as.numeric(substr(as.character(FINAL$Rate),1,10))




write.csv(FINAL, file="V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/Transition_Rates/smsc_Trans_Rates_FGC-DFGC_Q1.csv",row.names=FALSE, quote=FALSE)
# write.csv(WarningDF,"V:/IaraSpatialLayers/Dinamica_Runs/PatchesTransitions_SA/SA_V107/Transition_Rates/Warnings.csv")



new2<-Sys.time()-old2

###CF notes

# replace Y:/Lacher/IaraSpatialLayers_HF/PreparedRasters/StudyAreaBndy with V:/IaraSpatialLayers/SPProfessionalTrainingCourse/GIS/SpatialData/PreparedRasters/IndCntys

#replace Y:/Lacher/Dinamica_Runs/PatchesTransitions_SA with V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/PatchesTransitions_SA

#replace "_an" with "_smsc"
#replace "nlcd11_anC" with "nlcd11C_smsc"
