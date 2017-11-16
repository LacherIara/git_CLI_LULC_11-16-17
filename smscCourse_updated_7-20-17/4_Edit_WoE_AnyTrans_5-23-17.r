
############################
#PURPOSE: Changing range classes on Dynamic Weights of evidence tables so that they only include significant range classes. 
#INPUT: Folder containing Dynamica weights of evidence tables, and folder containing significance tables.
#OUTPUT: A folder containing edited weights of evidence tables.
#Version: 5/30/2017 by Iara Lacher & Craig Fergus
#CONTACT: LacherI@si.edu
#NOTES: 
#	Takes seconds for full study area.
#	Update file folder and file pattern for each version / subversion


############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################



# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations and input files 
# --------------------------------------------------------
# --------------------------------------------------------

# ----------------------------------------------
# FILE LOCATIONS  *** EDIT THIS PER VERSION ***
# ----------------------------------------------

# ----------------------------------------------
#path to folder containing the tables that have significance
SA_Significance_Tables_Folder <-"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/WOE/"

# ----------------------------------------------
#path to folder to write the edited weights of evidence tables
SA_Output_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/WOE_adj/"         


# ----------------------------------------------
# READ INPUT FILES  *** CHANGE TO FILE PATTERN NEEDED ***
# ----------------------------------------------

SA_Table_Files<- list.files(SA_Significance_Tables_Folder,pattern = ".*v107.*\\csv$")

file <- 1
v <- 1

# --------------------------------------------------------
# --------------------------------------------------------
# Loop through input files
# --------------------------------------------------------
# --------------------------------------------------------

old <- Sys.time() #Time script

for(file in 1:length(SA_Table_Files)){
  print(SA_Table_Files[file])
  
  WOE_adj <- read.csv(paste0(SA_Significance_Tables_Folder,SA_Table_Files[file]))
  WOE_adj$Variable. <- as.character(WOE_adj$Variable.)
  
  saWOE_Edited <- data.frame(matrix(nrow = 1, ncol = 5))
  names(saWOE_Edited) <- names(WOE_adj)[1:5]
 
  WOE_adj$X<-paste(WOE_adj$Transition_From., WOE_adj$Transition_To., WOE_adj$Variable., sep="_")
  
  Params <- unique(WOE_adj$X) #need something for more than one transition... # remove ?

  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~
  row <- 1
  
  for(v in 1:length(Params)){
   	Par <- subset(WOE_adj,WOE_adj$X == Params[v], select = names(WOE_adj[c(1:5,10)])) # for each unique transition, select only columns that matter for next step
   	
    saSignifRV <- subset(Par,Par$Significant == 1)
    
    if(dim(saSignifRV)[1] > 0){
      for(n in 1:nrow(saSignifRV)){
          if(n < nrow(saSignifRV)){ # if each row (n) is less than the number of total rows..basically for all but the last row.
            saSignifRV[n,5] <- saSignifRV[n+1,4] # calls the lower range limit for each row except for the first. and assigns that value as the upper range limit. 
          }
        }
      saSignifRV[1,4] <- Par[1,4] # Min
      saSignifRV[nrow(saSignifRV),5] <- Par[nrow(Par),5] # Max -groups the non sig next classes with the previous sig class
    }else{                    # if there are no significant range classes
      saSignifRV <- Par[1,]
      saSignifRV[1,4] <- Par[1,4] # Min
      saSignifRV[1,5] <- Par[nrow(Par),5] # Max
    }
    
    saSignifRV <- saSignifRV[,-6]
    names(saSignifRV) <- names(saWOE_Edited)
    
    saWOE_Edited <- rbind(saWOE_Edited,saSignifRV)
     
  }
  
  saWOE_Edited <- saWOE_Edited[-1,]
  saWOE_Edited$Weight <- 0
  names(saWOE_Edited) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
  
  write.csv(saWOE_Edited,file = paste0(SA_Output_Folder,"s",SA_Table_Files[file]),row.names = FALSE)
}  
new<-Sys.time()-old
print(new)
 