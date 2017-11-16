############################
#PURPOSE: 
	# Assign value of "-100" to protected areas variables. See if this removes development or lc pressure from them. **Full Study Area**
	# Assign value of ""
#INPUT: Folder containing *recalculated* Dinamica weights of evidence tables with significance vals assigned.
#OUTPUT: A folder containing edited weights of evidence tables.
#Version: 5/30/2017 by Iara Lacher & Craig Fergus
#CONTACT: LacherI@si.edu
#NOTES: 

#IMPORTANT: *** EDIT INPUT FILES PER VERSION ***

############################################################################################
# ~~~ CODE BEGINS ~~~ #
############################################################################################

# --------------------------------------------------------
# --------------------------------------------------------
# Setting up file locations, and list of files 
# --------------------------------------------------------
# --------------------------------------------------------

# ----------------------------------------------
# FILE LOCATIONS *** EDIT THIS PER VERSION ***
# ----------------------------------------------


# ----------------------------------------------
#path to folder containing the tables that have significance
SA_Recalc_Tables_Folder <-"V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/WOE_recalc/"

# ----------------------------------------------
#path to folder to write the edited weights of evidence tables
SA_Output_Folder <- "V:/IaraSpatialLayers/SPProfessionalTrainingCourse/Dinamica_Runs/SMSC_107/smsc_1071/Parameters/WOE_recalc/LPA/"         


# ----------------------------------------------
# LIST OF FILES *** CHANGE TO FILE PATTERN NEEDED ***
# ----------------------------------------------

SA_Table_Files<- list.files(SA_Recalc_Tables_Folder,pattern = ".*v107.*\\csv$")

# --------------------------------------------------------
# --------------------------------------------------------
# ASSIGN WOE
# --------------------------------------------------------
# --------------------------------------------------------

# --------------------------------------------------------
# CHANGE WOE FOR PROTECTED LANDS
# --------------------------------------------------------
# # Assign GEOLOGY areas woe of -100, for transitions To* Developed (3)
# # See what's going on here:
# geology <- filter(sav107allWOE_sabuff, Variable.==" nlcdC_11/geology")
# Loudoun <- filter(geology, Din_cty=="rv107_WOE04")
# View(Loudoun)
# View(geology)
# # Assigning WOE of -1 because the highest I found is for the county above it at -0.7. So, I'm going to be just a bit more forceful.

  
# Assign protected areas woe of -100, for transitions To* Developed (3), Crops (7)
NoForTr <- c(3,7)

old <- Sys.time() #Time script

WOE_pg <- list()
t_t <- 1
for(f in 1:length(SA_Table_Files)){
  print(f)
  print(paste(t_t, ':'))

  WOE_pg[[t_t]] <- read.csv(paste0(SA_Recalc_Tables_Folder,SA_Table_Files[f]))
  WOE_pg[[t_t]]<-WOE_pg[[f]][,c(1:6)]
  WOE_pg[[t_t]]$Variable. <- as.character(WOE_pg[[f]]$Variable.) # Factor->Character
  for(t in NoForTr){
  print(t)
	# Protected Lands:
  try(WOE_pg[[t_t]][grepl("prot_gap_11", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==t,]$Weight<- -100)
 	# Geology:
  try(WOE_pg[[t_t]][grepl("geology", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==3,]$Weight<- -1)
  }

  names(WOE_pg[[t_t]]) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
  write.csv(WOE_pg[[t_t]],file = paste0(SA_Output_Folder,"pg",SA_Table_Files[[f]]),row.names = FALSE)
  t_t<-t_t+1
  }
new<-Sys.time()-old
print(new)
# Time difference of 17.841 secs 
 
 

 
 
 
 
# --------------------------------------------------------
# --------------------------------------------------------
# TEST with one file and one transition
# --------------------------------------------------------
# --------------------------------------------------------
 
# WOE_prot1 <- read.csv(paste(SA_Recalc_Tables_Folder,"/",SA_Table_Files[4],sep = ""))
# WOE_protX<-WOE_prot1[,c(1:6)]
# WOE_protX$Variable. <- as.character(WOE_protX$Variable.) # Factor->Character
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==3,]$Weight<- -100)
# try(WOE_protX[grepl("geology", WOE_protX$Variable.)& WOE_protX$To.==3,]$Weight<- -1)



