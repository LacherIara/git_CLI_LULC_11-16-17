############################
#PURPOSE: 
	# Change WOE manually where needed.
		# Protected lands
			# For the full study area, assign value of "-100" to protected areas variables with values 1,2,3 and value of 
			
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
SA_Recalc_Tables_Folder <-"V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/WOE_recalc/"

# ----------------------------------------------
#path to folder to write the edited weights of evidence tables
SA_Output_Folder <- "V:/IaraSpatialLayers/Dinamica_Runs/StudyArea_V201/SA_V2011/Parameters/WOE_recalc/LPA/"         

### TEST ###
       


# ----------------------------------------------
# LIST OF FILES *** CHANGE TO FILE PATTERN NEEDED ***
# ----------------------------------------------

SA_recTable_Files<- list.files(SA_Recalc_Tables_Folder,pattern = ".*v201.*\\csv$")

# --------------------------------------------------------
# --------------------------------------------------------
# ASSIGN WOE
# --------------------------------------------------------
# --------------------------------------------------------

# --------------------------------------------------------
# CHANGE WOE FOR PROTECTED LANDS & GEOLOGY
# --------------------------------------------------------
# - 7-14-17: 
# # Protected lands: change WOE for 56 and 57 to -3 for GAP 1,2,3;  +3 for GAP status =4 and 5. 
	# # Protected lands: change WOE for 53, 63, & 73 to -100 for all GAP 
	# # geology = 2 WOE of -3

	
	
# # See what's going on here:
# geology <- filter(sav201allWOE_sabuff, Variable.==" nlcdC_11/geology")
# Loudoun <- filter(geology, Din_cty=="rv201_WOE04")
# View(Loudoun)
# View(geology)




NoForTr <- c(3,7)

old <- Sys.time() #Time script

WOE_pg <- list()
t_t <- 1
for(f in 1:length(SA_recTable_Files)){

  print(f)
  print(paste(t_t, ':'))

  WOE_pg[[t_t]] <- read.csv(paste0(SA_Recalc_Tables_Folder,SA_recTable_Files[f]))
  WOE_pg[[t_t]]<-WOE_pg[[f]][,c(1:6)]
  WOE_pg[[t_t]]$Variable. <- as.character(WOE_pg[[f]]$Variable.) # Factor->Character
  for(t in NoForTr){
  print(t)
	# Protected Lands:
	try(WOE_pg[[t_t]][grepl("prot_gap_11", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==t,]$Weight<- -100) # no crop or development in protected lands
	try(WOE_pg[[t_t]][grepl("prot_gap_11", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==6 & WOE_pg[[f]]$Range_Lower_Limit.==4,]$Weight<- 3) #increase prob of conversion to grasses in GAP 4 
	try(WOE_pg[[t_t]][grepl("prot_gap_11", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==6 & WOE_pg[[f]]$Range_Lower_Limit.<4 & WOE_pg[[f]]$Range_Upper_Limit.<=4,]$Weight<- -3) # decrease prob of conversion to grasses in GAP 1-3
 	
	# Geology:
	try(WOE_pg[[t_t]][grepl("geology", WOE_pg[[f]]$Variable.)& WOE_pg[[f]]$To.==3 & WOE_pg[[f]]$Range_Lower_Limit.==2,]$Weight<- -3)
  
  }

  names(WOE_pg[[t_t]]) <- c(paste("From","*",sep=""),paste("To","*",sep=""),paste("Variable","*",sep="") ,paste("Range_Lower_Limit","*",sep=""), paste("Range_Upper_Limit","*",sep="" ),"Weight")
  write.csv(WOE_pg[[t_t]],file = paste0(SA_Output_Folder,"pg",SA_recTable_Files[[f]]),row.names = FALSE)
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
 
# WOE_prot1 <- read.csv(paste(SA_Recalc_Tables_Folder,"/",SA_recTable_Files[30],sep = ""))
# WOE_protX<-WOE_prot1[,c(1:6)]
# # 'data.frame':	456 obs. of  6 variables:
 # # $ From.             : int  5 5 5 5 5 5 5 5 5 5 ...
 # # $ To.               : int  3 3 3 3 3 3 3 3 3 3 ...
 # # $ Variable.         : Factor w/ 12 levels " distance/distance_to_2",..: 1 1 1 1 1 1 1 2 2 2 ...
 # # $ Range_Lower_Limit.: int  0 100 200 300 400 700 800 0 100 300 ...
 # # $ Range_Upper_Limit.: int  100 200 300 400 700 800 1700 100 300 400 ...
 # # $ Weight            : num  0.7181 0.3472 -0.0782 -0.5632 -0.842 ...
 
# WOE_protX$Variable. <- as.character(WOE_protX$Variable.) # Factor->Character
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==3,]$Weight<- -100)
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==6 & WOE_protX$Range_Lower_Limit.==4,]$Weight<- -3)
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==6 & WOE_protX$Range_Lower_Limit.<4 & WOE_protX$Range_Upper_Limit.<=4,]$Weight<- -3)

# try(WOE_protX[grepl("geology", WOE_protX$Variable.)& WOE_protX$To.==3 & WOE_protX$Range_Lower_Limit.==2,]$Weight<- -3)

# # Try targeting it only if the bin remains distinguished (i.e. held significance)
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.) & WOE_protX$Range_Lower_Limit.==2 & WOE_protX$Range_Upper_Limit.==4,]$Weight<- 15)



# ~~~
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==3,]$Weight<- -100)
# try(WOE_protX[grepl("prot_gap_11", WOE_protX$Variable.)& WOE_protX$To.==6,]$Weight<- -3)
# try(WOE_protX[grepl("geology", WOE_protX$Variable.)& WOE_protX$To.==3,]$Weight<- -1)



