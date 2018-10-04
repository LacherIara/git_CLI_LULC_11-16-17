############################ 
#PURPOSE: Calculate the development density per class and zone. 
#INPUT: R workspace for JAGS model build.
#OUTPUT: 
#DEVELOPED: 
#CONTACT: LacherI@si.edu
#NOTES:
# As of 6-7-18, it works like this:
# - the JAGS model runs on the 2001-2011 data
# - the same JAGS model is then used to predict future development using:
# 1) population projections extrapolated from that of the Weldon Cooper center and other state agencies
# 2) the population projections from #1 that have been altered to illustrate increases of some sort.
# - as of 6-7-18, these population increases are a 24% increase. (see notes for details)

#IMPORTANT: *MUST READ*
# **!! this code needs a clean.
# Population estimates from both 1 and 2 above need to be scaled *together* before predict is run through them.
# the code below does this, but then you have to select either 1 or 2 and run the predict funciton on it
# this requires you to replace the name, and addresses where files are saved and read from for *every year*
# I want to find a way to make this cleaner... although not an immediate priority.


##### NEXT STEPS #####

############################

# ----------------------------------------------
# OPEN R WORKSPACE
# ----------------------------------------------

# **!! CHANGE ADDRESS TO REFLECT NEW FOLDER!
# save.image("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogGr_Region_5-18-18b.RData")


###########################################
# ~~~ CODE BEGINS ~~~ #
###########################################

# ----------------------------------------------
# ----------------------------------------------
# PREP FOR DEVELOPMENT IN SUBSEQUENT DECADES
# ----------------------------------------------
# ----------------------------------------------
# ----------------------------------------------
# PREP POPULATION DATA
# ----------------------------------------------

# Create Yearly Population Predictions (includes value for each region) 
# Pop for 2001 is loaded above as just Pop
# Each population projection provides information for the following year.

# SCALING POPULATION
 
 ### Create Yearly Population Predictions (includes value for each region) ##Pop for 2001 is loaded above as just Pop
 # Each population projection provides information for the following year.
 # Population=as.numeric(scale(Pop))
 
  # # Population Estimates:
  # WC proj:
  PopProj_o<-read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections/RegionalPopProjections_10-19-17.csv")
 prPop_o <- as.numeric(PopProj_o$prPop)
 Pop_o <- prPop_o[1:8] # Population for 2001. 
 

 # > Pop60
# [1] -0.3929014  2.3136572 -0.5768168 -0.3608829 -0.3822443 -0.6428076 -0.4634970  0.5054927
 
 # # # # # 24% increase:
 PopProj_h<-read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections/RegionalPopProjections_High_03_29_18.csv") # These are a 24% increase of the original population projections extrapolated from the counties.
 prPop_h <- as.numeric(PopProj_h$prPop)
 Pop_h <- prPop_h[1:8] # Population for 2001. 
# Pop60
 # [1] -0.3951152  2.3045441 -0.5830426 -0.3578568 -0.3836042 -0.6452534 -0.4696540  0.5299822

 
 Pop10_o <- prPop_o[9:16] # 10 years out - Population for 2011. 
 Pop20_o <- prPop_o[17:24]# 20 years out - Population for 2021. 
 Pop30_o <- prPop_o[25:32]# 30 years out - Population for 2031. 
 Pop40_o <- prPop_o[33:40]# 40 years out - Population for 2041.  
 Pop50_o <- prPop_o[41:48]# 50 years out - Population for 2051.  
 Pop60_o <- prPop_o[49:56]# 50 years out - Population for 2061. 
 
 Pop10_h <- prPop_h[9:16] # 10 years out - Population for 2011. 
 Pop20_h <- prPop_h[17:24]# 20 years out - Population for 2021. 
 Pop30_h <- prPop_h[25:32]# 30 years out - Population for 2031. 
 Pop40_h <- prPop_h[33:40]# 40 years out - Population for 2041.  
 Pop50_h <- prPop_h[41:48]# 50 years out - Population for 2051.  
 Pop60_h <- prPop_h[49:56]# 50 years out - Population for 2061.
 

 # test <- as.numeric(scale(c(Pop60_o, Pop60_h)))
  # > test
 #-0.4261071  2.2074175 -0.6050597 -0.3949526 -0.4157375 -0.6692698 -0.4947977  0.4480447 
 # -0.3875217  2.5595749 -0.5926736 -0.3468484 -0.3749557 -0.6605863 -0.4688924  0.6223655
 
  temp10 <- as.numeric(scale(c(Pop10_o, Pop10_h)))
  temp20 <- as.numeric(scale(c(Pop20_o, Pop20_h)))
  temp30 <- as.numeric(scale(c(Pop30_o, Pop30_h)))
  temp40 <- as.numeric(scale(c(Pop40_o, Pop40_h)))
  temp50 <- as.numeric(scale(c(Pop50_o, Pop50_h)))
  temp60 <- as.numeric(scale(c(Pop60_o, Pop60_h)))

   # # WC proj:
 # Pop10 <- temp10[1:8]
 # Pop20 <- temp20[1:8]
 # Pop30 <- temp30[1:8]
 # Pop40 <- temp40[1:8]
 # Pop50 <- temp50[1:8]
 # Pop60 <- temp60[1:8]
  
   # # # # # 24% increase:
 Pop10 <- temp10[9:16]
 Pop20 <- temp20[9:16]
 Pop30 <- temp30[9:16]
 Pop40 <- temp40[9:16]
 Pop50 <- temp50[9:16]
 Pop60 <- temp60[9:16]
  
  
  
 
# ----------------------------------------------
# ----------------------------------------------
 ### *** START ON ACTUAL 2011 PERCENT DEV.***
# ----------------------------------------------
# ----------------------------------------------


Nt <- Nt1 <- N2011  # Create new data objects to overwrite with predicted data


 
 for (i in 1:length(N2001)){    # Loop over observations
     log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop10[Region[i]] +
         beta3.Rm*(Class[i]==2) + beta4.Rm*(Class[i]=3)  # Log-scale growth rates
     Rm[i]=exp(log.Rm[i])   # Take exponential to get original-scale growth rates
 }
 
 for (t in 2:11){    # Loop over years
     for (i in 1:length(N2001)){   # Loop over all zones
         Nt1[i] <- Nt[i]+Rm[i]*Nt[i]*((K[Region[i]]-Nt[i])/K[Region[i]])  # Predict % development based on logistic growth function
     }
     Nt <- Nt1   # Overwrite % development for time t-1 to serve as baseline for next time step
 }
 
 bind <- cbind(CLI_Urban[c("Region", "Urb_Class", "ZoneID")], log.Rm, Rm, Nt1)
 colnames(bind) <- c("Region", "Urb_Class", "ZoneID","LogGrRate", "OrigGrRate", "PredPercDev")
 bind2 <- bind[!duplicated(bind), ]
 bind2$Year <- "2021"
 
 write.csv(bind2, "U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs10_5-18-18.csv", row.names = F)
 
 
 #20
 # Population=as.numeric(scale(Pop20))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs10_5-18-18.csv") #Grab previous years output to form new baseline
 NinD<- Nin$PredPercDev
 Nt <- Nt1 <- NinD  # Create new data objects to overwrite with predicted data
 
 for (i in 1:length(N2001)){    # Loop over observations
     log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop20[Region[i]] +
         beta3.Rm*(Class[i]==2) + beta4.Rm*(Class[i]=3)  # Log-scale growth rates
     Rm[i]=exp(log.Rm[i])   # Take exponential to get original-scale growth rates
 }
 
 for (t in 2:11){    # Loop over years
     for (i in 1:length(N2001)){   # Loop over all zones
         Nt1[i] <- Nt[i]+Rm[i]*Nt[i]*((K[Region[i]]-Nt[i])/K[Region[i]])  # Predict % development based on logistic growth function
     }
     Nt <- Nt1   # Overwrite % development for time t-1 to serve as baseline for next time step
 }
 
 bind <- cbind(CLI_Urban[c("Region", "Urb_Class", "ZoneID")], log.Rm, Rm, Nt1)
 colnames(bind) <- c("Region", "Urb_Class", "ZoneID","LogGrRate", "OrigGrRate", "PredPercDev")
 bind2 <- bind[!duplicated(bind), ]
 bind2$Year <- "2031"
 
 write.csv(bind2, "U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs20_5-18-18.csv", row.names = F)
 
 #30
 # Population=as.numeric(scale(Pop30))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs20_5-18-18.csv") #Grab previous years output to form new baseline
 NinD<- Nin$PredPercDev
 Nt <- Nt1 <- NinD  # Create new data objects to overwrite with predicted data
 
 for (i in 1:length(N2001)){    # Loop over observations
     log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop30[Region[i]] +
         beta3.Rm*(Class[i]==2) + beta4.Rm*(Class[i]=3)  # Log-scale growth rates
     Rm[i]=exp(log.Rm[i])   # Take exponential to get original-scale growth rates
 }
 
 for (t in 2:11){    # Loop over years
     for (i in 1:length(N2001)){   # Loop over all zones
         Nt1[i] <- Nt[i]+Rm[i]*Nt[i]*((K[Region[i]]-Nt[i])/K[Region[i]])  # Predict % development based on logistic growth function
     }
     Nt <- Nt1   # Overwrite % development for time t-1 to serve as baseline for next time step
 }
 
 bind <- cbind(CLI_Urban[c("Region", "Urb_Class", "ZoneID")], log.Rm, Rm, Nt1)
 colnames(bind) <- c("Region", "Urb_Class", "ZoneID","LogGrRate", "OrigGrRate", "PredPercDev")
 bind2 <- bind[!duplicated(bind), ]
 bind2$Year <- "2041"
 
write.csv(bind2, "U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs30_5-18-18.csv", row.names = F)
 
 
 #40
 # Population=as.numeric(scale(Pop40))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs30_5-18-18.csv") #Grab previous years output to form new baseline
 NinD<- Nin$PredPercDev
 Nt <- Nt1 <- NinD  # Create new data objects to overwrite with predicted data
 
 for (i in 1:length(N2001)){    # Loop over observations
     log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop40[Region[i]] +
         beta3.Rm*(Class[i]==2) + beta4.Rm*(Class[i]=3)  # Log-scale growth rates
     Rm[i]=exp(log.Rm[i])   # Take exponential to get original-scale growth rates
 }
 
 for (t in 2:11){    # Loop over years
     for (i in 1:length(N2001)){   # Loop over all zones
         Nt1[i] <- Nt[i]+Rm[i]*Nt[i]*((K[Region[i]]-Nt[i])/K[Region[i]])  # Predict % development based on logistic growth function
     }
     Nt <- Nt1   # Overwrite % development for time t-1 to serve as baseline for next time step
 }
 
 bind <- cbind(CLI_Urban[c("Region", "Urb_Class", "ZoneID")], log.Rm, Rm, Nt1)
 colnames(bind) <- c("Region", "Urb_Class", "ZoneID","LogGrRate", "OrigGrRate", "PredPercDev")
 bind2 <- bind[!duplicated(bind), ]
 bind2$Year <- "2051"
 
 write.csv(bind2, "U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs40_5-18-18.csv", row.names = F)
 
 
 #50
 # Population=as.numeric(scale(Pop50))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs40_5-18-18.csv") #Grab previous years output to form new baseline
 NinD<- Nin$PredPercDev
 Nt <- Nt1 <- NinD  # Create new data objects to overwrite with predicted data
 
 for (i in 1:length(N2001)){    # Loop over observations
     log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop50[Region[i]] +
         beta3.Rm*(Class[i]==2) + beta4.Rm*(Class[i]=3)  # Log-scale growth rates
     Rm[i]=exp(log.Rm[i])   # Take exponential to get original-scale growth rates
 }
 
 for (t in 2:11){    # Loop over years
     for (i in 1:length(N2001)){   # Loop over all zones
         Nt1[i] <- Nt[i]+Rm[i]*Nt[i]*((K[Region[i]]-Nt[i])/K[Region[i]])  # Predict % development based on logistic growth function
     }
     Nt <- Nt1   # Overwrite % development for time t-1 to serve as baseline for next time step
 }
 
 bind <- cbind(CLI_Urban[c("Region", "Urb_Class", "ZoneID")], log.Rm, Rm, Nt1)
 colnames(bind) <- c("Region", "Urb_Class", "ZoneID","LogGrRate", "OrigGrRate", "PredPercDev")
 bind2 <- bind[!duplicated(bind), ]
 bind2$Year <- "2061"
 
 write.csv(bind2, "U:/CLI/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs/Original_FutPop/LogRegOutputs50_5-18-18.csv", row.names = F)
 
###########################################
# ~~~ CODE ENDS ~~~ #
###########################################
