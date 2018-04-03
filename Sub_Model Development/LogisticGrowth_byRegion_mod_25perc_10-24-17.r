########################### 
#PURPOSE: Calculate the development density per class and zone. 
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
	# 10-24-17: Edited to improve model fit and convergence **
#CONTACT: LacherI@si.edu
#NOTES:
# Saved workspace here: > save.image("V:/IaraSpatialLayers/Dinamica_Runs/AAA_RScript/JAGS_Workspace_10-20-17.RData")

#IMPORTANT: 
	
##### NEXT STEPS #####

############################

# SET WORKING DIRECTORY
# setwd("Y:/Lacher/...") #Harvard CLUSTER
# setwd("Y:/Lacher/VarInSDM") #Harvard CLUSTER
# setwd("I:/...") #I Drive 


# ----------------------------------------------
###########################################
 # PACKAGES NEEDED
 library(plyr)
 library(jagsUI) # (must have downloaded and installed JAGS, see: https://faculty.washington.edu/jmiyamot/p548/installing.jags.pdf)

 library(dplyr)
 library(reshape)
 library(stringr)
 
 
 # ----------------------------------------------
 # FILE LOCATIONS: 
 
 # ----------------------------------------------
 # READ OUTPUT FILES:
 
 # ----------------------------------------------
 # READ INPUT FILES:
 
 # Name and Class Designation:
 # UrbanNames <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/dev_den_tables/Name_and_Class.csv")
 
 
 # Raw Data Extracted from Raster
 
 # # Join separate tables together:
 # CLI_Urb_sum <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/CLI_Urb_sum.csv")
 
 # CLI_Urb <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/CLI_Urb.csv")
 
 # CLI_Urban <- full_join(CLI_Urb, CLI_Urb_sum)
 # # str(CLI_Urban)
 # # 'data.frame':	452 obs. of  11 variables:
 # # $ Year       : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
 # # $ Region     : int  1 1 1 1 1 1 1 1 1 1 ...
 # # $ Urb_Class  : Factor w/ 3 levels "City","Metro",..: 1 3 1 3 1 3 1 3 1 3 ...
 # # $ ZoneID     : int  1 1 2 2 3 3 4 4 5 5 ...
 # # $ Area       : num  432000 4906800 2399400 20087100 5873400 ...
 # # $ NumDevPix  : int  462 4498 2241 13591 4625 15634 5005 14830 4849 9633 ...
 # # $ ZoneTotPop : num  570 2294 2815 6873 4796 ...
 # # $ DevArea    : int  415800 4048200 2016900 12231900 4162500 14070600 4504500 13347000 4364100 8669700 ...
 # # $ PerDev     : num  0.963 0.825 0.841 0.609 0.709 ...
 # # $ RegTotPop  : num  185142 185142 185142 185142 185142 ...
 # # $ UrbanTotPop: num  35053 109357 35053 109357 35053 ...
 
 # ### WRITE TO FILE ###
 # write.csv(CLI_Urban, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/CLI_Urban.csv", row.names = F)
 
 ### READ TO FILE ###
 CLI_Urban <- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/CLI_Urban_10-19-17.csv") # 452 obs. of  12 variables
 
 # sort by year, urban area name, and zone ID (travel time)
 CLI_Urban <- CLI_Urban[with(CLI_Urban, order(Year,Region,Urb_Class,ZoneID)), ]
 
 # #Save for use with with all zones
 # CLI_Urban16 <- CLI_Urban
 
 # #Create subset that excludes zone 16 in each region
 # CLI_Urban <- CLI_Urban[which(CLI_Urban$ZoneID<16),]
 
 ###########################################
 # ~~~ CODE BEGINS ~~~ #
 ###########################################
 
 
 
 # Extract the column for percent development for each zone in 2001 
 N2001 <- CLI_Urban$PerDev[which(CLI_Urban$Year==2001)]
 
 # Extract the column for percent development for each zone in 2011 
 N2011 <- CLI_Urban$PerDev[which(CLI_Urban$Year==2011)]
 
 # Create a data matrix for the model with NA values for percent development from 2002-2010
 dat <- cbind(N2001,matrix(NA,nrow=length(N2001),ncol=9),N2011)
 #dat <- cbind(N2001,matrix(NA,nrow=length(N2001),ncol=10))
 
 # Extract a vector of region numbers
 Region <- CLI_Urban$Region[which(CLI_Urban$Year==2001)]
 
 # Create an object giving the number of regions
 Nregion <- length(unique(Region))
 
 # Create a vector of classes (Metro, City, Town)
 Class <- as.numeric(CLI_Urban$Urb_Class)
 
 # Create an object giving the total number of zones across all regions (8 x 16)
 # All regions and classes??
 Nzones <- length(N2001) #gives 218 rows because not all regions have all zones.
 
 
 # Extract the travel time distances (ZoneID) for each observation
 Dist <- CLI_Urban$ZoneID[which(CLI_Urban$Year==2001)]
 
 # Extract 2001 population data (excluding Zone 16)
 #dt <- CLI_Urban[which(CLI_Urban$Year==2001 & CLI_Urban$ZoneID<16),]
 
 # Calculate the population for each urban center as the sum of zones 1-15
 #Pop <- as.numeric(aggregate(dt$RegTotPop, by=list(Category=dt$Region), FUN=median)[,2])
 
 # Population Estimates:
 # **!! Where are these population estimates from? **
 # PopProj<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections_10-19-17.csv")
  # PopProj<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections_15perc_10-24-17.csv")
    PopProj<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections_25perc_10-24-17.csv")
   # PopProj<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections_1stdev_10-24-17.csv")
 prPops <- as.numeric(PopProj$prPop)
 Pop <- prPops[1:8] # Population for 2001. 
 
 # # QC:
 # str(CLI_Urban);str(N2001);str(N2011);str(dat);str(Region);str(Nregion);str(Nzones);str(Dist);str(dt);str(PopA);str(PopA)
 
 # 'data.frame':	436 obs. of  11 variables:
 # $ Year       : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
 # $ Region     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Urb_Class  : Factor w/ 3 levels "City","Metro",..: 1 1 1 1 1 1 1 1 1 1 ...
 # $ ZoneID     : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ Area       : num  432000 2399400 5873400 7447500 9821700 ...
 # $ NumDevPix  : int  462 2241 4625 5005 4849 3652 2604 1603 596 125 ...
 # $ ZoneTotPop : num  570 2815 4796 5983 6342 ...
 # $ DevArea    : int  415800 2016900 4162500 4504500 4364100 3286800 2343600 1442700 536400 112500 ...
 # $ PerDev     : num  0.963 0.841 0.709 0.605 0.444 ...
 # $ RegTotPop  : num  185142 185142 185142 185142 185142 ...
 # $ UrbanTotPop: num  35053 35053 35053 35053 35053 ...
 # num [1:218] 0.963 0.841 0.709 0.605 0.444 ...
 # num [1:218] 0.983 0.908 0.807 0.643 0.514 ...
 # num [1:218, 1:11] 0.963 0.841 0.709 0.605 0.444 ...
 # - attr(*, "dimnames")=List of 2
 # ..$ : NULL
 # ..$ : chr [1:11] "N2001" "" "" "" ...
 # int [1:218] 1 1 1 1 1 1 1 1 1 1 ...
 # int 8
 # int 218
 # int [1:218] 1 2 3 4 5 6 7 8 9 10 ...
 # 'data.frame':	218 obs. of  11 variables:
 # $ Year       : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
 # $ Region     : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ Urb_Class  : Factor w/ 3 levels "City","Metro",..: 1 1 1 1 1 1 1 1 1 1 ...
 # $ ZoneID     : int  1 2 3 4 5 6 7 8 9 10 ...
 # $ Area       : num  432000 2399400 5873400 7447500 9821700 ...
 # $ NumDevPix  : int  462 2241 4625 5005 4849 3652 2604 1603 596 125 ...
 # $ ZoneTotPop : num  570 2815 4796 5983 6342 ...
 # $ DevArea    : int  415800 2016900 4162500 4504500 4364100 3286800 2343600 1442700 536400 112500 ...
 # $ PerDev     : num  0.963 0.841 0.709 0.605 0.444 ...
 # $ RegTotPop  : num  185142 185142 185142 185142 185142 ...
 # $ UrbanTotPop: num  35053 35053 35053 35053 35053 ...
 # num [1:8] 185142 1458647 60646 240400 191200 ...
 # num [1:8] 185142 1458647 60646 240400 191200 ...
 
 
 # Next three lines write a text file specify the model to analyze with JAGS
# Next three lines write a text file specify the model to analyze with JAGS
sink("lg.txt")
cat("
model {

##########################################
# Specify priors on all model parameters #
##########################################
beta1.Rm ~ dunif(-10,10)  # Coefficient for effect of travel time on development growth rate
beta2.Rm ~ dunif(-10,10)  # Coefficient for effect of total region population size on development growth rate
beta3.Rm ~ dunif(-10,10)  # Coefficient for intercept-adjustment for town
beta4.Rm ~ dunif(-10,10)  # Coefficient for intercept-adjustment for metro
mu.Rm ~ dunif(-10,10)     # Intercept value for mean development growth rate across all regions
sd.Rm ~ dunif(0,10)       # Standard deviation describing variation in growth rate intercepts across regions
tau.Rm <- 1/pow(sd.Rm,2)  # Convert above standard deviation to a precision

mu.K ~ dunif(0,1)       # Intercept value for max percent developed asymptote across all regions
sd.K ~ dunif(0,1)       # Standard deviation describing variation in development asymptotes across regions
tau.K <- 1/pow(sd.K,2)  # Convert above standard deviation to a precision

for (i in 1:Nregion){                  # Loop over regions
  alpha.Rm[i] ~ dnorm(mu.Rm,tau.Rm)  # Each region's growth rate intercept is treated as a random effect
  K[i] ~ dnorm(mu.K,tau.K)T(0,1)     # Each region's development asymptote is treated as a random effect
}

sd ~ dunif(0,1)     # Standard deviation describing residual error
tau <- 1/pow(sd,2)  # Convert above standard deviation to a precision

############################
# Specify Model Likelihood #
############################
for (i in 1:Nzones){   # Loop over total number of observations (Nzones = 16 zones x 8 regions)
  for (t in 2:11){     # Loop over years 2 through 11

    # Percent development is normally distributed with mean N_hat and residual error
    N[i,t] ~ dnorm(N_hat[i,t],tau)T(0,1)

    # Expected percent development (N_hat) is based on a logistic growth equation
    N_hat[i,t] <- N[i,t-1] + Rm[i,t] * N[i,t-1] * ((K[Region[i]] - N[i,t-1]) / K[Region[i]])
    
    # The logistic growth rate depends on the city-specific intercept, distance zone, and population size
    log(Rm[i,t]) <- alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Pop[Region[i]] +
                    beta3.Rm*equals(Class[i],2) + beta4.Rm*equals(Class[i],3) 
  }
}    
}",fill = TRUE)
sink()  # Sink all the text above to the model file for analysis

# Create a data list to submit to the JAGS model
jdata <- list(N = dat, Region=as.numeric(Region), Dist=Dist, Pop=as.numeric(scale(Pop)), Class=Class,
              Nregion=Nregion, Nzones=Nzones)

# Specify Reasonable Initial Values (guesses) for model parameters
inits <- function(){list(mu.K=runif(1,0.8,1),
                         sd.K=runif(1,0.05,0.2),
                         mu.Rm=runif(1,-4,-1),
                         sd.Rm=runif(1,0.5,1.5),
                         beta1.Rm=runif(1,-1,0),
                         beta2.Rm=runif(1,0,1),
                         beta3.Rm=runif(1,-1,0),
                         beta4.Rm=runif(1,0,1),
                         sd=runif(1,0,0.05))}

# Parameters monitored (things we estimate that we want to look at later)
parameters <- c("sd", "mu.Rm", "sd.Rm", "alpha.Rm", "beta1.Rm", "beta2.Rm",
                "beta3.Rm","beta4.Rm","mu.K", "sd.K", "K", "beta.Rm")

# Markov Chain settings to submit to JAGS # See notes in Stats One note
ni <- 25000   # Number of iterations per chain
nt <- 50      # Thinning rate (keep 1 in every nt posterior samples)
nb <- 15000   # Number of samples to discard as burn-in (discard pre-convergence samples)
nc <- 3       # Number of independent Markov Chains to run

# Call JAGS from R (Approximate run time = 17 minutes)
system.time(out <- jags(jdata, inits, parameters, "lg.txt", parallel = T, #codaOnly = c('po'),
                        n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, n.adapt = 5000))


 # Look at all the output/parameter estimates
 print(out, digits = 3)
 
 
 # Generating predictions from the model output
 alpha.Rm=out$mean$alpha.Rm         # Extract log-scale city-specific growth rate intercepts from JAGS object
 beta1.Rm=out$mean$beta1.Rm         # Extract log-scale coefficient for distance zone effects on growth rate
 beta2.Rm=out$mean$beta2.Rm         # Extract log-scale coefficient for population size effect on growth rate
 beta3.Rm=out$mean$beta3.Rm         # Extract log-scale coefficient for intercept change for metro
 beta4.Rm=out$mean$beta4.Rm         # Extract log-scale coefficient for intercept change for town
 K=out$mean$K        # Extract city-specific percent development asymptote
 log.Rm <- as.numeric()
 Rm <- as.numeric()
 
 
# ----------------------------------------------
# CAN UPDATE THE MODEL IF IT HASN'T CONVERGED
# ----------------------------------------------

out1 <- update(out,n.iter = 25000,nt=50,nb=10000)
out2 <- update(out1,n.iter = 30000,nt=100,nb=10000)
#out3 <- update(out1,n.iter = 20000,nt=10,nb=10000)

# Look at all the output/parameter estimates
print(out1, digits = 3)
 
 
 # Generating predictions from the model output
 alpha.Rm=out1$mean$alpha.Rm         # Extract log-scale city-specific growth rate intercepts from JAGS object
 beta1.Rm=out1$mean$beta1.Rm         # Extract log-scale coefficient for distance zone effects on growth rate
 beta2.Rm=out1$mean$beta2.Rm         # Extract log-scale coefficient for population size effect on growth rate
 beta3.Rm=out1$mean$beta3.Rm         # Extract log-scale coefficient for intercept change for metro
 beta4.Rm=out1$mean$beta4.Rm         # Extract log-scale coefficient for intercept change for town
 K=out1$mean$K        # Extract city-specific percent development asymptote
 log.Rm <- as.numeric()
 Rm <- as.numeric()
 
 # Look at all the output/parameter estimates
print(out2, digits = 3)
 
 
 # Generating predictions from the model output
 alpha.Rm=out2$mean$alpha.Rm         # Extract log-scale city-specific growth rate intercepts from JAGS object
 beta1.Rm=out2$mean$beta1.Rm         # Extract log-scale coefficient for distance zone effects on growth rate
 beta2.Rm=out2$mean$beta2.Rm         # Extract log-scale coefficient for population size effect on growth rate
 beta3.Rm=out2$mean$beta3.Rm         # Extract log-scale coefficient for intercept change for metro
 beta4.Rm=out2$mean$beta4.Rm         # Extract log-scale coefficient for intercept change for town
 K=out2$mean$K        # Extract city-specific percent development asymptote
 log.Rm <- as.numeric()
 Rm <- as.numeric()
 
 
 
 

# ----------------------------------------------
# ----------------------------------------------
# WITH ZONE 16 ADDED IN model build:
# ----------------------------------------------
# ----------------------------------------------
 

Population=as.numeric(scale(Pop))  # Scale total population data (as done for the JAGS analysis)
Nt <- Nt1 <- N2001  # Create new data objects to overwrite with predicted data


for (i in 1:length(N2001)){    # Loop over observations
  log.Rm[i]=alpha.Rm[Region[i]] + beta1.Rm*Dist[i] + beta2.Rm*Population[Region[i]] +
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
bind2$Year <- "2011"

 # write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs00_10-20-17.csv", row.names = F)
 
 # PLOT 
Actual <- (N2011-N2001)   # Calculate true difference between 2001 and 2011
Predicted <- (Nt1-N2001)  # Calculate difference between 2001 % development and model prediction
plot(Actual,Predicted,xlab="Actual",ylab="Predicted",ylim=c(0,0.2),xlim=c(0,0.2),pch=".",cex=3)  # Plot
segments(0,0,.2,.2)   # Draw 1-to-1 line
R2 <- 1 - (sum((Actual-Predicted)^2)/sum((Actual-mean(Actual))^2))  # Calculate R-squared
text(0.04,.2,labels=paste0("R-squared =",round(R2,2)),cex=1.2)   # Add R-squared to plot
 


# ----------------------------------------------
# ----------------------------------------------
# PREDICTION FOR SUBSEQUENT DECADES
# ----------------------------------------------
# ----------------------------------------------



 #10
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs00_10-20-17.csv") #Grab previous years output to form new baseline

 NinD<- Nin$PredPercDev
 Nt <- Nt1 <- NinD  # Create new data objects to overwrite with predicted data
 
# ----------------------------------------------
# ----------------------------------------------
 ### *** START ON ACTUAL 2011 PERCENT DEV.***
# ----------------------------------------------
# ----------------------------------------------

 # Population=as.numeric(scale(Pop10))  # Scale total population data (as done for the JAGS analysis)
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs10A_25perc_10-24-17.csv", row.names = F)
 
 
 #20
 # Population=as.numeric(scale(Pop20))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs10A_25perc_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs20A_25perc_10-24-17.csv", row.names = F)
 
 #30
 # Population=as.numeric(scale(Pop30))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs20A_25perc_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs30A_25perc_10-24-17.csv", row.names = F)
 
 
 #40
 # Population=as.numeric(scale(Pop40))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs30A_25perc_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs40A_25perc_10-24-17.csv", row.names = F)
 
 
 #50
 # Population=as.numeric(scale(Pop50))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs40A_25perc_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs50A_25perc_10-24-17.csv", row.names = F)
 
 

 
 # ~~~~~~~~~~~~~~~~~~~

 N2001 <- CLI_Urban$PerDev[which(CLI_Urban$Year==2001)]
 N2011 <- CLI_Urban$PerDev[which(CLI_Urban$Year==2011)]
 Actual <- (N2011-N2001)   # Calculate true difference between 2001 and 2011
 Predicted <- (Nt1-N2001)  # Calculate difference between 2001 % development and model prediction
 plot(Actual,Predicted,xlab="Actual",ylab="Predicted",ylim=c(0,0.2),xlim=c(0,0.2),pch=".",cex=3)  # Plot
 segments(0,0,.2,.2)   # Draw 1-to-1 line
 R2 <- 1 - (sum((Actual-Predicted)^2)/sum((Actual-mean(Actual))^2))  # Calculate R-squared
 text(0.04,.2,labels=paste0("R-squared =",round(R2,2)),cex=1.2)   # Add R-squared to plot
 #names <- do.call(paste, c(VWLdata[c("Region", "ZoneID")], sep = ""))  # Create point labels
 #text(Actual, Predicted, labels=names, cex= 0.7, pos=3)  # Messy, but can add labels to graph
 
 
 
 # Box Plot 
 par(mar=c(10,4,3,3))
boxplot(out$sims.list$Rm, las = 2, outline=F, ylim=c(0,0.1), col='light gray', ylab="Logistic growth rate",
        names=unique(Cities))
