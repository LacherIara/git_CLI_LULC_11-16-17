############################ 
#PURPOSE: Calculate the development density per class and zone. 
# modified to use RegionalPopProjections_1stdev_10-24-17.csv 
#INPUT: 
#OUTPUT: 
#DEVELOPED: 
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
   PopProj<-read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/RegionalPopProjections_1stdev_10-24-17.csv")
 prPops <- as.numeric(PopProj$prPop)
 Pop <- prPop[1:8] # Population for 2001. 
 
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

{
# SCALING POPULATION
# s_Pop10=as.numeric(scale(Pop10))
# s_Pop20=as.numeric(scale(Pop20))
# s_Pop30=as.numeric(scale(Pop30))
# s_Pop40=as.numeric(scale(Pop40))
# s_Pop50=as.numeric(scale(Pop50))

# Pop10
# Pop20
# Pop30
# Pop40
# Pop50

# s_Pop10
# s_Pop20
# s_Pop30
# s_Pop40
# s_Pop50

# > Pop10
# [1]  222152 1960535   72565  267724  227550   37049  151278  972457
# > Pop20
# [1]  255475 2254615   83450  307883  261683   42606  173970 1118326
# > Pop30
# [1]  293796 2592807   95968  354065  300935   48997  200066 1286075
# > Pop40
# [1]  337865 2981728  110363  407175  346075   56347  230076 1478986
# > Pop50
# [1]  388545 3428987  126917  468251  397986   64799  264587 1700834


# > s_Pop10
# [1] -0.4021972  2.2187662 -0.6277298 -0.3334882 -0.3940586 -0.6812773 -0.5090541  0.7290391
# > s_Pop20
# [1] -0.4021972  2.2187659 -0.6277298 -0.3334880 -0.3940583 -0.6812781 -0.5090540  0.7290395
# > s_Pop30
# [1] -0.4021975  2.2187659 -0.6277293 -0.3334885 -0.3940588 -0.6812781 -0.5090534  0.7290397
# > s_Pop40
# [1] -0.4021979  2.2187660 -0.6277295 -0.3334882 -0.3940590 -0.6812776 -0.5090533  0.7290396
# > s_Pop50
# [1] -0.4021975  2.2187659 -0.6277297 -0.3334883 -0.3940591 -0.6812775 -0.5090535  0.7290398


# s_prPop=as.numeric(scale(prPop))


# s_prPop
}

 # #### Create Yearly Population Predictions (includes value for each region) ##Pop for 2001 is loaded above as just Pop
 # # Each population projection provides information for the following year.
 # Pop10 <- prPop[9:16] # 10 years out - Population for 2011. 
 # Pop20 <- prPop[17:24]# 20 years out - Population for 2021. 
 # Pop30 <- prPop[25:32]# 30 years out - Population for 2031. 
 # Pop40 <- prPop[33:40]# 40 years out - Population for 2041.  
 # Pop50 <- prPop[41:48]# 50 years out - Population for 2051.  
 
 
s_prPop=as.numeric(scale(prPop[9:48]))
# Each population projection provides information for the following year.
 Pop10 <- s_prPop[1:8] # 10 years out - Population for 2011. 
 Pop20 <- s_prPop[9:16]# 20 years out - Population for 2021. 
 Pop30 <- s_prPop[17:24]# 30 years out - Population for 2031. 
 Pop40 <- s_prPop[25:32]# 40 years out - Population for 2041.  
 Pop50 <- s_prPop[33:40]# 50 years out - Population for 2051.   
 
 
 # > ss_prPop
 # [1] -0.45841099  0.80241958 -0.56690484 -0.42535811 -0.45449588 -0.59266421 -0.50981514  0.08577719
 # [9] -0.42278333  1.30230201 -0.55419414 -0.39933400 -0.42273256 -0.58816379 -0.47483371  0.22455984
# [17] -0.37927758  1.97791623 -0.53841402 -0.36982198 -0.38485650 -0.58290980 -0.42869951  0.39065109
# [25] -0.32615159  2.89103949 -0.51882251 -0.33635423 -0.33969129 -0.57677530 -0.36785645  0.58942315
# [33] -0.26127881  4.12516781 -0.49449993 -0.29840129 -0.28583348 -0.56961379 -0.28761552  0.82730788
# > os_prPop
 # [1] -0.4603345  1.5278485 -0.6314166 -0.4082139 -0.4541604 -0.6720358 -0.5413930  0.3977869
 # [9] -0.4453069  1.9231890 -0.6277582 -0.3849756 -0.4278728 -0.6736419 -0.5227175  0.4986307
# [17] -0.4139893  2.3721812 -0.6180387 -0.3585463 -0.3972872 -0.6744853 -0.5084873  0.6111369
# [25] -0.3874575  2.7693756 -0.6098478 -0.3375657 -0.3717024 -0.6757898 -0.4759082  0.7079566
# [33] -0.3540885  3.1506314 -0.5980570 -0.3057193 -0.3391450 -0.6788198 -0.4508555  0.8468810

# > prPopo
 # [1]  185282.00 1517345.13   60628.00  240396.34  196411.70   33436.31  115261.04  870683.50  222152.00
# [10] 1960535.22   72565.00  267724.10  227550.35   37049.31  151277.87  972457.14  235291.53 2306204.28
# [19]   75763.81  288042.62  250535.10   35644.99  167606.97 1060630.68  262674.32 2698784.06   84262.08
# [28]  311151.32  277277.90   34907.58  180049.21 1159001.34  285872.59 3046074.08   91423.92  329495.84
# [37]  299648.18   33767.00  208535.00 1243656.38  315049.00 3379428.00  101733.30  357341.00  328115.00
# [46]   31117.65  230440.00 1365126.00  340911.00 3736438.00  110179.00  381080.00  354281.00   27390.00
# [55]  252345.00 1467996.00
# > prPops
 # [1]  185282 1517345   60628  240396  196412   33436  115261  870684  222152 1960535   72565  267724  227550
# [14]   37049  151278  972457  271274 2649753   90090  303605  271344   43254  199509 1163805  331258 3581263
# [27]  111847  344295  323566   50498  263117 1392805  404506 4840241  138859  390439  385838   58956  347005
# [40] 1666864  493950 6541808  172394  442767  460095   68830  457638 1994850  603172 8841554  214028  502108
# [53]  548643   80358  603544 2387373

prPopo<-prPopo[9:48]
prPops<-prPops[9:48]
allPop <- c(prPopo, prPops)
Alls_prPop=as.numeric(scale(allPop))


prPopo<-Alls_prPop[1:40]
prPops<-Alls_prPop[41:80]


 # Each population projection provides information for the following year.
 Pop10 <- prPopo[1:8] # 10 years out - Population for 2011. 
 Pop20 <- prPopo[9:16]# 20 years out - Population for 2021. 
 Pop30 <- prPopo[17:24]# 30 years out - Population for 2031. 
 Pop40 <- prPopo[25:32]# 40 years out - Population for 2041.  
 Pop50 <- prPopo[33:40]# 50 years out - Population for 2051.  

 Pop10 <- prPops[1:8] # 10 years out - Population for 2011. 
 Pop20 <- prPops[9:16]# 20 years out - Population for 2021. 
 Pop30 <- prPops[17:24]# 30 years out - Population for 2031. 
 Pop40 <- prPops[25:32]# 40 years out - Population for 2041.  
 Pop50 <- prPops[33:40]# 50 years out - Population for 2051.  

# > Alls_prPop
 # [1] -0.4486615  1.0591560 -0.5784085 -0.4091338 -0.4439792 -0.6092136 -0.5101355  0.2021290 -0.4372647  1.3589782
# [11] -0.5756339 -0.3915101 -0.4240429 -0.6104317 -0.4959721  0.2786078 -0.4135138  1.6994893 -0.5682628 -0.3714664
# [21] -0.4008471 -0.6110713 -0.4851801  0.3639314 -0.3933923  2.0007174 -0.5620508 -0.3555549 -0.3814438 -0.6120606
# [31] -0.4604725  0.4373584 -0.3680857  2.2898579 -0.5531088 -0.3314029 -0.3567526 -0.6143586 -0.4414728  0.5427173
# [41] -0.4486615  1.0591558 -0.5784085 -0.4091338 -0.4439795 -0.6092139 -0.5101354  0.2021288 -0.4060547  1.6569613
# [51] -0.5632078 -0.3780118 -0.4059940 -0.6038319 -0.4683013  0.3680979 -0.3540265  2.4649231 -0.5443365 -0.3427186
# [61] -0.3606983 -0.5975487 -0.4131298  0.5667251 -0.2904935  3.5569200 -0.5209072 -0.3026948 -0.3066856 -0.5902125
# [71] -0.3403681  0.8044351 -0.2129127  5.0328043 -0.4918200 -0.2573072 -0.2422774 -0.5816481 -0.2444086  1.0889196


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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs10A_1stdevTEST_10-24-17.csv", row.names = F)
 
 
 #20
 # Population=as.numeric(scale(Pop20))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs10A_1stdevTEST_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs20A_1stdevTEST_10-24-17.csv", row.names = F)
 
 #30
 # Population=as.numeric(scale(Pop30))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs20A_1stdevTEST_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs30A_1stdevTEST_10-24-17.csv", row.names = F)
 
 
 #40
 # Population=as.numeric(scale(Pop40))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs30A_1stdevTEST_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs40A_1stdevTEST_10-24-17.csv", row.names = F)
 
 
 #50
 # Population=as.numeric(scale(Pop50))  # Scale total population data (as done for the JAGS analysis)
 Nin<- read.csv("V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs40A_1stdevTEST_10-24-17.csv") #Grab previous years output to form new baseline
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
 
 write.csv(bind2, "V:/IaraSpatialLayers/Dinamica_Runs/Sub_Model Development/UrbanGrowthPatterns/LogRegOutputs50A_1stdevTEST_10-24-17.csv", row.names = F)
 
 

 
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
