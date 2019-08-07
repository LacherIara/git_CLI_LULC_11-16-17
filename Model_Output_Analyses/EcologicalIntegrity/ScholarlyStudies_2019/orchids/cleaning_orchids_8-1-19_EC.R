
setwd("U:/CLI/Field Surveys/Orchid")

library(reshape2)

########################################################################################################
# Prep orchid data
#######################################################################################################
orchid.dat <- read.csv("U:/CLI/Field Surveys/Orchid/Orchids.csv")

# remove incidentals
orchid.dat <- orchid.dat[orchid.dat$Transect != "INCIDENTAL",]

# only where SiteVisit == 1 or 2, not 3
orchid.dat <- orchid.dat[orchid.dat$SiteVisit != 3,]

# only where NewFind == 1 or 0, not 2
orchid.dat <- orchid.dat[orchid.dat$NewFind != 2,]

# remove Kinloch_7 because it was only visited once (try running models again after)
orchid.dat <- orchid.dat[orchid.dat$PlotID != "Kinloch_7",]
  
# get rid of unnecessary columns
orchid.dat2 <- orchid.dat[,c(1,9,13)]

# reformat dataframe
orchid.cast <- cast(orchid.dat2, PlotID~GenusSpecies, sum)
orchid.cast <- orchid.cast[,-5]
names(orchid.cast)[1] <- "Site"
names(orchid.cast)[2] <- "Cypripedium.acaule"
names(orchid.cast)[3] <- "Galearis.spectabilis"
names(orchid.cast)[4] <- "Goodyera.pubescens"
names(orchid.cast)[5] <- "Platanthera.spp"
names(orchid.cast)[6] <- "Tipularia.discolor"

# calculate Abundance
orchid.cast$Abundance <- rowSums(orchid.cast[,c(2:6)])

# calculate SpRichness
orchid.cast$SpRichness <- apply(orchid.cast[,c(2:6)]>0, 1, sum)

# re-attach important variables
orchid.cast$Year <- 2019
orchid.cast$Lat <- 0
orchid.cast$Long <- 0

for (i in unique(orchid.cast$Site)) {
  orchid.cast$Lat[orchid.cast$Site == i] <- orchid.dat$Plot_Lat[orchid.dat$PlotID == i]
  orchid.cast$Long[orchid.cast$Site == i] <- orchid.dat$Plot_Long[orchid.dat$PlotID == i]
}

########################################################################################################
# Prep forest data
#######################################################################################################
forest.dat <- read.csv("U:/CLI/Field Surveys/Orchid/ForestHealth.csv")
  # average or sum everything across subplots to have one value for each site

forest.dat[is.na(forest.dat)==TRUE] <- 0

forest.dat.2 <- forest.dat[,c(1,11:26)]
forest.dat.melt.2 <- melt(forest.dat.2, id="PlotID")
forest.dat.cast.2 <- cast(forest.dat.melt.2, PlotID~variable, mean)

forest.dat.3 <- forest.dat[,c(1,42:56)]
forest.dat.melt.3 <- melt(forest.dat.3, id="PlotID")
forest.dat.cast.3 <- cast(forest.dat.melt.3, PlotID~variable, sum)
forest.dat.cast.3$DBH_total <- rowSums(forest.dat.cast.3[,c(4:16)])
forest.dat.cast.3$DBH_mean <- forest.dat.cast.3$DBH_total/forest.dat.cast.3$Trees.10

forest.dat.final <- merge(forest.dat.cast.2, forest.dat.cast.3, by="PlotID")

forest.dat.final$DomSpp1 <- NA
forest.dat.final$DomSpp2 <- NA
forest.dat.final$DomSpp3 <- NA

for (i in unique(forest.dat.final$PlotID)) {
  forest.dat.final$DomSpp1[forest.dat.final$PlotID == i] <- as.character(forest.dat$DomSpp1[forest.dat$PlotID == i][1])
  forest.dat.final$DomSpp2[forest.dat.final$PlotID == i] <- as.character(forest.dat$DomSpp2[forest.dat$PlotID == i][1])
  forest.dat.final$DomSpp3[forest.dat.final$PlotID == i] <- as.character(forest.dat$DomSpp3[forest.dat$PlotID == i][1])
}

forest.dat.final$DomSpp1 <- as.factor(forest.dat.final$DomSpp1)
forest.dat.final$DomSpp2 <- as.factor(forest.dat.final$DomSpp2)
forest.dat.final$DomSpp3 <- as.factor(forest.dat.final$DomSpp3)

########################################################################################################
# Merge orchid and forest data
#######################################################################################################

dat <- merge(orchid.cast, forest.dat.final, by.x="Site", by.y="PlotID")

write.csv(dat, file = "CLI_Orchids_8-2-19_EC.csv")










