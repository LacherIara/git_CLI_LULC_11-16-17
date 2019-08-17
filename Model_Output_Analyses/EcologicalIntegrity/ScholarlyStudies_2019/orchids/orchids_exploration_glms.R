
setwd("U:/CLI/Field Surveys/Orchid/Pilot2019/DataExploration")

library(ggplot2)
install.packages("ggplot2")
library(MuMIn)
library(corrplot)
install.packages("corrplot")
library(Hmisc)
install.packages("Hmisc")
library(PerformanceAnalytics)
library(reshape2)
library(RColorBrewer)

dat <- read.csv("U:/CLI/Field Surveys/Orchid/CLI_Orchids_Environmental_8-2-19.csv")

##########################################################################################################
# data distribution
##########################################################################################################

# orchids 

hist(dat$Abundance, breaks = 15, xlab="Abundance", main="Histogram of Abundance")

hist(dat$SpRichness, breaks=8, xlab="Species Richness", main="Histogram of Species Richness")

ggplot(data=dat) +
  geom_bar(aes(x=reorder(Site, -Abundance), y=Abundance), stat="identity", fill=Galearis.spectabilis) +
  labs(x="Site", title="Abundance by Site")

ggplot(data=dat) +
  geom_bar(aes(x=reorder(Site, -SpRichness), y=SpRichness), stat="identity") +
  scale_y_continuous(breaks=c(0,1,2)) +
  labs(x="Site", y="Species Richness", title="Species Richness by Site")

dat.melt <- melt(dat, id.vars = c("Site"), measure.vars = c("Cypripedium.acaule","Galearis.spectabilis","Goodyera.pubescens","Platanthera.spp","Tipularia.discolor"))

ggplot(data=dat.melt) +
  geom_bar(aes(x=reorder(Site, -value), y=value, fill=variable), position="stack", stat="identity") +
#  scale_fill_brewer(type='qual', palette = "Blues") +
#  scale_fill_manual(values=c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0")) +
  labs(x="Site", y="Abundance", title="Species Abundance by Site", fill="Species") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.8))

dat.melt.2 <- dat.melt
dat.melt.2$value[dat.melt.2$value>0] <- 1

ggplot(data=dat.melt.2) +
  geom_bar(aes(x=reorder(Site, -value), y=value, fill=variable), position="stack", stat="identity") +
  labs(x="Site", y="Abundance", title="Species Abundance by Site", fill="Species") +
  scale_y_continuous(breaks=c(0,1,2)) +
  theme_bw() +
  theme(legend.position = c(0.85,0.8))

# variables

dat.melt.cover <- melt(dat, id.vars = "Site", measure.vars=c("CanopyCover","MidstoryCover","UnderstoryCover"))

ggplot() +
  geom_point(data=dat, aes(x=reorder(Site, -Abundance), y=Abundance/134, color="red")) +
  geom_bar(data=dat.melt.cover, aes(x=Site, y=value, fill=variable), stat="identity", position="fill") +
  scale_fill_manual(values=c("#00441b","#238b45","#74c476")) +
  geom_point(data=dat, aes(x=reorder(Site, -Abundance), y=Abundance/134, color="red")) +
  scale_color_manual(name="", labels="Normalized Abundance", values = "red") +
  labs(y="Cover", x="Site", title="Cover by Site", fill="", colour="Normalized Abundance") +
  theme(legend.position = "bottom")


dat.melt.litterduff <- melt(dat, id.vars = "Site", measure.vars = c("LitterDuff","MineralSoil"))
    dat.melt.litterduff$steps <- NA
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Kinloch_6"] <- 1
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Deerfield_5"] <- 2
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Eldon_11"] <- 3
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Marriott_12"] <- 4
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Oxbow_17"] <- 5
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Castleton_8"] <- 6
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Oxbow_11"] <- 7
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Dubois_11"] <- 8
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Eldon_2"] <- 9
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Marriott_16"] <- 10
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Marriott_15"] <- 12
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Vernon_10"] <- 12
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="SCBI_14"] <- 13
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Learning_4"] <- 14
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="Eldon_4"] <- 15
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="SCBI_9"] <- 16
    dat.melt.litterduff$steps[dat.melt.litterduff$Site=="SCBI_6"] <- 17

ggplot() +
  geom_bar(data=dat.melt.litterduff, aes(x=reorder(Site, steps), y=value, fill=variable), stat="identity", position="fill") +
  geom_point(data=dat, aes(x=Site, y=Abundance/134, color="black")) +
  scale_color_manual(name="", labels="Normalized Abundance", values = "black") +
  labs(y="Cover", x="Site", title="Litter:Soil by Site", fill="", colour="Normalized Abundance") +
  theme(legend.position = "bottom")

dat.melt.groundcover <- melt(dat, id.vars = "Site", measure.vars = c("LitterDuff","MineralSoil","Moss","RoadTrail","Rock","Trash_Other","Wood"))

ggplot() +``
  geom_bar(data=dat.melt.groundcover, aes(x=Site, y=value, fill=variable), stat="identity", position="fill")


##########################################################################################################
# data dredge
##########################################################################################################

# remove non-predictor fields and fields where all or most values = 0, to get down to 30 var max for data drede
dat.dredge.abd <- dat[,-c(1:6,8:13,17:21,23,30:43,45:49,52:53,64)]
models.abd <- glm(Abundance ~ ., data=dat.dredge.abd, family="poisson", na.action = "na.fail")
dd.abd <- dredge(models.abd)

dat.dredge.spr <- dat[,-c(1:7,9:13,17:21,23,30:43,45:49,52:53,64)]
models.spr <- glm(Abundance ~ ., data=dat.dredge.spr, family="poisson", na.action = "na.fail")
dd.spr <- dredge(models.spr)


##########################################################################################################
# correlation matrix
##########################################################################################################
dat.cor <- dat[,c(7:8,2:6,13:18,21:22,24:29,44,49:51,53:71)]

cor <- rcorr(as.matrix(dat.cor))

corrplot(cor$r, type="upper", p.mat=cor$P, sig.level=0.1, tl.col="black", insig="label_sig", pch.cex=2)

##########################################################################################################
# plots
##########################################################################################################

# core forest at 1k
mod.corfor1k <- glm(dat$Abundance~dat$core_for1k, family="poisson")
  sum.corefor1k <- summary(mod.corfor1k)
  coeff.corefor1k <- sum.corefor1k$coefficients[2,1]
  p.corefor1k <- sum.corefor1k$coefficients[2,4]
  aic.corefor1k <- sum.corefor1k$aic
to.round <- c(coeff.corefor1k, p.corefor1k, aic.corefor1k)
metrics <- round(to.round, digits=3)
mylabel = paste0("coefficient = ", metrics[1], "\n", "p = ", metrics[2], "\n", "AIC = ", metrics[3])

ggplot(data=dat, aes(x=core_for1k, y=Abundance)) +
  geom_point() +
  geom_smooth(method="glm",
              method.args = list(family = "poisson"),
              se=TRUE,
              data=dat,
              aes(x=core_for1k, y=Abundance)) +
  annotate(geom="text", x=max(dat$core_for1k)*0.8, y=max(dat$Abundance)*0.85, label=mylabel)

# midstory cover (2-16 ft, cover class 1-7)
mod.midstory <- glm(dat$Abundance~dat$MidstoryCover, family="poisson")
  sum.midstory <- summary(mod.midstory)
  coeff.midstory <- sum.midstory$coefficients[2,1]
  p.midstory <- sum.midstory$coefficients[2,4]
  aic.midstory <- sum.midstory$aic
to.round <- c(coeff.midstory, p.midstory, aic.midstory)
metrics <- round(to.round, digits=3)
mylabel = paste0("coefficient = ", metrics[1], "\n", "p = ", metrics[2], "\n", "AIC = ", metrics[3])

ggplot(data=dat, aes(x=MidstoryCover, y=Abundance)) +
  geom_point() +
  geom_smooth(method="glm",
              method.args = list(family = "poisson"),
              se=TRUE,
              data=dat,
              aes(x=MidstoryCover, y=Abundance)) +
  annotate(geom="text", x=max(dat$MidstoryCover)*0.8, y=max(dat$Abundance)*0.85, label=mylabel)

# canopy cover (> 16 ft, cover class 1-7)
mod.canopy <- glm(dat$Abundance~dat$CanopyCover, family="poisson")
  sum.canopy <- summary(mod.canopy)
  coeff.canopy <- sum.canopy$coefficients[2,1]
  p.canopy <- sum.canopy$coefficients[2,4]
  aic.canopy <- sum.canopy$aic
to.round <- c(coeff.canopy, p.canopy, aic.canopy)
metrics <- round(to.round, digits=3)
mylabel = paste0("coefficient = ", metrics[1], "\n", "p = ", metrics[2], "\n", "AIC = ", metrics[3])

ggplot(data=dat, aes(x=CanopyCover, y=Abundance)) +
  geom_point() +
  geom_smooth(method="glm",
              method.args = list(family = "poisson"),
              se=TRUE,
              data=dat,
              aes(x=CanopyCover, y=Abundance)) +
  annotate(geom="text", x=6, y=max(dat$Abundance)*0.85, label=mylabel)

# canopy cover (> 16 ft, cover class 1-7)
mod.canopy <- glm(dat$Abundance~dat$CanopyCover, family="poisson")
sum.canopy <- summary(mod.canopy)
coeff.canopy <- sum.canopy$coefficients[2,1]
p.canopy <- sum.canopy$coefficients[2,4]
aic.canopy <- sum.canopy$aic
to.round <- c(coeff.canopy, p.canopy, aic.canopy)
metrics <- round(to.round, digits=3)
mylabel = paste0("coefficient = ", metrics[1], "\n", "p = ", metrics[2], "\n", "AIC = ", metrics[3])

ggplot(data=dat, aes(x=CanopyCover, y=Abundance)) +
  geom_point() +
  geom_smooth(method="glm",
              method.args = list(family = "poisson"),
              se=TRUE,
              data=dat,
              aes(x=CanopyCover, y=Abundance)) +
  annotate(geom="text", x=6, y=max(dat$Abundance)*0.85, label=mylabel)
  
# Number of stems <10 cm dbh
mod.stems <- glm(dat$Abundance~dat$Stems_10, family="poisson")
  sum.stems <- summary(mod.stems)
  coeff.stems <- sum.stems$coefficients[2,1]
  p.stems <- sum.stems$coefficients[2,4]
  aic.stems <- sum.stems$aic
to.round <- c(coeff.stems, p.stems, aic.stems)
metrics <- round(to.round, digits=3)
mylabel = paste0("coefficient = ", metrics[1], "\n", "p = ", metrics[2], "\n", "AIC = ", metrics[3])

ggplot(data=dat, aes(x=Stems_10, y=Abundance)) +
  geom_point() +
  geom_smooth(method="glm",
              method.args = list(family = "poisson"),
              se=TRUE,
              data=dat,
              aes(x=Stems_10, y=Abundance)) +
  annotate(geom="text", x=max(dat$Stems_10)*0.8, y=max(dat$Abundance)*0.85, label=mylabel)  
  
  
  
  
  
  
  
  
  