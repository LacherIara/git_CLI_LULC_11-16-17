
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats")
library(corrplot)
library(Hmisc)

# data prep
dat <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/grid_forest.csv")
dat[dat==-9999] <- NA

dat2 <- na.omit(dat[,c(3:14)])

dat.rcorr <- rcorr(as.matrix(dat), type="spearman")
corrplot(dat.rcorr$r, p.mat=dat.rcorr$P, sig.level=0.05, insig="label_sig",tl.col="black", tl.srt = 45)

dat.rcorr <- rcorr(as.matrix(dat2), type="spearman")
corrplot(dat.rcorr$r, p.mat=dat.rcorr$P, sig.level=0.05, insig="label_sig",tl.col="black", tl.srt = 45)

names(dat)

summary(dat$pi2016) # ordinal
summary(dat$WatershedC) # ordinal
summary(dat$EO_count) # continuous...
summary(dat$EcoCores) # ordinal
summary(dat$ClimateRes) # made it resilient/not resilient binary
summary(dat$FCV) # ordinal
# all ConserveVirginia (Scenic, AgForestry, CulturalHi, Floodplain, NaturalHab, ProtectedL) values binary 0/1

# selected four: Soil Productivity Index, Element Occurence, Watershed Conservation Priority, Climate Resilience

