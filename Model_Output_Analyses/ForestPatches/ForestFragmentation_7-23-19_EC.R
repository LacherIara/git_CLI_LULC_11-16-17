
setwd("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis")

library(stringr)
library(ggplot2)
library(ggalt)
library(ggcorrplot)
library(scales)
library(reshape)


#####################################################################################################
# DATA PREP
#####################################################################################################

# v1 - only scenario column
dat_2011 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011$scenario <- "2011"
dat_Q1 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q1.csv")
  dat_Q1$scenario <- "Q1"
dat_Q2 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q2.csv")
  dat_Q2$scenario <- "Q2"
dat_Q3 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q3.csv")
  dat_Q3$scenario <- "Q3"
dat_Q4 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q4.csv")
  dat_Q4$scenario <- "Q4"
dat_RT <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_RT.csv")
  dat_RT$scenario <- "RT"

dat_all_v1 <- rbind(dat_2011, dat_Q1, dat_Q2, dat_Q3, dat_Q4, dat_RT)
  
SA.counties <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")
  
dat_all_v1$LID <- str_sub(dat_all_v1$LID, -3, -2)
SA.counties$VALUE <- as.character(SA.counties$VALUE)
  
for (i in 1:20) {
    if (nchar(SA.counties$VALUE[i]) < 2) {
      SA.counties$VALUE[i] <- paste0("0", SA.counties$VALUE[i])
    }
  }
  
dat_allSA_v1 <- merge(SA.counties, dat_all_v1, by.x = "VALUE", by.y = "LID")

dat_allSA_v1$scenario <- as.factor(dat_allSA_v1$scenario)

# v2 - scenario and year column
dat_2011_Q1 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011_Q1$scenario <- "Q1"
  dat_2011_Q1$year <- "2011"
dat_2011_Q2 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011_Q2$scenario <- "Q2"
  dat_2011_Q2$year <- "2011"
dat_2011_Q3 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011_Q3$scenario <- "Q3"
  dat_2011_Q3$year <- "2011"
dat_2011_Q4 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011_Q4$scenario <- "Q4"
  dat_2011_Q4$year <- "2011"
dat_2011_RT <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2011_SAtif.csv")
  dat_2011_RT$scenario <- "RT"
  dat_2011_RT$year <- "2011"
      
dat_Q1 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q1.csv")
  dat_Q1$scenario <- "Q1"
  dat_Q1$year <- "2061"
dat_Q2 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q2.csv")
  dat_Q2$scenario <- "Q2"
  dat_Q2$year <- "2061"
dat_Q3 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q3.csv")
  dat_Q3$scenario <- "Q3"
  dat_Q3$year <- "2061"
dat_Q4 <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_Q4.csv")
  dat_Q4$scenario <- "Q4"
  dat_Q4$year <- "2061"
dat_RT <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SA_V2016/OutputProducts/Fragstats/FragmentationAnalysis/results2061_RT.csv")
  dat_RT$scenario <- "RT"
  dat_RT$year <- "2061"

dat_all_v2 <- rbind(dat_2011_Q1, dat_2011_Q2, dat_2011_Q3, dat_2011_Q4, dat_2011_RT, dat_Q1, dat_Q2, dat_Q3, dat_Q4, dat_RT)


SA.counties <- read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/SAcntyOnly.csv")

dat_all_v2$LID <- str_sub(dat_all_v2$LID, -3, -2)
SA.counties$VALUE <- as.character(SA.counties$VALUE)

for (i in 1:20) {
  if (nchar(SA.counties$VALUE[i]) < 2) {
    SA.counties$VALUE[i] <- paste0("0", SA.counties$VALUE[i])
  }
}

dat_allSA_v2 <- merge(SA.counties, dat_all_v2, by.x = "VALUE", by.y = "LID")

dat_allSA_v2$scenario <- as.factor(dat_allSA_v2$scenario)
dat_allSA_v2$year <- as.factor(dat_allSA_v2$year)

#####################################################################################################
# VISUALIZTAION
#####################################################################################################

gg1 <- ggplot(dat_allSA_v2, aes(x=VALUE)) +
  facet_wrap(~scenario) +
  geom_col(aes(y=NP[dat_allSA_v1$year=="2011"])) +
  geom_col(aes(y=NP[dat_allSA_v1$year=="2061"]))
gg1  


dumbelltry <- data.frame("NP2011" = dat_allSA_v2$NP[dat_allSA_v2$year=="2011"],
                         "NP2061" = dat_allSA_v2$NP[dat_allSA_v2$year=="2061"],
                         "County" = dat_allSA_v2$NAME[dat_allSA_v2$year=="2011"],
                         "Scenario" = dat_allSA_v2$scenario[dat_allSA_v2$year=="2011"])

gg2 <- ggplot(dumbelltry, aes(x=NP2011, xend=NP2061, y=County)) +
  geom_dumbbell() +
  facet_wrap(~Scenario) +
  geom_text(aes(x=NP2011, label=NP2011), size=2) +
  geom_text(aes(x=NP2061, label=NP2061), size=2)
gg2

gg3 <- ggplot(dat_allSA_v1) + 
  geom_col(aes(x=VALUE, y=CA, fill=scenario), position="dodge")
gg3


gg4 <- ggplot(dumbelltry, aes(x=County)) + 
  facet_wrap(~Scenario) +
  geom_col(aes(y=NP2061), fill="darkgreen") +
  geom_col(aes(y=NP2011), fill="red")
gg4

dat_reformat_prep <- dat_allSA_v2[,c(1,5:32)]
dat_reformat <- melt(dat_reformat_prep, id.vars = c("VALUE", "scenario", "year"))

dat_reformat2 <- dat_reformat[dat_reformat$variable=="CA",]

gg5 <- ggplot(dat_reformat2, aes(x=VALUE, y=value, fill=year)) +
  geom_col(position="dodge", width=0.75) +
  facet_wrap(~scenario)
gg5
