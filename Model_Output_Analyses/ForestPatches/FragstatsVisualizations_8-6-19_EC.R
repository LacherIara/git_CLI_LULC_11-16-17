# test

# set working directory, load packages
  setwd("/Users/erincarroll/Desktop/forestfragmentation/analysis")
  library(ggplot2)

# read data
# dat.2011 <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2011.csv")
# dat.Q1 <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2061_Q1.csv")
# dat.Q2 <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2061_Q2.csv")
# dat.Q3 <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2061_Q3.csv")
# dat.Q4 <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2061_Q4.csv")
# dat.RT <- read.csv("U:\CLI\Dinamica_Runs\StudyArea_V201\SA_V2016\OutputProducts\Fragstats\FragmentationAnalysis\results_SA_2061_RT.csv")

dat.SA.2011 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2011.csv")
  dat.SA.2011$LID <- "2011"
dat.SA.Q1 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2061_Q1.csv")
  dat.SA.Q1$LID <- "Q1"
dat.SA.Q2 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2061_Q2.csv")
  dat.SA.Q2$LID <- "Q2"
dat.SA.Q3 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2061_Q3.csv")
  dat.SA.Q3$LID <- "Q3"
dat.SA.Q4 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2061_Q4.csv")
  dat.SA.Q4$LID <- "Q4"
dat.SA.RT <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results_SA_2061_RT.csv")
  dat.SA.RT$LID <- "RT"

dat.cnty.2011 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2011_SAtif.csv")
dat.cnty.Q1 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2061_Q1.csv")
dat.cnty.Q2 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2061_Q2.csv")
dat.cnty.Q3 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2061_Q3.csv")
dat.cnty.Q4 <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2061_Q4.csv")
dat.cnty.RT <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/results2061_RT.csv")

###########################################################################################
# Study Area
###########################################################################################

dat.SA <- rbind(dat.SA.2011, dat.SA.Q1, dat.SA.Q2, dat.SA.Q3, dat.SA.Q4, dat.SA.RT)

dat.SA.change <- dat.SA[2:6,]
for (i in 1:5) {
  dat.SA.change[i,3:28] <- dat.SA[i+1,3:28] - dat.SA[1,3:28]
}

dat.SA.pct.change <- dat.SA[2:6,]
for (i in 1:5) {
  dat.SA.pct.change[i,3:28] <- ((dat.SA[i+1,3:28] - dat.SA[1,3:28])/dat.SA[1,3:28])*100
}

CA.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=CA, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Total Area (CA)", fill="")
CA.pct
ggsave("PctChange_CA.jpeg", width=4, height=4, units="in")

NP.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=NP, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Number of Patches (NP)", fill="")
NP.pct
ggsave("PctChange_NP.jpeg", width=4, height=4, units="in")

PD.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=PD, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Patch Density (PD)", fill="")
PD.pct
ggsave("PctChange_PD.jpeg", width=4, height=4, units="in")

areaMN.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=AREA_MN, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Mean Patch Size (AREA_MN)", fill="")
areaMN.pct
ggsave("PctChange_AREA_MN.jpeg", width=4, height=4, units="in")

gyrateAM.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=GYRATE_AM, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Radius of Gyration, Area-Weighted Mean (GYRATE_AM)", fill="")
gyrateAM.pct
ggsave("PctChange_GYRATE_AM.jpeg", width=4, height=4, units="in")

pafrac.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=PAFRAC, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Perimeter-Area Fractal Dimension (PAFRAC)", fill="")
pafrac.pct
ggsave("PctChange_PAFRAC.jpeg", width=4, height=4, units="in")

tca.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=TCA, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Total Core Area (TCA)", fill="")
tca.pct
ggsave("PctChange_PAFRAC.jpeg", width=4, height=4, units="in")

ndca.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=NDCA, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Number of Disjunct Core Areas (NDCA)", fill="")
ndca.pct
ggsave("PctChange_NDCA.jpeg", width=4, height=4, units="in")

dcad.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=DCAD, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Disjunct Core Area Density (DCAD)", fill="")
dcad.pct
ggsave("PctChange_DCAD.jpeg", width=4, height=4, units="in")

coreMN.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=CORE_MN, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Core Area Distribution (CORE_MN)", fill="")
coreMN.pct
ggsave("PctChange_CORE_MN.jpeg", width=4, height=4, units="in")

ennMN.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=ENN_MN, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Euclidean Nearest Neighbor Distance Distribution (ENN_MN)", fill="")
ennMN.pct
ggsave("PctChange_ENN_MN.jpeg", width=4, height=4, units="in")

econMN.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=ECON_MN, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Edge Contrast Index Distribution (ECON_MN)", fill="")
econMN.pct
ggsave("PctChange_ECON_MN.jpeg", width=4, height=4, units="in")

division.pct <- ggplot(data=dat.SA.pct.change) +
  geom_bar(aes(x=LID, y=DIVISION, fill=LID), stat="identity") +
  labs(x="Scenario", y="Percent Change, 2011-2061", title="Landscape Division Index (DIVISION)", fill="")
division.pct
ggsave("PctChange_DIVISION.jpeg", width=4, height=4, units="in")

###########################################################################################
# Counties
###########################################################################################

dat.cnty.2011$Scenario <- "2011"
dat.cnty.Q1$Scenario <- "Q1"
dat.cnty.Q2$Scenario <- "Q2"
dat.cnty.Q3$Scenario <- "Q3"
dat.cnty.Q4$Scenario <- "Q4"
dat.cnty.RT$Scenario <- "RT"

dat.cnty.all <- rbind(dat.cnty.2011, dat.cnty.Q1, dat.cnty.Q2, dat.cnty.Q3, dat.cnty.Q4, dat.cnty.RT)

counties <- read.csv("/Users/erincarroll/Desktop/forestfragmentation/analysis/temp/SAcntyOnly.csv")

for (i in 1:length(counties$VALUE)) {
  if (nchar(counties$VALUE[i])==1) {
    counties$VALUE[i] <- paste0(0, counties$VALUE[i])
  }
}

dat.cnty.all$LID <- str_sub(dat.cnty.all$LID, -3, -2)
dat.cnty <- merge(dat.cnty.all, counties, by.x="LID", by.y="VALUE", all=TRUE)
dat.cnty.SA <- dat.cnty[is.na(dat.cnty$NAME)==F,]

dat.cnty.SA.pct <- dat.cnty.SA[dat.cnty.SA$Scenario != "2011",]

dat.cnty.SA$PAFRAC <- as.numeric(as.character(dat.cnty.SA$PAFRAC))
dat.cnty.SA$ENN_MN <- as.numeric(as.character(dat.cnty.SA$ENN_MN))
dat.cnty.SA$ENN_RA <- as.numeric(as.character(dat.cnty.SA$ENN_RA))
dat.cnty.SA$ENN_SD <- as.numeric(as.character(dat.cnty.SA$ENN_SD))

for (i in 1:length(dat.cnty.SA.pct$LID)) {
  county <- dat.cnty.SA.pct$LID[i]
  scenario <- dat.cnty.SA.pct$Scenario[i]
  
  d.2011 <- dat.cnty.SA[dat.cnty.SA$LID==county & dat.cnty.SA$Scenario=="2011", 3:28]
  d.2061 <- dat.cnty.SA[dat.cnty.SA$LID==county & dat.cnty.SA$Scenario==scenario, 3:28]
  
  dat.cnty.SA.pct[i,3:28] <- ((d.2061 - d.2011)/d.2011)*100
}

str(dat.cnty.SA)

###### FIX: NAs for pct change for fields that are stuck as factors

CA.cnty.pct <- ggplot(data=dat.cnty.SA.pct) +
  geom_bar(aes(x=NAME, y=CA, fill=Scenario), stat="identity", position="dodge") +
  labs(x="County", y="Percent Change, 2011-2061", title="Total Area (CA)", fill="") +
  theme(axis.text.x = element_text(angle=45))
CA.cnty.pct
ggsave("Cnty_PctChange_CA.jpeg", width=4, height=4, units="in")


CA.cnty.pct.2 <- ggplot(data=dat.cnty.SA.pct[dat.cnty.SA.pct$LID==32 | dat.cnty.SA.pct$LID==49,]) +
  geom_bar(aes(x=NAME, y=CA, fill=Scenario), stat="identity", position="dodge") +
  labs(x="County", y="Percent Change, 2011-2061", title="Total Area (CA)", fill="")
CA.cnty.pct.2











