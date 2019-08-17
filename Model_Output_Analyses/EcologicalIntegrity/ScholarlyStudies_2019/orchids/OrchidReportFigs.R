
setwd("U:/CLI/Field Surveys/Orchid/Pilot2019/Report/Figs")

library(ggplot2)
library(scales)
library(reshape)

dat <- read.csv("U:/CLI/Field Surveys/Orchid/CLI_Orchids_Environmental_8-2-19.csv")

dat.plots <- read.csv("U:/CLI/Field Surveys/Orchid/Plots.csv")
dat.orchids <- read.csv("U:/CLI/Field Surveys/Orchid/Orchids.csv")

# date distribution
survey1 <- dat.plots[-c(1,3,9,17),c(3,7,8)]
  names(survey1)[3] <- "SurveyDate"
  survey1$type <- "Orchid1"
survey2 <- dat.plots[-c(1,3,17),c(3,11,10)]
  names(survey2)[2] <- "AssignedSurveyors"
  names(survey2)[3] <- "SurveyDate"
  survey2$type <- "Orchid2"
surveyFH <- dat.plots[-c(1,3,17),c(3,12)]
  names(surveyFH)[2] <- "SurveyDate"
  surveyFH$type <- "ForestHealth"
  surveyFH$AssignedSurveyors <- "VWL"
  
dat.plots2 <- rbind(survey1, survey2, surveyFH)
dat.plots2$type <- as.factor(dat.plots2$type)
dat.plots2$SurveyDate <- as.Date(dat.plots2$SurveyDate, format="%m/%d/%y")
dat.plots2$team <- NA
  dat.plots2$team[c(3:5,7,9:12,17,21:22,25,28,29,35)] <- "CitSci"
  dat.plots2$team[is.na(dat.plots2$team)==T] <- "VWL/CLI"
  
max(dat.plots2$SurveyDate) - min(dat.plots2$SurveyDate)

surveydates <- ggplot(data = dat.plots2, aes(x=SurveyDate, fill=type)) +
  geom_histogram(bins=71, colour="black") +
  scale_fill_manual(values=c("#2ca25f", "#756bb1", "#bcbddc"), name="Survey", breaks=c("Orchid1", "Orchid2", "ForestHealth")) +
  scale_x_date(limits = c(as.Date("2019-05-01"), as.Date("2019-07-31")),
               breaks=seq(as.Date("2019-05-01"), as.Date("2019-07-31"), 7),
               # breaks=seq(min(dat.plots2$SurveyDate), max(dat.plots2$SurveyDate), 7),
               labels=date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle=90)) +
  geom_vline(xintercept=as.Date("2019-06-15"), linetype="dashed") +
  labs(title="Survey Dates", y="Count")
ggsave(surveydates, file="SurveyDates.jpeg")

# pie charts
sum(dat.orchids$Count[dat.orchids$Transect != "INCIDENTAL" & dat.orchids$NewFind==1])
sum(dat.orchids$Count[dat.orchids$Transect == "INCIDENTAL" & dat.orchids$NewFind==1])
pie(c(102,260), labels=c("Incidental\nn = 102","Transect\nn = 260"), main="Incidental vs. Transect Orchid Finds", col=c("pink", "darkgreen"))

Species <- unique(dat.orchids$GenusSpecies[dat.orchids$NewFind==1])
for (i in Species) {
  val <- sum(dat.orchids$Count[dat.orchids$NewFind == 1 & dat.orchids$GenusSpecies == i])
  print(paste(i, ":", val))
}
#pie(c(152,156,37,12,2,2,1), labels=c("Galearis Spectabilis", "Goodyera pubescens", "Tipularia #discolor", "Cypripedium acaule", "Platanthers spp", "Liparis liliifolia", "Platanthera peramoena"), #col=rainbow((length(Species))))

orch.spp <- data.frame("Genus" = c("Galearis", "Goodyera", "Tipularia", "Cypripedium", "Platanthera", "Liparis", "Platanthera"), "Species" = c("spectabilis", "pubescens", "discolor", "acaule", "spp", "liliifolia", "peramoena"), "Count" = c(152,156,37,12,2,2,1))

pie <- ggplot(data = orch.spp, aes(x="", y=Count, fill=Species)) +
  geom_bar(stat="identity", width=1)
pie
pie <- pie + coord_polar("y", start=0) +
  geom_text(aes(label = paste0(Genus, "\n", Species, "\nn = ", Count)), position = position_stack(vjust = 0.5))
pie
pie <- pie + scale_fill_manual(values=rainbow(7))
pie
pie <- pie + labs(x = NULL, y = NULL, fill = NULL, title = "Orchid Species")
pie
pie <- pie + theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust=-6),
        legend.position = "none")
pie
# just do it in excel...

#######
# 1C
str(dat.plots2)
dat.plots2$order <- NA
  dat.plots2$order[dat.plots2$type=="Orchid1"] <- 1
  dat.plots2$order[dat.plots2$type=="Orchid2"] <- 2
  dat.plots2$order[dat.plots2$type=="ForestHealth"] <- 3
dat.plots2$type <- factor(dat.plots2$type, levels=unique(dat.plots2$type[order(dat.plots2$order)]))
levels(dat.plots2$type)

Surveyors <- ggplot(data=dat.plots2) +
  geom_bar(aes(x=type, fill=team)) +
  scale_fill_manual(values = c("#756bb1", "#2ca25f"))+
  labs(title="Survey Teams", y="Count", x="Survey Type", fill="Team")
ggsave(Surveyors, file="1C.Surveyors.jpeg")

######
# 2a
str(orchid.melt)

orchid.melt <- melt(dat.orchids[dat.orchids$NewFind==1,], id.vars = c("Transect", "GenusSpecies", "PlotID"), measure.vars = "Count")
orchid.melt$Transect <- as.character(orchid.melt$Transect)
orchid.melt$Transect[orchid.melt$Transect != "INCIDENTAL"] <- "On Plot"
orchid.melt$Transect <- as.factor(orchid.melt$Transect)

Spp <- unique(orchid.melt$GenusSpecies)
orchid.melt$Count <- 0
for (i in Spp) {
  orchid.melt$Count[orchid.melt$GenusSpecies==i] <- sum(orchid.melt$value[orchid.melt$GenusSpecies==i])
}

spp2a <- ggplot(data=orchid.melt, aes(x=reorder(GenusSpecies, -Count), y=value)) +
  geom_bar(aes(fill=Transect), stat = "identity", position="stack") +
  scale_fill_manual(values=c("#756bb1", "#2ca25f"), breaks=c("On Plot", "INCIDENTAL"), labels=c("On Plot", "Incidental")) +
  theme(axis.text.x = element_text(angle=30, vjust=0.7),
        legend.position = c(0.9, 0.9),
        legend.background = element_blank()) +
  labs(title="Orchid Species", y="Count", x="Species", fill="")
spp2a
ggsave(spp2a, file="2a.SpeciesbyTransect.jpeg")

######
# 2b

Site <- unique(orchid.melt$PlotID)
orchid.melt$Count <- NA
for (i in Site) {
  orchid.melt$Count[orchid.melt$PlotID==i] <- sum(orchid.melt$value[orchid.melt$PlotID==i])
}

spp2b <- ggplot(data=orchid.melt) +
  geom_bar(aes(x=reorder(PlotID, -Count), y=value, fill=Transect), stat="identity") +
  scale_fill_manual(values=c("#756bb1", "#2ca25f"), breaks=c("On Plot", "INCIDENTAL"), labels=c("On Plot", "Incidental")) +
  theme(axis.text.x = element_text(angle=30, vjust=0.7),
        legend.position = c(0.9, 0.9),
        legend.background = element_blank()) +
  labs(title="Orchid Abundance by Site", x="Site", y="Count", fill="")
spp2b
ggsave(spp2b, file="2b.AbundancebySite.jpeg")

#############################################
# actual model data
#############################################

hist(dat$Abundance)

hist(dat$SpRichness)

par(mfrow=c(1,4))
hist(dat$for_pct250, xlim=c(0,1), breaks=10)
hist(dat$for_pct500, xlim=c(0,1), breaks=10)
hist(dat$for_pct1k, xlim=c(0,1), breaks=10)
hist(dat$for_pct5k, xlim=c(0,1), breaks=10)

dat.melt.pctforest <- melt(dat, id.vars = "Site", measure.vars = c("for_pct250", "for_pct500", "for_pct1k", "for_pct5k"))
pctforest.dist <- ggplot(data = dat.melt.pctforest) +
  facet_wrap(~variable, nrow=1) +
  geom_histogram(aes(x=value, fill=variable), bins=10) +
  scale_fill_manual(values=c("#41ab5d", "#238b45", "#006d2c", "#00441b"), name="Radius", labels=c("250m", "500m", "1000m", "5000m")) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust=0, face="bold")) +
  ggtitle("Percent Forest")
pctforest.dist

hist(dat$core_for1k)

dat$cro_pct250





