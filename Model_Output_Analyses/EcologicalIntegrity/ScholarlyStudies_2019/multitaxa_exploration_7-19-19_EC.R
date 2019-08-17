
setwd("I:/EI_data/plots2016/7-26-19")

library(ggplot2)

## read in data
birds.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Environmental_6-17-19.csv")
bees.data <- read.csv("U:/CLI/Field Surveys/Bees/CLI_Bombus_Environmental_6-20-19.csv")
mammals.data <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammals_Environmental_7-1-19.csv")
    names(mammals.data)[26] <- "Abundance.og"
    names(mammals.data)[27] <- "SpRichness.og"
    mammals.data$Abundance <- round(mammals.data$Abundance_ *10, digits=0) 
    mammals.data$SpRichness <- round(mammals.data$SpRichne_1*10, digits=0)
invasives.data <- read.csv("U:/CLI/Field Surveys/Invasive/CLI_Invasives_Environmental_7-19-19.csv")


###########################################################################################################
# Abundance, Dev
###########################################################################################################

variables.250 = list(birds.data$dev_pct250, bees.data$dev_pct250, mammals.data$dev_pct250, invasives.data$dev_pct250)
  comment(variables.250) = "250m"
variables.500 = list(birds.data$dev_pct500, bees.data$dev_pct500, mammals.data$dev_pct500, invasives.data$dev_pct500)
  comment(variables.500) = "500m"
variables.1000 = list(birds.data$dev_pct1k, bees.data$dev_pct1k, mammals.data$dev_pct1k, invasives.data$dev_pct1k)
  comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$dev_pct5k, bees.data$dev_pct5k, mammals.data$dev_pct5k, invasives.data$dev_pct5k)
  comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$Abundance~distances[[i]][[1]], family = "poisson")
    sum.bird = summary(model.bird)
    AIC.bird = sum.bird$aic
    p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$Abundance~distances[[i]][[2]], family = "poisson")
    sum.bee = summary(model.bee)
    AIC.bee = sum.bee$aic
    p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$Abundance~distances[[i]][[3]], family = "poisson")
    sum.mammal = summary(model.mammal)
    AIC.mammal = sum.mammal$aic
    p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$Abundance~distances[[i]][[4]], family = "poisson")
    sum.invasive = summary(model.invasive)
    AIC.invasive = sum.invasive$aic
    p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Development") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 1000) +
  scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
  theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=1000*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("dev.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Forest
###########################################################################################################

variables.250 = list(birds.data$for_pct250, bees.data$for_pct250, mammals.data$for_pct250, invasives.data$for_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$for_pct500, bees.data$for_pct500, mammals.data$for_pct500, invasives.data$for_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$for_pct1k, bees.data$for_pct1k, mammals.data$for_pct1k, invasives.data$for_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$for_pct5k, bees.data$for_pct5k, mammals.data$for_pct5k, invasives.data$for_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$Abundance~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$Abundance~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$Abundance~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$Abundance~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Forest") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 1000) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=1000*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("for.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Grass
###########################################################################################################

variables.250 = list(birds.data$gra_pct250, bees.data$gra_pct250, mammals.data$gra_pct250, invasives.data$gra_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$gra_pct500, bees.data$gra_pct500, mammals.data$gra_pct500, invasives.data$gra_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$gra_pct1k, bees.data$gra_pct1k, mammals.data$gra_pct1k, invasives.data$gra_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$gra_pct5k, bees.data$gra_pct5k, mammals.data$gra_pct5k, invasives.data$gra_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$Abundance~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$Abundance~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$Abundance~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$Abundance~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Grass") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 1000) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=1000*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("gra.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Crop
###########################################################################################################

variables.250 = list(birds.data$cro_pct250, bees.data$cro_pct250, mammals.data$cro_pct250, invasives.data$cro_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$cro_pct500, bees.data$cro_pct500, mammals.data$cro_pct500, invasives.data$cro_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$cro_pct1k, bees.data$cro_pct1k, mammals.data$cro_pct1k, invasives.data$cro_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$cro_pct5k, bees.data$cro_pct5k, mammals.data$cro_pct5k, invasives.data$cro_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$Abundance~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$Abundance~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$Abundance~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$Abundance~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=Abundance, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=Abundance, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=Abundance, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Crop") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 1000) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=1000*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("cro.", name, ".Abundance.png"))
}

###########################################################################################################
# SpRichness, Dev
###########################################################################################################

variables.250 = list(birds.data$dev_pct250, bees.data$dev_pct250, mammals.data$dev_pct250, invasives.data$dev_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$dev_pct500, bees.data$dev_pct500, mammals.data$dev_pct500, invasives.data$dev_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$dev_pct1k, bees.data$dev_pct1k, mammals.data$dev_pct1k, invasives.data$dev_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$dev_pct5k, bees.data$dev_pct5k, mammals.data$dev_pct5k, invasives.data$dev_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$SpRichness~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$SpRichness~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$SpRichness~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$SpRichness~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Development") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 45) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=45*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("dev.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Forest
###########################################################################################################

variables.250 = list(birds.data$for_pct250, bees.data$for_pct250, mammals.data$for_pct250, invasives.data$for_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$for_pct500, bees.data$for_pct500, mammals.data$for_pct500, invasives.data$for_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$for_pct1k, bees.data$for_pct1k, mammals.data$for_pct1k, invasives.data$for_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$for_pct5k, bees.data$for_pct5k, mammals.data$for_pct5k, invasives.data$for_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$SpRichness~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$SpRichness~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$SpRichness~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$SpRichness~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Forest") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 45) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=45*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("for.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Grass
###########################################################################################################

variables.250 = list(birds.data$gra_pct250, bees.data$gra_pct250, mammals.data$gra_pct250, invasives.data$gra_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$gra_pct500, bees.data$gra_pct500, mammals.data$gra_pct500, invasives.data$gra_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$gra_pct1k, bees.data$gra_pct1k, mammals.data$gra_pct1k, invasives.data$gra_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$gra_pct5k, bees.data$gra_pct5k, mammals.data$gra_pct5k, invasives.data$gra_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$SpRichness~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$SpRichness~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$SpRichness~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$SpRichness~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Grass") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 45) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=45*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("gra.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Crop
###########################################################################################################

variables.250 = list(birds.data$cro_pct250, bees.data$cro_pct250, mammals.data$cro_pct250, invasives.data$cro_pct250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$cro_pct500, bees.data$cro_pct500, mammals.data$cro_pct500, invasives.data$cro_pct500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$cro_pct1k, bees.data$cro_pct1k, mammals.data$cro_pct1k, invasives.data$cro_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$cro_pct5k, bees.data$cro_pct5k, mammals.data$cro_pct5k, invasives.data$cro_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = glm(birds.data$SpRichness~distances[[i]][[1]], family = "poisson")
  sum.bird = summary(model.bird)
  AIC.bird = sum.bird$aic
  p.bird = sum.bird$coefficients[2,4]
  model.bee = glm(bees.data$SpRichness~distances[[i]][[2]], family = "poisson")
  sum.bee = summary(model.bee)
  AIC.bee = sum.bee$aic
  p.bee = sum.bee$coefficients[2,4]
  model.mammal = glm(mammals.data$SpRichness~distances[[i]][[3]], family = "poisson")
  sum.mammal = summary(model.mammal)
  AIC.mammal = sum.mammal$aic
  p.mammal = sum.mammal$coefficients[2,4]
  model.invasive = glm(invasives.data$SpRichness~distances[[i]][[4]], family = "poisson")
  sum.invasive = summary(model.invasive)
  AIC.invasive = sum.invasive$aic
  p.invasive = sum.invasive$coefficients[2,4]
  
  to.round = c(p.bird, AIC.bird, p.bee, AIC.bee, p.mammal, AIC.mammal, p.invasive, AIC.invasive)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird p=", metrics[1], ", AIC=", metrics[2], "\n", "bee p=", metrics[3], ", AIC=", metrics[4], "\n", "mammal p=", metrics[5], ", AIC=", metrics[6], "\n", "invasives p=", metrics[7], ", AIC=", metrics[8], "\n")
  
  name = names[i]
  
  multitaxa = ggplot() +
    geom_point(data=birds.data, aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=birds.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=bees.data,
                aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammals.data,
                aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=invasives.data,
                aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Crop") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 45) +
    scale_color_manual(name="Taxa", values=c("birds"="red", "bees"="orange", "mammals"="darkgreen", "invasives"="blue"), breaks=c("birds", "bees", "mammals", "invasives")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=45*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("cro.", name, ".SpRichness.png"))
}

###########################################################################################################
# Correlation Matrices
###########################################################################################################

# bees
dat.cor.bees <- bees.data[,c(16:17,5:15,18:41)]
cor.bees <- rcorr(as.matrix(dat.cor.bees))
corrplot(cor.bees$r, type="upper", p.mat=cor.bees$P, sig.level=0.1, tl.col="black", insig="label_sig", pch.cex=2)

# birds - can't include individual species, too many
dat.cor.birds <- birds.data[,c(104:105,106:129)]
cor.birds <- rcorr(as.matrix(dat.cor.birds))
corrplot(cor.birds$r, type="upper", p.mat=cor.birds$P, sig.level=0.1, tl.col="black", insig="label_sig", pch.cex=2)

# invasives
dat.cor.invasives <- invasives.data[,c(15:16,2:14,20:43)]
cor.invasives <- rcorr(as.matrix(dat.cor.invasives))
corrplot(cor.invasives$r, type="upper", p.mat=cor.invasives$P, sig.level=0.1, tl.col="black", insig="label_sig", pch.cex=2)

# mammals
dat.cor.mammals <- mammals.data[,c(58:59,3:25,34:57)]
cor.mammals <- rcorr(as.matrix(dat.cor.mammals))
corrplot(cor.mammals$r, type="upper", p.mat=cor.mammals$P, sig.level=0.1, tl.col="black", insig="label_sig", pch.cex=2)


#######################################################################################
# checking on zero-inflation

hist(birds.data$Abundance) # distribution looks approximately normal? few zeros
hist(bees.data$Abundance) # either poisson or negative binomial? lots of zeros
hist(mammals.data$Abundance) # either poisson or negative binomial? lots of zeros
hist(invasives.data$Abundance) # either poisson or negative binomial? lots of zeros

sum(birds.data$Abundance==0)
sum(birds.data$SpRichness==0)
  #bird Abundance and SpRichness never = 0
sum(bees.data$Abundance==0)
sum(bees.data$SpRichness==0)
  #bee abundance and sprichness never = 0
sum(mammals.data$Abundance==0)
sum(mammals.data$SpRichness==0)
  ## weird because adjusted and rounded. Non-adjusted abundance and sprichness values never = 0
sum(invasives.data$Abundance==0)
sum(invasives.data$SpRichness==0)

## but the zero inflation we're interested in is the explanatory variable, % lu?

# https://aosmith.rbind.io/2019/03/06/lots-of-zeros/
# look more into negative binomial distributions













