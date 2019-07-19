
setwd("I:/EI_data/plots2016")

## read in data
birds.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Environmental_6-17-19.csv")
bees.data <- read.csv("U:/CLI/Field Surveys/Bees/CLI_Bombus_Environmental_6-20-19.csv")
mammals.data <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammals_Environmental_7-1-19.csv")
invasives.data <- read.csv("U:/CLI/Field Surveys/Invasive/CLI_Invasives_Environmental_7-19-19.csv")

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
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=birds.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="birds")) +
    geom_point(data=bees.data, aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=bees.data,
                aes(y=SpRichness, x=distances[[i]][[2]], color="bees")) +
    geom_point(data=mammals.data, aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=mammals.data,
                aes(y=SpRichness, x=distances[[i]][[3]], color="mammals")) +
    geom_point(data=invasives.data, aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=invasives.data,
                aes(y=SpRichness, x=distances[[i]][[4]], color="invasives")) +
    xlab("Percent Grass") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, max(bees.data$SpRichness)) +
    scale_color_manual(name="Radius", labels=c("birds", "bees", "mammals", "invasives"), values=c("red", "orange", "darkgreen", "blue")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=max(bees.data$SpRichness)*0.75, label=mylabel)
  
  ggsave(multitaxa, file=paste0("gra.", name, ".SpRichness.png"))
}
