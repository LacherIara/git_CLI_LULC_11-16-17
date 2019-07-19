
setwd("I:/EI_data/plots2016")

library(ggplot2)

bird.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Environmental_6-17-19.csv")

## separate into grassland and shrubland species
bird.type = read.csv("U:/CLI/Field Surveys/Birds/AOUcodes_SppStatuses_4-23-19_EC.csv")
bird.grass = bird.type$Species[bird.type$Target.Species == "grassland"]
  # [1] AMKE BRTH BWWA DICK EAKI EAME GRSP HESP NOBO NOHA SAVS VESP
bird.shrub = bird.type$Species[bird.type$Target.Species == "shrubland"]
  # [1] BLGR BOBO COYE EABL EATO FISP INBU LOSH PRAW RWBL WEVI WIFL YBCH YBCU

bird.grass.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Grassland_Environmental_6-17-19.csv")
bird.shrub.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Shrub_Environmental_6-17-19.csv")

hist(bird.grass.data$Abundance)
hist(bird.grass.data$SpRichness)

hist(bird.shrub.data$Abundance)
hist(bird.shrub.data$SpRichness)

################################################################################################
## REDO EVERYTHING BELOW - WANT IT BY DISTANCE, ALL 3 GROUPS ON ONE PLOT

comment(bird.data) = "AllBirds"
comment(bird.grass.data) = "GrasslandBirds"
comment(bird.shrub.data) = "ShrubBirds"

for (i in list(bird.data, bird.grass.data, bird.shrub.data)) {
  fit250 <- glm(Abundance ~ cro_pct250, data=i, family = "poisson")
    sum250 = summary(fit250)
    p.250 = sum250$coefficients[2,4]
    AIC.250 = sum250$aic
  fit500 <- glm(Abundance ~ cro_pct500, data=i, family = "poisson")
    sum500 = summary(fit500)
    p.500 = sum500$coefficients[2,4]
    AIC.500 = sum500$aic
  fit1k <- glm(Abundance ~ cro_pct1k, data=i, family = "poisson")
    sum1k = summary(fit1k)
    p.1k = sum1k$coefficients[2,4]
    AIC.1k = sum1k$aic
  fit5k <- glm(Abundance ~ cro_pct5k, data=i, family = "poisson")
    sum5k = summary(fit5k)
    p.5k = sum5k$coefficients[2,4]
    AIC.5k = sum5k$aic
  
  to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)
  metrics = round(to.round, digits=3)
  mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
                 "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
                 "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
                 "5000m: p=", metrics[7], ", AIC =", metrics[8])
  
  name = comment(i)
  
  bmax = max(i$Abundance) + (max(i$Abundance)/10)
  blabel = max(i$Abundance) - (max(i$Abundance)/10)
  
  abundance.cro = ggplot() +
    geom_point(data=i, aes(y=Abundance, x=cro_pct250, color="250m")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=i,
                aes(y=Abundance, x=cro_pct250, color="250m")) +
    geom_point(data=i, aes(y=Abundance, x=cro_pct500, color="500m")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=i,
                aes(y=Abundance, x=cro_pct500, color="500m")) +
    geom_point(data=i, aes(y=Abundance, x=cro_pct1k, color="1000m")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=i,
                aes(y=Abundance, x=cro_pct1k, color="1000m")) +
    geom_point(data=i, aes(y=Abundance, x=cro_pct5k, color="5000m")) +
    geom_smooth(method="glm",
                se = F,
                method.args = list(family = "poisson"),
                fullrange = TRUE,
                data=i,
                aes(y=Abundance, x=cro_pct5k, color="5000m")) +
    xlab("Percent Crop") +
    ggtitle(paste0(name, ": Abundance, cro")) +
    xlim(0,1.0) +
    ylim(0, bmax) +
    scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=blabel, label=mylabel)
  
  ggsave(abundance.cro, file=paste0(name, ".cro.abundance.png"))
}







