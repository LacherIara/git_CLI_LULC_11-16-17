
setwd("I:/EI_data/plots2016/7-26-19")

invasive.data <- read.csv("U:/CLI/Field Surveys/Invasive/CLI_Invasives_Environmental_7-19-19.csv")

library(ggplot2)

###################################################################################################
# INVASIVES : ABUNDANCE, DEVELOPMENT
###################################################################################################

fit250 <- glm(Abundance ~ dev_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(Abundance ~ dev_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(Abundance ~ dev_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(Abundance ~ dev_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.dev = ggplot() +
  geom_point(data=invasive.data, aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Invasives: Abundance, Dev") +
  xlim(0,1.0) +
  ylim(0, 65) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=55, label=mylabel)
invasives.dev

ggsave(invasives.dev, file="invasives.dev.Abundance.png")

###################################################################################################
# INVASIVES : ABUNDANCE, FOREST
###################################################################################################

fit250 <- glm(Abundance ~ for_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(Abundance ~ for_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(Abundance ~ for_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(Abundance ~ for_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.for = ggplot() +
  geom_point(data=invasive.data, aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Invasives: Abundance, for") +
  xlim(0,1.0) +
  ylim(0, 65) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=55, label=mylabel)
invasives.for

ggsave(invasives.for, file="invasives.for.Abundance.png")

###################################################################################################
# INVASIVES : ABUNDANCE, GRASS
###################################################################################################

fit250 <- glm(Abundance ~ gra_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(Abundance ~ gra_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(Abundance ~ gra_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(Abundance ~ gra_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.gra = ggplot() +
  geom_point(data=invasive.data, aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Invasives: Abundance, gra") +
  xlim(0,1.0) +
  ylim(0, 65) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=55, label=mylabel)
invasives.gra

ggsave(invasives.gra, file="invasives.gra.Abundance.png")

###################################################################################################
# INVASIVES : ABUNDANCE, CROP
###################################################################################################

fit250 <- glm(Abundance ~ cro_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(Abundance ~ cro_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(Abundance ~ cro_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(Abundance ~ cro_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.cro = ggplot() +
  geom_point(data=invasive.data, aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Invasives: Abundance, cro") +
  xlim(0,1.0) +
  ylim(0, 65) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=55, label=mylabel)
invasives.cro

ggsave(invasives.cro, file="invasives.cro.Abundance.png")

###################################################################################################
# INVASIVES : SpRichness, DEVELOPMENT
###################################################################################################

fit250 <- glm(SpRichness ~ dev_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(SpRichness ~ dev_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(SpRichness ~ dev_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(SpRichness ~ dev_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.dev = ggplot() +
  geom_point(data=invasive.data, aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Invasives: SpRichness, Dev") +
  xlim(0,1.0) +
  ylim(0, 15) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=12, label=mylabel)
invasives.dev

ggsave(invasives.dev, file="invasives.dev.SpRichness.png")

###################################################################################################
# INVASIVES : SPRICHNESS, FOREST
###################################################################################################

fit250 <- glm(SpRichness ~ for_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(SpRichness ~ for_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(SpRichness ~ for_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(SpRichness ~ for_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.for = ggplot() +
  geom_point(data=invasive.data, aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Invasives: SpRichness, for") +
  xlim(0,1.0) +
  ylim(0, 15) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=12, label=mylabel)
invasives.for

ggsave(invasives.for, file="invasives.for.SpRichness.png")

###################################################################################################
# INVASIVES : SPRICHNESS, GRASS
###################################################################################################

fit250 <- glm(SpRichness ~ gra_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(SpRichness ~ gra_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(SpRichness ~ gra_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(SpRichness ~ gra_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.gra = ggplot() +
  geom_point(data=invasive.data, aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Invasives: SpRichness, gra") +
  xlim(0,1.0) +
  ylim(0, 15) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=12, label=mylabel)
invasives.gra

ggsave(invasives.gra, file="invasives.gra.SpRichness.png")

###################################################################################################
# INVASIVES : SPRICHNESS, CROP
###################################################################################################

fit250 <- glm(SpRichness ~ cro_pct250, data=invasive.data, family = "poisson")
fit500 <- glm(SpRichness ~ cro_pct500, data=invasive.data, family = "poisson")
fit1k <- glm(SpRichness ~ cro_pct1k, data=invasive.data, family = "poisson")
fit5k <- glm(SpRichness ~ cro_pct5k, data=invasive.data, family = "poisson")

sum250 = summary(fit250)
p.250 = sum250$coefficients[2,4]
AIC.250 = sum250$aic
sum500 = summary(fit500)
p.500 = sum500$coefficients[2,4]
AIC.500 = sum500$aic
sum1k = summary(fit1k)
p.1k = sum1k$coefficients[2,4]
AIC.1k = sum1k$aic
sum5k = summary(fit5k)
p.5k = sum5k$coefficients[2,4]
AIC.5k = sum5k$aic

to.round = c(p.250, AIC.250, p.500, AIC.500, p.1k, AIC.1k, p.5k, AIC.5k)

metrics = round(to.round, digits=3)

mylabel=paste0("250m: p=", metrics[1], ", AIC =", metrics[2], "\n",
               "500m: p=", metrics[3], ", AIC =", metrics[4], "\n",
               "1000m: p=", metrics[5], ", AIC =", metrics[6], "\n",
               "5000m: p=", metrics[7], ", AIC =", metrics[8])

invasives.cro = ggplot() +
  geom_point(data=invasive.data, aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_point(data=invasive.data, aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=invasive.data,
              aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Invasives: SpRichness, cro") +
  xlim(0,1.0) +
  ylim(0, 15) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=12, label=mylabel)
invasives.cro

ggsave(invasives.cro, file=paste0("invasives.cro.SpRichness.png"))


