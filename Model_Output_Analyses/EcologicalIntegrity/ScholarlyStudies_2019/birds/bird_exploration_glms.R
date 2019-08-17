
setwd("I:/EI_data/plots2016/7-26-19")

bird.data <- read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Environmental_6-17-19.csv")

library(ggplot2)
library(MuMIn)

hist(bird.data$Abundance)
hist(bird.data$SpRichness)


data.dredge.cro <- bird.data[,c(103,105,106,107,108)]
cro.models <- glm(Abundance ~ ., data=data.dredge.cro)
dd.cro <- dredge(cro.models)

data.dredge.gra <- bird.data[,c(103,117,118,119,120)]
gra.models <- glm(Abundance ~ ., data=data.dredge.gra)
dd.gra <- dredge(gra.models)

data.dredge.for <- bird.data[,c(103,113,114,115,116)]
for.models <- glm(Abundance ~ ., data=data.dredge.for)
dd.for <- dredge(for.models)

data.dredge.dev <- bird.data[,c(103,109,110,111,112)]
dev.models <- glm(Abundance ~ ., data=data.dredge.dev)
dd.dev <- dredge(dev.models)

###################################################################################################
# BIRDS : ABUNDANCE, DEVELOPMENT
###################################################################################################

fit250 <- glm(Abundance ~ dev_pct250, data=bird.data, family = "poisson")
fit500 <- glm(Abundance ~ dev_pct500, data=bird.data, family = "poisson")
fit1k <- glm(Abundance ~ dev_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(Abundance ~ dev_pct5k, data=bird.data, family = "poisson")

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

birds.dev = ggplot() +
  geom_point(data=bird.data, aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Birds: Abundance, Dev") +
  xlim(0,1.0) +
  ylim(0, 225) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=200, label=mylabel)
birds.dev

ggsave(birds.dev, file="birds.dev.abundance.png")

###################################################################################################
# BIRDS : ABUNDANCE, FOREST
###################################################################################################

fit250 <- glm(Abundance ~ for_pct250, data=bird.data, family = "poisson")
fit500 <- glm(Abundance ~ for_pct500, data=bird.data, family = "poisson")
fit1k <- glm(Abundance ~ for_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(Abundance ~ for_pct5k, data=bird.data, family = "poisson")

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

birds.for = ggplot() +
  geom_point(data=bird.data, aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Birds: Abundance, for") +
  xlim(0,1.0) +
  ylim(0, 225) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=200, label=mylabel)
birds.for

ggsave(birds.for, file="birds.for.abundance.png")


###################################################################################################
# BIRDS : ABUNDANCE, GRASS
###################################################################################################

fit250 <- glm(Abundance ~ gra_pct250, data=bird.data, family = "poisson")
fit500 <- glm(Abundance ~ gra_pct500, data=bird.data, family = "poisson")
fit1k <- glm(Abundance ~ gra_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(Abundance ~ gra_pct5k, data=bird.data, family = "poisson")

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

birds.gra = ggplot() +
  geom_point(data=bird.data, aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Birds: Abundance, gra") +
  xlim(0,1.0) +
  ylim(0, 225) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=200, label=mylabel)
birds.gra

ggsave(birds.gra, file="birds.gra.abundance.png")


###################################################################################################
# BIRDS : ABUNDANCE, CROP
###################################################################################################

fit250 <- glm(Abundance ~ cro_pct250, data=bird.data, family = "poisson")
fit500 <- glm(Abundance ~ cro_pct500, data=bird.data, family = "poisson")
fit1k <- glm(Abundance ~ cro_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(Abundance ~ cro_pct5k, data=bird.data, family = "poisson")

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

birds.cro = ggplot() +
  geom_point(data=bird.data, aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Birds: Abundance, cro") +
  xlim(0,1.0) +
  ylim(0, 225) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=200, label=mylabel)
birds.cro

ggsave(birds.cro, file="birds.cro.abundance.png")


###################################################################################################
# BIRDS : SPRICHNESS, DEVELOPMENT
###################################################################################################

fit250 <- glm(SpRichness ~ dev_pct250, data=bird.data, family = "poisson")
fit500 <- glm(SpRichness ~ dev_pct500, data=bird.data, family = "poisson")
fit1k <- glm(SpRichness ~ dev_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(SpRichness ~ dev_pct5k, data=bird.data, family = "poisson")

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

birds.dev = ggplot() +
  geom_point(data=bird.data, aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Birds: SpRichness, Dev") +
  xlim(0,1.0) +
  ylim(0, 50) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=45, label=mylabel)
birds.dev

ggsave(birds.dev, file="birds.dev.sprichness.png")

###################################################################################################
# BIRDS : SPRICHNESS, FOREST
###################################################################################################

fit250 <- glm(SpRichness ~ for_pct250, data=bird.data, family = "poisson")
fit500 <- glm(SpRichness ~ for_pct500, data=bird.data, family = "poisson")
fit1k <- glm(SpRichness ~ for_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(SpRichness ~ for_pct5k, data=bird.data, family = "poisson")

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

birds.for = ggplot() +
  geom_point(data=bird.data, aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Birds: SpRichness, for") +
  xlim(0,1.0) +
  ylim(0, 50) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=45, label=mylabel)
birds.for

ggsave(birds.for, file="birds.for.sprichness.png")

###################################################################################################
# BIRDS : SPRICHNESS, GRASS
###################################################################################################

fit250 <- glm(SpRichness ~ gra_pct250, data=bird.data, family = "poisson")
fit500 <- glm(SpRichness ~ gra_pct500, data=bird.data, family = "poisson")
fit1k <- glm(SpRichness ~ gra_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(SpRichness ~ gra_pct5k, data=bird.data, family = "poisson")

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

birds.gra = ggplot() +
  geom_point(data=bird.data, aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Birds: SpRichness, gra") +
  xlim(0,1.0) +
  ylim(0, 50) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=45, label=mylabel)
birds.gra

ggsave(birds.gra, file="birds.gra.sprichness.png")

###################################################################################################
# BIRDS : SPRICHNESS, CROP
###################################################################################################

fit250 <- glm(SpRichness ~ cro_pct250, data=bird.data, family = "poisson")
fit500 <- glm(SpRichness ~ cro_pct500, data=bird.data, family = "poisson")
fit1k <- glm(SpRichness ~ cro_pct1k, data=bird.data, family = "poisson")
fit5k <- glm(SpRichness ~ cro_pct5k, data=bird.data, family = "poisson")

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

birds.cro = ggplot() +
  geom_point(data=bird.data, aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_point(data=bird.data, aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=bird.data,
              aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Birds: SpRichness, cro") +
  xlim(0,1.0) +
  ylim(0, 50) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=45, label=mylabel)
birds.cro

ggsave(birds.cro, file="birds.cro.sprichness.png")







