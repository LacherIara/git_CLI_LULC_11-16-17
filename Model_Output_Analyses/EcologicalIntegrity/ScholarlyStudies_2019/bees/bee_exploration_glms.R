
setwd("I:/EI_data/plots2016")

bee.data <- read.csv("U:/CLI/Field Surveys/Bees/CLI_Bombus_Environmental_6-20-19.csv")

library(ggplot2)
library(MuMIn)

hist(bee.data$Abundance, breaks = 50)
hist(bee.data$SpRichness)

summary(bee.data$Abundance)

bee.data[bee.data$Abundance > 50,]

data.dredge.gra <- bee.data[,c(16,30,31,32,33)]
gra.models <- glm(Abundance ~ ., data=data.dredge.gra, na.action = "na.fail")
dd.gra <- dredge(gra.models)

data.dredge.dev <- bee.data[,c(16,22,23,24,25)]
dev.models <- glm(Abundance ~ ., data=data.dredge.dev, na.action = "na.fail")
dd.dev <- dredge(dev.models)

data.dredge.for <- bee.data[,c(16,26,27,28,29)]
for.models <- glm(Abundance ~ ., data=data.dredge.for, na.action = "na.fail")
dd.for <- dredge(for.models)

data.dredge.cro <- bee.data[,c(16,18,19,20,21)]
cro.models <- glm(Abundance ~ ., data=data.dredge.cro, na.action = "na.fail")
dd.cro <- dredge(cro.models)

###################################################################################################
# BEES : ABUNDANCE, DEVELOPMENT
###################################################################################################

fit250 <- glm(Abundance ~ dev_pct250, data=bee.data, family = "poisson")
fit500 <- glm(Abundance ~ dev_pct500, data=bee.data, family = "poisson")
fit1k <- glm(Abundance ~ dev_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(Abundance ~ dev_pct5k, data=bee.data, family = "poisson")

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

bees.dev = ggplot() +
  geom_point(data=bee.data, aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=dev_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=dev_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=dev_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Bumble Bees: Abundance, Dev") +
  xlim(0,1.0) +
  ylim(0, 1000) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=825, label=mylabel)
bees.dev

ggsave(bees.dev, file="bees.dev.abundance.png")

###################################################################################################
# BEES : ABUNDANCE, FOREST
###################################################################################################

fit250 <- glm(Abundance ~ for_pct250, data=bee.data, family = "poisson")
fit500 <- glm(Abundance ~ for_pct500, data=bee.data, family = "poisson")
fit1k <- glm(Abundance ~ for_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(Abundance ~ for_pct5k, data=bee.data, family = "poisson")

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

bees.for = ggplot() +
  geom_point(data=bee.data, aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=for_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=for_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=for_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Bumble Bees: Abundance, for") +
  xlim(0,1.0) +
  ylim(0, 1000) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=825, label=mylabel)
bees.for

ggsave(bees.for, file="bees.for.abundance.png")

###################################################################################################
# BEES : ABUNDANCE, GRASS
###################################################################################################

fit250 <- glm(Abundance ~ gra_pct250, data=bee.data, family = "poisson")
fit500 <- glm(Abundance ~ gra_pct500, data=bee.data, family = "poisson")
fit1k <- glm(Abundance ~ gra_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(Abundance ~ gra_pct5k, data=bee.data, family = "poisson")

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

bees.gra = ggplot() +
  geom_point(data=bee.data, aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=gra_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=gra_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=gra_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Bumble Bees: Abundance, gra") +
  xlim(0,1.0) +
  ylim(0, 1000) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=825, label=mylabel)
bees.gra

ggsave(bees.gra, file="bees.gra.abundance.png")

###################################################################################################
# BEES : ABUNDANCE, CROP
###################################################################################################

fit250 <- glm(Abundance ~ cro_pct250, data=bee.data, family = "poisson")
fit500 <- glm(Abundance ~ cro_pct500, data=bee.data, family = "poisson")
fit1k <- glm(Abundance ~ cro_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(Abundance ~ cro_pct5k, data=bee.data, family = "poisson")

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

bees.cro = ggplot() +
  geom_point(data=bee.data, aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=cro_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=cro_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=cro_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=Abundance, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Bumble Bees: Abundance, cro") +
  xlim(0,1.0) +
  ylim(0, 1000) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=825, label=mylabel)
bees.cro

ggsave(bees.cro, file="bees.cro.abundance.png")

###################################################################################################
# BEES : SPRICHNESS, DEVELOPMENT
###################################################################################################

fit250 <- glm(SpRichness ~ dev_pct250, data=bee.data, family = "poisson")
fit500 <- glm(SpRichness ~ dev_pct500, data=bee.data, family = "poisson")
fit1k <- glm(SpRichness ~ dev_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(SpRichness ~ dev_pct5k, data=bee.data, family = "poisson")

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

bees.dev = ggplot() +
  geom_point(data=bee.data, aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=dev_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=dev_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=dev_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Bumble Bees: SpRichness, Dev") +
  xlim(0,1.0) +
  ylim(0, 10) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=8.75, label=mylabel)
bees.dev

ggsave(bees.dev, file="bees.dev.sprichness.png")

###################################################################################################
# BEES : SPRICHNESS, FOREST
###################################################################################################

fit250 <- glm(SpRichness ~ for_pct250, data=bee.data, family = "poisson")
fit500 <- glm(SpRichness ~ for_pct500, data=bee.data, family = "poisson")
fit1k <- glm(SpRichness ~ for_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(SpRichness ~ for_pct5k, data=bee.data, family = "poisson")

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

bees.for = ggplot() +
  geom_point(data=bee.data, aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=for_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=for_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=for_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Bumble Bees: SpRichness, for") +
  xlim(0,1.0) +
  ylim(0, 10) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=8.75, label=mylabel)
bees.for

ggsave(bees.for, file="bees.for.sprichness.png")

###################################################################################################
# BEES : SPRICHNESS, GRASS
###################################################################################################

fit250 <- glm(SpRichness ~ gra_pct250, data=bee.data, family = "poisson")
fit500 <- glm(SpRichness ~ gra_pct500, data=bee.data, family = "poisson")
fit1k <- glm(SpRichness ~ gra_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(SpRichness ~ gra_pct5k, data=bee.data, family = "poisson")

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

bees.gra = ggplot() +
  geom_point(data=bee.data, aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=gra_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=gra_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=gra_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Bumble Bees: SpRichness, gra") +
  xlim(0,1.0) +
  ylim(0, 10) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=8.75, label=mylabel)
bees.gra

ggsave(bees.gra, file="bees.gra.sprichness.png")

###################################################################################################
# BEES : SPRICHNESS, CROP
###################################################################################################

fit250 <- glm(SpRichness ~ cro_pct250, data=bee.data, family = "poisson")
fit500 <- glm(SpRichness ~ cro_pct500, data=bee.data, family = "poisson")
fit1k <- glm(SpRichness ~ cro_pct1k, data=bee.data, family = "poisson")
fit5k <- glm(SpRichness ~ cro_pct5k, data=bee.data, family = "poisson")

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

bees.cro = ggplot() +
  geom_point(data=bee.data, aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=cro_pct250, color="250m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=cro_pct500, color="500m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=cro_pct1k, color="1000m")) +
  geom_point(data=bee.data, aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = F,
              method.args = list(family = "poisson"),
              fullrange = TRUE,
              data=bee.data,
              aes(y=SpRichness, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Bumble Bees: SpRichness, cro") +
  xlim(0,1.0) +
  ylim(0, 10) +
  scale_color_manual(name="Radius", labels=c("250m", "500m", "1000m", "5000m"), values=c("red", "orange", "darkgreen", "blue")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=8.75, label=mylabel)
bees.cro

ggsave(bees.cro, file="bees.cro.sprichness.png")


