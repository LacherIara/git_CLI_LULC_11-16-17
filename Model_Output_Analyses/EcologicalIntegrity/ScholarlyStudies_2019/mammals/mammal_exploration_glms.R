
setwd("I:/EI_data/plots2016/7-26-19")

mammal.data <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammals_Environmental_7-1-19.csv")

names(mammal.data)[32] <- "Abundance.adj"
names(mammal.data)[33] <- "SpRichness.adj"

library(ggplot2)
library(MuMIn)

hist(mammal.data$Abundance.adj, breaks=50)
hist(mammal.data$SpRichness.adj)

summary(mammal.data$Abundance)

mammal.data[mammal.data$Abundance > 50,]

data.dredge.gra <- mammal.data[,c(33,47,48,49,50)]
gra.models <- glm(Abundance.adj ~ ., data=data.dredge.gra, na.action = "na.fail")
dd.gra <- dredge(gra.models)

data.dredge.dev <- mammal.data[,c(33,39,40,41,42)]
dev.models <- glm(Abundance.adj ~ ., data=data.dredge.dev, na.action = "na.fail")
dd.dev <- dredge(dev.models)

data.dredge.for <- mammal.data[,c(33,43,44,45,46)]
for.models <- glm(Abundance.adj ~ ., data=data.dredge.for, na.action = "na.fail")
dd.for <- dredge(for.models)

data.dredge.cro <- mammal.data[,c(33,35,36,37,38)]
cro.models <- glm(Abundance.adj ~ ., data=data.dredge.cro, na.action = "na.fail")
dd.cro <- dredge(cro.models)

###################################################################################################
# MAMMALS : ABUNDANCE, DEVELOPMENT
###################################################################################################

mammal.data$Abundance.adj.2 <- round(mammal.data$Abundance.adj*10, digits=0)

fit250 <- glm(Abundance.adj.2 ~ dev_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(Abundance.adj.2 ~ dev_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(Abundance.adj.2 ~ dev_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(Abundance.adj.2 ~ dev_pct5k, data=mammal.data, family = "poisson")

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

mammals.dev = ggplot() +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=dev_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=dev_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=dev_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Mammals: Abundance.adj.2, Dev") +
  xlim(0,1.0) +
  ylim(0, 125) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=110, label=mylabel)
mammals.dev

ggsave(mammals.dev, file="mammals.dev.Abundance.adj.2.png")

###################################################################################################
# MAMMALS : ABUNDANCE, FOREST
###################################################################################################

fit250 <- glm(Abundance.adj.2 ~ for_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(Abundance.adj.2 ~ for_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(Abundance.adj.2 ~ for_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(Abundance.adj.2 ~ for_pct5k, data=mammal.data, family = "poisson")

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

mammals.for = ggplot() +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=for_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=for_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=for_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Mammals: Abundance.adj.2, for") +
  xlim(0,1.0) +
  ylim(0, 125) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=110, label=mylabel)
mammals.for

ggsave(mammals.for, file="mammals.for.Abundance.adj.2.png")

###################################################################################################
# MAMMALS : ABUNDANCE, GRASS
###################################################################################################

fit250 <- glm(Abundance.adj.2 ~ gra_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(Abundance.adj.2 ~ gra_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(Abundance.adj.2 ~ gra_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(Abundance.adj.2 ~ gra_pct5k, data=mammal.data, family = "poisson")

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

mammals.gra = ggplot() +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=gra_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=gra_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=gra_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Mammals: Abundance.adj.2, gra") +
  xlim(0,1.0) +
  ylim(0, 125) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=110, label=mylabel)
mammals.gra

ggsave(mammals.gra, file="mammals.gra.Abundance.adj.2.png")

###################################################################################################
# MAMMALS : ABUNDANCE, CROP
###################################################################################################

fit250 <- glm(Abundance.adj.2 ~ cro_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(Abundance.adj.2 ~ cro_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(Abundance.adj.2 ~ cro_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(Abundance.adj.2 ~ cro_pct5k, data=mammal.data, family = "poisson")

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

mammals.cro = ggplot() +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=cro_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=cro_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=cro_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=Abundance.adj.2, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=Abundance.adj.2, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Mammals: Abundance.adj.2, cro") +
  xlim(0,1.0) +
  ylim(0, 125) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=110, label=mylabel)
mammals.cro

ggsave(mammals.cro, file="mammals.cro.Abundance.adj.2.png")

###################################################################################################
# MAMMALS : SpRichness, DEVELOPMENT
###################################################################################################

mammal.data$SpRichness.adj.2 <- round(mammal.data$SpRichness.adj*10, digits=0)

fit250 <- glm(SpRichness.adj.2 ~ dev_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(SpRichness.adj.2 ~ dev_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(SpRichness.adj.2 ~ dev_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(SpRichness.adj.2 ~ dev_pct5k, data=mammal.data, family = "poisson")

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

mammals.dev = ggplot() +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=dev_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=dev_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=dev_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=dev_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=dev_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=dev_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=dev_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=dev_pct5k, color="5000m")) +
  xlab("Percent Development") +
  ggtitle("Mammals: SpRichness.adj.2, Dev") +
  xlim(0,1.0) +
  ylim(0, 8) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=6, label=mylabel)
mammals.dev

ggsave(mammals.dev, file="mammals.dev.SpRichness.adj.2.png")

###################################################################################################
# MAMMALS : SPRICHNESS, FOREST
###################################################################################################

fit250 <- glm(SpRichness.adj.2 ~ for_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(SpRichness.adj.2 ~ for_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(SpRichness.adj.2 ~ for_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(SpRichness.adj.2 ~ for_pct5k, data=mammal.data, family = "poisson")

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

mammals.for = ggplot() +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=for_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=for_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=for_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=for_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=for_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=for_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=for_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=for_pct5k, color="5000m")) +
  xlab("Percent Forest") +
  ggtitle("Mammals: SpRichness.adj.2, for") +
  xlim(0,1.0) +
  ylim(0, 8) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=6, label=mylabel)
mammals.for

ggsave(mammals.for, file="mammals.for.SpRichness.adj.2.png")

###################################################################################################
# MAMMALS : SPRICHNESS, GRASS
###################################################################################################

fit250 <- glm(SpRichness.adj.2 ~ gra_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(SpRichness.adj.2 ~ gra_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(SpRichness.adj.2 ~ gra_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(SpRichness.adj.2 ~ gra_pct5k, data=mammal.data, family = "poisson")

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

mammals.gra = ggplot() +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=gra_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=gra_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=gra_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=gra_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=gra_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=gra_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=gra_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=gra_pct5k, color="5000m")) +
  xlab("Percent Grass") +
  ggtitle("Mammals: SpRichness.adj.2, gra") +
  xlim(0,1.0) +
  ylim(0, 8) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=6, label=mylabel)
mammals.gra

ggsave(mammals.gra, file="mammals.gra.SpRichness.adj.2.png")

###################################################################################################
# MAMMALS : SPRICHNESS, CROP
###################################################################################################

fit250 <- glm(SpRichness.adj.2 ~ cro_pct250, data=mammal.data, family = "poisson")
fit500 <- glm(SpRichness.adj.2 ~ cro_pct500, data=mammal.data, family = "poisson")
fit1k <- glm(SpRichness.adj.2 ~ cro_pct1k, data=mammal.data, family = "poisson")
fit5k <- glm(SpRichness.adj.2 ~ cro_pct5k, data=mammal.data, family = "poisson")

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

mammals.cro = ggplot() +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=cro_pct250, color="250m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=cro_pct250, color="250m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=cro_pct500, color="500m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=cro_pct500, color="500m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=cro_pct1k, color="1000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=cro_pct1k, color="1000m")) +
  geom_point(data=mammal.data, aes(y=SpRichness.adj.2, x=cro_pct5k, color="5000m")) +
  geom_smooth(method="glm",
              se = T,
              method.args = list(family = "poisson"),
              fullrange = FALSE,
              data=mammal.data,
              aes(y=SpRichness.adj.2, x=cro_pct5k, color="5000m")) +
  xlab("Percent Crop") +
  ggtitle("Mammals: SpRichness.adj.2, cro") +
  xlim(0,1.0) +
  ylim(0, 8) +
  scale_color_manual(name="Radius", values=c("250m"="red", "500m"="orange", "1000m"="darkgreen", "5000m"="blue"), breaks=c("250m", "500m", "1000m", "5000m")) +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0)) +
  annotate(geom="text", x=0.8, y=6, label=mylabel)
mammals.cro

ggsave(mammals.cro, file="mammals.cro.SpRichness.adj.2.png")


