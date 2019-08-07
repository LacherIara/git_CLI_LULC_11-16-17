
setwd("I:/EI_data/plots2016/7-26-19/MammalGroupings")

library(ggplot2)

mammal.data <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammals_Environmental_7-1-19.csv")
  names(mammal.data)[26] <- "Abundance.og"
  names(mammal.data)[27] <- "SpRichness.og"
  mammal.data$Abundance <- round(mammal.data$Abundance_ *10, digits=0) 
  mammal.data$SpRichness <- round(mammal.data$SpRichne_1*10, digits=0)

# Wild:
  # [4] "Canis_latr" -> Canis latrans, Coyote, native
  # [5] "Didelphis" -> Didelphis virginiana, Virginia Opossum, native
  # [7] "Lynx_rufus" -> Lynx Rufus, Bobcat, native
  # [8] "Marmota_mo" -> Marmota monax, Woodchuck, native
  # [9] "Martes_Pen" -> Martes Pennanti, Fisher, native
  # [10] "Mephitis_m" -> Mephitis mephitis, striped skunk, native
  # [11] "Mustela_er" -> Mustela erminea, Ermine, ???
  # [12] "Mustela_fr" -> Mustela frenata, Long-tailed weasel, ???
  # [13] "Mustela_ni" -> Mustela nivalis, Least Weasel, ???
  # [14] "Mustela_sp" -> Mustela spp, Unknown Weasel, ???
  # [15] "Neotoma_fl" -> Neotoma floridana, Eastern woodrat, ???
  # [17] "Procyon_lo" -> Procyon lotor, Northern Raccoon, native
  # [20] "Spilogale" -> Spilogale putorius, Eastern Spotted Skunk, ? native ?
  # [21] "Sylvilagus" -> Sylvilagus floridanus, Eastern Cottontail, native
  # [22] "Tamias_str" -> Tamias striatus, Eastern Chipmunk, native
  # [23] "Urocyon_ci" -> Urocyon cinereoargenteus, Gray Fox, native
  # [24] "Ursus_amer" -> Ursus americanus, American Black Bear, native
  # [25] "Vulpes_vul" -> Vulpes vulpes, Red Fox, native

# Anthropogenic:
  # [3] "Canis_fami" -> Canis familiaris, Domestic Dog, non-native
  # [6] "Felis_catu" -> Felus catus, Domestic cat, non-native
  # [16] "Odocoileus" -> Odocoileus virginianus, White-tailed Deer, native
  # [18] "Sciurus_ca"  -> Sciurus carolinensis, Eastern Gray Squirrel, ? native ? 
  # [19] "Sciurus_ni"-> Sciurus niger, Eastern Fox Squirrel, native


mammal.data$Abundance.wild <- 0
    for (i in 1:length(mammal.data$FID)) {
      mammal.data$Abundance.wild[i] <- sum(mammal.data[i,c(4,5,7:15,17,20:25)])
    }
mammal.data$SpRichness.wild <- 0
    for (i in 1:length(mammal.data$FID)) {
      if (mammal.data$Canis_latr[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Didelphis[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Lynx_rufus[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Marmota_mo[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Martes_Pen[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Mephitis_m[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Mustela_er[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Mustela_fr[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Mustela_ni[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Mustela_sp[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Neotoma_fl[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Procyon_lo[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Spilogale[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Sylvilagus[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Tamias_str[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Urocyon_ci[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Ursus_amer[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
      if (mammal.data$Vulpes_vul[i] > 0) {mammal.data$SpRichness.wild[i] <- mammal.data$SpRichness.wild[i] + 1}
    }
mammal.data$Abundance.anthro <- 0
    for (i in 1:length(mammal.data$FID)) {
      mammal.data$Abundance.anthro[i] <- sum(mammal.data[i,c(3,6,16,18,19)])
    }
mammal.data$SpRichness.anthro <- 0
    for (i in 1:length(mammal.data$FID)) {
      if (mammal.data$Canis_fami[i] > 0) {mammal.data$SpRichness.anthro[i] <- mammal.data$SpRichness.anthro[i] + 1}
      if (mammal.data$Felis_catu[i] > 0) {mammal.data$SpRichness.anthro[i] <- mammal.data$SpRichness.anthro[i] + 1}
      if (mammal.data$Odocoileus[i] > 0) {mammal.data$SpRichness.anthro[i] <- mammal.data$SpRichness.anthro[i] + 1}
      if (mammal.data$Sciurus_ca[i] > 0) {mammal.data$SpRichness.anthro[i] <- mammal.data$SpRichness.anthro[i] + 1}
      if (mammal.data$Sciurus_ni[i] > 0) {mammal.data$SpRichness.anthro[i] <- mammal.data$SpRichness.anthro[i] + 1}
    }

mammal.data$Abundance.wild.adj <- round(((mammal.data$Abundance.wild)/mammal.data$DaysDeploy)*10, digits=0)
mammal.data$SpRichness.wild.adj <- round(((mammal.data$SpRichness.wild)/mammal.data$DaysDeploy)*10, digits=0)

mammal.data$Abundance.anthro.adj <- round(((mammal.data$Abundance.anthro)/mammal.data$DaysDeploy)*10, digits=0)
mammal.data$SpRichness.anthro.adj <- round(((mammal.data$SpRichness.anthro)/mammal.data$DaysDeploy)*10, digits=0)


###########################################################################################################
# Abundance, Dev
###########################################################################################################

variables.250 = list(mammal.data$dev_pct250, mammal.data$dev_pct250, mammal.data$dev_pct250)
  comment(variables.250) = "250m"
variables.500 = list(mammal.data$dev_pct500, mammal.data$dev_pct500, mammal.data$dev_pct500)
  comment(variables.500) = "500m"
variables.1000 = list(mammal.data$dev_pct1k, mammal.data$dev_pct1k, mammal.data$dev_pct1k)
  comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$dev_pct5k, mammal.data$dev_pct5k, mammal.data$dev_pct5k)
  comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$Abundance ~ distances[[i]][[1]], family = "poisson")
    sum.all = summary(fit.all)
    p.all = sum.all$coefficients[2,4]
    AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$Abundance.wild.adj ~ distances[[i]][[2]], family = "poisson")
    sum.wild = summary(fit.wild)
    p.wild = sum.wild$coefficients[2,4]
    AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$Abundance.anthro.adj ~ distances[[i]][[3]], family = "poisson")
    sum.anthro = summary(fit.anthro)
    p.anthro = sum.anthro$coefficients[2,4]
    AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=Abundance, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=Abundance.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=Abundance.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Development") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 125) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=125*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("dev.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Forest
###########################################################################################################

variables.250 = list(mammal.data$for_pct250, mammal.data$for_pct250, mammal.data$for_pct250)
  comment(variables.250) = "250m"
variables.500 = list(mammal.data$for_pct500, mammal.data$for_pct500, mammal.data$for_pct500)
  comment(variables.500) = "500m"
variables.1000 = list(mammal.data$for_pct1k, mammal.data$for_pct1k, mammal.data$for_pct1k)
  comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$for_pct5k, mammal.data$for_pct5k, mammal.data$for_pct5k)
  comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$Abundance ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$Abundance.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$Abundance.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=Abundance, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=Abundance.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=Abundance.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Forest") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 125) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=125*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("for.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Grass
###########################################################################################################

variables.250 = list(mammal.data$gra_pct250, mammal.data$gra_pct250, mammal.data$gra_pct250)
  comment(variables.250) = "250m"
variables.500 = list(mammal.data$gra_pct500, mammal.data$gra_pct500, mammal.data$gra_pct500)
  comment(variables.500) = "500m"
variables.1000 = list(mammal.data$gra_pct1k, mammal.data$gra_pct1k, mammal.data$gra_pct1k)
  comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$gra_pct5k, mammal.data$gra_pct5k, mammal.data$gra_pct5k)
  comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$Abundance ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$Abundance.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$Abundance.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=Abundance, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=Abundance.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=Abundance.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Grass") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 125) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=125*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("gra.", name, ".Abundance.png"))
}

###########################################################################################################
# Abundance, Crop
###########################################################################################################

variables.250 = list(mammal.data$cro_pct250, mammal.data$cro_pct250, mammal.data$cro_pct250)
  comment(variables.250) = "250m"
variables.500 = list(mammal.data$cro_pct500, mammal.data$cro_pct500, mammal.data$cro_pct500)
  comment(variables.500) = "500m"
variables.1000 = list(mammal.data$cro_pct1k, mammal.data$cro_pct1k, mammal.data$cro_pct1k)
  comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$cro_pct5k, mammal.data$cro_pct5k, mammal.data$cro_pct5k)
  comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$Abundance ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$Abundance.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$Abundance.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=Abundance, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=Abundance.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=Abundance.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=Abundance.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Crop") +
    ggtitle(paste0("Abundance, ", name)) +
    xlim(0,1.0) +
    ylim(0, 125) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=125*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("cro.", name, ".Abundance.png"))
}

###########################################################################################################
# SpRichness, Dev
###########################################################################################################

variables.250 = list(mammal.data$dev_pct250, mammal.data$dev_pct250, mammal.data$dev_pct250)
comment(variables.250) = "250m"
variables.500 = list(mammal.data$dev_pct500, mammal.data$dev_pct500, mammal.data$dev_pct500)
comment(variables.500) = "500m"
variables.1000 = list(mammal.data$dev_pct1k, mammal.data$dev_pct1k, mammal.data$dev_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$dev_pct5k, mammal.data$dev_pct5k, mammal.data$dev_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$SpRichness ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$SpRichness.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$SpRichness.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=SpRichness, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=SpRichness.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=SpRichness.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Development") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 10) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=10*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("dev.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Forest
###########################################################################################################

variables.250 = list(mammal.data$for_pct250, mammal.data$for_pct250, mammal.data$for_pct250)
comment(variables.250) = "250m"
variables.500 = list(mammal.data$for_pct500, mammal.data$for_pct500, mammal.data$for_pct500)
comment(variables.500) = "500m"
variables.1000 = list(mammal.data$for_pct1k, mammal.data$for_pct1k, mammal.data$for_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$for_pct5k, mammal.data$for_pct5k, mammal.data$for_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$SpRichness ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$SpRichness.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$SpRichness.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=SpRichness, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=SpRichness.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=SpRichness.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Forest") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 10) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=10*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("for.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Grass
###########################################################################################################

variables.250 = list(mammal.data$gra_pct250, mammal.data$gra_pct250, mammal.data$gra_pct250)
comment(variables.250) = "250m"
variables.500 = list(mammal.data$gra_pct500, mammal.data$gra_pct500, mammal.data$gra_pct500)
comment(variables.500) = "500m"
variables.1000 = list(mammal.data$gra_pct1k, mammal.data$gra_pct1k, mammal.data$gra_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$gra_pct5k, mammal.data$gra_pct5k, mammal.data$gra_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$SpRichness ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$SpRichness.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$SpRichness.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=SpRichness, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=SpRichness.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=SpRichness.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Grass") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 10) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=10*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("gra.", name, ".SpRichness.png"))
}

###########################################################################################################
# SpRichness, Crop
###########################################################################################################

variables.250 = list(mammal.data$cro_pct250, mammal.data$cro_pct250, mammal.data$cro_pct250)
comment(variables.250) = "250m"
variables.500 = list(mammal.data$cro_pct500, mammal.data$cro_pct500, mammal.data$cro_pct500)
comment(variables.500) = "500m"
variables.1000 = list(mammal.data$cro_pct1k, mammal.data$cro_pct1k, mammal.data$cro_pct1k)
comment(variables.1000) = "1000m"
variables.5000 = list(mammal.data$cro_pct5k, mammal.data$cro_pct5k, mammal.data$cro_pct5k)
comment(variables.5000) = "5000m"

distances = list(variables.250, variables.500, variables.1000, variables.5000)

names = c("250m", "500m", "1000m", "5000m")

for (i in 1:4) {
  fit.all <- glm(mammal.data$SpRichness ~ distances[[i]][[1]], family = "poisson")
  sum.all = summary(fit.all)
  p.all = sum.all$coefficients[2,4]
  AIC.all = sum.all$aic
  fit.wild <- glm(mammal.data$SpRichness.wild.adj ~ distances[[i]][[2]], family = "poisson")
  sum.wild = summary(fit.wild)
  p.wild = sum.wild$coefficients[2,4]
  AIC.wild = sum.wild$aic
  fit.anthro <- glm(mammal.data$SpRichness.anthro.adj ~ distances[[i]][[3]], family = "poisson")
  sum.anthro = summary(fit.anthro)
  p.anthro = sum.anthro$coefficients[2,4]
  AIC.anthro = sum.anthro$aic
  
  to.round = c(p.all, AIC.all, p.wild, AIC.wild, p.anthro, AIC.anthro)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All p=", metrics[1], ", AIC=", metrics[2], "\n", "Wild p=", metrics[3], ", AIC=", metrics[4], "\n", "Anthro p=", metrics[5], ", AIC=", metrics[6])
  
  name = names[i]
  
  mammalz <- ggplot(data = mammal.data) +
    geom_point(aes(x=distances[[i]][[1]], y=SpRichness, color="all")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness, x=distances[[i]][[1]], color="all")) +
    geom_point(aes(x=distances[[i]][[2]], y=SpRichness.wild.adj, color="wild")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.wild.adj, x=distances[[i]][[2]], color="wild")) +
    geom_point(aes(x=distances[[i]][[3]], y=SpRichness.anthro.adj, color="anthro")) +
    geom_smooth(method="glm",
                se = T,
                method.args = list(family = "poisson"),
                fullrange = FALSE,
                data=mammal.data,
                aes(y=SpRichness.anthro.adj, x=distances[[i]][[3]], color="anthro")) +
    xlab("Percent Crop") +
    ggtitle(paste0("SpRichness, ", name)) +
    xlim(0,1.0) +
    ylim(0, 10) +
    scale_color_manual(name="", values=c("all"="red", "wild"="orange", "anthro"="blue"), breaks=c("all", "wild", "anthro")) +
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) +
    annotate(geom="text", x=0.8, y=10*0.75, label=mylabel)
  
  ggsave(mammalz, file=paste0("cro.", name, ".SpRichness.png"))
}


