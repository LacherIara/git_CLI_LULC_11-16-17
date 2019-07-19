
setwd("I:/EI_data")

## read in data - natives
birds.data = read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_4-24-19.csv")
  birds.shrub = read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Shrub_4-24-19.csv")
  birds.grass = read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_Grass_4-24-19.csv")
bees.data = read.csv("U:/CLI/Field Surveys/Bees/CLI_Bombus_Environmental_4-30-19.csv")
mammal.native.data = read.csv("U:/CLI/Field Surveys/eMammal/CLI_NativeMammal_8-22-18.csv")

## read in data - invasives
mammal.nonnative.data = read.csv("U:/CLI/Field Surveys/eMammal/CLI_NonnativeMammal_8-22-18.csv")
invasives.data = read.csv("U:/CLI/Field Surveys/Invasive/CLI_Invasive_8-28-18.csv")

mammal.nonnative.data$SpRichness = apply(mammal.nonnative.data[,7:10]>0,1,sum)



######################################################################################################
# MULTI-TAXA
######################################################################################################

## By species

comment(birds.data) = "Birds"
comment(bees.data) = "Bees"
comment(mammal.native.data) = "Native Mammals"
comment(mammal.nonnative.data) = "NonNative Mammals"

for (i in list(birds.data, bees.data, mammal.native.data,  mammal.nonnative.data)){
  model250 = lm(SpRichness~dev_250, data=i)
      sum250 = summary(model250)
      r2.250 = sum250$adj.r.squared
      p.250 = sum250$coefficients[2,4]
  model500 = lm(SpRichness~dev_500, data=i)
      sum500 = summary(model500)
      r2.500 = sum500$adj.r.squared
      p.500 = sum500$coefficients[2,4]
  model1000 = lm(SpRichness~dev_1000, data=i)
      sum1000 = summary(model1000)
      r2.1000 = sum1000$adj.r.squared
      p.1000 = sum1000$coefficients[2,4]
  model5000 = lm(SpRichness~dev_5000, data=i)
      sum5000 = summary(model5000)
      r2.5000 = sum5000$adj.r.squared
      p.5000 = sum5000$coefficients[2,4]
  
  to.round = c(r2.250, p.250, r2.500, p.500, r2.1000, p.1000, r2.5000, p.5000)
  metrics = round(to.round, digits=3)
  mylabel = paste0("250m r^2=", metrics[1], ", p=", metrics[2], "\n", "500m r^2=", metrics[3], ", p=", metrics[4], "\n", "1000m r^2=", metrics[5], ", p=", metrics[6], "\n", "5000m r^2=", metrics[7], ", p=", metrics[8], "\n")
      
  name = comment(i)
  
  png(paste0(name,"_SpRichness_dev.png"))
  
  plot(x=i$dev_250, y=i$SpRichness, col="red", xlim=c(0,1), ylim=c(0,max(i$SpRichness)), pch=16, main=paste0(name,": SpRichness, dev"), xlab="Percent dev", ylab="SpRichness")
  points(x=i$dev_500, y=i$SpRichness, col="orange", pch=16)
  points(x=i$dev_1000, y=i$SpRichness, col="dark green", pch=16)
  points(x=i$dev_5000, y=i$SpRichness, col="blue", pch=16)
  abline(model250, col="red")
  abline(model500, col="orange")
  abline(model1000, col="dark green")
  abline(model5000, col="blue")
  legend("topright", legend = c("250m", "500m", "1000m", "5000m"), col=c("red", "orange", "dark green", "blue"), pch=c(16, 16, 16, 16), bty="n")
  text(x=0.8, y=max(i$SpRichness)/10, labels=mylabel)
  
  dev.off()
}


## by distance

variables.250 = list(birds.data$cro_250, bees.data$cro_250, mammal.native.data$cro_250, mammal.nonnative.data$cro_250)
  comment(variables.250) = "250m"
variables.500 = list(birds.data$cro_500, bees.data$cro_500, mammal.native.data$cro_500, mammal.nonnative.data$cro_500)
  comment(variables.500) = "500m"
variables.1000 = list(birds.data$cro_1000, bees.data$cro_1000, mammal.native.data$cro_1000, mammal.nonnative.data$cro_1000)
  comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$cro_5000, bees.data$cro_5000, mammal.native.data$cro_5000, mammal.nonnative.data$cro_5000)
  comment(variables.5000) = "5000m"

  
distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")
  
for (i in c(1,2,3,4)){

  model.bird = lm(birds.data$SpRichness~distances[[i]][[1]])
    sum.bird = summary(model.bird)
    r2.bird = sum.bird$adj.r.squared
    p.bird = sum.bird$coefficients[2,4]
  model.bee = lm(bees.data$SpRichness~distances[[i]][[2]])
    sum.bee = summary(model.bee)
    r2.bee = sum.bee$adj.r.squared
    p.bee = sum.bee$coefficients[2,4]
  model.native.mammal = lm(mammal.native.data$SpRichness~distances[[i]][[3]])
    sum.native.mammal = summary(model.native.mammal)
    r2.native.mammal = sum.native.mammal$adj.r.squared
    p.native.mammal = sum.native.mammal$coefficients[2,4]
  model.nonnative.mammal = lm(mammal.nonnative.data$SpRichness~distances[[i]][[4]])
    sum.nonnative.mammal = summary(model.nonnative.mammal)
    r2.nonnative.mammal = sum.nonnative.mammal$adj.r.squared
    p.nonnative.mammal = sum.nonnative.mammal$coefficients[2,4]
  
  to.round = c(r2.bird, p.bird, r2.bee, p.bee, r2.native.mammal, p.native.mammal, r2.nonnative.mammal, p.nonnative.mammal)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird r^2=", metrics[1], ", p=", metrics[2], "\n", "bee r^2=", metrics[3], ", p=", metrics[4], "\n", "native mammal r^2=", metrics[5], ", p=", metrics[6], "\n", "nonnative mammal r^2=", metrics[7], ", p=", metrics[8], "\n")

  name = names[i]
  
  png(paste0(name,"_SpRichness_cro.png"))
  
  plot(x=distances[[i]][[1]], y=birds.data$SpRichness, col="red", xlim=c(0,1), ylim=c(0, max(birds.data$SpRichness)), pch=16, main=paste0(name,": SpRichness, cro"), xlab=paste("Percent cro at", name), ylab="SpRichness")
  points(x=distances[[i]][[2]], y=bees.data$SpRichness, col="orange", pch=16)
  points(x=distances[[i]][[3]], y=mammal.native.data$SpRichness, col="dark green", pch=16)
  points(x=distances[[i]][[4]], y=mammal.nonnative.data$SpRichness, col="blue", pch=16)
  abline(model.bird, col="red")
  abline(model.bee, col="orange")
  abline(model.native.mammal, col="dark green")
  abline(model.nonnative.mammal, col="blue")
  legend("topright", legend = c("Birds", "Bees", "Native Mammal", "NonNative Mammal"), col=c("red", "orange", "dark green", "blue"), pch=c(16, 16, 16, 16), bty="n")
  text(x=0.7, y=max(birds.data$SpRichness)/2, labels=mylabel)
  
  dev.off()
}

#####################################################
# removing outliers
#####################################################

birds.IQR = as.numeric(summary(birds.data$Abundance)[5] - summary(birds.data$Abundance)[2])
birds.lowerlim = as.numeric(summary(birds.data$Abundance)[2]) - (birds.IQR*1.5)  
birds.upperlim = as.numeric(summary(birds.data$Abundance)[5]) + (birds.IQR*1.5)
birds.data.rm = birds.data[birds.data$Abundance < birds.upperlim,]

bees.IQR = as.numeric(summary(bees.data$Abundance)[5] - summary(bees.data$Abundance)[2])
bees.lowerlim = as.numeric(summary(bees.data$Abundance)[2]) - (bees.IQR*1.5)  
bees.upperlim = as.numeric(summary(bees.data$Abundance)[5]) + (bees.IQR*1.5)
bees.lowerlim
bees.upperlim
bees.data.rm = bees.data[bees.data$Abundance < bees.upperlim,]

summary(mammal.native.data$Abundance)
mammal.native.IQR = as.numeric(summary(mammal.native.data$Abundance)[5] - summary(mammal.native.data$Abundance)[2])
mammal.native.IQR
mammal.native.lowerlim = as.numeric(summary(mammal.native.data$Abundance)[2]) - (mammal.native.IQR*1.5)  
mammal.native.upperlim = as.numeric(summary(mammal.native.data$Abundance)[5]) + (mammal.native.IQR*1.5)
mammal.native.lowerlim
mammal.native.upperlim
mammal.native.data.rm = mammal.native.data[mammal.native.data$Abundance < mammal.native.upperlim,]

summary(mammal.nonnative.data$Abundance)
mammal.nonnative.IQR = as.numeric(summary(mammal.nonnative.data$Abundance)[5] - summary(mammal.nonnative.data$Abundance)[2])
mammal.nonnative.IQR
mammal.nonnative.lowerlim = as.numeric(summary(mammal.nonnative.data$Abundance)[2]) - (mammal.nonnative.IQR*1.5)  
mammal.nonnative.upperlim = as.numeric(summary(mammal.nonnative.data$Abundance)[5]) + (mammal.nonnative.IQR*1.5)
mammal.nonnative.lowerlim
mammal.nonnative.upperlim
mammal.nonnative.data.rm = mammal.nonnative.data[mammal.nonnative.data$Abundance < mammal.nonnative.upperlim,]

## single species

comment(birds.data.rm) = "Birds"
comment(bees.data.rm) = "Bees"
comment(mammal.native.data.rm) = "Native Mammals"
comment(mammal.nonnative.data.rm) = "NonNative Mammals"

for (i in list(birds.data.rm, bees.data.rm, mammal.native.data.rm, mammal.nonnative.data.rm)){
  model250 = lm(SpRichness~dev_250, data=i)
  sum250 = summary(model250)
  r2.250 = sum250$adj.r.squared
  p.250 = sum250$coefficients[2,4]
  model500 = lm(SpRichness~dev_500, data=i)
  sum500 = summary(model500)
  r2.500 = sum500$adj.r.squared
  p.500 = sum500$coefficients[2,4]
  model1000 = lm(SpRichness~dev_1000, data=i)
  sum1000 = summary(model1000)
  r2.1000 = sum1000$adj.r.squared
  p.1000 = sum1000$coefficients[2,4]
  model5000 = lm(SpRichness~dev_5000, data=i)
  sum5000 = summary(model5000)
  r2.5000 = sum5000$adj.r.squared
  p.5000 = sum5000$coefficients[2,4]
  
  to.round = c(r2.250, p.250, r2.500, p.500, r2.1000, p.1000, r2.5000, p.5000)
  metrics = round(to.round, digits=3)
  mylabel = paste0("250m r^2=", metrics[1], ", p=", metrics[2], "\n", "500m r^2=", metrics[3], ", p=", metrics[4], "\n", "1000m r^2=", metrics[5], ", p=", metrics[6], "\n", "5000m r^2=", metrics[7], ", p=", metrics[8], "\n")
  
  name = comment(i)
  
  png(paste0("RM_",name,"_SpRichness_dev.png"))
  
  plot(x=i$dev_250, y=i$SpRichness, col="red", xlim=c(0,1), pch=16, main=paste0(name,": SpRichness, dev"), xlab="Percent dev", ylab="SpRichness")
  points(x=i$dev_500, y=i$SpRichness, col="orange", pch=16)
  points(x=i$dev_1000, y=i$SpRichness, col="dark green", pch=16)
  points(x=i$dev_5000, y=i$SpRichness, col="blue", pch=16)
  abline(model250, col="red")
  abline(model500, col="orange")
  abline(model1000, col="dark green")
  abline(model5000, col="blue")
  legend("topright", legend = c("250m", "500m", "1000m", "5000m"), col=c("red", "orange", "dark green", "blue"), pch=c(16, 16, 16, 16), bty="n")
  text(x=0.8, y=max(i$SpRichness)/2, labels=mylabel)
  
  dev.off()
}

## single distance

variables.250 = list(birds.data.rm$cro_250, bees.data.rm$cro_250, mammal.native.data.rm$cro_250, mammal.nonnative.data.rm$cro_250)
comment(variables.250) = "250m"
variables.500 = list(birds.data.rm$cro_500, bees.data.rm$cro_500, mammal.native.data.rm$cro_500, mammal.nonnative.data.rm$cro_500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data.rm$cro_1000, bees.data.rm$cro_1000, mammal.native.data.rm$cro_1000, mammal.nonnative.data.rm$cro_1000)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data.rm$cro_5000, bees.data.rm$cro_5000, mammal.native.data.rm$cro_5000, mammal.nonnative.data.rm$cro_5000)
comment(variables.5000) = "5000m"


distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird = lm(birds.data.rm$SpRichness~distances[[i]][[1]])
  sum.bird = summary(model.bird)
  r2.bird = sum.bird$adj.r.squared
  p.bird = sum.bird$coefficients[2,4]
  model.bee = lm(bees.data.rm$SpRichness~distances[[i]][[2]])
  sum.bee = summary(model.bee)
  r2.bee = sum.bee$adj.r.squared
  p.bee = sum.bee$coefficients[2,4]
  model.native.mammal = lm(mammal.native.data.rm$SpRichness~distances[[i]][[3]])
  sum.native.mammal = summary(model.native.mammal)
  r2.native.mammal = sum.native.mammal$adj.r.squared
  p.native.mammal = sum.native.mammal$coefficients[2,4]
  model.nonnative.mammal = lm(mammal.nonnative.data.rm$SpRichness~distances[[i]][[4]])
  sum.nonnative.mammal = summary(model.nonnative.mammal)
  r2.nonnative.mammal = sum.nonnative.mammal$adj.r.squared
  p.nonnative.mammal = sum.nonnative.mammal$coefficients[2,4]
  
  to.round = c(r2.bird, p.bird, r2.bee, p.bee, r2.native.mammal, p.native.mammal, r2.nonnative.mammal, p.nonnative.mammal)
  metrics = round(to.round, digits=3)
  mylabel = paste0("bird r^2=", metrics[1], ", p=", metrics[2], "\n", "bee r^2=", metrics[3], ", p=", metrics[4], "\n", "native mammal r^2=", metrics[5], ", p=", metrics[6], "\n", "nonnative mammal r^2=", metrics[7], ", p=", metrics[8], "\n")
  
  name = names[i]
  
  png(paste0("RM_",name,"_SpRichness_cro.png"))
  
  plot(x=distances[[i]][[1]], y=birds.data.rm$SpRichness, col="red", xlim=c(0,1), ylim=c(0, 1010), pch=16, main=paste0(name,": SpRichness, cro"), xlab=paste("Percent cro at", name), ylab="SpRichness")
  points(x=distances[[i]][[2]], y=bees.data.rm$SpRichness, col="orange", pch=16)
  points(x=distances[[i]][[3]], y=mammal.native.data.rm$SpRichness, col="dark green", pch=16)
  points(x=distances[[i]][[4]], y=mammal.nonnative.data.rm$SpRichness, col="blue", pch=16)
  abline(model.bird, col="red")
  abline(model.bee, col="orange")
  abline(model.native.mammal, col="dark green")
  abline(model.nonnative.mammal, col="blue")
  legend("topright", legend = c("Birds", "Bees", "Native Mammal", "NonNative Mammal"), col=c("red", "orange", "dark green", "blue"), pch=c(16, 16, 16, 16), bty="n")
  text(x=0.7, y=1010/2, labels=mylabel)
  
  dev.off()
}

####################################################
#playing with different kinds of curves/models
###################################################

#quadratic?

Forest_1000 = bees.data.rm$for_1000
Forest_1000_2 = (bees.data.rm$for_1000)^2

linear.model = lm(bees.data.rm$Abundance ~ Forest_1000)
quadratic.model = lm(bees.data.rm$Abundance ~ Forest_1000 + Forest_1000_2)

summary(linear.model)
summary(quadratic.model)

abundancevalues <- seq(0,1, 0.05)
predictedabundance <- predict(quadratic.model,list(Forest_1000 = abundancevalues, Forest_1000_2 = abundancevalues^2))

plot(y=bees.data.rm$Abundance, x=bees.data.rm$for_1000)
abline(linear.model)
lines(abundancevalues, predictedabundance, col="red")

## poisson glm vs reg (what is regular... normal distribution?

fit.reg = glm(Abundance ~ for_1000, data=birds.data.rm)
summary(fit.reg)

fit.pois = glm(Abundance ~ for_1000, data=birds.data.rm, family=poisson())
summary(fit.pois)
Xval_pois = seq(0,1,0.05)
yval_pois = predict(fit.pois, list(for_1000=Xval_pois), type = "response")

plot(Abundance ~ for_1000, data=birds.data, xlim=c(0,1))
abline(fit.reg)
lines(x=Xval_pois, y=yval_pois, col="red")

####################################################
# BIRDS - ALL, GRASSLAND, SHRUB
###################################################

## graph all birds, grass and shrub by all four land uses, all four distances

variables.250 = list(birds.data$cro250, birds.shrub$cro250, birds.grass$cro250)
comment(variables.250) = "250m"
variables.500 = list(birds.data$cro500, birds.shrub$cro500, birds.grass$cro500)
comment(variables.500) = "500m"
variables.1000 = list(birds.data$cro1k, birds.shrub$cro1k, birds.grass$cro1k)
comment(variables.1000) = "1000m"
variables.5000 = list(birds.data$cro5k, birds.shrub$cro5k, birds.grass$cro5k)
comment(variables.5000) = "5000m"


distances = list(variables.250, variables.500, variables.1000, variables.5000)
names = c("250m", "500m", "1000m", "5000m")

for (i in c(1,2,3,4)){
  
  model.bird.all = lm(birds.data$Abundance~distances[[i]][[1]])
    sum.bird.all = summary(model.bird.all)
    r2.bird.all = sum.bird.all$adj.r.squared
    p.bird.all = sum.bird.all$coefficients[2,4]
  model.bird.shrub = lm(birds.shrub$Abundance~distances[[i]][[2]])
    sum.bird.shrub = summary(model.bird.shrub)
    r2.bird.shrub = sum.bird.shrub$adj.r.squared
    p.bird.shrub = sum.bird.shrub$coefficients[2,4]
  model.bird.grass = lm(birds.grass$Abundance~distances[[i]][[3]])
    sum.bird.grass = summary(model.bird.grass)
    r2.bird.grass = sum.bird.grass$adj.r.squared
    p.bird.grass = sum.bird.grass$coefficients[2,4]
  
  to.round = c(r2.bird.all, p.bird.all, r2.bird.shrub, p.bird.shrub, r2.bird.grass, p.bird.grass)
  metrics = round(to.round, digits=3)
  mylabel = paste0("All Birds r^2=", metrics[1], ", p=", metrics[2], "\n", "Shrub r^2=", metrics[3], ", p=", metrics[4], "\n", "grass r^2=", metrics[5], ", p=", metrics[6])
  
  name = names[i]
  
  png(paste0(name,"_Abundance_cro_BIRDS.png"))
  
  plot(x=distances[[i]][[1]], y=birds.data$Abundance, col="black", xlim=c(0,1), pch=16, main=paste0(name,": Abundance, cro"), xlab=paste("Percent cro at", name), ylab="Abundance")
  points(x=distances[[i]][[2]], y=birds.shrub$Abundance, col="red", pch=16)
  points(x=distances[[i]][[3]], y=birds.grass$Abundance, col="blue", pch=16)
  abline(model.bird.all, col="black")
  abline(model.bird.shrub, col="red")
  abline(model.bird.grass, col="blue")
  legend("topright", legend=c("All Birds", "Shrub", "grassland"), col=c("black","red", "blue"), pch=c(16, 16, 16), bty="n")
  text(x=0.7, y=max(birds.data$Abundance)/2, labels=mylabel)
  
  dev.off()
}

## removing outliers: chose not to re-do with outliers because only a few pieces were removed and they weren't that far beyond ranges

birds.shrub.IQR = as.numeric(summary(birds.shrub$Abundance)[5] - summary(birds.shrub$Abundance)[2])
birds.shrub.lowerlim = as.numeric(summary(birds.shrub$Abundance)[2]) - (birds.shrub.IQR*1.5)  
birds.shrub.upperlim = as.numeric(summary(birds.shrub$Abundance)[5]) + (birds.shrub.IQR*1.5)
birds.shrub.rm = birds.shrub[birds.shrub$Abundance < birds.shrub.upperlim,]

birds.grass.IQR = as.numeric(summary(birds.grass$Abundance)[5] - summary(birds.grass$Abundance)[2])
birds.grass.lowerlim = as.numeric(summary(birds.grass$Abundance)[2]) - (birds.grass.IQR*1.5)  
birds.grass.upperlim = as.numeric(summary(birds.grass$Abundance)[5]) + (birds.grass.IQR*1.5)
birds.grass.rm = birds.grass[birds.grass$Abundance < birds.grass.upperlim,]













