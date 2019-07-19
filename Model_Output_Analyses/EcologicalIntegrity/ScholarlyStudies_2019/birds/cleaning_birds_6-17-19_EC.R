birds = read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_6-17-19_EC.csv")

sites = read.csv("U:/CLI/Field Surveys/Birds/Sites_Birds_4-24-19_EC.csv")
sites = sites[sites$Pole == "A",]

birds_sites = merge(birds, sites, by="Site")

CLI_birds = data.frame("Site" = birds_sites[,1], birds_sites[,104:105], birds_sites[,2:100])
CLI_birds[is.na(CLI_birds)] <- 0

write.csv(CLI_birds, file="U:/CLI/Field Surveys/Birds/CLI_Birds_6-17-19.csv")

# separate out into target grassland and shrub species
bird.type = read.csv("U:/CLI/Field Surveys/Birds/AOUcodes_SppStatuses_4-23-19_EC.csv")
bird.grass = bird.type$Species[bird.type$Target.Species == "grassland"]
bird.shrub = bird.type$Species[bird.type$Target.Species == "shrubland"]

CLI_birds_grass = data.frame(CLI_birds[,1:3], CLI_birds[,c(7,25,27,40,41,53,63,66)])
CLI_birds_shrub = data.frame(CLI_birds[,1:3], CLI_birds[,c(20,24,37,39,43,48,58,60,73,84,93,94,97,98)])

rows = seq(1:108)

for(i in rows){
  CLI_birds_grass$Abundance[i] <- sum(CLI_birds_grass[i,4:11])
}
for(i in rows){
  CLI_birds_shrub$Abundance[i] <- sum(CLI_birds_shrub[i,4:17])
}

CLI_birds_grass$SpRichness = 0

for (i in rows){
    if (CLI_birds_grass[i,4] > 0) {
      CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
    }
  if (CLI_birds_grass[i,5] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  }
  if (CLI_birds_grass[i,6] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  } 
  if (CLI_birds_grass[i,7] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  } 
  if (CLI_birds_grass[i,8] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  } 
  if (CLI_birds_grass[i,9] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  } 
  if (CLI_birds_grass[i,10] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  }
  if (CLI_birds_grass[i,11] > 0) {
    CLI_birds_grass$SpRichness[i] <- CLI_birds_grass$SpRichness[i] + 1
  }
}

CLI_birds_shrub$SpRichness = 0

for (i in rows){
  if (CLI_birds_shrub[i,4] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  }
  if (CLI_birds_shrub[i,5] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  }
  if (CLI_birds_shrub[i,6] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,7] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,8] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,9] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,10] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  }
  if (CLI_birds_shrub[i,11] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,12] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,13] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  }
  if (CLI_birds_shrub[i,14] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,15] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,16] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
  if (CLI_birds_shrub[i,17] > 0) {
    CLI_birds_shrub$SpRichness[i] <- CLI_birds_shrub$SpRichness[i] + 1
  } 
}

write.csv(CLI_birds_grass, file="U:/CLI/Field Surveys/Birds/CLI_Birds_Grass_6-17-19.csv")
write.csv(CLI_birds_shrub, file="U:/CLI/Field Surveys/Birds/CLI_Birds_Shrub_6-17-19.csv")



  # compare my new version to Craig's old version:
old.CLIBIRD = read.csv("U:/CLI/Field Surveys/Birds/CLI_Birds_8-28-18.csv")
summary(old.CLIBIRD$Bird_Abundance)
summary(CLI_birds$Abundance)




