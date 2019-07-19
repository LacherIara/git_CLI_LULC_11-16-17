library(dplyr)
library(reshape)
library(stringr)

data16<-read.csv("U:/CLI/EcologySubModels/Invasibility/Tabular Data/FieldSurveys/FieldSurvey_AnalysisFormat_2016.csv")
data17<-read.csv("U:/CLI/EcologySubModels/Invasibility/Tabular Data/FieldSurveys/FieldSurvey_AnalysisFormat_2017.csv")

data<-rbind(data16,data17)

# remove unnecessary columns for clarity
data <- data[,c(1,3:7,10:22)]

# 2016 survey covered 13 species:
    # AIAL ALPE AMBR BETH CEOR ELUM LISI LOJA MIVI OPHI PEPE PUMO ROMU
# 2017 survey covered 14 species:
    # AIAL ALPE AMBR BETH CEOR ELUM LISI LOJA MIVI OPHI PEPE PUMO ROMU RUPH
# remove all records where species = RUPH 
data <- data[data$Species != "RUPH",]


## to avoid false replicates, only include the survey plot that was farther into the woods. Each transect ID as _1 and _2 - we believe that _1 was closer to the forest edge, and _2 was further from the forest edge. Not totally sure if it is smarter to have 50% _1 and 50% _2 or just all _2. Going with the latter for now.
data <- data[str_sub(data$Transect.ID, -2) == "02",]

# remove nulls/blanks/X/?. Entire transect set because need a full 10 record transect for each of the 13 species to get comparable sprichness/abundance metrics
data <- data[data$Transect.ID != "MAR02_02",]
data <- data[data$Transect.ID != "STLK01_02",]
data <- data[data$Transect.ID != "WSF01_02",]
data <- data[data$Transect.ID != "SNP01_02",]
data <- data[data$Transect.ID != "BLAN01_02",]
data <- data[data$Transect.ID != "WENG01_02",]
data <- data[data$Transect.ID != "WES01_02",]

# There are a few sites with the same - GWNF sites were visited in both 2016 and 2017. Same names but very different GPS coordinates. Keep both but rename all transect ID with a year identifier to avoid confusion
    # GWNF02_02 
    # GWNF03_02
    # GWNF04_02
    # GWNF05_02
    # GWNF06_02

data$year <- str_sub(data$Date, -3-1)
data$Transect.ID.2 <- NA

for (i in 1:2262) {
  if (data$year[i] == "2016") {
    data$Transect.ID.2[i] <- paste0(data$Transect.ID[i], "_16")
  }
  if (data$year[i] == "2017") {
    data$Transect.ID.2[i] <- paste0(data$Transect.ID[i], "_17")
  }
}

write.csv(data, "U:/CLI/Field Surveys/Invasive/CLI_InvasiveSurveyRaw_7-19-19_EC.csv")
