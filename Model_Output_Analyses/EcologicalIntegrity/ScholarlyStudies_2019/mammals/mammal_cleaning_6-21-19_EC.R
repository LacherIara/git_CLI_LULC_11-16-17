
#####################################
# set working directory
setwd("U:/CLI/Field Surveys/eMammal")

library(stringr)

####################################
# read in raw data
sites <- read.csv("U:/CLI/Field Surveys/eMammal/All_species_CLI_covs_fix.csv")

speciesdat <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammal_DATA_12042018.csv")

site.daysactive <- read.csv("U:/CLI/Field Surveys/eMammal/bear_det.csv")

###################################
# fix dates in sites

sites$actual_dat2 <- as.character(sites$actual_dat)
sites$retrieval_2 <- as.character(sites$retrieval_)

rows <- seq(1, length(sites$deployment_name))

## standardize year as 4 digits
for (i in rows) {
  if (str_sub(sites$actual_dat2[i], -3, -3) == "/") {
    sites$actual_dat2[i] <- paste0(str_sub(sites$actual_dat2[i], 1, -3),
                                       "20",
                                       str_sub(sites$actual_dat2[i], -2, -1))
  }
  if (str_sub(sites$retrieval_2[i], -3, -3) == "/") {
    sites$retrieval_2[i] <- paste0(str_sub(sites$retrieval_2[i], 1, -3),
                                   "20",
                                   str_sub(sites$retrieval_2[i], -2, -1))
  }
}

## standardize day as 2 digits
for (i in rows) {
  if (str_sub(sites$actual_dat2[i], 2, 2) == "/") {
    sites$actual_dat2[i] <- paste0("0",
                                   str_sub(sites$actual_dat2[i], 1))
  }
  if (str_sub(sites$retrieval_2[i], 2, 2) == "/") {
    sites$retrieval_2[i] <- paste0("0",
                                   str_sub(sites$retrieval_2[i], 1))
  }
}

## standardize month as 2 digits
for (i in rows) {
  if (str_sub(sites$actual_dat2[i], 5, 5) == "/") {
    sites$actual_dat2[i] <- paste0(str_sub(sites$actual_dat2[i], 1, 3),
                                   "0",
                                   str_sub(sites$actual_dat2[i], 4))
  }
  if (str_sub(sites$retrieval_2[i], 5, 5) == "/") {
    sites$retrieval_2[i] <- paste0(str_sub(sites$retrieval_2[i], 1, 3),
                                   "0",
                                   str_sub(sites$retrieval_2[i], 4))
  }
}

sites$actual_dat3 <- as.Date(sites$actual_dat2, format="%d/%m/%Y")
sites$retrieval_3 <- as.Date(sites$retrieval_2, format="%d/%m/%Y")

hist(sites$actual_dat3, breaks="months")
hist(sites$retrieval_3, breaks ="months")

## find 30 days from start date (for later - if camera trap siting is >30 days after initial date, delete it)
sites$actual_dat4 <- sites$actual_dat3 + 30

## calculate days deployed (up to 30)
site.daysactive$days_deployed <- NA

site.daysactive[site.daysactive == 0] <- 1

site.daysactive$days_deployed <- rowSums(site.daysactive[,c(2:31)], na.rm = TRUE)

## create data frame that includes, for each site, the name, deployment date, & days deployed
cleaned.sites <- merge(sites, site.daysactive, by="deployment_name")
cleaned.sites <- cleaned.sites[,c(1,4:10,18:19,78:80,111)]
names(cleaned.sites)
names(cleaned.sites)[11] <- "actual_dat"
names(cleaned.sites)[12] <- "retrieval_dat"
names(cleaned.sites)[13] <- "actual_dat.30"

## fix dates in speciesdat

speciesdat$Begin.Time2 <- as.character(speciesdat$Begin.Time)
unique(nchar(speciesdat$Begin.Time2))

rows2 <- seq(1, length(speciesdat$Project))

for (i in rows2) {
  if (nchar(speciesdat$Begin.Time2[i])==19) {
    speciesdat$Begin.Time2[i] <- str_sub(speciesdat$Begin.Time2[i], 1, 10)
  }
}

## standardize month as 2 digits
for (i in rows2) {
  if (str_sub(speciesdat$Begin.Time2[i], 2, 2) == "/") {
    speciesdat$Begin.Time2[i] <- paste0("0",
                                   str_sub(speciesdat$Begin.Time2[i], 1))
  }
}

## standardize day as 2 digits
for (i in rows2) {
  if (str_sub(speciesdat$Begin.Time2[i], 5, 5) == "/") {
    speciesdat$Begin.Time2[i] <- paste0(str_sub(speciesdat$Begin.Time2[i], 1, 3),
                                        "0",
                                        str_sub(speciesdat$Begin.Time2[i], 4))
  }
}

## check if year is already all 4 digits
speciesdat$yeartest <- "wrong"

for (i in rows2) {
  if (str_sub(speciesdat$Begin.Time2[i], 7, 8) == "20") {
    speciesdat$yeartest[i] <- "good"
  }
}
unique(speciesdat$yeartest[nchar(speciesdat$Begin.Time2) > 10])

## final date
speciesdat$Begin.Time2 <- str_sub(speciesdat$Begin.Time2, 1, 10)

for (i in rows2) {
  if (str_sub(speciesdat$Begin.Time2[i], 5,5) == "-") {
    speciesdat$Begin.Time2[i] <- paste0(str_sub(speciesdat$Begin.Time2[i],6,7),
                                        "/",
                                        str_sub(speciesdat$Begin.Time2[i],9,10),
                                        "/",
                                        str_sub(speciesdat$Begin.Time2[i],1,4))
  }
}

speciesdat$Begin.Time3 <- as.Date(speciesdat$Begin.Time2, format="%m/%d/%Y")

names(speciesdat)[4] <- "deployment_name"

cleaned.speciesdat <- speciesdat[,c(1:18,21)]

## join cleaned.sites to all speciesdat

cleaned.data <- merge(cleaned.sites, cleaned.speciesdat, by="deployment_name")


## remove camera trap sitings that are >30 days from initial date
cleaned.data$keep <- "yes"

rows3 <- seq(1, length(cleaned.data$deployment_name))

for (i in rows3) {
  if (cleaned.data$Begin.Time3[i] > cleaned.data$actual_dat.30[i]) {
    cleaned.data$keep[i] <- "no"
  }
}

cleaned.data2 <- cleaned.data[cleaned.data$keep == "yes",]

# Remove sites where Species.Name = "Camera Trapper"
unique(cleaned.data2$Species.Name)

cleaned.data3 <- cleaned.data2[cleaned.data2$Species.Name != "Camera Trapper",]
unique(cleaned.data3$Common.Name)

# Unkown Weasel and Least Weasal only appear at different places - so won't double count for species richness. keep unknown weasal

write.csv(cleaned.data3, file="eMammal_RawData_7-1-19.csv")



