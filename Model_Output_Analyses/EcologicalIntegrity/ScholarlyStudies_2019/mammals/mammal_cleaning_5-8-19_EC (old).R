
#####################################
# set working directory
setwd("U:/CLI/Field Surveys/eMammal")

library(stringr)

####################################
# read in raw data
sites <- read.csv("U:/CLI/Field Surveys/eMammal/All_species_CLI_covs_fix.csv")

speciesdat <- read.csv("U:/CLI/Field Surveys/eMammal/CLI_Mammal_DATA_12042018.csv")

site.daysactive <- read.csv("U:/CLI/Field Surveys/eMammal/bear_det.csv")

####################################
# extract sites that Mike has "cleaned" (he wasn't able to recreate/explain his methods)

names(sites)[1] <- "Deployment.Name"

species.site.merge <- merge(speciesdat, sites, by="Deployment.Name")

howmanysites <- length(unique(species.site.merge$Deployment.Name))

deployID <- length(unique(species.site.merge$Deploy.ID))

species.site.merge <- species.site.merge[,1:34]

write.csv(species.site.merge, file="U:/CLI/Field Surveys/eMammal/eMammal_RawData_5-8-19_EC.csv")

##########################################
# add column showing how many days each SITE (deployment name) was deployed for
## also need to find a way to delete sitings after 30 days? Under 30 days divide by # of days?

site.daysactive$days_deployed <- NA

site.daysactive[site.daysactive == 0] <- 1

site.daysactive$days_deployed <- rowSums(site.daysactive[,c(2:31)], na.rm = TRUE)

AllSites <- sites[,c(1:10,18,19)]

AllSites$days_deployed <- site.daysactive$days_deployed

names(speciesdat)[4] <- "deployment_name"

AllSites.dat <- merge(AllSites, speciesdat, by="deployment_name")

Option1 <- AllSites.dat[AllSites.dat$days_deployed < 30,]



###

AllSites2 <- AllSites[AllSites$days_deployed == 30,]
AllSites2$actual_dat2 <- as.character(AllSites2$actual_dat)
AllSites2$actual_dat2.length <- nchar(AllSites2$actual_dat2)


# if str_sub(AllSites2$actual_dat2, -3, -3) == "/"

rows <- seq(1, length(AllSites2$deployment_name))

AllSites2$actual_dat3 <- AllSites2$actual_dat2

for (i in rows) {
  if (str_sub(AllSites2$actual_dat3[i], -3, -3) == "/") {
    AllSites2$actual_dat3[i] <- paste0(str_sub(AllSites2$actual_dat3[i], 1, -3),
                                       "20",
                                       str_sub(AllSites2$actual_dat3[i], -2, -1))
  }
}



###
sites.where.longer.than.month <- unique(AllSites.dat$deployment_name[AllSites.dat$days_deployed == 30])

sites.where.longer.than.month

helpme <- data.frame("deployment_name" = sites.where.longer.than.month)

helpme2 <- merge(helpme, AllSites.dat, by="deployment_name")
helpme2$actual_dat2 <- as.Date(helpme2$actual_dat, format="%d/%m/%Y")

hist(helpme2$actual_dat2, breaks="months")

length(helpme2$actual_dat)

Option1 <- AllSites.dat[AllSites.dat$days_deployed < 30,]

helpme2$actual_dat3 <- as.character(helpme2$actual_dat)
helpme2$actual_dat3_length <- nchar(helpme2$actual_dat3)

str(helpme2$actual_dat3)

unique(nchar(helpme2$actual_dat3))

