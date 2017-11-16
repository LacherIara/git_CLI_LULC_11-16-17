############################ 
#PURPOSE: Notes on corrections I've made to code.
#INPUT: 
#OUTPUT: 
#DEVELOPED: (V1) 2/18/2016 
#CONTACT: LacherI@si.edu
#NOTES:
#IMPORTANT: 
##### NEXT STEPS #####

############################


# From "SAbc_PatchStats.r", "FGC-GCF version"

# TRANSITIONS <- list("2" = data.frame(inc = c(11,16,13),dec = NA),"4" =data.frame( inc = 24, dec = c(11,5)),"5" = data.frame(inc = 5,dec = c(16,24)), "6" = data.frame(inc = NA,dec = 13))
  # #This is a somewhat confusing input (looking for a better way)
  
  # **Modify each time you change transitions in landuse **
  # TRANSITIONS <- list("2" = data.frame(inc = c(11,16,13),dec = NA),"4" =data.frame( inc = c(24,NA, NA), dec = c(NA,11,5)),"5" = data.frame(inc = c(5, NA, NA),dec = c(NA,16,24)), "6" = data.frame(inc = NA,dec = 13))
  
  TRANSITIONS <- list("2" = data.frame(inc = c(42,52,62),dec = NA),"4" =data.frame( inc = c(54,NA, NA), dec = c(NA,42,45)),"5" = data.frame(inc = c(45, NA, NA),dec = c(NA,52,54)), "6" = data.frame(inc = NA,dec = 62))
  