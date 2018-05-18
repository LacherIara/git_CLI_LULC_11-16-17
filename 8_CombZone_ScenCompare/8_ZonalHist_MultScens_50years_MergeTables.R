#PURPOSE: Once tables are produced for timestpes 1-5 increase efficiency of the merged and region tables. 
#Creator: Sarah Halperin 
#Contact: halperins@si.edu
#------------------------------------------------#

#set inputs
version<-"/StudyArea_V201/SA_V2016"
version_table<-paste0("U:/CLI/Dinamica_Runs",version, "/BasicDataAnalyses/Zonal_Histogram/")

#--------------------------------------------------------------------#
Comb_outputMelt<-paste0(version_table,"Tables/Melt/")
Comb_outputRegion<-paste0(version_table,"Tables/Region/")
Comb_outputReshape<-paste0(version_table,"Tables/Reshape/")

#Read in files for NLCD
Folder<-list.files(paste0(version_table, "NL"), pattern=".txt", full.names = TRUE) #Read in NCLD files 
NCLD<-lapply(Folder,function(i){
  read.csv(i)
})


NCLD[[1]]$TimeStep<-1
NCLD[[2]]$TimeStep<-2

NLCD<-do.call(rbind.data.frame,NCLD)

#Read in All scenarios 
RTFolder<-list.files(paste0(version_table,"RT"), pattern=".txt", full.names = TRUE) 
TablesRT<-lapply(RTFolder,function(i){
  read.csv(i)
})

TablesRT[[1]]$TimeStep<-3
TablesRT[[2]]$TimeStep<-4
TablesRT[[3]]$TimeStep<-5
TablesRT[[4]]$TimeStep<-6
TablesRT[[5]]$TimeStep<-7

Q1Folder<-list.files(paste0(version_table,"Q1"), pattern=".txt", full.names = TRUE) 
TablesQ1<-lapply(Q1Folder,function(i){
  read.csv(i)
})

TablesQ1[[1]]$TimeStep<-3
TablesQ1[[2]]$TimeStep<-4
TablesQ1[[3]]$TimeStep<-5
TablesQ1[[4]]$TimeStep<-6
TablesQ1[[5]]$TimeStep<-7

Q2Folder<-list.files(paste0(version_table,"Q2"), pattern=".txt", full.names = TRUE) 
TablesQ2<-lapply(Q2Folder,function(i){
  read.csv(i)
})

TablesQ2[[1]]$TimeStep<-3
TablesQ2[[2]]$TimeStep<-4
TablesQ2[[3]]$TimeStep<-5
TablesQ2[[4]]$TimeStep<-6
TablesQ2[[5]]$TimeStep<-7

Q3Folder<-list.files(paste0(version_table,"Q3"), pattern=".txt", full.names = TRUE) 
TablesQ3<-lapply(Q3Folder,function(i){
  read.csv(i)
})

TablesQ3[[1]]$TimeStep<-3
TablesQ3[[2]]$TimeStep<-4
TablesQ3[[3]]$TimeStep<-5
TablesQ3[[4]]$TimeStep<-6
TablesQ3[[5]]$TimeStep<-7

Q4Folder<-list.files(paste0(version_table,"Q4"), pattern=".txt", full.names = TRUE) 
TablesQ4<-lapply(Q4Folder,function(i){
  read.csv(i)
})

TablesQ4[[1]]$TimeStep<-3
TablesQ4[[2]]$TimeStep<-4
TablesQ4[[3]]$TimeStep<-5
TablesQ4[[4]]$TimeStep<-6
TablesQ4[[5]]$TimeStep<-7

#Combined timesteps 
CombinedRT<-do.call(rbind.data.frame,TablesRT)
CombinedQ1<-do.call(rbind.data.frame,TablesQ1)
CombinedQ2<-do.call(rbind.data.frame,TablesQ2)
CombinedQ3<-do.call(rbind.data.frame,TablesQ3)
CombinedQ4<-do.call(rbind.data.frame,TablesQ4)


CombinedRT<-rbind(NLCD,CombinedRT)
CombinedQ1<-rbind(NLCD,CombinedQ1)
CombinedQ2<-rbind(NLCD,CombinedQ2)
CombinedQ3<-rbind(NLCD,CombinedQ3)
CombinedQ4<-rbind(NLCD,CombinedQ4)

CombinedList<-list(CombinedRT, CombinedQ1, CombinedQ2, CombinedQ3, CombinedQ4)

#------------------------------------------------#
#Melt Tables and Region sum tables 
ScenarioMelt<-list("RT_Melt.csv", "Q1_Melt.csv", "Q2_Melt.csv", "Q3_Melt.csv", "Q4_Melt.csv")
ScenarioRegion<-list("RT_Region.csv", "Q1_Region.csv", "Q2_Region.csv", "Q3_Region.csv", "Q4_Region.csv")


CombinedMelt<-lapply(CombinedList, function(x){
  melt(x,id=c("Rowid_", "LABEL", "TimeStep"))
})

for(i in 1:length(CombinedMelt)){
      CombinedMelt[[i]]$valuekm<-CombinedMelt[[i]]$value*(900/1000000)
      write.csv(CombinedMelt[[i]], paste0(Comb_outputMelt,(ScenarioMelt[[i]])), row.names=FALSE)
      region_sum<-aggregate(valuekm~LABEL + TimeStep, CombinedMelt[[i]], sum)
      write.csv(region_sum, paste0(Comb_outputRegion,(ScenarioRegion[[i]])), row.names=FALSE)
}



#-----------------------------------------------------------------#
#Reshape Tables 
ScenarioReshape<-list("RT_Reshape", "Q1_Reshape", "Q2_Reshape", "Q3_Reshape", "Q4_Reshape")

for( i in 1:length(CombinedList)){
  Combined<-CombinedList[[i]]
LABEL3<-Combined %>%
  filter(LABEL==3)
LABEL3<-LABEL3[,2:11]
LABEL3<-t(LABEL3)
LABEL3<-LABEL3[1:9,]
colnames(LABEL3)<-c("2001.3", "2011.3", "2021.3","2031.3","2041.3", "2051.3","2061.3")
#Land Cover Type #5
LABEL5<-Combined %>%
  filter(LABEL==5)
LABEL5<-LABEL5[,2:11]
LABEL5<-t(LABEL5)
LABEL5<-LABEL5[1:9,]
colnames(LABEL5)<-c("2001.5", "2011.5","2021.5","2031.5","2041.5", "2051.5","2061.5")
#Land Cover Type #6
LABEL6<-Combined %>%
  filter(LABEL==6)
LABEL6<-LABEL6[,2:11]
LABEL6<-t(LABEL6)
LABEL6<-LABEL6[1:9,]
colnames(LABEL6)<-c("2001.6", "2011.6", "2021.6","2031.6","2041.6", "2051.6","2061.6")
#Land Cover Type #7
LABEL7<-Combined %>%
  filter(LABEL==7)
LABEL7<-LABEL7[,2:11]
LABEL7<-t(LABEL7)
LABEL7<-LABEL7[1:9,]
colnames(LABEL7)<-c("2001.7", "2011.7"," 2021.7","2031.7","2041.7", "2051.7","2061.7")

CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
write.csv(CombinedReshape, paste0(Comb_outputReshape,(ScenarioReshape[[i]]),".csv")) 

}

