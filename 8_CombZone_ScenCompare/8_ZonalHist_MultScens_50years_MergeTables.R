#PURPOSE: Once tables are produced for timestpes 1-5 increase efficiency of melted tables for use in ggplot and combining all scenarios. 
#Creator: Sarah Halperin 
#Contact: halperins@si.edu
#------------------------------------------------#
library(plyr) # General data manipulation
library(dplyr) # General data manipulation
library(raster) # read and edit rasters
library(rgdal)
library(reshape) #manipulation of output tables 
library(ggplot2) #graphs 
library(ggpubr)



#set inputs
version<-"/StudyArea_V201/SA_V2016"
version_table<-paste0("U:/CLI/Dinamica_Runs",version, "/BasicDataAnalyses/Zonal_Histogram/PL_Gap/")
tables<-paste0("Tables/", "v2016_")#make sure change version
#--------------------------------------------------------------------#
Comb_outputCounty<-paste0(version_table, tables, "County/")
Comb_outputSA<-paste0(version_table, tables,"StudyArea/")
Comb_outputBuffer<-paste0(version_table,tables, "Buffer/")
Comb_outputRegion<-paste0(version_table, tables, "Region/")
Comb_outputReshape<-paste0(version_table, tables, "County/v2016_Reshape/") #manually change for this one

Region_SA<-read.csv("U:/CLI/Dinamica_Runs/StudyArea_V201/CountyNmsGEOIDRegion_cnty.csv")
Region_SA$variable<-paste0(Region_SA$GEOID_, Region_SA$GEOID)
Region_SA<-Region_SA[,5:7]
Region_SA[1,]<-"LABEL"

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
Version_Name<-"v2016_"

ScenarioMelt<-list(paste0(Version_Name, "ZonalHistogram_RT_ctny.csv"), paste0(Version_Name,"ZonalHistogram_Q1_ctny.csv"), paste0(Version_Name, "ZonalHistogram_Q2_ctny.csv"), paste0(Version_Name, "ZonalHistogram_Q3_ctny.csv"), paste0(Version_Name, "ZonalHistogram_Q4_ctny.csv"))
ScenarioRegion<-list(paste0(Version_Name, "ZonalHistogram_RT_rgn.csv"), paste0(Version_Name, "ZonalHistogram_Q1_rgn.csv"), paste0(Version_Name, "ZonalHistogram_Q2_rgn.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_rgn.csv"),paste0(Version_Name,"ZonalHistogram_Q4_rgn.csv"))
ScenarioSum<-list(paste0(Version_Name, "ZonalHistogram_RT_buffer.csv"), paste0(Version_Name, "ZonalHistogram_Q1_buffer.csv"), paste0(Version_Name, "ZonalHistogram_Q2_buffer.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_buffer.csv"),paste0(Version_Name,"ZonalHistogram_Q4_buffer.csv"))
ScenarioSA_sum<-list(paste0(Version_Name, "ZonalHistogram_RT_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_SA.csv"))
ScenarioSA_Region<-list(paste0(Version_Name, "ZonalHistogram_RT_rgn_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_rgn_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_rgn_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_rgn_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_rgn_SA.csv"))
                                                                 
ScenarioSA_County<-list(paste0(Version_Name, "ZonalHistogram_RT_ctny_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q1_ctny_SA.csv"), paste0(Version_Name, "ZonalHistogram_Q2_ctny_SA.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_ctny_SA.csv"),paste0(Version_Name,"ZonalHistogram_Q4_ctny_SA.csv"))



CombinedMelt<-lapply(CombinedList, function(x){
  melt(x,id=c("Rowid_", "LABEL", "TimeStep"))
})



for(i in 1:length(CombinedMelt)){
      CombinedMelt[[i]]$valuekm<-CombinedMelt[[i]]$value*(900/1000000)
      CombinedMelt[[i]]<-merge(CombinedMelt[[i]], Region_SA, by="variable")
        write.csv(CombinedMelt[[i]], paste0(Comb_outputCounty,(ScenarioMelt[[i]])), row.names=FALSE)
      Melt_region<-aggregate(valuekm~LABEL+TimeStep+Region,CombinedMelt[[i]], sum)
      write.csv(Melt_region, paste0(Comb_outputRegion,(ScenarioRegion[[i]])), row.names=FALSE)
      Full_sum<-aggregate(valuekm~LABEL + TimeStep, CombinedMelt[[i]], sum)
      SA_Region<-subset(Melt_region, Melt_region$Region %in% c(1,2,3,4,5))
      SA_County<-subset(CombinedMelt[[i]], CombinedMelt[[i]]$SA==1)
      SA_Sum<-aggregate(valuekm~LABEL + TimeStep, SA_Region, sum)
      write.csv(SA_Region,paste0(Comb_outputRegion,(ScenarioSA_Region[[i]])), row.names=FALSE )
      write.csv(SA_Sum,paste0(Comb_outputSA,(ScenarioSA_sum[[i]])), row.names=FALSE )
      write.csv(Full_sum,paste0(Comb_outputBuffer,(ScenarioSum[[i]])), row.names=FALSE )
      write.csv(SA_Region, paste0(Comb_outputRegion,(ScenarioSA_Region[[i]])), row.names=FALSE)
      write.csv(SA_County, paste0(Comb_outputCounty,(ScenarioSA_County[[i]])), row.names=FALSE)
}



#-----------------------------------------------------------------#
#Reshape Tables 
ScenarioReshape<-list(paste0(Version_Name, "ZonalHistogram_RT_Reshape.csv"), paste0(Version_Name, "ZonalHistogram_Q1_Reshape.csv"), paste0(Version_Name, "ZonalHistogram_Q2_Reshape.csv"),paste0(Version_Name,  "ZonalHistogram_Q3_Reshape.csv"),paste0(Version_Name,"ZonalHistogram_Q4_Reshape.csv"))

for( i in 1:length(CombinedList)){
  Combined<-CombinedList[[i]]
LABEL3<-Combined %>%
  filter(LABEL==3)
LABEL3<-LABEL3[,2:58] #change if number of rows changes (aka if want regions)
LABEL3<-t(LABEL3)
LABEL3<-LABEL3[1:57,] #change if number of rows changes (aka if want regions)
colnames(LABEL3)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
LABEL3<-as.data.frame(LABEL3)
LABEL3$Change3<-LABEL3$`2061`-LABEL3$`2011`
#Land Cover Type #5
LABEL5<-Combined %>%
  filter(LABEL==5)
LABEL5<-LABEL5[,2:58]
LABEL5<-t(LABEL5)
LABEL5<-LABEL5[1:57,]
colnames(LABEL5)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
LABEL5<-as.data.frame(LABEL5)
LABEL5$Change5<-LABEL5$`2061`-LABEL5$`2011`
#Land Cover Type #6
LABEL6<-Combined %>%
 filter(LABEL==6)
LABEL6<-LABEL6[,2:58]
LABEL6<-t(LABEL6)
LABEL6<-LABEL6[1:57,]
colnames(LABEL6)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
LABEL6<-as.data.frame(LABEL6)
LABEL6$Change6<-LABEL6$`2061`-LABEL6$`2011`
#Land Cover Type #7
LABEL7<-Combined %>%
  filter(LABEL==7)
LABEL7<-LABEL7[,2:58]
LABEL7<-t(LABEL7)
LABEL7<-LABEL7[1:57,]
colnames(LABEL7)<-c("2001", "2011", "2021","2031","2041", "2051","2061")
LABEL7<-as.data.frame(LABEL7)
LABEL7$Change7<-LABEL7$`2061`-LABEL7$`2011`

CombinedReshape<-cbind(LABEL3, LABEL5,LABEL6,LABEL7) 
write.csv(CombinedReshape, paste0(Comb_outputReshape,(ScenarioReshape[[i]]),".csv")) 

}


