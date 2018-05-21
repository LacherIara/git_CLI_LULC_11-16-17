#PURPOSE: Combine merged tables and use them to make graphs. 
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

Comb_outputMelt<-paste0(version_table,"Tables/Melt/")
Comb_outputSum<-paste0(version_table,"Tables/Sum/")
Comb_outputSA<-paste0(version_table, "Tables/SA/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TABLES COMPARED ACROSS SCENARIOS OVER TIME 
#Combines all scenarios 

#Read in county Melt csvs to combine all scnenarios 
FolderM<-list.files(Comb_outputMelt, pattern="C.csv", full.names = TRUE) 
CSV_Melt<-lapply(FolderM,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Melt[[1]]$Scenario<-"Q1"
CSV_Melt[[2]]$Scenario<-"Q2"
CSV_Melt[[3]]$Scenario<-"Q3"
CSV_Melt[[4]]$Scenario<-"Q4"
CSV_Melt[[5]]$Scenario<-"RT"

CombinedMeltLC<-do.call(rbind.data.frame,CSV_Melt)
CombinedMeltLC<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedMeltLC, paste0(Comb_outputMelt,"CombinedMelt_C", ".csv"), row.names=FALSE)

#read in region melt csvs to combine all scenarios 
FolderMR<-list.files(Comb_outputMelt, pattern="R.csv", full.names = TRUE) 
CSV_Region<-lapply(FolderM,function(i){
  read.csv(i)
})


#ADD SCENARIO 
CSV_Region[[1]]$Scenario<-"Q1"
CSV_Region[[2]]$Scenario<-"Q2"
CSV_Region[[3]]$Scenario<-"Q3"
CSV_Region[[4]]$Scenario<-"Q4"
CSV_Region[[5]]$Scenario<-"RT"

CombinedRegionLC<-do.call(rbind.data.frame,CSV_Region)
CombinedRegionLC<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep > 1) #IF STILL NEED AFTER 2001
write.csv(CombinedRegionLC, paste0(Comb_outputMeltRegion,"CombinedMelt_R", ".csv"), row.names=FALSE)


##read in sum csv to combine all scenarios 
FolderS<-list.files(Comb_outputSum, pattern=".csv", full.names = TRUE) 
CSV_Sum<-lapply(FolderR,function(i){
  read.csv(i)
})


CSV_Sum[[1]]$Scenario<-"Q1"
CSV_Sum[[2]]$Scenario<-"Q2"
CSV_Sum[[3]]$Scenario<-"Q3"
CSV_Sum[[4]]$Scenario<-"Q4"
CSV_Sum[[5]]$Scenario<-"RT"

CombinedSumLC<-do.call(rbind.data.frame,CSV_Sum)
write.csv(CombinedSumLC, paste0(Comb_outputSum,"Combined_sum", ".csv"), row.names=FALSE)


#PERCENT CHANGE INDIVIDUAL COUNTIES
CombinedMeltLC$Rowid_<-NULL

CombinedMeltLCT2<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==2)
CombinedMeltLCT7<-subset(CombinedMeltLC, CombinedMeltLC$TimeStep ==7)
CombinedMeltLC27<-cbind(CombinedMeltLCT2,CombinedMeltLCT7)


CombinedMeltLC27<-CombinedMeltLC27[,c(1,2,3,5,6,7,8,13)]
CombinedMeltLC27<-mutate(CombinedMeltLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedMeltLC27$PercentChange<-round(CombinedMeltLC27$PercentChange, digits = 0)
CombinedMeltLC27$PercentChange<-paste0(CombinedMeltLC27$PercentChange,"%")


CombinedMeltLC27$TimeStep<-7
PercentChangeMelt<-CombinedMeltLC27[,c(1,2,3,7,9)]

CombinedMeltPC<-merge(CombinedMeltLC,PercentChangeMelt, by=c("Scenario","TimeStep","LABEL","variable"), all.x=TRUE)


#Subset by land cover type 
DevelopmentM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "3")
ForestM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "5")
GrassM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "6")
CropM<-subset(CombinedMeltPC, CombinedMeltPC$LABEL == "7")

#Subset by county examples
Loudoun<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51107")
Frederick<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51069")
Fauquier<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51061")
Shenandoah<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51171")
Albemarle<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51003")
Rockingham<-subset(CombinedMeltPC, CombinedMeltPC$variable == "GEOID_51165")



#subset by county and land cover type 
FauquierD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51061")
FauquierF<-subset(ForestM, ForestM$variable == "GEOID_51061")
FauquierG<-subset(GrassM, GrassM$variable == "GEOID_51061")
FauquierC<-subset(CropM, CropM$variable == "GEOID_51061")


FrederickD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51069")
FrederickF<-subset(ForestM, ForestM$variable == "GEOID_51069")
FrederickG<-subset(GrassM, GrassM$variable == "GEOID_51069")
FrederickC<-subset(CropM, CropM$variable == "GEOID_51069")

AlbemarleD<-subset(DevelopmentM, DevelopmentM$variable == "GEOID_51003")
AlbemarleF<-subset(ForestM, ForestM$variable == "GEOID_51003")
AlbemarleG<-subset(GrassM, GrassM$variable == "GEOID_51003")
AlbemarleC<-subset(CropM, CropM$variable == "GEOID_51003")
#---------------------------------------------------#
#PERCENT CHANGE REGION
CombinedRegionLC$Rowid_<-NULL

CombinedRegionLCT2<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==2)
CombinedRegionLCT7<-subset(CombinedRegionLC, CombinedRegionLC$TimeStep ==7)
CombinedRegionLC27<-cbind(CombinedRegionLCT2,CombinedRegionLCT7)


CombinedRegionLC27<-CombinedRegionLC27[,c(1,2,3,5,6,7,8,13)]
CombinedRegionLC27<-mutate(CombinedRegionLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100)
CombinedRegionLC27$PercentChange<-round(CombinedRegionLC27$PercentChange, digits = 0)
CombinedRegionLC27$PercentChange<-paste0(CombinedRegionLC27$PercentChange,"%")


CombinedRegionLC27$TimeStep<-7
PercentChangeRegion<-CombinedRegionLC27[,c(1,2,3,7,9)]

CombinedRegionPC<-merge(CombinedRegionLC,PercentChangeRegion, by=c("Scenario","TimeStep","LABEL","variable"), all.x=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PERCENT CHANGE of SUM 
CombinedSumLCT2<-subset(CombinedSumLC, CombinedSumLC$TimeStep ==2)
CombinedSumLCT7<-subset(CombinedSumLC, CombinedSumLC$TimeStep ==7)
CombinedSumLC27<-cbind(CombinedSumLCT2,CombinedSumLCT7)
CombinedSumLC27<-CombinedSumLC27[,c(1,2,4,5,7,10)]
CombinedSumLC27<-mutate(CombinedSumLC27, PercentChange=((valuekm.1-valuekm)/valuekm)*100) #calculate percent change after only haveing time step 2 and 7
CombinedSumLC27$PercentChange<-round(CombinedSumLC27$PercentChange, digits = 2)


CombinedSumLC27$TimeStep<-7
PercentChange<-CombinedSumLC27[,c(1,2,3,7)]

CombinedSumPC<-merge(CombinedSumLC,PercentChange, by=c("Scenario","TimeStep","LABEL"), all.x=TRUE)

#Subset By Land Cover
DevelopmentPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "3")
ForestPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "5")
GrassPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "6")
CropPC<-subset(CombinedSumPC, CombinedSumPC$LABEL == "7")

#USE FOR NOW (WITHOUT Percent)
DevelopmentPC<-subset(CombinedSumLC, CombinedSumLC$LABEL == "3")
ForestPC<-subset(CombinedSumLC, CombinedSumLC$LABEL == "5")
GrassPC<-subset(CombinedSumLC, CombinedSumLC$LABEL == "6")
CropPC<-subset(CombinedSumLC, CombinedSumLC$LABEL == "7")


#Remove Timestep 1
DevelopmentPC<-subset(DevelopmentPC, DevelopmentPC$TimeStep > 1)
ForestPC<-subset(ForestPC, ForestPC$TimeStep > 1)
GrassPC<-subset(GrassPC, GrassPC$TimeStep >1)
CropPC<-subset(CropPC, CropPC$TimeStep>1)

#--------------------------------------------------------#

#--------------------------------------------------------------#
#Graphs





#IF GRAPH LOOKS weird make sure LABEL is set as a factor 
#Graphs for individual counties 
#When saved as individual graph v2015_Fred_crop
#when saved for ggarrange crop,development, forest, grass 

#CHANGE TO SIZE 40 IF GRAPHS ARE NOT GOING TO BE ARRANGED
windows()

#FAUQUIER
v2015_Fauq_development<-ggplot(FauquierD, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #Forest#scale_y_continuous(name =expression('Total Area km'^2), limits = c(650,800), breaks=c(650,675,700,725,750,775,800))+
  #grass# scale_y_continuous(name =expression('Total Area km'^2), limits=c(550,650), breaks=c(550,575,600,625))+
  #development#
  scale_y_continuous(name =expression('Total Area km'^2),  limits=c(0,200), breaks=c(50,100,150,200))+
  #crop#scale_y_continuous(name =expression('Total Area km'^2), limits=c(100,175), breaks=c(100,125,150,175))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=20, colour="black"),
        axis.title.x=element_text(size=20), axis.title.y =element_text(size=20, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=20, show.legend=FALSE)+
  theme(panel.border=element_blank())+
  theme(axis.line = element_line(size=1.5, colour="black")) 

#FREDERICK
v2015_Fred_forest<-ggplot(FauquierF, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  #Forest# scale_y_continuous(name =expression('Total Area km'^2), limits=c(525,625), breaks=c(525,550,575,600,625))+
  #grass#scale_y_continuous(name =expression('Total Area km'^2), limits=c(250,350), breaks=c(250,275,300,325,350))+
  #development#scale_y_continuous(name =expression('Total Area km'^2), limits=c(50,200), breaks=c(50,100,150,200))+
  #crop#scale_y_continuous(name =expression('Total Area km'^2), limits=c(5,20), breaks=c(5,10,15,20))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=20, colour="black"),
        axis.title.x=element_text(size=20), axis.title.y =element_text(size=20, face="bold"), legend.text=element_text(size=20), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), size=10, show.legend=FALSE)+
  theme(panel.border=element_blank())+
  theme(axis.line = element_line(size=1.5, colour="black")) 


#-----------------------------------------------------------#
#Table to show percent change

#Frederick County 
#Fred_PC<-subset(PercentChangeMelt, variable=="GEOID_51069")
#Fred_Q1<-subset(Fred_PC, Scenario =="Q1")
#Fred_Q2<-subset(Fred_PC, Scenario =="Q2")
#Fred_Q3<-subset(Fred_PC, Scenario =="Q3")
#Fred_Q4<-subset(Fred_PC, Scenario =="Q4")
#Fred_RT<-subset(Fred_PC, Scenario =="RT")

#Fred_Table<-cbind(Fred_RT[,5],Fred_Q1[,5],Fred_Q2[,5],Fred_Q3[,5],Fred_Q4[,5])
#Fred_Table<-as.data.frame(Fred_Table)
#colnames(Fred_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
#rownames(Fred_Table)<-c("Development", "Forest", "Grass", "Crop")

#Fred_Table_plot<-ggtexttable(Fred_Table, theme=ttheme("mBlackWhite", base_size=15))

#windows()
#FrederickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

#windows()
#ggarrange(FrederickGraph, Fred_Table_plot, ncol=2, nrow=1, widths =c(1,.35))


#Fauquier County 
#Fauq_PC<-subset(PercentChangeMelt, variable=="GEOID_51061")
#Fauq_Q1<-subset(Fauq_PC, Scenario =="Q1")
#Fauq_Q2<-subset(Fauq_PC, Scenario =="Q2")
#Fauq_Q3<-subset(Fauq_PC, Scenario =="Q3")
#Fauq_Q4<-subset(Fauq_PC, Scenario =="Q4")
#Fauq_RT<-subset(Fauq_PC, Scenario =="RT")

#Fauq_Table<-cbind(Fauq_RT[,5],Fauq_Q1[,5],Fauq_Q2[,5],Fauq_Q3[,5],Fauq_Q4[,5])
#Fauq_Table<-as.data.frame(Fauq_Table)
#colnames(Fauq_Table)<-c("RT", "Q1", "Q2", "Q3", "Q4")
#rownames(Fauq_Table)<-c("Development", "Forest", "Grass", "Crop")

#Fauq_Table_plot<-ggtexttable(Fauq_Table, theme=ttheme("mBlackWhite", base_size=15))

#windows()
#FauqerickGraph<-ggarrange(development, forest, grass, crop, labels=c("Development", "Forest", "Grass", "Crop"), common.legend= TRUE, legend="left")

#windows()
#ggarrange(FauqerickGraph, Fauq_Table_plot, ncol=2, nrow=1, widths =c(1,.35))


#-------------------------------------------------------------#
#export graphs 
setwd("X:/Scenario Planning/Graphics/Map Images/4_17")
png("v2015_Fauq_development.png", width=480, height=480, units="px", res=300) #can't put units and resolution
v2015_Fauq_development
dev.off()


ggsave(file="v2015_Fauq_development.png", dpi=300,width=15, height=15)



#Graphs for entire study region. All scenarios but one type of land cover 

#-------------------------------------------------------------------------#
library(ggplot2)

windows()
ggplot(GrassPC, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c( "2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Total Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))#+
#geom_label_repel(aes(label=ifelse(is.na(PercentChange),"",paste0(PercentChange,"%"))), hjust=2,vjust=2, size=5, show.legend=FALSE)


#export graph 
setwd("X:/Scenario Planning/Graphics/Map Images/IALE Presentation")
png("CropSA.png", width=480, height=480, units="px", res=300) #can't put units and resolution
CropSA
dev.off()


ggsave(file="CropSA.png", dpi=300, width=15, height=15)


#-------------------------------------------------------#
#Graph of Percent Change 
windows()
ggplot(DevelopmentPC, aes(x=Scenario, y=PercentChange, fill=Scenario))+
  geom_bar(stat="identity", position = 'dodge')+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name="Percent Change", limits=c(-100,120), labels=c("-100%","-50%", "-0%","50%", "100%", "120%"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text.y =element_text(size=40),
        axis.text.x =element_blank(),
        axis.title.x=element_blank(), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))+
  theme(panel.border=element_blank())+
  geom_hline(yintercept=0, size=1.5)+
  geom_text(aes(label=paste0(PercentChange,"%")), vjust=1.6, size=10, colour="white")+
  theme(axis.line.y =element_line(size=1.5))

