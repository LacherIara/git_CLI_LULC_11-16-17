############################ 
#PURPOSE:Graphics for Changing Landscape Initiative. Use as a reference 
#INPUT: NA
#OUTPUT: 
#DEVELOPED: 4/13/18
#CONTACT: halperinS@si.edu
#NOTES:

################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Colors
#Q1 = #FF0404
#Q2 = #FF9933
#Q3 = #106A0F
#Q4 = #0070C1
#RT = #330066


#Fragstats Mean Patch Size
#Two counties compared here Frederick and Fauquier are compared 
ggplot(C_statsCombinedFF, aes(x=class, y=mean.patch.areakm, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("3", "10"), labels=c("Frederick County", "Fauquier County"))+ 
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+ 
  scale_y_continuous(name =expression('Mean Patch Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_blank(), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))


#Number of Patches 
ggplot(C_statsCombinedFF, aes(x=class, y=n.patches, fill=Scenario))+
  geom_bar(stat="identity", position="dodge")+
  scale_x_discrete(breaks= c("3", "10"), labels=c("Frederick", "Fauquier"))+
  scale_fill_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  xlab("County")+
  scale_y_continuous(name = "Number of Patches")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))


#Zonal Histogram Graph 
#Same for both study area and individual counties 
ggplot(FrederickC, aes(x=TimeStep, y=valuekm, colour=Scenario, group=Scenario))+
  geom_line(size=2)+
  #facet_grid(~LABEL)+
  scale_x_continuous(name= "Time Step", breaks= c(2,3,4,5,6,7), labels=c("2011", "2021", "2031", "2041", "2051", "2061"))+
  scale_colour_manual(values=c("#FF0404", "#FF9933","#106A0F", "#0070C1","#330066"))+
  scale_y_continuous(name =expression('Total Area km'^2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(axis.text=element_text(size=40),
        axis.title.x=element_text(size=40,face="bold"), axis.title.y =element_text(size=40,face="bold"), legend.text=element_text(size=40), legend.title=element_blank(), legend.key.height= unit(1,"in"))+
  theme(plot.margin=unit(c(1,1,1,1), "in"))
