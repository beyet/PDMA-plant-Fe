library(readxl)
library(agricolae)
library(ggplot2)
app = read_excel("activeiron.xlsx",sheet = 1) 

color2<-c("#FFFFFF","#808080","#808080","#808080")
app1<-app[1:21,1:2]
hei_mean<-aggregate(ironp~indexp,data = app1,mean)
hei_sd<-aggregate(ironp~indexp,data = app1,sd)
p1<-ggplot(hei_mean,aes(x=indexp,y=ironp))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=ironp-hei_sd$ironp,ymax=ironp+hei_sd$ironp),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,10))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Active iron in young leaves (μg·g-1FW)")+  
  geom_text(aes(label=c("b","a","a","a"),y=ironp+hei_sd$ironp+0.5,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p1  
