library(readxl)
library(agricolae)
library(ggplot2)
app = read_excel("gene.xlsx",sheet = 1) 

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(irt1p~indexp,data = app,mean)
hei_sd<-aggregate(irt1p~indexp,data = app,sd)
p1<-ggplot(hei_mean,aes(x=indexp,y=irt1p))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=irt1p-hei_sd$irt1p,ymax=irt1p+hei_sd$irt1p),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,1.5))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Relative expression of AhIRT1")+  
  geom_text(aes(label=c("a","b","b","b"),y=irt1p+hei_sd$irt1p+0.1,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p1  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(fro1p~indexp,data = app,mean)
hei_sd<-aggregate(fro1p~indexp,data = app,sd)
p2<-ggplot(hei_mean,aes(x=indexp,y=fro1p))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=fro1p-hei_sd$fro1p,ymax=fro1p+hei_sd$fro1p),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,3))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Relative expression of AhFRO1")+  
  geom_text(aes(label=c("a","a","a","a"),y=fro1p+hei_sd$fro1p+0.2,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p2  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(FROp~indexp,data = app,mean)
hei_sd<-aggregate(FROp~indexp,data = app,sd)
p3<-ggplot(hei_mean,aes(x=indexp,y=FROp))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=FROp-hei_sd$FROp,ymax=FROp+hei_sd$FROp),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,1.2),breaks = c(0,0.3,0.6,0.9,1.2))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Ferric reductase acticity (μM·g-1FW·2h)")+  
  geom_text(aes(label=c("a","d","b","c"),y=FROp+hei_sd$FROp+0.1,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p3  
