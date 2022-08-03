library(readxl)
library(agricolae)
library(ggplot2)
app = read_excel("gene.xlsx",sheet = 1) 

bartlett.test(ysl1p~indexp, data = app) 
aovapp <- aov(ysl1p~indexp,data = app) 
summary(aovapp)
lsdapp <- LSD.test(aovapp,'indexp',p.adj = 'none') 
lsdapp$groups

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(ysl1p~indexp,data = app,mean)
hei_sd<-aggregate(ysl1p~indexp,data = app,sd)
p1<-ggplot(hei_mean,aes(x=indexp,y=ysl1p))+
  geom_bar(stat="identity",fill = color2,
           colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=ysl1p-hei_sd$ysl1p,ymax=ysl1p+hei_sd$ysl1p),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,4))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Relative expression of AhYSL1")+  
  geom_text(aes(label=c("b","b","a","a"),y=ysl1p+hei_sd$ysl1p+0.1,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p1  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(fitp~indexp,data = app,mean)
hei_sd<-aggregate(fitp~indexp,data = app,sd)
p2<-ggplot(hei_mean,aes(x=indexp,y=fitp))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=fitp-hei_sd$fitp,ymax=fitp+hei_sd$fitp),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,2.5))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Relative expression of AhFIT")+  
  geom_text(aes(label=c("a","a","a","a"),y=fitp+hei_sd$fitp+0.2,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p2  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(nramp1p~indexp,data = app,mean)
hei_sd<-aggregate(nramp1p~indexp,data = app,sd)
p3<-ggplot(hei_mean,aes(x=indexp,y=nramp1p))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=nramp1p-hei_sd$nramp1p,ymax=nramp1p+hei_sd$nramp1p),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,2.5))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Relative expression of AhNRAMP1")+  
  geom_text(aes(label=c("a","a","a","a"),y=nramp1p+hei_sd$nramp1p+0.2,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p3  
