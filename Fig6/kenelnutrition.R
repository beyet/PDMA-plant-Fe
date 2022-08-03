library(readxl)
library(agricolae)
library(ggplot2)
app = read_excel("kenelnutrition.xlsx",sheet = 1) 

color2<-c("#FFFFFF","#808080","#808080","#808080")
app1<-app[1:12,1:2]
hei_mean<-aggregate(keiron~index,data = app1,mean)
hei_sd<-aggregate(keiron~index,data = app1,sd)
p1<-ggplot(hei_mean,aes(x=index,y=keiron))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=keiron-hei_sd$keiron,ymax=keiron+hei_sd$keiron),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,8))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Iron in kernel (mg·plant-1)")+  
  geom_text(aes(label=c("b","ab","a","a"),y=keiron+hei_sd$keiron+0.2,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p1  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(kezinc ~index,data = app,mean)
hei_sd<-aggregate(kezinc ~index,data = app,sd)
p2<-ggplot(hei_mean,aes(x=index,y=kezinc ))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=kezinc -hei_sd$kezinc ,ymax=kezinc +hei_sd$kezinc ),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,15))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Zinc in kernel (mg·plant-1)")+  
  geom_text(aes(label=c("c","bc","ab","a"),y=kezinc +hei_sd$kezinc +1,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p2  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(kemn  ~index,data = app,mean)
hei_sd<-aggregate(kemn  ~index,data = app,sd)
p3<-ggplot(hei_mean,aes(x=index,y=kemn ))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=kemn -hei_sd$kemn ,ymax=kemn +hei_sd$kemn ),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,6))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Manganese in kernel (mg·plant-1)")+  
  geom_text(aes(label=c("c","b","ab","a"),y=kemn +hei_sd$kemn +0.5,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p3  

color2<-c("#FFFFFF","#808080","#808080","#808080")
hei_mean<-aggregate(kecu  ~index,data = app,mean)
hei_sd<-aggregate(kecu  ~index,data = app,sd)
p4<-ggplot(hei_mean,aes(x=index,y=kecu))+
  geom_bar(stat="identity",fill = color2,colour="black",width=0.65)+ 
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+ 
  geom_errorbar(aes(ymin=kecu-hei_sd$kecu,ymax=kecu+hei_sd$kecu),
                width = 0.25,
                position = position_dodge(0.6))+ 
  theme(legend.position = "top")+ 
  guides(fill=guide_legend(title = NULL))+ 
  scale_y_continuous(expand = c(0,0),limits=c(0,4))+ 
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("Copper in kernel (mg·plant-1)")+  
  geom_text(aes(label=c("c","bc","a","ab"),y=kecu+hei_sd$kecu+0.2,family = "Times"),vjust=0)+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))
p4  
