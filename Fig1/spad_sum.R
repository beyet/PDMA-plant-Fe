library(readxl)
library(agricolae)
library(ggplot2)
color2<-c("#91D1C2FF","#F39B7FFF","#E64B35FF","#8491B4FF","#91D1C2FF","#F39B7FFF","#E64B35FF","#8491B4FF","#91D1C2FF","#F39B7FFF","#E64B35FF","#8491B4FF","#91D1C2FF","#F39B7FFF","#E64B35FF","#8491B4FF")
color<-c("#FFFFFF","#808080","#808080","#808080","#FFFFFF","#808080","#808080","#808080","#FFFFFF","#808080","#808080","#808080","#FFFFFF","#808080","#808080","#808080")
aqq = read_excel("SPAD_sum.xlsx",sheet = 1) 
aqqp <- aqq[1:120,1:3]
q <- ggplot(aqqp,aes(x = treatP, y = SPylP))+
  stat_boxplot(geom = "errorbar", width = 0.4, size = 0.3)+
  geom_boxplot(fill = color,size = 0.3, outlier.size = 0.7, outlier.alpha = 0.8)+
  stat_summary(fun = mean,geom = "point",
               shape = 23,size = 3,fill = "white")+
  #geom_dotplot(binaxis = "y",binwidth = 0.5, stackdir = "center",fill = NA,alpha = 0.3)+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),axis.line = element_blank())+
  guides(fill=guide_legend(title = NULL))+
  theme(axis.title.y = element_text(vjust = 0.5,hjust = 0.5,size=rel(1),family = "Times"))+ 
  ylab("SPAD value of young leaves")+
  theme(axis.text.x = element_text(angle=40,hjust = 1,vjust = 1,
                                   size=rel(0.8),colour="black",family = "Times"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size=rel(0.8),colour="black",family = "Times"))+
  scale_y_continuous(expand = c(0,0),limits=c(10,60))+
  scale_x_discrete(limits = c("CK","PDMA","PDMA-Fe","EDTAr"),
                   labels = c("CK","PDMA","PDMA-Fe","EDTA-Fe"))+
  facet_grid(. ~ stageP, scales = "free_x")+
  theme(strip.background = element_rect(fill = "#DCDCDC"),
        strip.text = element_text(size = 10,colour="black",family = "Times"))
q

