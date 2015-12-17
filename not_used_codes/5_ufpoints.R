require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv', stringsAsFactors = FALSE)

ufmult<-select(miningdata,ANO,AREA_HA,FASE,UF)
ufmult<- na.omit(ufmult) 
ufmult<-group_by (ufmult,UF,ANO,FASE)
ufmultsum<-summarize (ufmult, AREA_HA = sum(AREA_HA))
ufmultsum<-group_by (ufmultsum,UF,FASE)
ufmultsum<-mutate(ufmultsum,AREA_HA_CUM=cumsum(AREA_HA))
ufmultsum<-group_by (ufmultsum,UF)
ufmultsum<-mutate(ufmultsum,AREA_HA_TOTAL=sum(AREA_HA))

ufmultsum<-group_by (ufmultsum,UF)
ufmultsum<-mutate(ufmultsum,AREA_MINING=sum(AREA_HA[FASE=="CONCESSÃO DE LAVRA"]))
ufmultsum<-group_by (ufmultsum,UF)
ufmultsum<-mutate (ufmultsum, PERC_MINING = (AREA_MINING/AREA_HA_TOTAL))

graphpointsuf<-ggplot(ufmultsum,aes(x=AREA_MINING,y=AREA_HA_TOTAL, group=UF, color=UF))+
  geom_point()+
  #scale_x_continuous(limits = c(0, 290000),breaks=c(0,90530,117621,123757,277006,287590))+
  #scale_y_continuous(limits = c(0, 8000000),breaks=c(0,618621, 1318720, 1814166,2182947,2309540,2960539,4439544,7567469))+
  coord_flip()
 
plot(graphpointsuf)
ggsave (graphpointsuf, file="graphpointsUF.pdf", width=12, height=12)

