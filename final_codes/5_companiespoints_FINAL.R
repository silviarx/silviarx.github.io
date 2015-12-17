require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv', stringsAsFactors = FALSE)

compsmult<-select(miningdata,ANO,AREA_HA,FASE,NOME)
compsmult<- na.omit(compsmult) 
compsmult<-group_by (compsmult,NOME,ANO,FASE)
compsmultsum<-summarize (compsmult, AREA_HA = sum(AREA_HA))
compsmultsum<-group_by (compsmultsum,NOME,FASE)
compsmultsum<-mutate(compsmultsum,AREA_HA_CUM=cumsum(AREA_HA))
compsmultsum<-group_by (compsmultsum,NOME)
compsmultsum<-mutate(compsmultsum,AREA_HA_TOTAL=sum(AREA_HA))

compsmultsum<-group_by (compsmultsum,NOME)
compsmultsum<-mutate(compsmultsum,AREA_MINING=sum(AREA_HA[FASE=="CONCESSÃO DE LAVRA"]))
compsmultsum<-mutate (compsmultsum, NOME_NEW = ifelse(AREA_HA_TOTAL>1500000, as.character(NOME), "Other"))
compsmultsum<-group_by (compsmultsum,NOME)
compsmultsum<-mutate (compsmultsum, PERC_MINING = (AREA_MINING/AREA_HA_TOTAL))

#SELECTION TO VERIFY AREAS EASIER
select<-select(compsmultsum, PERC_MINING, NOME, AREA_MINING, AREA_HA_TOTAL)

#CREATING SCATTER PLOT
graphpoints<-ggplot(compsmultsum,aes(x=AREA_MINING,y=AREA_HA_TOTAL, group=NOME, color=NOME_NEW))+
  geom_point()+
  scale_x_continuous(limits = c(0, 290000),breaks=c(0,90530,117621,123757,277006,287590))+
  scale_y_continuous(limits = c(0, 8000000),breaks=c(0,618621, 1318720, 1814166,2182947,2309540,2960539,4439544,7567469))+
  coord_flip()
   #7567469 is the biggest total area
  #287590.5 is the biggest area mining, hold by Companhia Brasileira de Alumínio
  #2nd biggest mining is Vale, 3rd is Mineracao Rio do Norte S A, 4th is Mineração Taboca S.a., 5th is Mineração Paragominas S A
plot(graphpoints)
ggsave (graphpoints, file="graphpointsAREAFINAL.pdf", width=12, height=12)
#ggsave (graphpoints, file="graphpointsPERC.pdf", width=12, height=12)
