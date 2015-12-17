require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
compsmult<-read.csv('regime_auto_conc.csv', stringsAsFactors = FALSE)

compsmult<-select(compsmult,ANO,AREA_HA,FASE,NOME)
compsmult<- na.omit(compsmult) 
compsmult<-group_by (compsmult,NOME,ANO,FASE)
compsmultsum<-summarize (compsmult, AREA_HA = sum(AREA_HA))
compsmultsum<-group_by (compsmultsum,NOME,FASE)

#Tried to create 'AREA_HA' data for all companies in all years in all regimes, even if the value is 0, not successful
#full_data <- expand.grid(ANO = seq(1934, 2013, 1), NOME = unique(compsmultsum$NOME))
#compsmultsum <- left_join(full_data, compsmultsum, by=c('NOME', 'ANO'))
#compsmultsum$AREA_HA[is.na(compsmultsum$AREA_HA)] <- 0
compsmultsum<-mutate(compsmultsum,AREA_HA_CUM=cumsum(AREA_HA))

#CREATING GRAPH TO SEE TOP ONES IN MINING OPERATION
mining<- filter(compsmultsum, FASE == "CONCESSÃO DE LAVRA")
miningsum<- group_by (mining,NOME)
miningsum <-mutate (miningsum, TOTAL_AREA_MIN = sum (AREA_HA)) 
miningsum<-mutate (miningsum, NOME_MIN = ifelse(TOTAL_AREA_MIN>30000, as.character(NOME), "Other"))
graphmin<-ggplot(miningsum,aes(x=ANO,y=AREA_HA_CUM, group=NOME, colour=NOME_MIN))+
  geom_line()+
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))
#plot(graphmin)
#ggsave (graphmin, file="linescompmin.pdf", width=16, height=8)

#CREATING GRAPH TO SEE TOP ONES IN MINING REQUEST
reqmining<- filter(compsmultsum, FASE == "REQUERIMENTO DE LAVRA")
reqminingsum<- group_by (reqmining,NOME)
reqminingsum <-mutate (reqminingsum, TOTAL_AREA_REQMIN = sum (AREA_HA)) 
reqminingsum<-mutate (reqminingsum, NOME_MIN = ifelse(TOTAL_AREA_REQMIN>30000, as.character(NOME), "Other"))
graphreqmin<-ggplot(reqminingsum,aes(x=ANO,y=AREA_HA_CUM, group=NOME, colour=NOME_MIN))+
  geom_line()+
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))
#plot(graphreqmin)
#ggsave (graphreqmin, file="linescompreqmin.pdf", width=16, height=8)

#CREATING GRAPH TO SEE TOP ONES IN RESEARCH REQUEST
reqresearch<- filter(compsmultsum, FASE == "REQUERIMENTO DE PESQUISA")
reqresearchsum<- group_by (reqresearch,NOME)
reqresearchsum <-mutate (reqresearchsum, TOTAL_AREA_REQRES = sum (AREA_HA)) 
reqresearchsum<-mutate (reqresearchsum, NOME_MIN = ifelse(TOTAL_AREA_REQRES>500000, as.character(NOME), "Other"))
graphreqres<-ggplot(reqresearchsum,aes(x=ANO,y=AREA_HA_CUM, group=NOME, colour=NOME_MIN))+
  geom_line()+
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))
#plot(graphreqres)
#ggsave (graphreqres, file="linescompreqres.pdf", width=16, height=8)

#CREATING GRAPH TO SEE TOP ONES IN RESEARCH 
research<- filter(compsmultsum, FASE == "AUTORIZAÇÃO DE PESQUISA")
researchsum<- group_by (research,NOME)
researchsum <-mutate (researchsum, TOTAL_AREA_RES = sum (AREA_HA)) 
researchsum<-mutate (researchsum, NOME_MIN = ifelse(TOTAL_AREA_RES>500000, as.character(NOME), "Other"))
graphres<-ggplot(researchsum,aes(x=ANO,y=AREA_HA_CUM, group=NOME, colour=NOME_MIN))+
  geom_line()+
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))
#plot(graphres)
#ggsave (graphres, file="linescompres.pdf", width=16, height=8)

#CREATING FACET_WRAP WITH ALL PHASES
graphall<-ggplot(compsmultsum,aes(x=ANO,y=AREA_HA_CUM, group=NOME))+
  geom_line()+
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))+
  scale_y_continuous(limits = c(0, 7500000),breaks=c(0,500000,1000000,2000000,3000000,4000000,5000000,6000000,7000000,7500000))+
  facet_wrap(~FASE)
#plot(graphall)
#ggsave (graphall, file="graphall.pdf", width=12, height=12)

