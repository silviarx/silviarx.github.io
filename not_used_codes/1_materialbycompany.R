require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv')

materialbycompany<-select(miningdata,SUBS,AREA_HA,FASE,NOME)
materialbycompany<-filter(materialbycompany,FASE == "CONCESSÃO DE LAVRA")
materialbycompany<-group_by (materialbycompany, SUBS, NOME)
materialbycompany<-summarise (materialbycompany, AREA_HA = sum (AREA_HA))

materialbycompany <- mutate(materialbycompany, NEW_NOME = ifelse(AREA_HA > 10000, as.character(NOME), "Other"))
materialbycompany <- mutate(materialbycompany, NEW_SUBS = ifelse(AREA_HA > 10000, as.character(SUBS), "Other"))


graphmaterialbycompany<-ggplot(materialbycompany,aes(x=NEW_NOME ,y=AREA_HA, fill=NEW_SUBS))+
  geom_bar(stat='identity')
  #scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))+
  
plot(graphmaterialbycompany)
#ggsave (graphcsm2, file="companyline.pdf", width=16, height=8)