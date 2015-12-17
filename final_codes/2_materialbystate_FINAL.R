require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv')

materialbyuf<-select(miningdata,SUBS,AREA_HA,FASE,UF)
materialbyuf<-filter(materialbyuf,FASE == "CONCESSÃO DE LAVRA")
materialbyuf<-group_by (materialbyuf, SUBS, UF)
materialbyuf<-summarise (materialbyuf, AREA_HA = sum (AREA_HA))

materialbyuf <- mutate(materialbyuf, NEW_SUBS = ifelse(AREA_HA > 20000, as.character(SUBS), "Other"))
materialbyuf<-group_by (materialbyuf, UF)
materialbyuf <- mutate(materialbyuf, AREA_PER_UF = sum (AREA_HA))

graphmaterialbyuf<-ggplot(materialbyuf,aes(x=reorder(UF, AREA_PER_UF) ,y=AREA_HA, fill=NEW_SUBS, order=as.numeric(AREA_HA)))+
  geom_bar(stat='identity')+
  scale_y_continuous(limits = c(0, 850000),breaks=c(0,100000,300000, 500000,700000,850000))+
  coord_flip()


plot(graphmaterialbyuf)
ggsave (graphmaterialbyuf, file="graphmaterialbyufFINALV2.pdf", width=16, height=14)