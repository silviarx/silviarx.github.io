require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv')

ufbylandandtime<-select(miningdata,UF,AREA_HA,FASE,ANO)
ufbylandandtime<-filter(ufbylandandtime,FASE == "CONCESSÃO DE LAVRA")

ufbylandandtime<-group_by (ufbylandandtime, UF, ANO)
ufbylandandtime <- summarize(ufbylandandtime, AREA_HA = sum(AREA_HA))


full_data <- expand.grid(ANO = seq(1934, 2013, 1), UF = unique(ufbylandandtime$UF))
ufbylandandtime <- left_join(full_data, ufbylandandtime, by=c('UF', 'ANO'))
ufbylandandtime$AREA_HA[is.na(ufbylandandtime$AREA_HA)] <- 0

ufbylandandtime <- group_by(ufbylandandtime, UF)
#ufbylandandtime <- mutate(ufbylandandtime, AREA_CUM = cumsum(AREA_HA))
ufbylandandtime <- mutate(ufbylandandtime, PERC = cumsum(AREA_HA)/851487659.9)

#ufbylandandtime <- mutate(ufbylandandtime, NEW_UF = ifelse(AREA_CUM > 100000, as.character(UF), "Other"))


graphufbylandandtime<-ggplot(ufbylandandtime,aes(x=ANO,y=PERC, order=as.numeric(PERC)))+
  geom_area(aes(fill=UF, group=UF)) +
  scale_x_continuous(limits = c(1934, 2013),breaks=c(1930,1934,1940,1950,1960,1970,1980,1990,2000,2013))+
  scale_fill_discrete()

plot(graphufbylandandtime)
ggsave (graphufbylandandtime, file="graphufbylandandtimePERCFINAL.pdf", width=20, height=12)
