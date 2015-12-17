require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv')

materialbylandandtime<-select(miningdata,SUBS,AREA_HA,FASE,ANO)
materialbylandandtime<-filter(materialbylandandtime,FASE == "CONCESSÃO DE LAVRA")

materialbylandandtime <- group_by(materialbylandandtime, SUBS)
materialbylandandtime <- mutate(materialbylandandtime, TOTAL_AREA = sum(AREA_HA))
materialbylandandtime <- mutate(materialbylandandtime, NEW_SUBS = ifelse(TOTAL_AREA > 50000, as.character(SUBS), "Other"))
materialbylandandtime <- group_by(materialbylandandtime, NEW_SUBS, ANO)


full_data <- expand.grid(ANO = seq(1934, 2013, 1), NEW_SUBS = unique(materialbylandandtime$NEW_SUBS))
materialbylandandtime <- left_join(full_data, materialbylandandtime, by=c('NEW_SUBS', 'ANO'))
materialbylandandtime$AREA_HA[is.na(materialbylandandtime$AREA_HA)] <- 0

materialbylandandtime <- group_by(materialbylandandtime, NEW_SUBS)
materialbylandandtime <- mutate(materialbylandandtime, AREA_CUM = cumsum(AREA_HA))
materialbylandandtime <- mutate(materialbylandandtime, PERC = cumsum(AREA_HA)/851487659.9)

#materialbylandandtime <- mutate(materialbylandandtime, NEW_SUBS = ifelse(AREA_CUM > 100000, as.character(SUBS), "Other"))


graphmatbylandandtime<-ggplot(materialbylandandtime,aes(x=ANO,y=PERC, order=as.numeric(PERC)))+
  geom_area(aes(fill=NEW_SUBS, group=NEW_SUBS)) +
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))+
  scale_fill_discrete()

plot(graphmatbylandandtime)
ggsave (graphmatbylandandtime, file="graphmatbylandandtimePERC.pdf", width=16, height=8)
