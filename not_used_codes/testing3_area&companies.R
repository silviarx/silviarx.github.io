require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
#min<-read.csv('BRmining.csv')

min2<-select(min,ANO,AREA_HA,FASE,NOME,SUBS)
area_nome<-select(min, AREA_HA,NOME,ANO)
area_nome <- group_by(area_nome, NOME)
area_nome <- mutate(area_nome, TOTAL_AREA = sum(AREA_HA))
area_nome <- mutate(area_nome, NEW_NOME = ifelse(TOTAL_AREA > 2000000, as.character(NOME), "Other"))
area_nome <- group_by(area_nome, NEW_NOME, ANO)
area_nome <- summarize(area_nome, AREA_HA = sum(AREA_HA))

full_data <- expand.grid(ANO = seq(1934, 2013, 1), NEW_NOME = unique(area_nome$NEW_NOME))
area_nome <- left_join(full_data, area_nome, by=c('NEW_NOME', 'ANO'))
area_nome$AREA_HA[is.na(area_nome$AREA_HA)] <- 0


area_nome <- group_by(area_nome, NEW_NOME)
area_nome <- mutate(area_nome, PERC = cumsum(AREA_HA)/851487659.9)

#area_nome$NEW_NOME <- factor (area_nome$NEW_NOME, levels = area_nome$NEW_NOME(order(area_nome$PERC)))
#area_nome$NEW_NOME <- reorder (area_nome$NEW_NOME,area_nome$PERC)
#area_nome$NEW_NOME <- factor (area_nome$NEW_NOME,(levels=(area_nome$PERC)))

#area_nome2<-summarize(group_by (area_nome,ANO,NOME), AREA_HA = sum (AREA_HA, na.rm=TRUE))
#area_nome3<-mutate(area_nome2,PERC=AREA_HA/851487659.9)

#851487659.9 is the area of Brazil in HA

graph2<-ggplot(area_nome,aes(x=ANO,y=PERC, order=as.numeric(PERC)))+
  geom_area(aes(fill=NEW_NOME, group=NEW_NOME)) +
  scale_x_continuous(limits = c(1934, 2014),breaks=c(1934,1944,1954,1964,1974,1984,1994,2004,2014))+
  scale_fill_discrete()

#graph2<-ggplot(area_nome,aes(x=ANO,y=PERC,group=NEW_NOME, fill=NEW_NOME))+
  #geom_area()+
  #scale_y_continuous(labels = percent)


plot(graph2)
ggsave (graph2, file="areacompany.pdf", width=16, height=8)
