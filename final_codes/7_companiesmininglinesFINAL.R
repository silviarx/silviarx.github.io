require(ggplot2)
require(scales)
require(lubridate)
require(tidyr)
require(dplyr)
miningdata<-read.csv('regime_auto_conc.csv', stringsAsFactors = FALSE)

min2<-select(miningdata,ANO,AREA_HA,FASE,NOME,SUBS)
comapanieslines<-select(min2, AREA_HA,NOME,ANO, FASE)
comapanieslines <- filter(complinessum, FASE == "CONCESSÃO DE LAVRA")
comapanieslines <- group_by(comapanieslines, NOME)
comapanieslines <- mutate(comapanieslines, TOTAL_AREA = sum(AREA_HA))
comapanieslines <- group_by(comapanieslines, NOME, ANO, FASE)
comapanieslines <- summarize(comapanieslines, AREA_HA = sum(AREA_HA))

full_data <- expand.grid(ANO = seq(1934, 2013, 1), NOME = unique(comapanieslines$NOME))
comapanieslines <- left_join(full_data, comapanieslines, by=c('NOME', 'ANO'))
comapanieslines$AREA_HA[is.na(comapanieslines$AREA_HA)] <- 0

comapanieslines <- group_by(comapanieslines, NOME)
comapanieslines <- mutate(comapanieslines, PERC = cumsum(AREA_HA))

graphlines<-ggplot(comapanieslines,aes(x=ANO,y=PERC, group=NOME))+
  geom_line()+
  scale_y_continuous(limits = c(0, 290000),breaks=c(0,90530,117621,123757,277006,287590))+
  scale_x_continuous(limits = c(1934, 2013),breaks=c(1930,1934,1940,1950,1960,1970,1980,1990,2000,2013))
plot(graphlines)

#ggsave (graphlines, file="comapanieslines.pdf", width=15, height=8)
