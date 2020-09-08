library(tidyverse)

df <- read_csv('nama_10r_3gdp_1_Data.csv')

df %>%
  filter(gdppc!=':',!is.na(gdppc)) %>%
  mutate(gdppc=log(as.numeric(gdppc))) %>%
  group_by(country,nuts3) %>%
  summarise(gdppc=last(gdppc)) %>%
  group_by(country) %>%
  mutate(sd=sd(gdppc)) %>%
  ungroup() -> data

ggplot(data,aes(x=reorder(country,gdppc,FUN=median),y=gdppc,fill=sd)) + geom_boxplot() + xlab('Country') + ylab('log GDP per head at purchasing power parity') + ggtitle('Regional dispersion of GDP per head in the EU') + labs(subtitle='by NUTS 3 region',fill='Standard deviation') + scale_fill_gradient2(low='white',high='red') + ggsave('nuts3_gdppc.png',width=10,height=100/16,type='cairo')

ggplot(filter(data,country %in% c('FR','UK','DE')),aes(x=country,y=gdppc,fill=country)) + geom_violin()
