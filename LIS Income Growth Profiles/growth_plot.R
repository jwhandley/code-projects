library(tidyverse)
library(glue)

df <- read_csv('data.csv')

decades <- seq(1960,2010,10)

df %>%
  mutate(decade = cut(year,seq(1960,2020,10),include.lowest = T,right=F,glue("{decades}s"))) %>%
  group_by(country,quantile,decade) %>%
  summarise(growth = mean(growth,na.rm=T)) %>%
  filter(!is.na(growth)) %>%
  ggplot(aes(x=quantile,y=growth,color=country)) +
  geom_line() +
  facet_wrap(~decade) +
  ggsave('test.png',width=8,height=5,type='cairo')



