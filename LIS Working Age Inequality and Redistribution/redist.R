library(tidyverse)
library(glue)
library(ggrepel)
library(scales)

df <- read_csv('lis_working_age_gini.csv')

decades <- seq(1970,2010,10)

df %>%
  mutate(decade = cut(year,seq(1970,2020,10),labels=c(glue("{decades}-{decades+10}")),include.lowest = T)) %>%
  group_by(country) %>%
  filter(decade %in% c('1980-1990','2010-2020')) %>%
  ungroup() %>%
  group_by(country,decade) %>%
  filter(ifelse(decade == '1980-1990',year==min(year),year==max(year))) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise_at(vars(gini_disp,gini_lab,redist),~.x - lag(.x)) %>%
  filter_at(vars(gini_disp,gini_lab,redist),~!is.na(.x)) -> reg_data

ggplot(reg_data,aes(x=gini_lab,y=gini_disp,color=redist,label=country)) +
  geom_point() +
  geom_text_repel()

df %>%
  filter(country != 'Poland', country != 'Mexico',country != 'Israel') %>%
  ggplot(aes(x=year,y=redist)) +
  geom_line() +
  geom_point() +
  facet_wrap(~country) +
  labs(x='year',
       y='Reduction in inequality due to taxes and transfers',
       title='Redistribution for the working age (25-59) population') +
  scale_y_continuous(labels=percent)
