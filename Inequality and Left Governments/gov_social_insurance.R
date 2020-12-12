library(tidyverse)
library(haven)
library(readxl)
library(zoo)

cpds <- read_dta('CPDS_1960-2018_Update_2020.dta') %>%
  mutate(country=ifelse(country=='USA','United States',country))
cwed <- read_csv('cwed-subset.csv')
sied <- read_excel('SIED 1930-2015 190320.xlsx')

sied %>%
  group_by(country) %>%
  mutate_at(vars(px2indst,pturatpa,uz4ind,uduratio,sz4ind,sduratio),~approx(year,.x,year)$y) %>%
  ungroup() %>%
  mutate(pension = scale(scale(px2indst) + scale(pturatpa))[,1]) %>%
  mutate(unempl = scale(scale(uz4ind) + scale(uduratio))[,1]) %>%
  mutate(sick = scale(scale(sz4ind) + scale(sduratio))[,1]) %>%
  mutate(gen = 1/3*(pension+unempl+sick)) %>%
  select(country,year,pension,unempl,sick,gen) -> sied2

sied2 %>%
  write_csv('sied_gen.csv')

left_join(cpds,sied2,by=c('country','year')) %>%
  group_by(country) %>%
  filter(sum(!is.na(gen))>3) %>%
  mutate_at(vars(gen,pension,unempl,sick),~approx(year,.x,year)$y) %>%
  filter(country %in% c('Australia','Canada','Finland','France','Germany','Ireland','Italy','Japan','Norway','Sweden','United Kingdom','United States')) %>%
  select(country,year,gen,gov_left1,gov_cent1) %>% 
  ggplot(aes(x=year,y=gen,color=gov_left1 + gov_cent1*as.numeric(country %in% c('United States','Canada')))) +
  geom_line() +
  facet_wrap(~country) + 
  scale_color_gradient(low='blue',high='red') +
  labs(color='Left cabinet share',
       x='Year',
       y='Generosity of social insurance',
       title='Social insurance and government composition') +
  theme(panel.spacing.x = unit(5, "mm")) +
  ggsave('socins_left_gov.png',width=8*1.2,height=5*1.2,type='cairo')

