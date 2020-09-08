library(tidyverse)
library(readxl)

df <- read_excel('SIED/SIED 1930-2015 190320.xlsx')
crosswalk <- read_csv('SIED/country_lookup.csv')

data <- inner_join(df,crosswalk)

data %>%
  mutate(strat = pratmins-pratfuls) %>%
  mutate_at(vars(pcontper,ucontper,uwaiting,scontper,swaiting),~-.x) %>%
  mutate_at(vars(pnermisi,pnerswsi,pcontper,uzrr26si,ucontper,uwaiting,uduratio,szrr26si,scontper,swaiting,sduratio,strat),~scale(.x)[,1]+2) %>%
  mutate(pension = (2*pnermisi + 2*pnerswsi + pcontper + strat)*(pturatpa)) %>%
  mutate(ucov = ifelse(ucovratl==0 & umeantst==1,0.5*mean(ucovratl,na.rm=T),ucovratl)) %>%
  mutate(unemp = (2*uzrr26si + ucontper + uwaiting + uduratio)*(ucov)) %>%
  mutate(scov = ifelse(scovratl==0 & smeantst==1,0.5*mean(scovratl,na.rm=T),scovratl)) %>%
  mutate(sickness = (2*szrr26si + scontper + swaiting + sduratio)*(scov)) %>%
  mutate(total = pension + unemp + sickness) %>%
  select(country_name,year,pension,unemp,sickness,total) -> decom
    
decom %>%
  group_by(country_name) %>%
  filter(length(total)>17) %>%
  ungroup() %>%
  group_by(country_name,year) %>%
  gather(key='dimension',value='index',c(pension,unemp,sickness)) %>%
  ggplot(aes(x=year,y=index,color=dimension)) +
  geom_point() +
  geom_line() +
  facet_wrap(~country_name) +
  labs(y='Decommodification index',x='Year',color='Programme',title='Long-term Trends in Decommodification in 18 Countries',subtitle='Stockhold University Social Policy Indicators Database',caption='@jwhandley17') +
  scale_color_discrete(labels=c('Pension','Sick leave','Unemployment')) +
  ggsave('decom_18_countries.png',width=16,height=10,type='cairo')

decom %>%
  group_by(country_name) %>%
  filter(length(total)>17) %>%
  ungroup() %>%
  select(country_name,year,total) %>%
  group_by(year) %>%
  spread(key=country_name,value=total) -> table

write.csv(table,'decom_table.csv')  
write.csv(decom,'decom_wide.csv')
write.csv(
  decom %>%
    group_by(country_name) %>%
    filter(length(total)>17) %>%
    ungroup(),
  'decom_wide_18.csv'
)
