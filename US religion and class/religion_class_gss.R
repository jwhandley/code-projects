library(tidyverse)
library(scales)

df <- read_csv('GSS/class_relig_gss.csv')

# Clean up data
df %>%
  rename(educ = EDUC,id=ID_,realinc=REALINC,year=YEAR) %>%
  filter(educ<97,RACE>0,RELIG<98,DENOM<98,FUND>0,realinc>0,realinc<999998) %>%
  mutate(race = recode(RACE,`1`='White',`2`='Black',`3`='Other',.default=NA_character_), race = factor(race,levels=c('White','Black','Other'))) %>%
  mutate(relig = recode(RELIG,`1`='Protestant',`2`='Catholic',`3`='Jewish',`4`= 'None',.default='Other'), relig = factor(relig,levels=c('Protestant','Catholic','Jewish','None','Other'))) %>%
  mutate(fund = recode(FUND,`1`='Fundamentalist',`2`='Moderate',`3`='Liberal',.default="Don't know"),fund = factor(fund,levels=c('Liberal','Moderate','Fundamentalist',"Don't know"))) %>%
  select(id,year,race,educ,realinc,relig,fund) %>%
  mutate(prot = as.numeric(relig=='Protestant'),fundamental = as.numeric(fund=='Fundamentalist'),fund_prot = prot*fundamental) -> data

# Plot share identifying as fundamentalist protestants by year
data %>%
  group_by(year) %>%
  summarise('Share' = mean(fund_prot)) %>%
  ggplot(aes(x=year,y=`Share`)) + 
  geom_point() +
  geom_smooth() +
  scale_y_continuous(labels=percent) +
  labs(x='Year',
       y='Share',
       title='Share identifying as fundamentalist protestants in the US',
       subtitle='General Social Survey',
       caption='John Handley')

data %>%
  filter(relig=='Protestant',fund!="Don't know",race=='White') %>%
  gather(stat,value,c(realinc,educ)) %>%
  ggplot(aes(x=year,y=value,color=fund)) +
  geom_smooth() +
  facet_wrap(~stat,scales='free_y') +
  labs(x='Year',
       y='Average years of education / Average family income')

data %>%
  filter(relig=='Protestant',fund!="Don't know",race=='White') %>%
  ggplot(aes(x=year,y=realinc,color=fund)) +
  geom_smooth() -> inc

data %>%
  filter(relig=='Protestant',fund!="Don't know",race=='White') %>%
  ggplot(aes(x=year,y=educ,color=fund)) +
  geom_smooth() -> educ
  

data %>%
  mutate(relig_fund = ifelse(relig=='Jewish','Other',ifelse(prot==1,as.character(fund),as.character(relig)))) %>%
  group_by(year) %>%
  mutate(pop = n()) %>%
  ungroup() %>%
  group_by(year,relig_fund) %>%
  summarise(share = n()/mean(pop),prot=mean(prot)) %>%
  mutate(prot = recode(prot,`0`='Not protestant',`1`='Protestant')) %>%
  filter(relig_fund!="Don't know") %>%
  ggplot(aes(x=year,y=share,color=relig_fund)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~prot)


data %>%
  mutate(relig_fund = ifelse(relig=='Jewish','Other',ifelse(prot==1,as.character(fund),as.character(relig)))) %>%
  group_by(year,relig_fund) %>%
  filter(relig_fund!="Don't know") %>%
  ggplot(aes(x=year,y=educ,color=relig_fund)) +
  geom_smooth() +
  facet_wrap(~prot,scales = 'free_y')
