library(tidyverse)
library(readr)
library(Hmisc)

df <- read_csv('MPDataset_MPDS2019b.csv')

df %>%
  mutate(year = round(date/100)) %>%
  filter(parfam %in% c(20,30)) %>%
  group_by(countryname) %>%
  mutate(mean_vote=mean(pervote)) %>%
  ungroup() %>%
  filter(mean_vote>10) %>%
  filter(year>=1945,!is.na(pervote),!is.na(rile)) %>%
  gather(key='measure',value='value',c(pervote,rile)) -> data

ggplot(data,aes(x=year,y=value)) + geom_smooth(color='red',span=0.5) + facet_wrap(~measure,ncol=2,scales='free_y',labeller=labeller(measure=c(pervote='Vote share',rile='Left-right position'))) + theme(axis.title.y=element_blank()) + labs(title='Vote share and ideology of left parties',subtitle='Countries with average left vote share > 10%',caption='Source: Manifesto Project Database, @jwhandley17') + ggsave('left_vote_ideology.png',width=8,height=5,type='cairo')

ggplot(data,aes(x=year,y=value,color=measure)) + geom_smooth()
  
  