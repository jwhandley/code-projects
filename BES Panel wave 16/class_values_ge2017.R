library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W16_v0.2.dta')


df %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)),turnout = na_if(profile_turnout_2017,9999)) %>%
  mutate_at(vars(lr_scale,al_scale),~(.x-wtd.mean(.x,wt_new_,na.rm=T))/sqrt(wtd.var(.x,wt_new_,na.rm=T))) %>%
  filter(!is.na(occ_class),!is.na(turnout)) %>%
  group_by(occ_class,turnout) %>%
  summarise(`Left-Right` = wtd.mean(lr_scale,wt_new_,na.rm=T),`Authoritarian-Libertarian` = wtd.mean(al_scale,wt_new_,na.rm=T),lr_se = (wtd.var(lr_scale,wt_new_,na.rm=T)),al_se = (wtd.var(al_scale,wt_new_,na.rm=T))) %>%
  gather(key='axis',value='position',c(`Left-Right`,`Authoritarian-Libertarian`)) -> data

ggplot(data,aes(x=occ_class,y=position,fill=as_factor(turnout))) + geom_bar(stat='identity',position = 'dodge') + facet_wrap(~axis) + xlab('NS-SEC Analytic Class') + ylab('Mean z-score') + labs(title='Ideological positions by social class and GE2017 turnout',subtitle='BES Internet Panel Wave 16',caption='@jwhandley17',fill='GE2017 Turnout') + ggsave('turnout_class_ideology.png',width=8,height=5,type='cairo')

df %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)),turnout = na_if(profile_turnout_2017,9999)) %>%
  filter(!is.na(occ_class),!is.na(turnout)) %>%
  group_by(occ_class) %>%
  summarise(turnout = wtd.mean(turnout,wt_new_,na.rm=T)) %>%
  ggplot(aes(x=occ_class,y=turnout)) + geom_bar(stat='identity') + xlab('NS-SEC Analytic Class') + ylab('Turnout') + labs(title='GE2017 turnout by social class',subtitle='BES Internet Panel Wave 16',caption='@jwhandley17') + ggsave('turnout_class.png',width=8,height=5,type='cairo')

df %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)),lab = as.numeric(profile_past_vote_2017==2)) %>%
  mutate_at(vars(lr_scale,al_scale),~as.numeric(.x)) %>%
  select(occ_class,lr_scale,al_scale,lab) -> data1

res <- glm('lab ~ lr_scale + al_scale',data=data1,family=binomial)
summary(res)
