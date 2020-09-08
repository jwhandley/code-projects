library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W16_v0.2.dta')

df %>%
  mutate(tenure = recode(as.numeric(profile_house_tenure),`1`='Own outright',`2`='Own with mortgage',`3`='Other',`4`='Private rent',`5`='Social rent',`6`='Social rent',`7`='Other',`8`='Live with parents',`9`='Other')) %>%
  mutate(tenure = factor(tenure,levels=c('Own outright','Own with mortgage','Private rent','Social rent','Live with parents','Other'))) %>%
  filter(!is.na(tenure)) %>%
  mutate_at(vars(lr_scale,al_scale),~(.x-wtd.mean(.x,wt_new_,na.rm=T))/sqrt(wtd.var(.x,wt_new_,na.rm=T))) %>%
  group_by(tenure) %>%
  summarise(lr = wtd.mean(lr_scale,wt_new_,na.rm=T), al = wtd.mean(al_scale,wt_new_,na.rm=T)) %>%
  gather(key='axis',value='position',c(al,lr)) %>%
  ggplot(aes(x=tenure,y=position,fill=axis)) +
  geom_bar(stat='identity',position='dodge') +
  labs(x='House Tenure',y='Average z-score',fill='Axis',title='Average ideological positions by house tenure',subtitle='BES Panel Wage 16',caption='@jwhandley17') +
  scale_fill_discrete(labels=c('Authoritarian-Libertarian','Left-Right')) +
  ggsave('tenure_pos.png',width=8*1.2,height=5*1.2,type='cairo')

df %>%
  mutate(tenure = recode(as.numeric(profile_house_tenure),`1`='Own outright',`2`='Own with mortgage',`3`='Other',`4`='Private rent',`5`='Social rent',`6`='Social rent',`7`='Other',`8`='Live with parents',`9`='Other')) %>%
  mutate(tenure = factor(tenure,levels=c('Own outright','Own with mortgage','Private rent','Social rent','Live with parents','Other'))) %>%
  filter(!is.na(tenure)) %>%
  mutate_at(vars(lr_scale,al_scale),~(.x-wtd.mean(.x,wt_new_,na.rm=T))/sqrt(wtd.var(.x,wt_new_,na.rm=T))) -> data

lr.reg <- lm(lr_scale ~ tenure + age,data)
summary(lr.reg)

al.reg <- lm(al_scale ~ tenure + age,data)
summary(al.reg)

library(stargazer)

stargazer(lr.reg,al.reg,out='res_tenure.html',dep.var.labels = c('Left-right','Auth-lib'),covariate.labels = c('Own with mortgage','Private rent','Social rent','Live with parents','Other','Age in years'))
