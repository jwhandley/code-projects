library(tidyverse)
library(haven)
library(stargazer)

ge18 <- read_dta('SND1067 VALU 2018/en/valu2018-eng.dta')


# Social democrat vote by occupation
ge18 %>%
  filter(f1!=999,f1!=84,f1!=85,f1!=86) %>%
  mutate(sd = as.numeric(f1==5)) %>%
  mutate(occ = as_factor(na_if(f23,999))) %>%
  filter(!is.na(occ)) %>%
  group_by(occ) %>%
  summarise(sd = mean(sd,na.rm=T)) %>%
  ggplot(aes(x=reorder(occ,sd),y=sd)) +
  geom_bar(stat='identity') +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(x='Occupational group',
       y='Share voting for the Social Democrats',
       title='Occupational class voting in the 2018 Swedish general election',
       subtitle='VALU 2018 exit poll',
       caption='@jwhandley17') +
  ggsave('2018_occ_vote.png',width=8*1.5,height=5*1.5,type='cairo')

# Social democrat vote by union
ge18 %>%
  filter(f1!=999,f1!=84,f1!=85,f1!=86) %>%
  mutate(sd = as.numeric(f1==5)) %>%
  mutate(union = as_factor(na_if(f25,999))) %>%
  filter(!is.na(union)) %>%
  group_by(union) %>%
  summarise(sd = mean(sd,na.rm=T)) %>%
  ggplot(aes(x=union,y=sd)) +
  geom_bar(stat='identity') +
  labs(x='Union membership',
       y='Share voting for the Social Democrats',
       title='Union membership and voting in the 2018 Swedish general election',
       subtitle='VALU 2018 exit poll',
       caption='@jwhandley17') +
  ggsave('2018_union_vote.png',width=8,height=5,type='cairo')

# Social democrat vote by occupation and union
ge18 %>%
  filter(f1!=999,f1!=84,f1!=85,f1!=86) %>%
  mutate(sd = as.numeric(f1==5)) %>%
  mutate(occ = as_factor(na_if(f23,999)),union = as_factor(na_if(f25,999))) %>%
  filter(!is.na(occ),!is.na(union),occ!='Farmer') %>%
  group_by(occ,union) %>%
  summarise(sd = mean(sd,na.rm=T)) %>%
  spread(key=union,value=sd)

# Social democrat vs. Sweden democrat vote by occupation
ge18 %>%
  filter(f1!=999,f1!=84,f1!=85,f1!=86) %>%
  mutate(`Social Democrats` = as.numeric(f1==5),`Sweden Democrats` = as.numeric(f1==81)) %>%
  mutate(occ = as_factor(na_if(f23,999))) %>%
  filter(!is.na(occ)) %>%
  group_by(occ) %>%
  summarise(`Social Democrats` = mean(`Social Democrats`,na.rm=T), `Sweden Democrats` = mean(`Sweden Democrats`,na.rm=T)) %>%
  gather(key='Party',value='Vote share',c(`Social Democrats`,`Sweden Democrats`)) %>%
  ggplot(aes(x=reorder(occ,`Vote share`),y=`Vote share`,fill=Party)) +
  geom_bar(stat='identity',position='dodge') +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  labs(x='Occupational group',
       title='Occupational class voting in the 2018 Swedish general election',
       subtitle='VALU 2018 exit poll',
       caption='@jwhandley17') +
  ggsave('2018_occ_vote1.png',width=8*1.5,height=5*1.5,type='cairo')

# Cleanup for regressions
ge18 %>%
  filter(f1!=999,f1!=84,f1!=85,f1!=86) %>%
  mutate(sap = as.numeric(f1==5),sd = as.numeric(f1==81)) %>%
  mutate(occ = as_factor(na_if(f23,999)),union = as_factor(na_if(f25,999))) %>%
  mutate(union = factor(union,levels=c('No','Yes, a LO union','Yes, a TCO union','Yes, a SACO union'))) %>%
  filter(!is.na(occ),!is.na(union),occ!='Farmer') %>%
  select(sap,sd,occ,union) -> data

# Social democrat vote by occupation
reg.sap <- glm(sap ~ occ,data,family=binomial)
# Social democrat vote by occupation and union
reg.sap.union <- glm(sap ~ occ + union ,data,family=binomial)

# Sweden democrat vote by occupation
reg.sd <- glm(sd ~ occ,data,family=binomial)
# Sweden democrat vote by union
reg.sd.union <- glm(sd ~ occ + union,data,family=binomial)

# Regression table
stargazer(reg.sap,reg.sap.union,reg.sd,reg.sd.union,out='reg2018.html',dep.var.labels = c('Social Democrats','Sweden Democrats'),covariate.labels = c('White collar supervisor','Senior white collar','Blue collar','Blue collar supervisor','Self-employed: no employee','Self-employed: >=1 employee','Never had paid work','LO member','TCO member','SACO member'),dep.var.caption = 'Vote in 2018 general election')
