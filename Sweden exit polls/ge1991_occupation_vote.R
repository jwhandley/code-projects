library(tidyverse)
library(haven)
library(stargazer)

ge91 <- read_dta('SND0278 VALU 1991/en/0278e.dta')

ge91 %>%
  filter(v8!=99,v8!=10,v8!=11,v8!=12) %>%
  mutate(sap = as.numeric(v8==4)) %>%
  mutate(occ = recode(as.numeric(v15),`1`='White-collar',`2`='White-collar supervisory',`3`='Executive',`4`='Blue collar',`5`='Blue collar supervisor',`6`='Blue collar',`7`='Farmer',`8`='Farmer',`9`='Self employed: no employee',`10`='Self employed: >=1 employee',`11`='Self employed: >=1 employee',`12`='Never been employed',`99`=NA_character_)) %>%
  filter(occ!='Farmer') %>%
  mutate(occ = factor(occ,levels=c('Executive','Self employed: >=1 employee','White-collar supervisory','White-collar','Self employed: no employee','Blue collar supervisor','Blue collar'))) %>%
  filter(!is.na(occ)) %>%
  group_by(occ) %>%
  summarise(sap = mean(sap,na.rm=T)) %>%
  ggplot(aes(x=reorder(occ,sap),y=sap)) +
  geom_bar(stat='identity') +
  labs(x='Occupation',
       y='Share voting for Social Democrats',
       title='Occupational voting in the 1991 Swedish general election',
       subtitle='VALU 1991 exit poll',
       caption='@jwhandley17') +
  ggsave('1991_occ_vote.png',width=8*1.5,height=5*1.5,type='cairo')

ge91 %>%
  filter(v8!=99,v8!=10,v8!=11,v8!=12) %>%
  mutate(sap = as.numeric(v8==4)) %>%
  mutate(union = as_factor(na_if(v17,9))) %>%
  filter(!is.na(union)) %>%
  group_by(union) %>%
  summarise(sap = mean(sap,na.rm=T)) %>%
  ggplot(aes(x=union,y=sap)) +
  geom_bar(stat='identity') +
  labs(x='Union membership',
       y='Share voting for the Social Democrats',
       title='Voting by union membership in the 1991 Swedish general election',
       subtitle='VALU 2018 exit poll',
       caption='@jwhandley17') +
  ggsave('1991_union_vote.png',width=8,height=5,type='cairo')
