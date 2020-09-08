library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W16_v0.2.dta')

df %>%
  mutate(partyId = recode(as.numeric(partyId),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`10`='None',`9999`="Don't know",.default='Other'),occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7))) %>%
  mutate(partyId = factor(partyId, levels=c('Conservative','Labour','Liberal Democrat','Other','None',"Don't know"))) %>%
  filter(!is.na(occ_class)) %>%
  group_by(occ_class) %>%
  mutate(pop=sum(wt_new_)) %>%
  ungroup() %>%
  group_by(occ_class,partyId) %>%
  summarise(n=sum(wt_new_)/mean(pop)) %>%
  ggplot(aes(x=occ_class,y=n,fill=partyId)) + geom_bar(stat='identity',position='dodge') + scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','grey','darkgrey','black')) + labs(x='NS-SEC Analytic Class',y='Share identifying with party',fill='Party ID',title='Party identification by occupational class after GE2019',subtitle='BES Panel Wage 19',caption='@jwhandley17') + ggsave('class_partyid.png',width=8,height=5,type='cairo')

