library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(classocc = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)), wc = as.numeric(subjClass==2),mc = as.numeric(subjClass==1),pwc = as.numeric(subjClassSqueeze==2),pmc = as.numeric(subjClassSqueeze==1)) %>%
  mutate_at(vars(mc,wc,pmc,pwc),~replace_na(.x,0)) %>%
  filter(!is.na(classocc)) %>%
  group_by(classocc) %>%
  summarise(`Working class unprompted` = wtd.mean(wc,wt,na.rm=T),`Working class prompted` = wtd.mean(pwc,wt,na.rm=T),`Middle class unprompted` = wtd.mean(mc,wt,na.rm=T),`Middle class prompted` = wtd.mean(pmc,wt,na.rm=T)) %>%
  gather(key='Class ID',value='Share',c(`Working class unprompted`,`Working class prompted`,`Middle class unprompted`,`Middle class prompted`)) -> data

ggplot(data,aes(x=classocc,y=Share,fill=`Class ID`)) + geom_bar(stat='identity',position='dodge') + scale_fill_manual(values=c('#a6ddff','#0087DC','#E67673','#DC241f')) + xlab('NS-SEC Analytic Class') + labs(title='Class identity by occupational class',subtitle='BES Internet Panel Wave 19',caption='@jwhandley17') + ggsave('classid.png',width=8,height=5,type='cairo')


df %>%
  mutate(classocc = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)), wc = as.numeric(subjClass==2),mc = as.numeric(subjClass==1),pwc = as.numeric(subjClassSqueeze==2),pmc = as.numeric(subjClassSqueeze==1)) %>%
  summarise(`Working class unprompted` = wtd.mean(wc,wt,na.rm=T),`Working class prompted` = wtd.mean(pwc,wt,na.rm=T),`Middle class unprompted` = wtd.mean(mc,wt,na.rm=T),`Middle class prompted` = wtd.mean(pmc,wt,na.rm=T))

df %>%
  mutate(wc = as.numeric(subjClass==2),mc = as.numeric(subjClass==1),pwc = as.numeric(subjClassSqueeze==2),pmc = as.numeric(subjClassSqueeze==1)) %>%
  mutate_at(vars(mc,wc,pmc,pwc),~replace_na(.x,0)) %>%
  mutate(classid = 2*wc + pwc - 2*mc - pmc) %>%
  mutate(classid = factor(recode(classid,`2`='Working class unprompted',`1`='Working class prompted',`0`=NA_character_,`-1`='Middle class prompted',`-2`='Middle class unprompted'),levels=c('Working class unprompted','Working class prompted','Middle class prompted','Middle class unprompted'))) %>%
  filter(!is.na(classid)) %>%
  mutate_at(vars(likeStarmer,likeCorbyn,likeBoris), ~na_if(.x,9999)) %>%
  group_by(classid) %>%
  summarise_at(vars(likeStarmer,likeCorbyn,likeBoris),~wtd.mean(.x,wt,na.rm=T)) %>%
  gather(key='person',value='like',c(likeStarmer,likeCorbyn,likeBoris)) %>%
  ggplot(aes(x=classid,y=like,fill=person)) + geom_bar(stat='identity',position='dodge') + xlab('Subjective class identification') + ylab('Likeability (0-10 scale)') + labs(title='Popularity of Starmer, Johnson and Corbyn by subjective social class',subtitle='BES Internet Panel Wave 19',caption='@jwhandley17',fill='Person') + scale_fill_manual(values=c('#0087DC','#FF355E','#DC241f'),labels=c('Johnson','Corbyn','Starmer')) + ggsave('popularity_classid.png',width=8,height=5,type='cairo')


df %>%
  mutate(wc = as.numeric(subjClass==2),mc = as.numeric(subjClass==1),pwc = as.numeric(subjClassSqueeze==2),pmc = as.numeric(subjClassSqueeze==1)) %>%
  mutate_at(vars(mc,wc,pmc,pwc),~replace_na(.x,0)) %>%
  mutate(classid = 2*wc + pwc - 2*mc - pmc) %>%
  mutate(classid = factor(recode(classid,`2`='Working class unprompted',`1`='Working class prompted',`0`=NA_character_,`-1`='Middle class prompted',`-2`='Middle class unprompted'),levels=c('Working class unprompted','Working class prompted','Middle class prompted','Middle class unprompted'))) %>%
  filter(!is.na(classid)) %>%
  mutate_at(vars(likeLab,likeCon,likeLD,likeBrexitParty), ~na_if(.x,9999)) %>%
  group_by(classid) %>%
  summarise_at(vars(likeLab,likeCon,likeLD,likeBrexitParty),~wtd.mean(.x,wt,na.rm=T)) %>%
  gather(key='Party',value='like',c(likeLab,likeCon,likeLD,likeBrexitParty)) %>%
  ggplot(aes(x=classid,y=like,fill=Party)) + geom_bar(stat='identity',position='dodge') + xlab('Subjective class identification') + ylab('Likeability (0-10 scale)') + labs(title='Party popularity by subjective social class',subtitle='BES Internet Panel Wave 19',caption='@jwhandley17') + scale_fill_manual(values=c('#12B6CF','#0087DC','#DC241f','#FAA61A'),labels=c('Brexit Party','Conservatives','Labour','Lib Dems')) + ggsave('party_popularity_classid.png',width=8,height=5,type='cairo')
