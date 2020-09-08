library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(ns_sec_analytic = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7))) %>%
  mutate_at(vars(likeStarmer,likeLab,likeCon,likeLongBailey,likeBoris,likeRayner),~na_if(.x,9999)) %>%
  filter(!is.na(ns_sec_analytic)) %>%
  group_by(ns_sec_analytic) %>%
  summarise(Starmer=wtd.mean(likeStarmer,wt,na.rm=T),Labour=wtd.mean(likeLab,wt,na.rm=T),Conservatives=wtd.mean(likeCon,wt,na.rm=T),RLB=wtd.mean(likeLongBailey,wt,na.rm=T),Boris=wtd.mean(likeBoris,wt,na.rm=T),Rayner=wtd.mean(likeRayner,wt,na.rm=T)) %>%
  gather(key='person',value='like',c(Starmer,RLB,Boris,Rayner)) -> data

ggplot(data,aes(x=ns_sec_analytic,y=like,fill=person)) + geom_bar(stat='identity',position='dodge') + ylab('Average likeability 0-10') + xlab('NS-SEC Analytic Class') + labs(title='Popularity of Starmer, RLB and Boris by social class',subtitle = 'BES Internet Panel Wave 19',fill='Person') + ggsave('class_popularity.png',width=8,height=5,type='cairo')

df %>%
  mutate(gor = as_factor(gor)) %>%
  mutate_at(vars(likeStarmer,likeLab,likeCon,likeLongBailey,likeBoris),~na_if(.x,9999)) %>%
  group_by(gor) %>%
  summarise(Starmer=wtd.mean(likeStarmer,wt,na.rm=T),Labour=wtd.mean(likeLab,wt,na.rm=T),Conservatives=wtd.mean(likeCon,wt,na.rm=T),RLB=wtd.mean(likeLongBailey,wt,na.rm=T),Boris=wtd.mean(likeBoris,wt,na.rm=T)) %>%
  gather(key='person',value='like',c(Starmer,RLB,Boris)) -> data1

ggplot(data1,aes(x=gor,y=like,fill=person)) + geom_bar(stat='identity',position='dodge') + ylab('Average likeability 0-10') + xlab('Region') + labs(title='Popularity of Starmer, RLB and Boris by region',subtitle = 'BES Internet Panel Wave 19',fill='Person') + theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5)) + ggsave('region_popularity.png',width=8*1.2,height=5*1.2,type='cairo')

df %>%
  mutate(ns_sec_analytic = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7)),gor=as_factor(gor)) %>%
  mutate_at(vars(likeStarmer,likeLab,likeCon,likeLongBailey,likeBoris,likeRayner,likeCorbyn,EUIntegrationSelf),~na_if(.x,9999)) %>%
  select(likeStarmer,likeLab,likeCon,likeLongBailey,likeBoris,likeRayner,likeCorbyn,ns_sec_analytic,EUIntegrationSelf,gor,wt) -> data2

df %>%
  mutate(ns_sec_analytic = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7))) %>%
  mutate_at(vars(likeLab,likeCon,likeLD,likeBrexitParty), ~na_if(.x,9999)) %>%
  filter(!is.na(ns_sec_analytic)) %>%
  group_by(ns_sec_analytic) %>%
  summarise_at(vars(likeLab,likeCon,likeLD,likeBrexitParty),~wtd.mean(.x,wt,na.rm=T)) %>%
  gather(key='Party',value='like',c(likeLab,likeCon,likeLD,likeBrexitParty)) %>%
  ggplot(aes(x=ns_sec_analytic,y=like,fill=Party)) + geom_bar(stat='identity',position='dodge') + xlab('NS-SEC Analytic Class') + ylab('Likeability (0-10 scale)') + labs(title='Party popularity by social class',subtitle='BES Internet Panel Wave 19',caption='@jwhandley17') + scale_fill_manual(values=c('#12B6CF','#0087DC','#DC241f','#FAA61A'),labels=c('Brexit Party','Conservatives','Labour','Lib Dems')) + ggsave('party_popularity_nssec.png',width=8,height=5,type='cairo')

  

class <- lm('likeStarmer ~ ns_sec_analytic',data=data2)
region <- lm('likeStarmer ~ gor',data=data2)
eu <- lm('likeStarmer ~ EUIntegrationSelf',data=data2)
class.region <- lm('likeStarmer ~ ns_sec_analytic + gor',data=data2)
class.eu <- lm('likeStarmer ~ ns_sec_analytic + EUIntegrationSelf',data=data2)
class.region.eu <- lm('likeStarmer ~ ns_sec_analytic + gor + EUIntegrationSelf',data=data2)
summary(class.region.eu)

library(stargazer)

stargazer(class,class.eu,out='res.html',dep.var.labels = 'Like Starmer (0-10 scale)')
