library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,2,3,4,5,6,7))) %>%
  mutate_at(vars(likeStarmer,likeCorbyn,likeBoris),~na_if(.x,9999)) %>%
  filter(!is.na(occ_class)) %>%
  group_by(occ_class) %>%
  summarise_at(vars(likeStarmer,likeCorbyn,likeBoris),~wtd.mean(.x,wt,na.rm=T)) %>%
  gather(key='person',value='like',c(likeStarmer,likeCorbyn,likeBoris)) %>%
  ggplot(aes(x=occ_class,y=like,fill=person)) + geom_bar(stat='identity',position='dodge') + xlab('NS-SEC Analytic Class') + ylab('Likeability (0-10 scale)') + labs(title='Popularity of Starmer, Johnson and Corbyn by social class',subtitle='BES Internet Panel Wave 19',caption='@jwhandley17',fill='Person') + scale_fill_manual(values=c('#0087DC','#FF355E','#DC241f'),labels=c('Johnson','Corbyn','Starmer')) + ggsave('popularity_nssec.png',width=8,height=5,type='cairo')

