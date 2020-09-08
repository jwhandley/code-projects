library(tidyverse)
library(haven)
library(Hmisc)

ge17 <- read_dta("GE2017/stata/stata11/bes_2017_f2f_ukds.dta")

ge17 %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,3,4,5,6,7))) %>%
  mutate(vote=as_factor(b02)) %>%
  mutate(lab=as.numeric(vote=='Labour'),con=as.numeric(vote=='Conservatives'),lib=as.numeric(vote=='Liberal Democrats'),dnv=as.numeric(b01==2)) %>%
  filter(!is.na(occ_class)) %>%
  group_by(occ_class) %>%
  summarise_at(vars(lab,con,lib,dnv),~wtd.mean(.x,wt_vote,na.rm=T)) %>%
  mutate(year=2017,oth=1-lab-con-lib) %>%
  gather(key='party',value='vote',c(lab,con,lib,oth)) -> vote17

ggplot(vote17,aes(x=occ_class,y=vote,fill=reorder(party,-vote))) + geom_bar(stat='identity',position='dodge') + scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','darkgrey'),labels=c('Conservatives','Labour','Liberal Democrats','Other')) + labs(title='Vote by occupational class in GE2017',subtitle='BES 2017 post-election face to face survey',caption='@jwhandley17',fill='Party') + xlab('NS-SEC Analytic Class') + ylab('Vote Share') + ggsave('ge2017_class_vote.png',width=8,height=5,type='cairo')

ge15 <- read_dta("GE2015/stata/stata11/bes_2015_f2f_ukda.dta")

ge15 %>%
  mutate(occ_class = factor(recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),levels=c(1.1,1.2,3,4,5,6,7))) %>%
  mutate(vote=as_factor(b02)) %>%
  mutate(lab=as.numeric(vote=='Labour'),con=as.numeric(vote=='Conservatives'),lib=as.numeric(vote=='Liberal Democrats'),dnv=as.numeric(b01==2)) %>%
  filter(!is.na(occ_class)) %>%
  group_by(occ_class) %>%
  summarise_at(vars(lab,con,lib,dnv),~wtd.mean(.x,wt_combined_main,na.rm=T)) %>%
  mutate(year=2015,oth=1-lab-con-lib) %>%
  gather(key='party',value='vote',c(lab,con,lib,oth)) -> vote15

ggplot(vote15,aes(x=occ_class,y=vote,fill=reorder(party,-vote))) + geom_bar(stat='identity',position='dodge') + scale_fill_manual(values=c('#0087DC','#DC241f','darkgrey','#FAA61A'),labels=c('Conservatives','Labour','Other','Liberal Democrats')) + labs(title='Vote by occupational class in GE2015',subtitle='BES 2015 post-election face to face survey',caption='@jwhandley17',fill='Party') + xlab('NS-SEC Analytic Class') + ylab('Vote Share') + ggsave('ge2015_class_vote.png',width=8,height=5,type='cairo')

ge10 <- read_dta("GE2010/stata11/bes_2010_survey_data.dta")

ge10 %>%
  mutate(occ_class = factor(recode(as.numeric(aq73_1),`1`=1.1,`2`=1.2,`3`=2,`4`=3,`5`=4,`6`=5,`7`=6,`8`=7),levels=c(1.1,1.2,2,3,4,5,6,7))) %>%
  mutate(vote = as_factor(bq12_2)) %>%
  mutate(lab=as.numeric(vote=='Labour'),con=as.numeric(vote=='Conservatives'),lib=as.numeric(vote=='Liberal Democrats')) %>%
  filter(!is.na(occ_class)) %>%
  group_by(occ_class) %>%
  summarise_at(vars(lab,con,lib),~wtd.mean(.x,postwgt,na.rm=T)) %>%
  mutate(year=2010,oth=1-lab-con-lib) %>%
  gather(key='party',value='vote',c(lab,con,lib,oth)) -> vote10

ggplot(vote10,aes(x=occ_class,y=vote,fill=reorder(party,-vote))) + geom_bar(stat='identity',position='dodge') + scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','darkgrey'),labels=c('Conservatives','Labour','Liberal Democrats','Other')) + labs(title='Vote by occupational class in GE2010',subtitle='BES 2010 post-election face to face survey',caption='@jwhandley17',fill='Party') + xlab('NS-SEC Analytic Class') + ylab('Vote Share') + ggsave('ge2010_class_vote.png',width=8,height=5,type='cairo')