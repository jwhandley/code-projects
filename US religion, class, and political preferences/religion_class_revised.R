library(tidyverse)
library(scales)
library(ggrepel)

df <- read_csv('SDA extract/sub-data.csv')
lookup <- read_csv('GSS extract/denom_lookup.csv')

df <- inner_join(df,lookup)

df %>%
  filter(EDUC<97,RACE>0,RELIG<98,DENOM<98,FUND>0,REALINC>0,REALINC<999998) %>%
  rename(educ = EDUC,id=ID,realinc=REALINC,year=YEAR) %>%
  mutate(class = recode(CLASS,`1`='Lower class',`2`='Working class',`3`='Middle class',`4`='Upper class',`5`='No class',.default=NA_character_), class = factor(class,levels=c('Middle class','Lower class','Working class','Upper class'))) %>%
  mutate(partyid = recode(PARTYID,`0`='Democrat',`1`='Democrat',`2`='Independent',`3`='Independent',`4`='Independent',`5`='Republican',`6`='Republican',.default=NA_character_)) %>%
  mutate(race = recode(RACE,`1`='White',`2`='Black',`3`='Other',.default=NA_character_), race = factor(race,levels=c('White','Black','Other'))) %>%
  mutate(relig = recode(RELIG,`1`='Protestant',`2`='Catholic',`3`='Jewish',`4`= 'None',.default='Other'), relig = factor(relig,levels=c('Protestant','Catholic','Jewish','None','Other'))) %>%
  mutate(relig_trad = coalesce(denom_group,relig)) %>%
  mutate(fund = recode(FUND,`1`='Fundamentalist',`2`='Moderate',`3`='Liberal',.default="Don't know"),fund = factor(fund,levels=c('Liberal','Moderate','Fundamentalist',"Don't know"))) %>%
  mutate(type = ifelse(denom_group!='Other',
                       ifelse(race=='Black',
                              'Historical black',
                              ifelse(fund=='Fundamentalist',
                                     'Evangelical',
                                     'Mainline')),
                       ifelse(fund=='Fundamentalist',
                              'Evangelical',
                              'Mainline'))) %>%
  mutate(relig_type = coalesce(denom_type,type,relig)) %>%
  mutate(relig_type = factor(relig_type,levels=c('Mainline','Evangelical','Historical black','Catholic','Jewish','None','Other'))) %>%
  #mutate(relig_trad = factor(relig_trad,levels=c('Episcopal','Lutheran','Presbyterian','Baptist','Methodist','Nondenominational','Catholic','Jewish','None','Other'))) %>%
  mutate(dem = as.numeric(partyid=='Democrat')) %>%
  select(year,id,relig_trad,relig_type,relig,race,partyid,educ,realinc,class,dem) -> data

data %>%
  filter(!is.na(partyid),relig_type!='Other') %>%
  mutate(person = 1) %>%
  spread(partyid,person) %>%
  mutate_at(vars(`Republican`,`Independent`,`Democrat`),~replace_na(.x,0)) %>%
  select(year,relig_type,`Republican`,`Independent`,`Democrat`) %>%
  gather(party,party_id,c(`Republican`,`Independent`,`Democrat`)) %>%
  mutate(party = factor(party,levels=c('Republican','Independent','Democrat'))) %>%
  ggplot(aes(x=year,y=party_id,color=party)) +
  geom_smooth() +
  facet_wrap(~relig_type) +
  labs(x='Year',
       y='Share identifying',
       color='Party',
       title='Party identification by religious group') +
  scale_y_continuous(labels=percent) +
  theme(panel.spacing.x = unit(8,'mm')) +
  ggsave('relig_partyid.png',width=8,height=5,type='cairo')

data %>%
  filter(!is.na(class),relig_type!='Other') %>%
  mutate(person = 1) %>%
  spread(class,person) %>%
  mutate_at(vars(`Lower class`,`Working class`,`Middle class`,`Upper class`),~replace_na(.x,0)) %>%
  select(year,relig_type,`Lower class`,`Working class`,`Middle class`,`Upper class`) %>%
  gather(class,class_id,c(`Lower class`,`Working class`,`Middle class`,`Upper class`)) %>%
  ggplot(aes(x=year,y=class_id,color=class)) +
  geom_smooth() +
  facet_wrap(~relig_type) +
  labs(x='Year',
       y='Share identifying',
       color='Subjective class',
       title='Subjective class identification by religious group') +
  scale_y_continuous(labels=percent) +
  theme(panel.spacing.x = unit(8,'mm')) +
  ggsave('relig_subj_class.png',width=8,height=5,type='cairo')

data %>%
  filter(!is.na(class),relig %in% c('Protestant','Catholic','None')) %>%
  mutate(person = 1) %>%
  spread(class,person) %>%
  mutate_at(vars(`Lower class`,`Working class`,`Middle class`,`Upper class`),~replace_na(.x,0)) %>%
  select(year,relig_trad,`Lower class`,`Working class`,`Middle class`,`Upper class`) %>%
  gather(class,class_id,c(`Lower class`,`Working class`,`Middle class`,`Upper class`)) %>%
  ggplot(aes(x=year,y=class_id,color=class)) +
  geom_smooth() +
  facet_wrap(~relig_trad) +
  labs(x='Year',
       y='Share identifying',
       color='Subjective class',
       title='Subjective class identification by religious group') +
  scale_y_continuous(labels=percent) +
  ggsave('denom_subj_class.png',width=8,height=5,type='cairo')

data %>%
  filter(relig_type!='Other') %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ)[,1],income_z = scale(log(realinc))[,1]) %>%
  ungroup() %>%
  select(year,relig_type,income_z,educ_z) %>%
  gather(stat,value,c(income_z,educ_z)) %>%
  ggplot(aes(x=year,y=value,color=stat)) +
  geom_smooth() +
  facet_wrap(~relig_type) +
  labs(x='Year',
       y='Average z-score',
       color='Statistic',
       title='Average relative income and education by religious group') +
  scale_color_discrete(labels=c('Education','Income')) +
  theme(panel.spacing.x = unit(8,'mm')) +
  ggsave('relig_class.png',width=8,height=5,type='cairo')



data %>%
  mutate(relig_trad = factor(relig_trad,levels=c('Catholic','Jewish','Methodist','Baptist','Presbyterian','Lutheran','Episcopal','Nondenominational','Other Christian'))) %>%
  mutate(rep = as.numeric(partyid=='Republican'), ind = as.numeric(partyid=='Independent')) %>%
  filter(year==1988,relig %in% c('Protestant','Catholic','Jewish'),!(data$relig_trad=='Baptist'&data$race=='Black')) %>%
  group_by(relig_trad) %>%
  summarise(rep = mean(rep,na.rm=T),dem = mean(dem,na.rm=T))

data %>%
  mutate(rep = as.numeric(partyid=='Republican'), ind = as.numeric(partyid=='Independent')) %>%
  filter(year==2018) %>%
  summarise(rep = mean(rep,na.rm=T),dem = mean(dem,na.rm=T))
  

data %>%
  mutate(relig_trad = factor(relig_trad,levels=c('Catholic','Jewish','Methodist','Baptist','Presbyterian','Lutheran','Episcopal','Nondenominational','Other Christian'))) %>%
  filter(year==2018,relig %in% c('Protestant','Catholic','Jewish'),!(data$relig_trad=='Baptist'&data$race=='Black')) %>%
  mutate(income_z = scale(realinc)) %>%
  mutate(income_class = cut(income_z,breaks=c(-2,-0.15,0.85,5),include.lowest=T,labels=c('Lower','Middle','Upper'))) %>%
  mutate(lower_income = as.numeric(income_class=='Lower'), middle_income = as.numeric(income_class=='Middle'), upper_income = as.numeric(income_class=='Upper')) %>%
  group_by(relig_trad) %>%
  summarise_at(vars(lower_income,middle_income,upper_income),~mean(.x)) %>%
  write_csv('relig_trad_class.csv')
  
data %>%
  mutate(relig_trad = factor(relig_trad,levels=c('Catholic','Jewish','Methodist','Baptist','Presbyterian','Lutheran','Episcopal','Nondenominational','Other Christian'))) %>%
  filter(year==1972,relig %in% c('Protestant','Catholic','Jewish'),!(data$relig_trad=='Baptist'&data$race=='Black')) %>%
  mutate(income_z = scale(realinc)) %>%
  mutate(income_class = cut(income_z,breaks=c(-2,-0.15,0.85,5),include.lowest=T,labels=c('Lower','Middle','Upper'))) %>%
  mutate(lower_income = as.numeric(income_class=='Lower'), middle_income = as.numeric(income_class=='Middle'), upper_income = as.numeric(income_class=='Upper')) %>%
  group_by(relig_trad) %>%
  summarise_at(vars(lower_income,middle_income,upper_income),~mean(.x)) %>%
  write_csv('relig_trad_class_1972.csv')

data %>%
  mutate(relig_trad = factor(relig_trad,levels=c('Catholic','Jewish','Methodist','Baptist','Presbyterian','Lutheran','Episcopal','Nondenominational','Other Christian'))) %>%
  filter(year==1972,relig %in% c('Protestant','Catholic','Jewish'),!(data$relig_trad=='Baptist'&data$race=='Black')) %>%
  mutate(income_z = scale(realinc)) %>%
  mutate(income_class = cut(income_z,breaks=c(-2,-0.15,0.85,5),include.lowest=T,labels=c('Lower','Middle','Upper'))) %>%
  mutate(lower_income = as.numeric(income_class=='Lower'), middle_income = as.numeric(income_class=='Middle'), upper_income = as.numeric(income_class=='Upper')) %>%
  summarise_at(vars(lower_income,middle_income,upper_income),~mean(.x))

