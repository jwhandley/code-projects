library(tidyverse)
library(scales)
library(ggrepel)
library(stargazer)
library(nnet)

df <- read_csv('SDA extract/sub-data.csv')
lookup <- read_csv('GSS extract/denom_lookup.csv')

df <- inner_join(df,lookup)


# Clean up data
df %>%
  rename(educ = EDUC,id=ID,realinc=REALINC,year=YEAR) %>%
  filter(educ<97,RACE>0,RELIG<98,DENOM<98,FUND>0,realinc>0,realinc<999998) %>%
  mutate(race = recode(RACE,`1`='White',`2`='Black',`3`='Other',.default=NA_character_), race = factor(race,levels=c('White','Black','Other'))) %>%
  mutate(relig = recode(RELIG,`1`='Protestant',`2`='Catholic',`3`='Jewish',`4`= 'None',.default='Other'), relig = factor(relig,levels=c('Protestant','Catholic','Jewish','None','Other'))) %>%
  mutate(fund = recode(FUND,`1`='Fundamentalist',`2`='Moderate',`3`='Liberal',.default="Don't know"),fund = factor(fund,levels=c('Liberal','Moderate','Fundamentalist',"Don't know"))) %>%
  mutate(class = recode(CLASS,`1`='Lower class',`2`='Working class',`3`='Middle class',`4`='Upper class',`5`='No class',.default=NA_character_), class = factor(class,levels=c('Lower class','Working class','Middle class','Upper class'))) %>%
  mutate(partyid = recode(PARTYID,`0`='Democrat',`1`='Democrat',`2`='Independent',`3`='Independent',`4`='Independent',`5`='Republican',`6`='Republican',.default=NA_character_)) %>%
  mutate(polviews = na_if(na_if(na_if(POLVIEWS,0),8),9)) %>%
  select(id,year,race,educ,realinc,relig,fund,denom,class,partyid,polviews) %>%
  mutate(prot = as.numeric(relig=='Protestant'),fundamental = as.numeric(fund=='Fundamentalist'),fund_prot = prot*fundamental) %>%
  group_by(denom) %>%
  mutate(black=as.numeric(mean(as.numeric(race=='Black'))>0.45),fund = as.numeric(mean(fundamental)>0.5)) %>%
  mutate(trad = ifelse(prot==0,NA_character_,ifelse(black==1,'Historical Black',ifelse(fund==0,'Mainline','Evangelical')))) %>%
  mutate(relig_trad = coalesce(trad,relig),trad = ifelse(is.na(trad),'Not protestant',trad)) %>%
  mutate(relig_trad = factor(relig_trad,levels=c('Mainline','Evangelical','Historical Black','Catholic','Jewish','None','Other'))) %>%
  ungroup() -> data

# Subjective class identification by religious group
data %>%
  filter(!is.na(class)) %>%
  group_by(relig_trad) %>%
  mutate(pop = n()) %>%
  ungroup() %>%
  group_by(relig_trad,class) %>%
  summarise(share=n()/mean(pop)) %>%
  spread(class,share) -> subj_class

# Subjective class ID whole population reference
data %>%
  filter(!is.na(class)) %>%
  mutate(pop=n()) %>%
  group_by(class) %>%
  summarise(share = n()/mean(pop)) %>%
  spread(class,share) %>%
  mutate(relig_trad = 'Total Population') -> subj_class_ref

subj_class <- bind_rows(subj_class,subj_class_ref)

# Income class by religious group
data %>%
  group_by(year) %>%
  mutate(income_z = scale(log(realinc))) %>%
  mutate(income_p = pnorm(income_z)) %>%
  ungroup() %>%
  mutate(income_class = cut(income_p,3,labels=c('Lower','Middle','Upper'))) %>%
  group_by(relig_trad) %>%
  mutate(pop = n()) %>%
  ungroup() %>%
  group_by(relig_trad,income_class) %>%
  summarise(share = n()/mean(pop)) %>%
  spread(income_class,share) -> income_class

# Income class whole population reference
data %>%
  group_by(year) %>%
  mutate(income_z = scale(log(realinc))) %>%
  mutate(income_p = pnorm(income_z)) %>%
  ungroup() %>%
  mutate(income_class = cut(income_p,3,labels=c('Lower','Middle','Upper'))) %>%
  mutate(pop = n()) %>%
  group_by(income_class) %>%
  summarise(share=n()/mean(pop)) %>%
  spread(income_class,share) %>%
  mutate(relig_trad = 'Total Population') -> income_class_ref
  
income_class <- bind_rows(income_class,income_class_ref)

# Education class by religious group
data %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ)) %>%
  mutate(educ_p = pnorm(educ_z)) %>%
  ungroup() %>%
  mutate(educ_class = cut(educ_p,3,labels=c('Lower','Middle','Upper'))) %>%
  group_by(relig_trad) %>%
  mutate(pop = n()) %>%
  ungroup() %>%
  group_by(relig_trad,educ_class) %>%
  summarise(share = n()/mean(pop)) %>%
  spread(educ_class,share) -> educ_class

# Education class whole population reference
data %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ)) %>%
  mutate(educ_p = pnorm(educ_z)) %>%
  ungroup() %>%
  mutate(educ_class = cut(educ_p,3,labels=c('Lower','Middle','Upper'))) %>%
  mutate(pop = n()) %>%
  group_by(educ_class) %>%
  summarise(share=n()/mean(pop)) %>%
  spread(educ_class,share) %>%
  mutate(relig_trad = 'Total Population') -> educ_class_ref

educ_class <- bind_rows(educ_class,educ_class_ref)

# Income and education by denomination and religious group

data %>%
  filter(prot==1) %>%
  group_by(year) %>%
  mutate(income_z = scale(log(realinc))) %>%
  mutate(educ_z = scale(educ)) %>%
  ungroup() %>%
  group_by(denom,relig_trad) %>%
  summarise(income = mean(income_z), educ = mean(educ_z)) %>%
  ggplot(aes(x=educ,y=income,color=relig_trad,label=denom)) +
  geom_point() +
  geom_text_repel() +
  labs(x='Education z-score',
       y='Family income z-score',
       title='Average income and education by protestant denomination',
       subtitle='General Social Survey',
       caption='John Handley',
       color='Tradition') +
  ggsave('denomination_trad_class.png',width=8,height=5,type='cairo')

data %>%
  filter(!is.na(class),relig_trad!='Other') %>%
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
       title='Subjective class identification by religious tradition') +
  scale_y_continuous(labels=percent) +
  ggsave('relig_subj_class.png',width=8,height=5,type='cairo')

data %>%
  filter(relig_trad!='Other') %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ)[,1],income_z = scale(log(realinc))[,1]) %>%
  ungroup() %>%
  select(year,relig_trad,income_z,educ_z) %>%
  gather(stat,value,c(income_z,educ_z)) %>%
  ggplot(aes(x=year,y=value,color=stat)) +
  geom_smooth() +
  facet_wrap(~relig_trad) +
  labs(x='Year',
       y='Average z-score',
       color='Statistic',
       title='Average relative income and education by religious tradition') +
  scale_color_discrete(labels=c('Education','Income')) +
  ggsave('relig_class.png',width=8,height=5,type='cairo')

data %>%
  filter(!is.na(partyid),relig_trad!='Other') %>%
  mutate(person = 1) %>%
  spread(partyid,person) %>%
  mutate_at(vars(`Republican`,`Independent`,`Democrat`),~replace_na(.x,0)) %>%
  select(year,relig_trad,`Republican`,`Independent`,`Democrat`) %>%
  gather(party,party_id,c(`Republican`,`Independent`,`Democrat`)) %>%
  ggplot(aes(x=year,y=party_id,color=party)) +
  geom_smooth() +
  facet_wrap(~relig_trad) +
  labs(x='Year',
       y='Share identifying',
       color='Party',
       title='Party identification by religious tradition') +
  scale_y_continuous(labels=percent) +
  scale_color_manual(values=c('#3333FF','black','#E81B23')) +
  ggsave('relig_party_id.png',width=8*1.2,height=5*1.2,type='cairo')

data %>%
  group_by(relig_trad) %>%
  summarise(n=n())
  