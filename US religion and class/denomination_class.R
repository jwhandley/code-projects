library(tidyverse)
library(scales)
library(ggrepel)

df <- read_csv('GSS/class_relig_gss.csv')
lookup <- read_csv('GSS/denom_lookup.csv')

df <- inner_join(df,lookup)

# Clean up data
df %>%
  rename(educ = EDUC,id=ID_,realinc=REALINC,year=YEAR) %>%
  filter(educ<97,RACE>0,RELIG<98,DENOM<98,FUND>0,realinc>0,realinc<999998) %>%
  mutate(race = recode(RACE,`1`='White',`2`='Black',`3`='Other',.default=NA_character_), race = factor(race,levels=c('White','Black','Other'))) %>%
  mutate(relig = recode(RELIG,`1`='Protestant',`2`='Catholic',`3`='Jewish',`4`= 'None',.default='Other'), relig = factor(relig,levels=c('Protestant','Catholic','Jewish','None','Other'))) %>%
  mutate(fund = recode(FUND,`1`='Fundamentalist',`2`='Moderate',`3`='Liberal',.default="Don't know"),fund = factor(fund,levels=c('Liberal','Moderate','Fundamentalist',"Don't know"))) %>%
  select(id,year,race,educ,realinc,relig,fund,denom) %>%
  mutate(prot = as.numeric(relig=='Protestant'),fundamental = as.numeric(fund=='Fundamentalist'),fund_prot = prot*fundamental) -> data

data %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ),income_z = scale(realinc)) %>%
  ungroup() %>%
  filter(prot==1) %>%
  group_by(denom) %>%
  summarise(inc = mean(income_z),educ = mean(educ_z),black=as.numeric(mean(as.numeric(race=='Black'))>0.6),fund = as.numeric(mean(fundamental)>0.5),n=n()) %>%
  mutate(trad = ifelse(black==1,'Historical Black',ifelse(fund==0,'Mainline','Evangelical'))) %>%
  mutate(black = recode(black,`0`='White',`1`='Black'), fund = recode(fund,`0`='Mainline',`1`='Evangelical')) %>%
  ggplot(aes(x=educ,y=inc,label=denom,color=as_factor(trad))) +
  geom_text_repel() +
  geom_point() +
  labs(x='Education z-score',
       y='Family income z-score',
       title='Average income and education by protestant denomination',
       subtitle='General Social Survey',
       caption='John Handley',
       color='Tradition') +
  ggsave('denomination_trad_class.png',width=8,height=5,type='cairo')

data %>%
  group_by(year) %>%
  mutate(educ_z = scale(educ)[,1],income_z = scale(realinc)[,1]) %>%
  ungroup() %>%
  group_by(denom) %>%
  mutate(black=as.numeric(mean(as.numeric(race=='Black'))>0.6),fund = as.numeric(mean(fundamental)>0.5)) %>%
  mutate(trad = ifelse(prot==0,NA_character_,ifelse(black==1,'Historical Black',ifelse(fund==0,'Mainline','Evangelical')))) %>%
  mutate(relig_trad = coalesce(trad,relig),trad = ifelse(is.na(trad),'Not protestant',trad)) %>%
  ungroup() -> trad

trad %>%
  mutate(class = pnorm(scale(educ_z + 2*income_z))) %>%
  mutate(class = cut(class,3,labels=c('Lower','Middle','Upper'))) %>%
  group_by(relig_trad) %>%
  mutate(pop=n()) %>%
  ungroup() %>%
  group_by(relig_trad,class) %>%
  summarise(share=n()/mean(pop)) %>%
  spread(class,share) %>%
  write_csv('relig_class.csv')

trad %>%
  mutate(class = pnorm(scale(educ_z + 2*income_z))) %>%
  mutate(class = cut(class,3,labels=c('Lower','Middle','Upper'))) %>%
  mutate(pop = n()) %>%
  group_by(class) %>%
  summarise(n=n()/mean(pop)) %>%
  spread(class,n) %>%
  write_csv('class.csv')
