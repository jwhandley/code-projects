library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(scales)
library(maps)
library(mapproj)
library(cowplot)

context <- read_csv('contextual_data.csv')

oct <- read_dta('kff_october_2020.dta') %>%
  select(cstate,cregion,racethn,receduc,age,rsex,income,M4ALL,aca)
may <- read_dta('kff_may_2020.dta') %>%
  select(cstate,cregion,racethn,receduc,age,rsex,income,M4ALL,aca)
feb <- read_dta('kff_february_2020.dta') %>%
  select(cstate,cregion,racethn,receduc,age,rsex,income,M4ALL,aca)
jan <- read_dta('kff_january_2020.dta') %>%
  select(cstate,cregion,racethn,receduc,age,rsex,income,M4ALL,aca)

oct %>%
  bind_rows(may) %>%
  bind_rows(feb) %>%
  bind_rows(jan) %>% 
  rename(state = cstate,
         region = cregion,
         race = racethn,
         educ = receduc,
         sex = rsex,
         m4a = M4ALL) %>% 
  mutate(age = cut(age,c(18,25,35,45,55,65,Inf),include.lowest=T,labels=c('18-24','25-34','35-44','45-54','55-64','65+'))) %>% 
  inner_join(context) %>%
  mutate(m4a.support = as.numeric(m4a<3),
         aca.support = as.numeric(aca<3)) -> df

census <- read_dta('usa_00077.dta') %>%
  filter(ftotinc != 9999999) %>%
  mutate(age = cut(age,c(18,25,35,45,55,65,Inf),include.lowest=T,labels=c('18-24','25-34','35-44','45-54','55-64','65+')),
         income = cut(ftotinc,c(-Inf,20000,30000,40000,50000,75000,90000,100000,Inf),seq(1,8,1),include.lowest=T),
         race = case_when(race == 1 & hispan == 0 ~ 1,
                          race == 2 & hispan == 0 ~ 2,
                          hispan != 0 ~ 3,
                          hispan == 0 ~ 4,
                          TRUE ~ 9),
         sex = as.numeric(sex),
         educ = case_when(educ < 6 ~ 1,
                          educ == 6 ~ 2,
                          educ > 6 & educ < 10 ~ 3,
                          educ >= 10 ~ 4),
         state = statefip) %>%
  group_by(state,race,educ,sex,income,age) %>%
  summarise(n = sum(perwt),
            region = mean(round(region/10))) %>%
  inner_join(context)

res.m4a <- glmer(m4a.support ~ (1|sex) + (1|age) + (1|educ) + (1|race) + (1|income) + (1|race:educ) + (1|income:race) + (1|income:region) + (1|race:region) + log(median_income) + degree + dem20,df,family=binomial)
summary(res.m4a)

res.aca <- glmer(aca.support ~ (1|sex) + (1|age) + (1|educ) + (1|race) + (1|income) + (1|race:educ) + (1|income:race) + (1|income:educ) + (1|race:region) + log(median_income) + degree + dem20,df,family=binomial)
summary(res.aca)

save.image('fitted_models.RData')

census$aca <- predict(res.aca,census,allow.new.levels=T,type='response')
census$m4a <- predict(res.m4a,census,allow.new.levels=T,type='response')

census %>%
  group_by(state.name) %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n)) %>%
  write_csv('m4a_aca_popularity.csv')

census %>%
  group_by(state.name) %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n)) -> state.means

census %>%
  ungroup() %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n))

us.map <- map_data('state')

census %>%
  group_by(state.name,educ,income) %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n)) %>%
  mutate(educ = recode(educ,`1`='No high school',
                       `2`='High school',
                       `3`='Some college',
                       `4`='Degree'),
         income = recode(as.numeric(income),`1`='<$20k',
                         `2`='$20k-$30k',
                         `3`='$30k-$40k',
                         `4`='$40k-$50k',
                         `5`='$50k-$75k',
                         `6`='$75k-$90k',
                         `7`='$90k-$100k',
                         `8`='>$100k')) %>%
  mutate(educ = factor(educ,levels=c('No high school','High school','Some college','Degree')),
         income = factor(income,levels=c('<$20k','$20k-$30k','$30k-$40k','$40k-$50k','$50k-$75k','$75k-$90k','$90k-$100k','>$100k'))) -> income.educ

census %>%
  group_by(state.name,educ,race) %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n)) %>%
  mutate(educ = recode(educ,`1`='No high school',
                       `2`='High school',
                       `3`='Some college',
                       `4`='Degree'),
         race = recode(race,`1`='White',`2`='Black',`3`='Hispanic',`4`='Other')) %>%
  mutate(educ = factor(educ,levels=c('No high school','High school','Some college','Degree')),
         race = factor(race,levels=c('White','Black','Hispanic','Other'))) -> race.educ

census %>%
  group_by(state.name,race,income) %>%
  summarise(m4a = wtd.mean(m4a,n),
            aca = wtd.mean(aca,n)) %>%
  mutate(race = recode(race,`1`='White',`2`='Black',`3`='Hispanic',`4`='Other'),
         income = recode(as.numeric(income),`1`='<$20k',
                         `2`='$20k-$30k',
                         `3`='$30k-$40k',
                         `4`='$40k-$50k',
                         `5`='$50k-$75k',
                         `6`='$75k-$90k',
                         `7`='$90k-$100k',
                         `8`='>$100k')) %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic','Other')),
         income = factor(income,levels=c('<$20k','$20k-$30k','$30k-$40k','$40k-$50k','$50k-$75k','$75k-$90k','$90k-$100k','>$100k'))) -> race.income


data <- left_join(us.map,mutate(income.educ,region=tolower(state.name)))
data.race.educ <- left_join(us.map,mutate(race.educ,region=tolower(state.name)))
data.state <- left_join(us.map,mutate(state.means,region=tolower(state.name)))
data.race.income <- left_join(us.map,(mutate(race.income,region=tolower(state.name))))

data %>%
  ggplot(aes(x=long,y=lat,group=group,fill=aca)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(educ),cols=vars(income),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for the affordable care act by income and education',
       fill='Strongly favor or favor') +
  ggsave('aca_income_education.png',width=30,height=10,type='cairo')

data %>%
  ggplot(aes(x=long,y=lat,group=group,fill=m4a)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(educ),cols=vars(income),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for Medicare for All by income and education',
       fill='Strongly favor or favor') +
  ggsave('m4a_income_education.png',width=30,height=10,type='cairo')

data.race.educ %>%
  ggplot(aes(x=long,y=lat,group=group,fill=aca)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for the affordable care act by race and education',
       fill='Strongly favor or favor') +
  ggsave('aca_race_eduation.png',width=16,height=10,type='cairo')

data.race.educ %>%
  ggplot(aes(x=long,y=lat,group=group,fill=m4a)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for Medicare for All by race and education',
       fill='Strongly favor or favor') +
  ggsave('m4a_race_eduation.png',width=16,height=10,type='cairo')

data.state %>%
  ggplot(aes(x=long,y=lat,group=group,fill=aca)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for the affordable care act by state',
       fill='Strongly favor or favor') +
  ggsave('aca_state.png',width=16,height=10,type='cairo')

data.state %>%
  ggplot(aes(x=long,y=lat,group=group,fill=m4a)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for Medicare for All by state',
       fill='Strongly favor or favor') +
  ggsave('m4a_state.png',width=16,height=10,type='cairo')

data.race.income %>%
  ggplot(aes(x=long,y=lat,group=group,fill=m4a)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(income),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for Medicare for All by income and race',
       fill='Strongly favor or favor') +
  ggsave('m4a_income_race.png',width=30,height=10,type='cairo')

data.race.income %>%
  ggplot(aes(x=long,y=lat,group=group,fill=aca)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(income),switch='y') +
  theme(strip.placement = 'outside',plot.title = element_text(hjust = 0.5)) +
  labs(title='Support for Affordable Care Act by income and race',
       fill='Strongly favor or favor') +
  ggsave('aca_income_race.png',width=30,height=10,type='cairo')
