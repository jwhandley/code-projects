library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(mapproj)
library(scales)
library(cowplot)

context16 <- read_csv('context16.csv')
df16 <- read_spss('31116396_National2016.por') %>%
  mutate(educ = as_factor(EDUC12R),
         race = as_factor(RACE),
         income = droplevels(as_factor(na_if(INCOME16,9))),
         state = str_trim(as_factor(STANUM)),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  inner_join(context16) %>%
  filter(!is.na(race),!is.na(educ)) %>%
  select(vote,educ,race,income,state,degree,black,hispanic,dem16)

context12 <- read_csv('context12.csv')
df12 <- read_spss('usmi2012-natelec.por') %>%
  mutate(educ = case_when(EDUC10 <=2 ~ 'High school or less',
                          EDUC10 == 3 ~ 'Some college/assoc. degree',
                          EDUC10 == 4 ~ 'College graduate',
                          EDUC10 == 5 ~ 'Postgraduate study'),
         race = as_factor(RACE),
         income = as_factor(INCOME12),
         state = str_trim(as_factor(STANUM)),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  mutate(educ = factor(educ,levels=levels(df16$educ))) %>%
  inner_join(context12) %>%
  filter(!is.na(race),!is.na(educ)) %>%
  select(vote,educ,race,income,state,degree,black,hispanic,dem12)

res.16 <- glmer(vote ~ (1|race) + (1|educ) + (1|state) + (1|race:educ) + (1|state:race) + (1|state:race:educ) + degree + black + hispanic + dem16,df16,family=binomial(link='logit'))
summary(res.16)

res.12 <- glmer(vote ~ (1|race) + (1|state) + (1|race:educ) + (1|state:race) + (1|state:race:educ) + degree + black + hispanic + dem12,df12,family=binomial(link='logit'))
summary(res.12)

output.16 <- expand_grid(state=unique(df16$state),educ=levels(df16$educ),race=levels(df16$race)) %>%
  inner_join(context16)
output.16$clinton <- predict(res.16,output.16,allow.new.levels=T,type='response')

output.12 <- expand_grid(state=unique(df12$state),educ=levels(df12$educ),race=levels(df12$race)) %>%
  inner_join(context12)
output.12$obama <- predict(res.12,output.12,allow.new.levels=T,type='response')

output <- expand_grid(state=unique(df16$state),educ=levels(df16$educ),race=levels(df16$race)) %>%
  inner_join(select(output.12,state,educ,race,obama)) %>%
  inner_join(select(output.16,state,educ,race,clinton)) %>%
  mutate(swing = clinton - obama)

data <- left_join(map_data('state'),mutate(output,region=tolower(state)))

data %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic/Latino','Asian','Other')),
         educ = factor(educ,levels=c('High school or less','Some college/assoc. degree','College graduate','Postgraduate study'))) %>%
  filter(!is.na(educ),!is.na(race)) %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=swing)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='#E81B23',mid='white',high='#3333FF',midpoint=0,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  labs(title='Swing between 2012 and 2016 presidential elections by race and education',
       fill='Swing') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('race_educ_swing.png',width=16,height=10,type='cairo')

data %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic/Latino','Asian','Other')),
         educ = factor(educ,levels=c('High school or less','Some college/assoc. degree','College graduate','Postgraduate study'))) %>%
  filter(!is.na(educ),!is.na(race)) %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=obama)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='#E81B23',mid='white',high='#3333FF',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  labs(title='2012 presidential election vote by race and education',
       fill='Two-party vote share') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('race_educ_obama.png',width=16,height=10,type='cairo')

data %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic/Latino','Asian','Other')),
         educ = factor(educ,levels=c('High school or less','Some college/assoc. degree','College graduate','Postgraduate study'))) %>%
  filter(!is.na(educ),!is.na(race)) %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=clinton)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='#E81B23',mid='white',high='#3333FF',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  labs(title='2016 presidential election vote by race and education',
       fill='Two-party vote share') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('race_educ_clinton.png',width=16,height=10,type='cairo')
