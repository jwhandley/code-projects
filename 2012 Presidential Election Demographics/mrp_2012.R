library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(mapproj)
library(scales)
library(cowplot)

context <- read_csv('context.csv')

indiv <- read_spss('usmi2012-natelec.por') %>%
  mutate(state = as_factor(STANUM)) %>%
  inner_join(context) %>%
  mutate(race = as_factor(RACE),
         educ = as_factor(EDUC10),
         age = as_factor(AGE),
         sex = as_factor(SEX),
         income = case_when(INCOME12 == 1 ~ 'Under $30,000',
                            INCOME12 == 2 ~ '$30,000 - $49,999',
                            INCOME12 == 3 ~ '$50,000 - $99,999',
                            INCOME12 >= 4 ~ '$100,000 or more'),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_),
         weight = WEIGHT) %>%
  mutate(income = factor(income,levels=c('Under $30,000','$30,000 - $49,999','$50,000 - $99,999','$100,000 or more'))) %>%
  select(weight,vote,age,sex,educ,race,income,state,dem12,vap12,degree,median_hh_income,white,black,hispanic,asian,evangelical,region)


turnout <- read_dta('cps_00188.dta') %>%
  inner_join(context) %>%
  mutate(age = case_when(age >= 18 & age < 30 ~ '18-29',
                         age >= 30 & age < 45 ~ '30-44',
                         age >= 45 & age <= 65 ~ '45-65',
                         age > 65 ~ '65+',
                         TRUE ~ NA_character_),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 2 ~ 'Female',
                         TRUE ~ NA_character_),
         race = case_when(race == 100 & hispan == 0 ~ 'White',
                          race == 200 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic/Latino',
                          race >= 650 & race <= 652 ~ 'Asian',
                          TRUE ~ 'Other'),
         educ = case_when(educ < 70 | educ == 71 ~ 'No high school diploma',
                          educ == 70 | educ == 72 | educ == 73 ~ 'High school graduate',
                          educ > 73 & educ < 110 ~ 'Some college/assoc. degree',
                          educ >= 110 & educ <= 111 ~ 'College graduate',
                          educ > 110 ~ 'Postgraduate study'),
         income = case_when(faminc <= 600 | faminc == 710 ~ 'Under $30,000',
                            faminc >= 720 & faminc <= 740 ~ '$30,000 - $49,999',
                            faminc >= 810 & faminc <= 841 ~ '$50,000 - $99,999',
                            faminc == 841 | faminc == 843 ~ '$100,000 or more',
                            TRUE ~ NA_character_),
         voted = as.numeric(voted==2)) %>%
  mutate(age = factor(age,levels=levels(indiv$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(indiv$race)),
         educ = factor(educ,levels=levels(indiv$educ)),
         income = factor(income,levels=levels(indiv$income))) %>%
  select(voted,age,sex,educ,race,income,state,dem12,vap12,degree,median_hh_income,white,black,hispanic,asian,evangelical,region)

census <- read_dta('usa_00076.dta') %>%
  inner_join(context) %>%
  mutate(age = case_when(age >= 18 & age < 30 ~ '18-29',
                         age >= 30 & age < 45 ~ '30-44',
                         age >= 45 & age <= 65 ~ '45-65',
                         age > 65 ~ '65+',
                         TRUE ~ NA_character_),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 2 ~ 'Female',
                         TRUE ~ NA_character_),
         race = case_when(race == 1 & hispan == 0 ~ 'White',
                          race == 2 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          race >= 4 & race <= 6 ~ 'Asian',
                          TRUE ~ 'Other'),
         educ = case_when(educ < 6 ~ 'No high school diploma',
                          educ == 6 ~ 'High school graduate',
                          educ > 6 & educ < 10 ~ 'Some college/assoc. degree',
                          educ == 10 ~ 'College graduate',
                          educ > 10 ~ 'Postgraduate study'),
         income = cut(ftotinc,c(-Inf,30000,50000,100000,Inf),labels=levels(indiv$income))) %>%
  mutate(age = factor(age,levels=levels(indiv$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(indiv$race)),
         educ = factor(educ,levels=levels(indiv$educ)),
         income = factor(income,levels=levels(indiv$income))) %>%
  group_by(state,age,sex,race,educ,income) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context)

res.vote <- glmer(vote ~ black + hispanic + evangelical + dem12 + (1|age) + (1|sex) + (1|income) + (1|state) + (1|race:educ) + (1|race:state) + (1|race:educ:state),indiv,family=binomial)
summary(res.vote)
census$vote <- predict(res.vote,census,allow.new.levels=T,type='response')

res.turnout <- glmer(voted ~ black + hispanic + evangelical + vap12 + (1|age) + (1|sex) + (1|educ) + (1|income) + (1|race:educ) + (1|race:state) + (1|race:educ:state),turnout,family=binomial)
summary(res.turnout)
census$turnout <- predict(res.turnout,census,allow.new.levels=T,type='response')

census %>%
  group_by(state) %>%
  summarise(ratio = mean(vap12)/wtd.mean(turnout,n)) -> turnout.ratio

census %>%
  inner_join(turnout.ratio) %>%
  mutate(n_votes = n*turnout*ratio) %>%
  group_by(state) %>%
  summarise(vote = wtd.mean(vote,n_votes,na.rm=T),
            vote.actual = mean(dem12),
            turnout = wtd.mean(turnout*ratio,n,na.rm=T),
            turnout.actual = mean(vap12)) %>%
  write_csv('state_mrp_results.csv')

census %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state,race,educ) %>%
  summarise(vote = wtd.mean(vote,n_votes,na.rm=T),
            turnout = wtd.mean(turnout,n,na.rm=T)) -> race.educ

plot_usmap(data=filter(race.educ,race=='White',educ=='Some college/assoc. degree'),values='vote') +
  scale_fill_gradient(name='Obama two-party vote share',low='red',high='blue')
