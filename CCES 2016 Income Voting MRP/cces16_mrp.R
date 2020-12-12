library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

context <- read_csv('contextual_data.csv')

indiv <- read_dta('CCES16_Common_OUTPUT_Feb2018_VV.dta') %>%
  filter(faminc < 97) %>%
  mutate(age = 2016 - birthyr,
         age = case_when(age <= 24 ~ 1,
                         age > 24 & age <= 34 ~ 2,
                         age > 34 & age <= 44 ~ 3,
                         age > 44 & age <= 54 ~ 4,
                         age > 54 & age <= 64 ~ 5,
                         age > 64 ~ 6),
         income = case_when(faminc <= 5 ~ 1,
                            faminc > 5 & faminc <= 9 ~ 2,
                            faminc > 9 ~ 3),
         race = case_when(race == 1 ~ 1,
                          race == 2 ~ 2,
                          race == 3 ~ 3,
                          TRUE ~ 4),
         educ = case_when(educ <= 2 ~ 1,
                          educ > 2 & educ <= 4 ~ 2,
                          educ > 4 ~ 3),
         gender = as.numeric(gender==2),
         statefip = inputstate,
         clinton = ifelse(CC16_410a %in% c(1,2,3,4,5,8),as.numeric(CC16_410a==2),NA_real_),
         trump = ifelse(CC16_410a %in% c(1,2,3,4,5,8),as.numeric(CC16_410a==1),NA_real_)) %>%
  select(statefip,income,race,educ,age,gender,clinton,trump) %>%
  inner_join(context)

turnout <- read_dta('cps_00179.dta') %>%
  filter(faminc < 995) %>%
  mutate(income = case_when(faminc < 800 ~ 1,
                            faminc > 800 & faminc <= 841 ~ 2,
                            faminc > 841 ~ 3),
         race = case_when(race == 100 & hispan == 0 ~ 1,
                          race == 200 & hispan == 0 ~ 2,
                          hispan != 0 ~ 3,
                          TRUE ~ 4),
         educ = case_when(educ <= 73 ~ 1,
                          educ > 73 & educ <= 100 ~ 2,
                          educ > 100 ~ 3),
         age = case_when(age <= 24 ~ 1,
                         age > 24 & age <= 34 ~ 2,
                         age > 34 & age <= 44 ~ 3,
                         age > 44 & age <= 54 ~ 4,
                         age > 54 & age <= 64 ~ 5,
                         age > 64 ~ 6),
         gender = as.numeric(sex==2),
         voted = as.numeric(voted==2)) %>%
  select(statefip,income,race,educ,age,gender,voted) %>%
  inner_join(context)

post <- read_dta('usa_00073.dta') %>%
  mutate(income = cut(ftotinc,c(-Inf,50000,100000,Inf),include.lowest = T,labels=seq(1,3,1)),
         race = case_when(race == 1 & hispan == 0 ~ 1,
                          race == 2 & hispan == 0 ~ 2,
                          hispan != 0 ~ 3,
                          TRUE ~ 4),
         educ = case_when(educ <= 6 ~ 1,
                          educ > 6 & educ <= 9 ~ 2,
                          educ > 9 ~ 3),
         age = case_when(age <= 24 ~ 1,
                         age > 24 & age <= 34 ~ 2,
                         age > 34 & age <= 44 ~ 3,
                         age > 44 & age <= 54 ~ 4,
                         age > 54 & age <= 64 ~ 5,
                         age > 64 ~ 6),
         gender = as.numeric(sex==2)) %>%
  group_by(statefip,income,race,educ,age,gender) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context)

res.clinton <- glmer(clinton ~ obama + degree + median_hh_income + black + hispanic + evangelical + (1|gender) + (1|race) + (1|educ) + (1|state) + (1|region) + (1|age:educ) + (1|gender:educ) + (1|race:age) + (1|race:educ) + (1|race:gender) + (1|race:region),indiv,family=binomial)
summary(res.clinton)
ranef(res.clinton)

post$clinton <- predict(res.clinton,post,allow.new.levels=T,type='response')

res.trump <- glmer(trump ~ romney + degree + median_hh_income + black + hispanic + evangelical  + (1|gender) + (1|age) + (1|race) + (1|educ) + (1|state) + (1|age:educ) + (1|gender:educ) + (1|race:age) + (1|race:educ) + (1|race:gender) + (1|race:region),indiv,family=binomial)
summary(res.trump)
ranef(res.trump)

post$trump <- predict(res.trump,post,allow.new.levels=T,type='response')

res.turnout <- glmer(voted ~ vap2012 + degree + median_hh_income + black + hispanic + evangelical + (1|gender) + (1|age) + (1|race) + (1|educ) + (1|state) + (1|age:educ) + (1|race:age) + (1|race:educ) + (1|race:gender) + (1|race:region),turnout,family=binomial)
summary(res.turnout)
ranef(res.turnout)

post$turnout <- predict(res.turnout,post,allow.new.levels=T,type='response')

post %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  summarise(clinton = wtd.mean(clinton,n_votes,na.rm=T),
            trump = wtd.mean(trump,n_votes,na.rm=T),
            turnout = wtd.mean(turnout,n,na.rm=T))
