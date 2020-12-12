library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

df <- read_dta('National2016.dta')
context <- read_csv('contextual_data.csv')

df %>%
  mutate(age = as_factor(AGE8),
         income = droplevels(as_factor(na_if(INCOME16,9))),
         educ = as_factor(EDUC12R),
         sex = as_factor(sex),
         race = as_factor(race),
         turnout = replace_na(as.numeric(pres != 0),0),
         oth_pres = as.numeric(pres != 0 & pres > 2),
         clinton = as.numeric(pres == 1),
         trump = as.numeric(pres == 2),
         vote = ifelse(clinton+trump==1,clinton,NA_real_),
         state = as_factor(stanum)) %>%
  select(state,age,income,educ,sex,race,turnout,oth_pres,clinton,trump,vote,weight) %>%
  inner_join(context) -> data

data %>%
  summarise_at(vars(turnout,oth_pres,clinton,trump,vote),~wtd.mean(.x,weight,na.rm=T))

res.vote <- glmer(vote ~ degree + median_hh_income + black + hispanic + evangelical + (1|age) + (1|educ) + (1|income) + (1|race) + (1|sex) + (1|race:educ) + (1|race:sex) + (1|state),data,family=binomial)
summary(res.vote)
ranef(res.vote)
