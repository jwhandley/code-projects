library(tidyverse)
library(Hmisc)
library(haven)
library(brms)
library(lme4)

df <- read_dta('marist_feb.dta')
turnout <- read_dta('cps_00177.dta')
census <- read_dta('usa_00068.dta')
context <- read_csv('contextual_data.csv')

turnout %>%
  filter(faminc < 995,age>=18,voted<96,educ<999) %>%
  mutate(income = case_when(faminc <= 740 ~ 'Less than $50,000',
                            faminc >= 800 ~ '$50,000 or more'),
         educ = as.numeric(educ>100),
         race = case_when(race == 100 & hispan == 0 ~ 'White',
                          race == 200 & hispan == 0 ~ 'African-American',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Asian-Other'),
         age = case_when(age <= 29 ~ '18 to 29',
                               age >= 30 & age < 45 ~ '30 to 44',
                               age >= 45 & age < 60 ~ '45 to 59',
                               age >= 60 ~ '60 or older'),
         gender = as.numeric(sex==2),
         vote = as.numeric(voted == 2)) %>%
  mutate(income = factor(income,levels=c('Less than $50,000','$50,000 or more')),
         race = factor(race,levels=c('White','African-American','Hispanic','Asian-Other')),
         age = factor(age,levels=c('18 to 29','30 to 44','45 to 59','60 or older'))) %>%
  select(statefip,income,race,educ,age,gender,vote) %>%
  inner_join(context) -> turnout

census %>%
  filter(age>=18) %>%
  mutate(gender = as.numeric(sex==2),
         race = case_when(race == 1 & hispan == 0 ~ 'White',
                          race == 2 & hispan == 0 ~ 'African-American',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Asian-Other'),
         educ = as.numeric(educ > 9),
         income = factor(ifelse(ftotinc>=50000,'$50,000 or more','Less than $50,000'),levels=c('Less than $50,000','$50,000 or more')),
         age = case_when(age <= 29 ~ '18 to 29',
                         age >= 30 & age < 45 ~ '30 to 44',
                         age >= 45 & age < 60 ~ '45 to 59',
                         age >= 60 ~ '60 or older')) %>%
  mutate(income = factor(income,levels=c('Less than $50,000','$50,000 or more')),
         race = factor(race,levels=c('White','African-American','Hispanic','Asian-Other')),
         age = factor(age,levels=c('18 to 29','30 to 44','45 to 59','60 or older'))) %>%
  group_by(statefip,gender,race,educ,income,age) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context) -> pstrat

df %>%
  mutate(sanders = recode(as.numeric(BSDT2020),`1`=1,`2`=0,.default=NA_real_),
         biden = recode(as.numeric(JBDT2020),`1`=1,`2`=0,.default=NA_real_),
         educ = recode(as.numeric(collegep),`1`=0,`2`=1,.default=NA_real_),
         income = droplevels(as_factor(na_if(INCREC50,9))),
         race = droplevels(as_factor(na_if(RACEWTL2,9))),
         gender = as.numeric(gender==2),
         age = droplevels(as_factor(na_if(ageepwt,9))),
         statefip = states,
         rv = replace_na(totalrv,0)) %>%
  filter_at(vars(educ,income,race,wtfactor),~!is.na(.x)) %>%
  select(sanders,biden,educ,income,race,gender,age,statefip,wtfactor,rv) %>%
  inner_join(context) -> data

res.turnout <- brm(vote ~ (1|educ) + (1|race) + (1|gender) + (1|income) + (1|educ:race) + (1|educ:gender) + (1|race:gender) + (1|state) + (1|race:state) + vap2012,turnout,family=bernoulli(),chains=4,cores=6)
summary(res.turnout)
#pstrat$turnout <- predict(res.turnout,pstrat,type='response',allow.new.levels=T)

res.sanders <- brm(sanders ~ (1|educ) + (1|race) + (1|gender) + (1|income) + (1|educ:race) + (1|educ:gender) + (1|race:gender) + (1|state) + (1|race:state) + median_hh_income + black + hispanic + evangelical + obama,data,family=bernoulli())
summary(res.sanders)
#pstrat$sanders <- predict(res.sanders,pstrat,type='response',allow.new.levels=T)

res.biden <- brm(biden ~ (1|educ) + (1|race) + (1|gender) + (1|income) + (1|educ:race) + (1|educ:gender) + (1|race:gender) + (1|state) + (1|race:state) + median_hh_income + black + hispanic + evangelical + obama,data,family=binomial)
summary(res.biden)
#pstrat$biden <- predict(res.biden,pstrat,type='response',allow.new.levels=T)


pstrat %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  summarise_at(vars(biden,sanders),~wtd.mean(.x,n_votes,na.rm = T)) %>%
  write_csv('mrp_results.csv')

pstrat %>%
  ungroup() %>%
  mutate(n_votes = n*turnout) %>%
  summarise_at(vars(biden,sanders),~wtd.mean(.x,n_votes,na.rm = T))
