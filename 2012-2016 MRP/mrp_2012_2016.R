library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

# Contextual data in 2016
context16 <- read_csv('context16.csv')
# 2016 Exit poll
df16 <- read_spss('31116396_National2016.por') %>%
  mutate(educ = as_factor(EDUC12R),
         race = as_factor(RACE),
         age = as_factor(AGE),
         income = recode(as.numeric(INCOME16),`1`='<$30k',`2`='$30k-$50k',`3`='$50k-$100k',`4`='>$100k',`5`='>$100k',`6`='>$100k',.default=NA_character_),
         state = str_trim(as_factor(STANUM)),
         sex = as_factor(SEX),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  inner_join(context16) %>%
  filter(!is.na(race),!is.na(income)) %>%
  mutate(income = factor(income,levels=c('<$30k','$30k-$50k','$50k-$100k','>$100k'))) %>%
  select(vote,age,educ,race,income,sex,state,region,degree,black,hispanic,dem16)

# Contextual data in 2012
context12 <- read_csv('context12.csv')
# 2012 Exit poll
df12 <- read_spss('usmi2012-natelec.por') %>%
  mutate(educ = case_when(EDUC10 <=2 ~ 'High school or less',
                          EDUC10 == 3 ~ 'Some college/assoc. degree',
                          EDUC10 == 4 ~ 'College graduate',
                          EDUC10 == 5 ~ 'Postgraduate study'),
         race = as_factor(RACE),
         age = as_factor(AGE),
         income = recode(as.numeric(INCOME12),`1`='<$30k',`2`='$30k-$50k',`3`='$50k-$100k',`4`='>$100k',`5`='>$100k',`6`='>$100k'),
         state = str_trim(as_factor(STANUM)),
         sex = as_factor(SEX),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  mutate(educ = factor(educ,levels=levels(df16$educ))) %>%
  inner_join(context12) %>%
  filter(!is.na(race),!is.na(income)) %>%
  mutate(income = factor(income,levels=c('<$30k','$30k-$50k','$50k-$100k','>$100k'))) %>%
  select(vote,age,educ,race,income,sex,state,region,degree,black,hispanic,dem12)

# Turnout in 2016
turnout16 <- read_dta('cps_00189.dta') %>%
  inner_join(context16) %>%
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
         educ = case_when(educ <= 73 ~ 'High school or less',
                          educ > 73 & educ < 110 ~ 'Some college/assoc. degree',
                          educ >= 110 & educ <= 111 ~ 'College graduate',
                          educ > 110 ~ 'Postgraduate study'),
         income = case_when(faminc <= 710 ~ '<$30k',
                            faminc >= 720 & faminc <= 740 ~ '$30k-$50k',
                            faminc >= 820 & faminc <= 841 ~ '$50k-$100k',
                            faminc >= 842 & faminc <= 843 ~ '>$100k',
                            TRUE ~ NA_character_),
         voted = as.numeric(voted==2)) %>%
  mutate(age = factor(age,levels=levels(df16$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df16$race)),
         educ = factor(educ,levels=levels(df16$educ)),
         income = factor(income,levels=levels(df16$income))) %>%
  select(voted,age,sex,educ,race,income,state,vap16,degree,black,hispanic,region)

# Turnout in 2012
turnout12 <- read_dta('cps_00190.dta') %>%
  inner_join(context12) %>%
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
         educ = case_when(educ <= 73 ~ 'High school or less',
                          educ > 73 & educ < 110 ~ 'Some college/assoc. degree',
                          educ >= 110 & educ <= 111 ~ 'College graduate',
                          educ > 110 ~ 'Postgraduate study'),
         income = case_when(faminc <= 710 ~ '<$30k',
                            faminc >= 720 & faminc <= 740 ~ '$30k-$50k',
                            faminc >= 820 & faminc <= 841 ~ '$50k-$100k',
                            faminc >= 842 & faminc <= 843 ~ '>$100k',
                            TRUE ~ NA_character_),
         voted = as.numeric(voted==2)) %>%
  mutate(age = factor(age,levels=levels(df12$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df12$race)),
         educ = factor(educ,levels=levels(df12$educ)),
         income = factor(income,levels=levels(df12$income))) %>%
  select(voted,age,sex,educ,race,income,state,vap12,degree,black,hispanic,region)

census16 <- read_dta('usa_00078.dta') %>%
  filter(year==2016) %>%
  inner_join(context16) %>%
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
         educ = case_when(educ <= 6 ~ 'High school or less',
                          educ > 6 & educ < 10 ~ 'Some college/assoc. degree',
                          educ == 10 ~ 'College graduate',
                          educ > 10 ~ 'Postgraduate study'),
         income = cut(ftotinc,c(-Inf,30000,50000,100000,Inf),labels=levels(df16$income))) %>%
  mutate(age = factor(age,levels=levels(df16$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df16$race)),
         educ = factor(educ,levels=levels(df16$educ)),
         income = factor(income,levels=levels(df16$income))) %>%
  group_by(state,age,sex,race,income,educ) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context16) %>%
  select(state,region,age,sex,race,income,educ,n,degree,black,hispanic,dem16,vap16)

census12 <- read_dta('usa_00078.dta') %>%
  filter(year==2012) %>%
  inner_join(context12) %>%
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
         educ = case_when(educ <= 6 ~ 'High school or less',
                          educ > 6 & educ < 10 ~ 'Some college/assoc. degree',
                          educ == 10 ~ 'College graduate',
                          educ > 10 ~ 'Postgraduate study'),
         income = cut(ftotinc,c(-Inf,30000,50000,100000,Inf),labels=levels(df12$income))) %>%
  mutate(age = factor(age,levels=levels(df12$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df12$race)),
         educ = factor(educ,levels=levels(df12$educ)),
         income = factor(income,levels=levels(df12$income))) %>%
  group_by(state,age,sex,race,income,educ) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context12) %>%
  select(state,region,age,sex,race,income,educ,n,degree,black,hispanic,dem12,vap12)

gc()

res.vote12 <- glmer(vote ~ degree + black + hispanic + dem12 + (1|age) + (1|educ) + (1|race) + (1|income) + (1|sex) + (1|state) + (1|region) + (1|age:educ) + (1|age:sex) + (1|educ:income) + (1|educ:race) + (1|sex:race) + (1|income:race) + (1|race:region) + (1|income:region) + (1|race:educ:region) + (1|race:income:region),df12,family=binomial)
summary(res.vote12)
census12$vote <- predict(res.vote12,census12,type='response',allow.new.levels=T)

save.image('2012_vote_model.RData')

gc()

res.vote16 <- glmer(vote ~ degree + black + hispanic + dem16 + (1|age) + (1|educ) + (1|race) + (1|income) + (1|sex) + (1|state) + (1|educ:income) + (1|race:age) + (1|race:educ) + (1|race:income) + (1|race:region) + (1|age:income) + (1|income:region) + (1|sex:race) + (1|race:educ:region),df16,family=binomial)
summary(res.vote16)
census16$vote <- predict(res.vote16,census16,type='response',allow.new.levels=T)

save.image('2012_2016_vote_models.RData')

gc()

res.turnout12 <- glmer(voted ~ degree + black + hispanic + vap12 + (1|age) + (1|educ) + (1|race) + (1|income) + (1|sex) + (1|state) + (1|age:educ) + (1|race:region) + (1|race:state) + (1|income:state) + (1|educ:state),turnout12,family=binomial)
summary(res.turnout12)
census12$turnout <- predict(res.turnout12,census12,type='response',allow.new.levels=T)

gc()

res.turnout16 <- glmer(voted ~ degree + black + hispanic + vap16 + (1|age) + (1|educ) + (1|race) + (1|income) + (1|sex) + (1|state) + (1|age:educ) + (1|race:region) + (1|race:state) + (1|income:state) + (1|educ:state),turnout16,family=binomial)
summary(res.turnout16)
census16$turnout <- predict(res.turnout16,census16,type='response',allow.new.levels=T)

census12 %>%
  group_by(state) %>%
  summarise(vote12 = wtd.mean(vote,n*turnout),
            vote.actual12 = mean(dem12),
            turnout12 = wtd.mean(turnout,n),
            turnout.actual12 = mean(vap12)) -> vote12

census16 %>%
  group_by(state) %>%
  summarise(vote16 = wtd.mean(vote,n*turnout),
            vote.actual16 = mean(dem16),
            turnout16 = wtd.mean(turnout,n)) -> vote16

vote12 %>%
  inner_join(vote16) %>%
  write_csv('mrp_state_results.csv')
