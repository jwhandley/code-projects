library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(blme)

df <- read_dta('anes_timeseries_cdf.dta') %>%
  filter(VCF0004==2016)

turnout <- read_dta('cps_00177.dta')

pstrat <- read_dta('pstrat_data.dta')
context <- read_csv('contextual_data.csv')

pstrat <- inner_join(pstrat,context) %>%
  mutate_at(vars(faminc,race,educ,age_group),~as_factor(.x)) %>%
  filter_at(vars(faminc,race,educ,age_group),~!is.na(.x))

turnout %>%
  filter(faminc < 995,age>=18,voted<96,educ<999) %>%
  mutate(faminc = case_when(faminc <= 720 ~ 'Low income',
                            faminc > 720 & faminc <= 830 ~ 'Middle income',
                            faminc > 830 ~ 'High income'),
         educ = case_when(educ <= 73 ~ 'High school',
                          educ > 73 & educ <= 100 ~ 'Some college',
                          educ > 100 ~ 'Degree'),
         race = case_when(race == 100 & hispan == 0 ~ 'White',
                          race == 200 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         age_group = case_when(age <= 24 ~ '17-24',
                               age >= 25 & age < 35 ~ '25-34',
                               age >= 35 & age < 45 ~ '35-44',
                               age >= 45 & age < 55 ~ '45-54',
                               age >= 55 & age < 65 ~ '55-64',
                               age >= 65 & age < 75 ~ '65-74',
                               age >= 75 ~ '75+'),
         gender = as.numeric(sex==2),
         vote = as.numeric(voted == 2)) %>%
  mutate(faminc = factor(faminc,levels=c('Low income','Middle income','High income')),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age_group = factor(age_group,levels=c('17-24','25-34','35-44','45-54','55-64','65-74','75+'))) %>%
  select(statefip,faminc,race,educ,age_group,gender,vote) %>%
  inner_join(context) -> turnout
  
  

df %>%
  mutate(faminc = recode(as.numeric(VCF0114),`1`='Low income',`2`='Low income',`3`='Middle income',`4`='High income',`5`='High income'),
         educ = recode(as.numeric(VCF0110),`1`='High school',`2`='High school',`3`='Some college',`4`='Degree',.default=NA_character_),
         race = recode(as.numeric(VCF0105b),`1`='White',`2`='Black',`3`='Hispanic',`4`='Other',.default=NA_character_),
         age_group = recode(as.numeric(VCF0102),`1`='17-24',`2`='25-34',`3`='35-44',`4`='45-54',`5`='55-64',`6`='65-74',`7`='75+',.default=NA_character_),
         gender = as.numeric(VCF0104==2),
         libcon = VCF0803,
         gov_jobs = VCF0809,
         aid_blacks = VCF0830,
         pid7 = VCF0301,
         wgt = VCF0009x,
         state = VCF0901b,
         statefip = VCF0901a,
         vote = recode(as.numeric(VCF0706),`1`='Clinton',`2`='Trump',`3`='Other',`4`='Other',`7`="Didn't vote")) %>%
  mutate(faminc = factor(faminc,levels=c('Low income','Middle income','High income')),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age_group = factor(age_group,levels=c('17-24','25-34','35-44','45-54','55-64','65-74','75+')),
         vote = factor(vote,levels=c('Clinton','Trump','Other',"Didn't vote")),
         turnout = 1-as.numeric(vote=="Didn't vote"),
         vote2 = recode(vote,`Clinton`=1,`Trump`=0,.default=NA_real_),
         clinton = ifelse(turnout==1,as.numeric(vote=='Clinton'),NA_real_),
         trump = ifelse(turnout==1,as.numeric(vote=='Trump'),NA_real_)) %>%
  mutate_at(vars(libcon,gov_jobs,aid_blacks),~ifelse(.x > 7, NA_real_,as.numeric(.x))) %>%
  filter_at(vars(state,faminc,educ,race,age_group,gender,vote),~!is.na(.x)) %>%
  select(wgt,statefip,faminc,educ,race,age_group,gender,libcon,gov_jobs,aid_blacks,pid7,clinton,trump,vote,turnout,vote2) %>%
  inner_join(context) -> data


res.turnout <- bglmer(vote ~ vap2012 + (1|gender) + (1|age_group) + (1|race) + (1|educ) + (1|state) + (1|race:gender) + (1|race:region),turnout,family=binomial)
summary(res.turnout)
pstrat$turnout <- predict(res.turnout,pstrat,type='response',allow.new.levels=T)

res.clinton <- bglmer(clinton ~ obama + black + hispanic + evangelical + (1|gender) + (1|age_group) + (1|race) + (1|educ) + (1|state) + (1|region) + (1|age:educ) + (1|gender:educ) + (1|race:age_group) + (1|race:educ) + (1|race:gender) + (1|race:region),data,family=binomial)
summary(res.clinton)
pstrat$clinton <- predict(res.clinton,pstrat,type='response',allow.new.levels=T)

res.trump <- bglmer(trump ~ romney + black + hispanic + evangelical + (1|gender) + (1|age_group) + (1|race) + (1|educ) + (1|state) + (1|region) + (1|age:educ) + (1|gender:educ) + (1|race:age_group) + (1|race:educ) + (1|race:gender) + (1|race:region),data,family=binomial)
summary(res.trump)
pstrat$trump <- predict(res.trump,pstrat,type='response',allow.new.levels=T)

res.vote2 <- bglmer(vote2 ~ I(obama - romney) + black + hispanic + evangelical + (1|gender) + (1|age_group) + (1|race) + (1|educ) + (1|state) + (1|region) + (1|age:educ) + (1|gender:educ) + (1|race:age_group) + (1|race:educ) + (1|race:gender) + (1|race:region),data,family=binomial)
summary(res.vote2)
pstrat$clinton2p <- predict(res.vote2,pstrat,type='response',allow.new.levels=T)

pstrat %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  summarise(trump = wtd.mean(trump,n_votes,na.rm=T),
            clinton = wtd.mean(clinton,n_votes,na.rm=T),
            clinton2p = wtd.mean(clinton2p,n_votes,na.rm=T),
            turnout = wtd.mean(turnout,n,na.rm=T)) %>%
  write_csv('mrp_results.csv')
  