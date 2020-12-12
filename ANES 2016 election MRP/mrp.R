library(tidyverse)
library(haven)
library(Hmisc)
library(brms)

df <- read_dta('anes_timeseries_cdf.dta') %>%
  filter(VCF0004==2016)

pstrat <- read_dta('pstrat_data.dta')
context <- read_csv('contextual_data.csv')

pstrat_frame <- inner_join(pstrat,context) %>%
  mutate_at(vars(faminc,race,educ,age_group),~as_factor(.x)) %>%
  filter_at(vars(faminc,race,educ,age_group),~!is.na(.x))

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
         vote = recode(as.numeric(VCF0706),`1`='Clinton',`2`='Trump',`3`='Other',`4`='Other',`7`="Didn't vote",.default="Didn't vote")) %>%
  mutate(faminc = factor(faminc,levels=c('Low income','Middle income','High income')),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age_group = factor(age_group,levels=c('17-24','25-34','35-44','45-54','55-64','65-74','75+')),
         vote = factor(vote,levels=c('Clinton','Trump','Other',"Didn't vote")),
         turnout = replace_na(as.numeric(vote!="Didn't vote"),0),
         clinton = ifelse(turnout==1,as.numeric(vote=='Clinton'),NA_real_),
         trump = ifelse(turnout==1,as.numeric(vote=='Trump'),NA_real_),
         vote2 = ifelse(clinton+trump==1,clinton,NA_real_)) %>%
  mutate_at(vars(libcon,gov_jobs,aid_blacks),~ifelse(.x > 7, NA_real_,as.numeric(.x))) %>%
  filter_at(vars(state,faminc,educ,race,age_group,gender),~!is.na(.x)) %>%
  select(wgt,statefip,faminc,educ,race,age_group,gender,libcon,gov_jobs,aid_blacks,pid7,vote,clinton,trump,vote2,turnout) %>%
  inner_join(context) %>%
  mutate(race.state = interaction(race,state),
         race.gender = interaction(race,gender),
         educ.gender = interaction(educ,gender)) -> data

reg.turnout <- brm(turnout ~ (1|race) + (1|age_group) + (1|gender) + (1|educ) + (1|faminc) + (1|state) + vap2012,data,family=bernoulli(),cores = 6)
summary(reg.turnout)
pstrat_frame$turnout <- predict(reg.turnout,pstrat_frame,type='response',allow.new.levels=T)[,'Estimate']

reg.clinton <- brm(clinton ~ (1|race) + (1|age_group) + (1|gender) + (1|educ) + (1|faminc) + (1|state) + evangelical + black + hispanic + degree + obama,data,family=bernoulli())
summary(reg.clinton)
pstrat_frame$clinton <- predict(reg.clinton,pstrat_frame,type='response',allow.new.levels=T)[,'Estimate']

reg.trump <- brm(trump ~ (1|race) + (1|age_group) + (1|gender) + (1|educ) + (1|faminc) + (1|state) + degree + evangelical + black + hispanic + romney,data,family=bernoulli(),cores=3)
summary(reg.trump)
pstrat_frame$trump <- predict(reg.trump,pstrat_frame,type='response',allow.new.levels=T)[,'Estimate']

results <- read_csv('actual_results.csv') %>%
  select(state,turnout) %>%
  rename(actual_turnout = turnout)

pstrat_frame %>%
  group_by(state) %>%
  summarise(turnout = wtd.mean(turnout,n,na.rm=T)) %>%
  inner_join(results) %>%
  mutate(ratio = actual_turnout/turnout) %>%
  select(state,ratio) -> turnout_adjust

pstrat_frame %>%
  inner_join(turnout_adjust) %>%
  mutate(n_votes = turnout*n*ratio) %>%
  group_by(state) %>%
  summarise(turnout = wtd.mean(turnout*ratio,n,na.rm=T),
            clinton = wtd.mean(clinton,n_votes,na.rm=T),
            trump = wtd.mean(trump,n_votes,na.rm=T)) %>%
  write_csv('mrp_results.csv')


