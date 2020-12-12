library(tidyverse)
library(Hmisc)
library(haven)

census <- read_dta('usa_00068.dta')
context <- read_csv('contextual_data.csv')

census %>%
  filter(age>=18) %>%
  mutate(gender = as.numeric(sex==2),
         race = case_when(race == 1 & hispan == 0 ~ 'White',
                          race == 2 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         educ = case_when(educ <= 6 ~ 'High school',
                          educ > 6 & educ <= 9 ~ 'Some college',
                          educ > 9 ~ 'Degree'),
         faminc = cut(ftotinc,wtd.quantile(ftotinc,perwt,probs=seq(0,1,1/3)),labels=c('Low income','Middle income','High income')),
         age_group = case_when(age <= 24 ~ '17-24',
                               age >= 25 & age < 35 ~ '25-34',
                               age >= 35 & age < 45 ~ '35-44',
                               age >= 45 & age < 55 ~ '45-54',
                               age >= 55 & age < 65 ~ '55-64',
                               age >= 65 & age < 75 ~ '65-74',
                               age >= 75 ~ '75+')) %>%
  mutate(faminc = factor(faminc,levels=c('Low income','Middle income','High income')),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age_group = factor(age_group,levels=c('17-24','25-34','35-44','45-54','55-64','65-74','75+'))) %>%
  group_by(statefip,gender,race,educ,faminc,age_group) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context) %>%
  mutate(race.state = interaction(race,state),
         race.gender = interaction(race,gender),
         educ.gender = interaction(educ,gender)) -> pstrat

pstrat %>%
  write_dta('pstrat_data.dta')
