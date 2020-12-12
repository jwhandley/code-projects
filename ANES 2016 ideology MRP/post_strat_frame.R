library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('usa_00066.dta')

df %>%
  filter(age >= 17) %>%
  mutate(race = case_when(race == 1 & hispan == 0 ~ 'White non-hispanic',
                          race == 2 & hispan == 0 ~ 'Black non-hispanic',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         educ = case_when(educ <= 6 ~ 'High school',
                          educ <= 9 ~ 'Some college',
                          educ >9 ~ 'Degree'),
         age_cat = case_when(age <= 24 ~ '17-24',
                             age >= 25 & age < 35 ~ '25-34',
                             age >= 35 & age < 45 ~ '35-44',
                             age >= 45 & age < 55 ~ '45-54',
                             age >= 55 & age < 65 ~ '55-64',
                             age >= 65 & age < 75 ~ '65-74',
                             age >= 75 ~ '75+'),
         gender = as.numeric(sex==2)) %>%
  mutate(age_cat = factor(age_cat,levels=c('17-24','25-34','35-44','45-54','55-64','75+')),
         race = factor(race,levels=c('White non-hispanic','Black non-hispanic','Hispanic','Other')),
         educ = factor(educ,levels=c('High school','Some college','Degree'))) %>%
  select(statefip,perwt,gender,age_cat,race,educ) %>%
  group_by(statefip,age_cat,gender,race,educ) %>%
  summarise(n = sum(perwt)) -> data

data %>%
  write_csv('post_strat.csv')
