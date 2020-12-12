library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('usa_00067.dta')

df %>%
  filter(age>=18) %>%
  mutate(region_code = round(region/10),
         region = recode(region_code,`1`='Northeast',`2`='Midwest',`3`='South',`4`='West'),
         region = factor(region,levels=c('Northeast','Midwest','South','West')),
         sex = as.numeric(sex==2),
         race = case_when(race == 1 & hispan == 0 ~ 'White',
                          race == 2 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         educ = case_when(educ <= 6 ~ 'High school',
                          educ > 6 & educ <= 9 ~ 'Some college',
                          educ > 9 ~ 'Degree'),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         age_group = case_when(age >= 18 & age < 25 ~ '18-24',
                               age >= 25 & age < 35 ~ '25-34',
                               age >= 35 & age < 45 ~ '35-44',
                               age >= 45 & age < 55 ~ '45-54',
                               age >= 55 & age < 65 ~ '55-64',
                               age >= 65 ~ '65+'),
         age_group = factor(age_group,levels=c('18-24','25-34','35-44','45-54','55-64','65+')),
         faminc = case_when(ftotinc < 50000 ~ '<$50,0000',
                            ftotinc >= 50000 & ftotinc < 100000 ~ '$50,000-$100,000',
                            ftotinc > 100000 ~ '>$100,000')) %>%
  group_by(statefip,region,sex,educ,faminc,race,age_group) %>%
  summarise(n = sum(perwt)) -> ps_data

ps_data %>%
  write_dta('poststrat_data.dta')
