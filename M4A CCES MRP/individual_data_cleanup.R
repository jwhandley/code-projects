library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

cces <- read_dta('cces18_common_vv.dta')

cces %>%
  filter(faminc_new < 97,region<=4) %>%
  mutate(m4a = as.numeric(CC18_327a==1),
         gun_control = as.numeric(CC18_320c==1),
         abortion = as.numeric(CC18_321a==1),
         border_wall = as.numeric(CC18_322a==1),
         corp_tax = as.numeric(CC18_325a==1),
         income_tax = as.numeric(CC18_325f_new==1),
         aca = as.numeric(CC18_327c==1),
         age = 2018 - birthyr,
         race = recode(as.numeric(race),`1`='White',`2`='Black',`3`='Hispanic',`98`=NA_character_,`99`=NA_character_,.default='Other'),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         region = as_factor(region),
         faminc = case_when(faminc_new < 6 ~ '<$50,000',
                            faminc_new >= 6 & faminc_new < 10 ~ '$50,000-$100,000',
                            faminc_new >= 10 & faminc_new <= 16 ~ '>$100,000',
                            TRUE ~ NA_character_),
         faminc = factor(faminc,levels=c('<$50,000','$50,000-$100,000','>$100,000')),
         age_group = case_when(age >= 18 & age < 25 ~ '18-24',
                               age >= 25 & age < 35 ~ '25-34',
                               age >= 35 & age < 45 ~ '35-44',
                               age >= 45 & age < 55 ~ '45-54',
                               age >= 55 & age < 65 ~ '55-64',
                               age >= 65 ~ '65+'),
         age_group = factor(age_group,levels=c('18-24','25-34','35-44','45-54','55-64','65+')),
         statefip = inputstate,
         educ = case_when(educ <= 2 ~ 'High school',
                          educ > 2 & educ <= 4 ~ 'Some college',
                          educ > 4 & educ < 8 ~ 'Degree',
                          TRUE ~ NA_character_),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         sex = as.numeric(gender == 2)) %>%
  select(m4a,gun_control,abortion,border_wall,corp_tax,income_tax,aca,sex,educ,faminc,race,age_group,statefip,region,commonweight) -> reg_data

reg_data %>%
  write_dta('individual_data.dta')

res <- glmer(m4a ~ as_factor(educ) + faminc + race + age_group + (1|region), reg_data, family=binomial)
summary(res)