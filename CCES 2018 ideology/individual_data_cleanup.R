library(tidyverse)
library(haven)
library(Hmisc)


cces <- read_dta('cces18_common_vv.dta')

cces %>%
  filter(faminc_new < 97,region<=4) %>%
  mutate(m4a = as.numeric(CC18_327a==1),
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
  mutate(gun_control1 = CC18_320a,gun_control2 = CC18_320c,gun_control3 = CC18_320d,
         abortion1 = CC18_321a,abortion2 = CC18_321b,abortion3 = CC18_321c,abortion4 = CC18_321d,abortion5 = CC18_321e,abortion6 = CC18_321f,
         immigration1 = CC18_322a,immigration2 = CC18_322b,immigration3 = CC18_322c_new,immigration4 = CC18_322d_new,immigration5 = CC18_322c,immigration6 = CC18_322f,
         tax1 = CC18_325a,tax2 = CC18_325b,tax3 = CC18_325c,tax4 = CC18_325d,tax5 = CC18_325e_new,tax6 = CC18_325f_new,
         health1 = CC18_327a,health2 = CC18_327c,health3 = CC18_327d,health4 = CC18_327e) %>%
  mutate_at(vars(gun_control1,gun_control2,gun_control3,abortion1,abortion2,abortion3,abortion4,abortion5,abortion6,immigration1,immigration1,immigration2,immigration3,immigration4,immigration5,immigration6,tax1,tax2,tax3,tax4,tax5,tax6,health1,health2,health3,health4),~as.numeric(.x==1)) %>%
  dplyr::select(m4a,gun_control1,gun_control2,gun_control3,abortion1,abortion2,abortion3,abortion4,abortion5,abortion6,immigration1,immigration1,immigration2,immigration3,immigration4,immigration5,immigration6,tax1,tax2,tax3,tax4,tax5,tax6,health1,health2,health3,health4,sex,educ,faminc,race,age_group,statefip,region,commonweight) -> reg_data

reg_data %>%
  write_dta('individual_data.dta')
