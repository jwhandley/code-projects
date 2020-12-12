library(tidyverse)
library(scales)
library(Hmisc)

df <- read_csv('gss_extract.csv')

df %>%
  mutate(redist = case_when(EQWLTH == 0 ~ NA_real_,
                            EQWLTH == 8 ~ NA_real_,
                            EQWLTH == 9 ~ NA_real_,
                            TRUE ~ EQWLTH),
         class = case_when(CLASS == 1 ~ 'Lower class',
                           CLASS == 2 ~ 'Working class',
                           CLASS == 3 ~ 'Middle class',
                           CLASS == 4 ~ 'Upper class',
                           TRUE ~ NA_character_),
         year = YEAR,
         age = ifelse(AGE < 98, AGE, NA_real_),
         sex = recode(SEX,`1`='Male',`2`='Female'),
         race = recode(RACE,`1`='White',`2`='Black',`3`='Other',.default=NA_character_),
         hispanic = case_when(HISPANIC > 1 & HISPANIC < 98 ~ 1,
                              HISPANIC == 1 ~ 0,
                              TRUE ~ NA_real_),
         educ = case_when(EDUC <= 12 ~ 'High school',
                          EDUC > 12 & EDUC < 16 ~ 'Some college',
                          EDUC >= 16 ~ 'Degree'),
         hompop = ifelse(HOMPOP < 98, HOMPOP, NA_real_),
         coninc = ifelse(CONINC > 0 & CONINC < 999998, CONINC, NA_real_),
         faminc = coninc/sqrt(hompop),
         pid7 = ifelse(PARTYID < 7, PARTYID+1, NA_real_),
         pid3 = case_when(pid7 < 4 ~ 'Democrat',
                          pid7 > 4 ~ 'Republican',
                          pid7 == 4 ~ 'Independent'),
         polviews = ifelse(POLVIEWS > 0 & POLVIEWS < 8, POLVIEWS, NA_real_),
         relig = case_when(RELIG == 1 ~ 'Protestant',
                           RELIG == 2 ~ 'Catholic',
                           RELIG == 4 ~ 'None',
                           RELIG == 0 ~ NA_character_,
                           RELIG == 98 ~ NA_character_,
                           RELIG == 99 ~ NA_character_,
                           TRUE ~ 'Other'),
         fund = case_when(FUND == 1 ~ 'Fundamentalist',
                          FUND == 2 ~ 'Moderate',
                          FUND == 3 ~ 'Liberal',
                          TRUE ~ NA_character_),
         wt = COMPWT) %>%
  group_by(year) %>%
  mutate(quintile = cut(faminc,wtd.quantile(faminc,wt,seq(0,1,0.2)),c('Bottom quintile','Second quintile','Middle quintile','Fourth quintile','Top quintile'))) %>%
  ungroup() %>%
  mutate(race = factor(race,levels=c('White','Black','Other')),
         relig = factor(relig,levels=c('Protestant','Catholic','Other')),
         fund = factor(fund,levels=c('Moderate','Liberal','Fundamentalist')),
         educ = factor(educ,levels=c('Some college','High school','Degree')),
         class = factor(class,levels=c('Working class','Lower class','Middle class','Upper class'))) %>%
  select(year,wt,sex,age,redist,class,race,hispanic,educ,faminc,quintile,pid7,pid3,polviews,relig,fund) -> data

res <- lm(redist ~ race + class + log(faminc) + relig + fund + sex + age + educ,data)
summary(res)
