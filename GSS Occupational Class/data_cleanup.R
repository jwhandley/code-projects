library(tidyverse)
library(Hmisc)

gss <- read_csv('gss_extract.csv') %>%
  mutate(redist = ifelse(EQWLTH > 0 & EQWLTH < 8,EQWLTH,NA_real_),
         wgt = COMPWT,
         isco08 = ifelse(ISCO08 > 0 & ISCO08 < 9998,ISCO08,NA_real_),
         year = YEAR,
         age = AGE,
         sex = as.numeric(SEX==1),
         race = case_when(RACE == 1 ~ 'White',
                          RACE == 2 ~ 'Black',
                          RACE == 3 ~ 'Other',
                          TRUE ~ NA_character_),
         educ = ifelse(EDUC < 97, EDUC, NA_real_),
         hompop = ifelse(HOMPOP < 98, HOMPOP,NA_real_),
         income = ifelse(CONINC > 0 & CONINC < 999998,CONINC,NA_real_),
         faminc = income/sqrt(hompop),
         pid7 = ifelse(PARTYID<7,PARTYID+1,NA_real_),
         polviews = ifelse(POLVIEWS > 0 & POLVIEWS < 8, POLVIEWS, NA_real_),
         relig = case_when(RELIG == 1 ~ 'Protestant',
                           RELIG == 2 ~ 'Catholic',
                           RELIG == 4 ~ 'None',
                           TRUE ~ 'Other'),
         fund = case_when(FUND == 1 ~ 'Fundamentalist',
                          FUND == 2 ~ 'Moderate',
                          FUND == 3 ~ 'Liberal',
                          TRUE ~ NA_character_),
         class = case_when(CLASS == 1 ~ 'Lower class',
                           CLASS == 2 ~ 'Working class',
                           CLASS == 3 ~ 'Middle class',
                           CLASS == 4 ~ 'Upper class',
                           TRUE ~ NA_character_),
         union = as.numeric(UNION > 0 & UNION < 4)) %>%
  select(year,wgt,sex,race,age,educ,isco08,faminc,class,relig,fund,union,pid7) %>%
  group_by(year) %>%
  mutate(quintile = cut(faminc,wtd.quantile(faminc,wgt,seq(0,1,0.2)),labels=c('Bottom quintile','Second quintile','Middle quintile','Third quintile','Top quintile'))) %>%
  ungroup() %>%
  mutate(religd = case_when(relig == 'Protestant' & fund == 'Fundamentalist' & race == 'White' ~ 'White evangelical protestant',
                            relig == 'Protestant' & fund == 'Liberal' ~ 'Mainline protestant',
                            relig == 'Protestant' & fund != 'Liberal' & race == 'Black' ~ 'Historical black protestant',
                            relig == 'Protestant' ~ 'Mainline protestant',
                            TRUE ~ relig))

oesch <- read_csv('oesch_crosswalk.csv')

df <- inner_join(gss,oesch) %>%
  mutate(oesch_class = case_when(oesch8 == 1 ~ 'Self-employed professionals and large employers',
                                 oesch8 == 2 ~ 'Small business owners',
                                 oesch8 == 3 ~ 'Technical professionals',
                                 oesch8 == 4 ~ 'Production workers',
                                 oesch8 == 5 ~ 'Managers',
                                 oesch8 == 6 ~ 'Clerks',
                                 oesch8 == 7 ~ 'Socio-cultural professionals',
                                 oesch8 == 8 ~ 'Service workers'))

res <- lm(pid7 ~ oesch_class + log(faminc) + religd + race + educ + age + sex + union,df)
summary(res)

df %>%
  write_csv('data_cleaned.csv')

