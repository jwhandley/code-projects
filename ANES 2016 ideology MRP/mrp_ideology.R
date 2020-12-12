library(tidyverse)
library(Hmisc)
library(lme4)
library(ggrepel)

anes <- read_csv('anes_extract.csv')
data <- read_csv('contextual_data.csv')
ps_frame <- read_csv('post_strat.csv')

post_strat <- inner_join(ps_frame,data)

df <- inner_join(anes,data,by='statefip') %>%
  mutate(gender = as.numeric(gender==2),
         age_cat = recode(age_cat,
                          `1`='17-24',
                          `2`='25-34',
                          `3`='35-44',
                          `4`='45-54',
                          `5`='55-64',
                          `6`='65-74',
                          `7`='75+',
                          .default=NA_character_),
         race = recode(race,
                       `1`='White non-hispanic',
                       `2`='Black non-hispanic',
                       `3`='Hispanic',
                       `4`='Other',
                       .default=NA_character_),
         educ = recode(educ,
                       `1`='High school',
                       `2`='High school',
                       `3`='Some college',
                       `4`='Degree',
                       .default=NA_character_)) %>%
  mutate(age_cat = factor(age_cat,levels=c('17-24','25-34','35-44','45-54','55-64','75+')),
         race = factor(race,levels=c('White non-hispanic','Black non-hispanic','Hispanic','Other')),
         educ = factor(educ,levels=c('High school','Some college','Degree'))) %>%
  mutate_at(vars(gov_jobs_income,gov_healthcare,aid_blacks),~-scale(.x)[,1]) -> df

res.jobs <- lmer(gov_jobs_income ~ age_cat + gender + race + educ + high_school + some_col + degree + nh_white + nh_black + hispanic + log(median_hh_income) + dem16 + evangelical + (1|state),df)
post_strat$pred_jobs <- predict(res.jobs,post_strat)

res.hc <- lmer(gov_healthcare ~ age_cat + gender + race + educ + high_school + some_col + degree + nh_white + nh_black + hispanic + log(median_hh_income) + dem16 + evangelical + (1|state),df)
post_strat$pred_hc <- predict(res.hc,post_strat)

res.aid <- lmer(aid_blacks ~ age_cat + gender + race + educ + high_school + some_col + degree + nh_white + nh_black + hispanic + log(median_hh_income) + dem16 + evangelical + (1|state),df)
post_strat$pred_aid <- predict(res.aid,post_strat)

post_strat %>%
  group_by(state) %>%
  summarise(gov_jobs_income = wtd.mean(pred_jobs,n,na.rm=T),
            gov_healthcare = wtd.mean(pred_hc,n,na.rm=T),
            aid_blacks = wtd.mean(pred_aid,n,na.rm=T)) %>%
  write_csv('mrp_results.csv')

post_strat %>%
  filter(state != 'Hawaii',
         state != 'District of Columbia') %>%
  group_by(state) %>%
  summarise(gov_jobs_income = wtd.mean(pred_jobs,n,na.rm=T),
            gov_healthcare = wtd.mean(pred_hc,n,na.rm=T),
            aid_blacks = wtd.mean(pred_aid,n,na.rm=T)) %>%
  ggplot(aes(x=gov_jobs_income,y=aid_blacks,label=state)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method='lm') +
  labs(x='Guaranteed jobs and income',
       y='Aid to minorities',
       title='State-level ideology')

