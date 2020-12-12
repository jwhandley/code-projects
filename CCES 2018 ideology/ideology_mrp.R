library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(brms)

indiv <- read_dta('individual_data_adj.dta') %>%
  mutate_at(vars(educ,race,age_group,faminc,state),~as_factor(.x))
post <- read_dta('poststrat_data.dta')
context <- read_csv('contextual_data.csv')

post.data <- inner_join(post,context) %>%
  mutate(faminc = factor(faminc,levels=c('<$50,000','$50,000-$100,000','>$100,000'))) %>%
  mutate_at(vars(educ,race,age_group,state),~as_factor(.x)) %>%
  filter_at(vars(sex,educ,race,age_group,faminc,state),~!is.na(.x)) %>%
  mutate_at(vars(nh_white,nh_black,hispanic,dem16,evangelical,degree),~.x/100)

reg.m4a <- glmer(m4a ~ dem16 + nh_black + hispanic + evangelical + (1|sex) + (1|age_group) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv,family=binomial)
summary(reg.m4a)
post.data$m4a <- predict(reg.m4a,type='response',post.data)

reg.ideology <- lmer(ideology ~ dem16 + nh_black + hispanic + evangelical + (1|sex) + (1|age_group) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv)
summary(reg.ideology)
post.data$ideology <- predict(reg.ideology,post.data)

reg.health <- lmer(health ~ dem16 + nh_black + hispanic + (1|age_group) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv)
summary(reg.health)
post.data$health <- predict(reg.health,post.data)

reg.tax <- lmer(tax ~ dem16 + nh_black + hispanic + evangelical + log(median_hh_income) + (1|sex) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv)
summary(reg.tax)
post.data$tax <- predict(reg.tax,post.data)

reg.immigration <- lmer(immigration ~ dem16 + nh_black + hispanic + evangelical + (1|sex) + (1|age_group) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ),data=indiv)
summary(reg.immigration)
post.data$immigration <- predict(reg.immigration,post.data)

reg.abortion <- lmer(abortion ~ dem16 + nh_black + hispanic + evangelical + (1|sex) + (1|age_group) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|sex:educ) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv)
summary(reg.abortion)
post.data$abortion <- predict(reg.abortion,post.data)

reg.gun <- lmer(gun_control ~ dem16 + nh_black + hispanic + evangelical + (1|sex) + (1|race) + (1|educ) + (1|faminc) + (1|state) + (1|race:age_group) + (1|race:educ) + (1|race:sex),data=indiv)
summary(reg.gun)
post.data$gun_control <- predict(reg.gun,post.data)

post.data %>%
  group_by(state) %>%
  summarise_at(vars(ideology,health,tax,immigration,abortion,gun_control),~wtd.mean(.x,n,na.rm=T)) %>%
  mutate_at(vars(ideology,health,tax,immigration,abortion,gun_control),~scale(.x)[,1]) %>%
  mutate(left_right = scale(tax - health)[,1],
         lib_con = scale(immigration + abortion + gun_control)[,1]) %>%
  write_csv('state_ideology_mrp.csv')

post.data %>%
  group_by(state) %>%
  summarise(m4a = wtd.mean(m4a,n,na.rm=T)) %>%
  write_csv('medicare_for_all.csv')
