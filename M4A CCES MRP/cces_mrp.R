library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(ggrepel)
library(scales)

indiv <- read_dta('individual_data.dta') %>%
  mutate(state = as_factor(statefip))
pstrat <- read_dta('poststrat_data.dta') %>%
  mutate(state = as_factor(statefip),
         faminc=factor(faminc,levels=c('<$50,000','$50,000-$100,000','>$100,000')))
context <- read_csv('contextual_data.csv')

df <- inner_join(indiv,select(context,-statefip),by='state') %>%
  mutate(region = recode(as.numeric(region),`1`='Northeast',`2`='Midwest',`3`='South',`4`='West',.default=NA_character_)) %>%
  mutate(region = factor(region,levels=c('Northeast','Midwest','South','West'))) %>%
  mutate_at(vars(educ,faminc,race,age_group,region),~as_factor(.x))

data <- inner_join(pstrat,select(context,-state),by='statefip') %>%
  mutate_at(vars(educ,faminc,race,age_group,region),~as_factor(.x))
df %>%
  group_by(state) %>%
  summarise(statefip = mean(statefip)) %>%
  right_join(select(data,-state),by='statefip') -> data
data <- data[complete.cases(data),]

reg.m4a <- glmer(m4a ~ log(median_hh_income) + degree + nh_white + nh_black + hispanic + dem16 + evangelical + (1|sex) + (1|educ) + (1|faminc) + (1|race) + (1|age_group) + (1|state) + (1|region),df,family=binomial)
summary(reg.m4a)
data$pred.m4a <- predict(reg.m4a,data,type='response')

reg.abortion <- glmer(abortion ~ log(median_hh_income) + degree + nh_white + nh_black + hispanic + dem16 + evangelical + (1|sex) + (1|educ) + (1|faminc) + (1|race) + (1|age_group) + (1|state) + (1|region),df,family=binomial)
summary(reg.abortion)
data$pred.abortion <- predict(reg.abortion,data,type='response')

reg.tax <- glmer(corp_tax ~ log(median_hh_income) + degree + nh_white + nh_black + hispanic + dem16 + evangelical + (1|sex) + (1|educ) + (1|faminc) + (1|race) + (1|age_group) + (1|state) + (1|region),df,family=binomial)
summary(reg.tax)
data$pred.tax <- predict(reg.tax,data,type='response')

reg.wall <- glmer(border_wall ~ log(median_hh_income) + degree + nh_white + nh_black + hispanic + dem16 + evangelical + (1|sex) + (1|educ) + (1|faminc) + (1|race) + (1|age_group) + (1|state) + (1|region),df,family=binomial)
summary(reg.wall)
data$pred.wall <- predict(reg.wall,data,type='response')



data %>%
  group_by(state) %>%
  summarise(m4a = wtd.mean(pred.m4a,n,na.rm=T),
            abortion = wtd.mean(pred.abortion,na.rm=T),
            tax = wtd.mean(pred.tax,na.rm=T),
            wall = wtd.mean(pred.wall,na.rm=T)) %>%
  write_csv('mrp_results.csv')
