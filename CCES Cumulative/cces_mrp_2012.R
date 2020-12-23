library(tidyverse)
library(haven)
library(Hmisc)
library(scales)
library(lme4)
library(maps)
library(mapproj)
library(cowplot)

context <- read_csv('context12.csv')
df <- read_dta('cumulative_2006-2019.dta') %>%
  filter(year==2012) %>%
  mutate(income = case_when(faminc <= 3 ~ '<$30k',
                            faminc > 3 & faminc <= 5 ~ '$30k-$50k',
                            faminc > 5 & faminc <= 9 ~ '$50k-$100k',
                            faminc > 9 & faminc <= 12 ~ '>$100k',
                            TRUE ~ NA_character_),
         race = case_when(race == 1 ~ 'White',
                          race == 2 ~ 'Black',
                          race == 3 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         age = case_when(age >= 18 & age < 30 ~ '18-29',
                               age >= 30 & age < 45 ~ '30-44',
                               age >= 45 & age <= 65 ~ '45-65',
                               age > 65 ~ '65+',
                               TRUE ~ NA_character_),
         sex = as_factor(gender),
         educ = case_when(educ <= 2 ~ 'High school or less',
                          educ > 2 & educ <= 4 ~ 'Some college',
                          educ == 5 ~ 'College graduate',
                          educ == 6 ~ 'Postgraduate study',
                          TRUE ~ NA_character_),
         state = as_factor(state),
         vote = ifelse(voted_pres_12 %in% c(1,2),as.numeric(voted_pres_12==1),NA_real_)) %>%
  mutate(income = factor(income,levels=c('<$30k','$30k-$50k','$50k-$100k','>$100k')),
         income.z = case_when(income == '<$30k' ~ 1,
                              income == '$30k-$50k' ~ 2,
                              income == '$50k-$100k' ~ 3,
                              income == '>$100k' ~ 4),
         educ = factor(educ,levels=c('High school or less','Some college','College graduate','Postgraduate study')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age = factor(age,levels=c('18-29','30-44','45-65','65+'))) %>%
  filter(!is.na(vote)) %>%
  select(state,income,income.z,race,age,sex,educ,vote) %>%
  inner_join(context)

turnout <- read_dta('cps_00190.dta') %>%
  inner_join(context) %>%
  mutate(age = case_when(age >= 18 & age < 30 ~ '18-29',
                         age >= 30 & age < 45 ~ '30-44',
                         age >= 45 & age <= 65 ~ '45-65',
                         age > 65 ~ '65+',
                         TRUE ~ NA_character_),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 2 ~ 'Female',
                         TRUE ~ NA_character_),
         race = case_when(race == 100 & hispan == 0 ~ 'White',
                          race == 200 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         educ = case_when(educ <= 73 ~ 'High school or less',
                          educ > 73 & educ < 110 ~ 'Some college',
                          educ >= 110 & educ <= 111 ~ 'College graduate',
                          educ > 110 ~ 'Postgraduate study'),
         income = case_when(faminc <= 710 ~ '<$30k',
                            faminc >= 720 & faminc <= 740 ~ '$30k-$50k',
                            faminc >= 820 & faminc <= 841 ~ '$50k-$100k',
                            faminc >= 842 & faminc <= 843 ~ '>$100k',
                            TRUE ~ NA_character_),
         voted = as.numeric(voted==2)) %>%
  mutate(age = factor(age,levels=levels(df$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df$race)),
         educ = factor(educ,levels=levels(df$educ)),
         income = factor(income,levels=levels(df$income)),
         income.z = case_when(income == '<$30k' ~ 1,
                              income == '$30k-$50k' ~ 2,
                              income == '$50k-$100k' ~ 3,
                              income == '>$100k' ~ 4)) %>%
  select(voted,age,sex,educ,race,income,income.z,state,vap12,degree,black,hispanic,evangelical,median_hh_income,region)


census <- read_dta('usa_00079.dta') %>%
  filter(year==2012) %>%
  inner_join(context) %>%
  mutate(age = case_when(age >= 18 & age < 30 ~ '18-29',
                         age >= 30 & age < 45 ~ '30-44',
                         age >= 45 & age <= 65 ~ '45-65',
                         age > 65 ~ '65+',
                         TRUE ~ NA_character_),
         sex = case_when(sex == 1 ~ 'Male',
                         sex == 2 ~ 'Female',
                         TRUE ~ NA_character_),
         race = case_when(race == 1 & hispan == 0 ~ 'White',
                          race == 2 & hispan == 0 ~ 'Black',
                          hispan != 0 ~ 'Hispanic',
                          TRUE ~ 'Other'),
         educ = case_when(educ <= 6 ~ 'High school or less',
                          educ > 6 & educ < 10 ~ 'Some college',
                          educ == 10 ~ 'College graduate',
                          educ > 10 ~ 'Postgraduate study'),
         income = cut(ftotinc,c(-Inf,30000,50000,100000,Inf),labels=levels(df$income))) %>%
  mutate(age = factor(age,levels=levels(df$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(df$race)),
         educ = factor(educ,levels=levels(df$educ)),
         income = factor(income,levels=levels(df$income)),
         income.z = case_when(income == '<$30k' ~ 1,
                              income == '$30k-$50k' ~ 2,
                              income == '$50k-$100k' ~ 3,
                              income == '>$100k' ~ 4)) %>%
  group_by(state,age,sex,race,income,educ) %>%
  summarise(n = sum(perwt),income.z=mean(income.z)) %>%
  inner_join(context) %>%
  select(state,region,age,sex,race,income,income.z,educ,n,degree,white,black,hispanic,evangelical,median_hh_income,dem12,vap12)

res.vote <- glmer(vote ~ dem12 + degree + black + hispanic + income.z*log(median_hh_income) + (1|region) + (1|state) + (1|age) + (1|sex) + (1|race) + (1|educ) + (1|race:educ) + (1|income:race),df,family=binomial)
summary(res.vote)
census$vote <- predict(res.vote,census,allow.new.levels=T,type='response')

res.turnout <- glmer(voted ~ vap12 + degree + black + hispanic + income.z + (1|age) + (1|sex) + (1|race) + (1|income) + (1|educ) + (1|race:educ) + (1|race:state),turnout,family=binomial)
summary(res.turnout)
census$turnout <- predict(res.turnout,census,allow.new.levels=T,type='response')

census %>%
  group_by(state) %>%
  summarise(dem.pred = wtd.mean(vote,n*turnout)-.0387,
            dem.act = mean(dem12),
            turnout.pred = wtd.mean(turnout,n),
            turnout.act = mean(vap12)) -> state.predictions

state.predictions %>%
  mutate(pred = as.numeric(dem.pred>0.5),
         act = as.numeric(dem.act>0.5),
         correct = as.numeric(pred==act),
         error = (dem.pred-dem.act)^2,
         abs.error = dem.pred-dem.act) %>%
  summarise(pred = sum(pred),
            act = sum(act),
            correct = sum(correct),
            rmse = sqrt(mean(error)),
            abs.error = mean(abs.error))

census %>%
  group_by(state) %>%
  mutate(pop = sum(n)) %>%
  group_by(state,race,income) %>%
  summarise(dem.pred = wtd.mean(vote,n*turnout),
            share = sum(n)/mean(pop)) %>%
  filter(share>=0.01) -> race.income

us.map <- map_data('state')

map.race.income <- left_join(us.map,mutate(race.income,region=tolower(state)))

map.race.income %>%
  filter(race=='White') %>%
  ggplot(aes(x=long,y=lat,group=region,fill=dem.pred)) +
  geom_polygon(color='grey',size=0.25) +
  coord_map('albers',45,39) +
  scale_fill_gradient2(midpoint=0.5) +
  theme_map() +
  facet_wrap(~income)

save.image('fitted_models.RData')
