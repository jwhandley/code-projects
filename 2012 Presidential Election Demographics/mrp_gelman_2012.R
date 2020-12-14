library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(mapproj)
library(scales)
library(cowplot)

# Load state-level contextual data
# Includes actual election vote share, share of population with a degree,
# median household income, racial composition, share of evangelicals, and region
context <- read_csv('context.csv')

# Individual response data from the 2012 exit poll
# I have state, race, education, age, sex, and income data
# but am only using race, income, and state data for this analysis
indiv <- read_spss('usmi2012-natelec.por') %>%
  mutate(state = str_trim(as_factor(STANUM))) %>%
  inner_join(context) %>%
  mutate(race = as_factor(RACE),
         educ = as_factor(EDUC10),
         age = as_factor(AGE),
         sex = as_factor(SEX),
         income = case_when(INCOME12 == 1 ~ 'Under $30,000',
                            INCOME12 == 2 ~ '$30,000 - $49,999',
                            INCOME12 == 3 ~ '$50,000 - $99,999',
                            INCOME12 >= 4 ~ '$100,000 or more'),
         z.income = scale(INCOME12)[,1],
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_),
         weight = WEIGHT) %>%
  mutate(income = factor(income,levels=c('Under $30,000','$30,000 - $49,999','$50,000 - $99,999','$100,000 or more'))) %>%
  select(weight,vote,age,sex,educ,race,income,z.income,state,dem12,vap12,degree,median_hh_income,white,black,hispanic,asian,evangelical,region)

# Individual responses from the November CPS to model turnout
# Similar individual-level predictors
turnout <- read_dta('cps_00188.dta') %>%
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
                          hispan != 0 ~ 'Hispanic/Latino',
                          race >= 650 & race <= 652 ~ 'Asian',
                          TRUE ~ 'Other'),
         educ = case_when(educ < 70 | educ == 71 ~ 'No high school diploma',
                          educ == 70 | educ == 72 | educ == 73 ~ 'High school graduate',
                          educ > 73 & educ < 110 ~ 'Some college/assoc. degree',
                          educ >= 110 & educ <= 111 ~ 'College graduate',
                          educ > 110 ~ 'Postgraduate study'),
         income = case_when(faminc <= 600 | faminc == 710 ~ 'Under $30,000',
                            faminc >= 720 & faminc <= 740 ~ '$30,000 - $49,999',
                            faminc >= 810 & faminc <= 841 ~ '$50,000 - $99,999',
                            faminc == 841 | faminc == 843 ~ '$100,000 or more',
                            TRUE ~ NA_character_),
         voted = as.numeric(voted==2)) %>%
  mutate(age = factor(age,levels=levels(indiv$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(indiv$race)),
         educ = factor(educ,levels=levels(indiv$educ)),
         income = factor(income,levels=levels(indiv$income))) %>%
  select(voted,age,sex,educ,race,income,state,dem12,vap12,degree,median_hh_income,white,black,hispanic,asian,evangelical,region)

# Get the mean level of z.income (scaled linear income variable) for each level of income (categorical income variable)
# Use for post-stratification later
indiv %>%
  group_by(income) %>%
  summarise(z.income=mean(z.income)) %>%
  filter(!is.na(income))-> z.income.lookup

# Census data for post-stratification.
# Join with contextual data and income lookup to use for predicting later.
census <- read_dta('usa_00076.dta') %>%
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
                          hispan != 0 ~ 'Hispanic/Latino',
                          race >= 4 & race <= 6 ~ 'Asian',
                          TRUE ~ 'Other'),
         educ = case_when(educ < 6 ~ 'No high school diploma',
                          educ == 6 ~ 'High school graduate',
                          educ > 6 & educ < 10 ~ 'Some college/assoc. degree',
                          educ == 10 ~ 'College graduate',
                          educ > 10 ~ 'Postgraduate study'),
         income = cut(ftotinc,c(-Inf,30000,50000,100000,Inf),labels=levels(indiv$income))) %>%
  mutate(age = factor(age,levels=levels(indiv$age)),
         sex = factor(sex,levels=c('Male','Female')),
         race = factor(race,levels=levels(indiv$race)),
         educ = factor(educ,levels=levels(indiv$educ)),
         income = factor(income,levels=levels(indiv$income))) %>%
  group_by(state,race,income) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context) %>%
  inner_join(z.income.lookup)

gc()

# Vote model, taken from Ghitza and Gelman (2013) to estimate two-party vote share by income and race.
res.vote <- glmer(vote ~ z.income*dem12 + (1|income) + (1 + z.income|race) + (z.income-1|state) + (z.income-1|region) + (1|income:state) + (1|income:region) + (1|race:state) + (1|race:region),indiv,family=binomial)
summary(res.vote)

# Use model predictions to find vote share by demographic and geographic group
census$vote <- predict(res.vote,census,allow.new.levels=T,type='response')

# (Much simpler) turnout model using CPS data to estimate turnout by demographic group
res.turnout <- glmer(voted ~ vap12 + (1|income) + (1|race) + (1|state),turnout,family=binomial)
summary(res.turnout)
census$turnout <- predict(res.turnout,census,allow.new.levels=T,type='response')

census %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  mutate(pop = sum(n)) %>%
  group_by(state,race,income) %>%
  summarise(vote = wtd.mean(vote,n_votes),
            turnout = wtd.mean(turnout,n),
            share = n/mean(pop)) %>%
  filter(share>0.01) -> output

data <- left_join(map_data('state'),mutate(output,region=tolower(state)))

data %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=vote)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='#E81B23',mid='white',high='#3333FF',midpoint=0.5,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(income),switch='y') +
  labs(title='2012 presidential election results by income and race',
       fill='Democratic two-party vote share') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('race_income_vote.png',width=16*1.5,height=7*1.5,type='cairo')

census %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  mutate(pop = sum(n)) %>%
  group_by(region,income) %>%
  summarise(vote = wtd.mean(vote,n_votes),
            turnout = wtd.mean(turnout,n)) %>%
  ggplot(aes(x=income,y=vote,group=region,color=region)) +
  geom_line()

census %>%
  mutate(n_votes = n*turnout) %>%
  group_by(state) %>%
  summarise(vote = wtd.mean(vote,n_votes),
            turnout = wtd.mean(turnout,n)) %>%
  inner_join(select(context,state,dem12,vap12)) %>%
  ggplot(aes(x=turnout,y=vap12)) +
  geom_point() +
  geom_smooth(method='lm')
  
