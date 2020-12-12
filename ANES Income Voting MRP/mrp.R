library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

context <- read_csv('context.csv')
results <- read_csv('actual_results.csv')

post <- read_dta('usa_00071.dta') %>%
  filter(age>=18,citizen %in% c(0,1,2)) %>%
  mutate(gender = as.numeric(sex==1),
         race = case_when(race == 1 & hispan == 0 ~ 1,
                          race == 2 & hispan == 0 ~ 2,
                          hispan != 0 ~ 3,
                          TRUE ~ 4),
         educ = as.numeric(educ >= 10),
         income = cut(ftotinc,wtd.quantile(ftotinc,perwt,c(0,.16,.33,.66,.95,1)),include.lowest = T,labels=seq(1,5))) %>%
  group_by(statefip,gender,race,educ,income) %>%
  summarise(n = sum(perwt)) %>%
  inner_join(context)

indiv <- read_csv('anes_extract.csv') %>%
  filter(year==2016,gender!=0,race!=9,educ!=0,income!=0) %>%
  mutate(gender = as.numeric(gender==1),educ = as.numeric(educ==4)) %>%
  select(statefip,year,wgt,gender,race,educ,income,vote)

df <- inner_join(indiv,context)

data <- df %>%
  filter(vote %in% c(1,2)) %>%
  mutate(vote = as.numeric(vote==1))

data1 <- df %>%
  filter(vote != 0) %>%
  mutate(turnout = 1-as.numeric(vote==7))

data2 <- df %>%
  filter(vote %in% c(1,2,3,4)) %>%
  mutate(third_party = as.numeric(vote>=3))

res <- glmer(vote ~ (1|gender) + (1|race) + (1|race:educ) + (1|educ:gender) + (1|state) + (1|income) + degree + dem2012,data,family=binomial)
summary(res)
ranef(res)

res.turnout <- glmer(turnout ~ (1|gender) + (1|race) + (1|educ) + (1|race:educ) + (1|income) + degree + turnout2012,data1,family=binomial)
summary(res.turnout)
ranef(res.turnout)

res.3p <- glmer(third_party ~ (1|state) + (1|income) + degree + dem2012,data2,family=binomial)
summary(res.3p)

post$turnout <- predict(res.turnout,post,allow.new.levels=T,type='response')
post$third_party <- predict(res.3p,post,allow.new.levels=T,type='response')
post$vote <- predict(res,post,allow.new.levels=T,type='response')

post %>%
  mutate(n_votes = turnout*n,
         n_votes2p = n_votes*(1-third_party)) %>%
  group_by(state) %>%
  summarise(vote = wtd.mean(vote,n_votes2p),
            turnout_pred = wtd.mean(turnout,n)) %>%
  inner_join(results,by='state') -> output

output %>%
  ggplot(aes(x=vote,y=clinton2p)) + 
  geom_point() +
  geom_smooth(method='lm')

output %>%
  write_csv('mrp_results.csv')
