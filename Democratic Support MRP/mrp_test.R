library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(tools)

df <- read_dta('marist_feb.dta')
context <- read_csv('context.csv') %>%
  rename(state.name = state) %>%
  mutate(state = fips)
df <- read_dta('marist_feb.dta') %>%
  mutate(educ = as.numeric(collegep==2),
         race = case_when(racet == 100 ~ 1,
                          racet == 200 ~ 2,
                          racet == 300 ~ 3,
                          TRUE ~ 4),
         age = cut(ager,c(18,30,50,65,Inf),seq(1,4),include.lowest=T),
         sex = as.numeric(gender == 2),
         fips = as.numeric(states),
         sanders = as.numeric(BSDT2020==1),
         biden = as.numeric(JBDT2020==1),
         turnout = as.numeric(replace_na(VT2016X,1) == 1 & rvx == 1)) %>%
  inner_join(state.fips) %>%
  mutate(state = fips) %>%
  select(sanders,biden,turnout,educ,race,age,sex,state) %>%
  inner_join(context) %>%
  inner_join(select(state.fips,fips,division))

census <- read_dta('usa_00081.dta') %>%
  mutate(educ = as.numeric(educ>=10),
         age = cut(age,c(18,30,50,65,Inf),seq(1,4),include.lowest=T),
         race = case_when(race == 1 & hispan == 0 ~ 1,
                          race == 2 & hispan == 0 ~ 2,
                          hispan != 0 ~ 3,
                          TRUE ~ 4),
         sex = as.numeric(sex == 2),
         fips = as.numeric(statefip)) %>%
  group_by(fips,educ,age,race,sex) %>%
  summarise(n=sum(perwt)) %>%
  inner_join(context) %>%
  inner_join(select(state.fips,fips,division))
  
res.turnout <- glmer(turnout ~ (1|educ) + (1+hispanic|race) + (1|age) + (1|sex) + (1|race:age) + (1|race:division) + (1|race:educ:division) + white + degree + evangelical + vap16,df,family=binomial)
summary(res.turnout)
census$turnout <- predict(res.turnout,census,allow.new.levels=T,type='response')

res.biden <- glmer(biden ~ (1+hispanic|race) + (1|sex) + (1|race:age) + (1|sex:educ) + (1|division) + (1|race:educ:division) + white + degree + evangelical + dem16,df,family=binomial)
summary(res.biden)
census$biden <- predict(res.biden,census,allow.new.levels=T,type='response')

res.sanders <- glmer(sanders ~ (1+hispanic|race) + (1|age) + (1|sex) + (1|race:age) + (1|sex:educ) + (1|division) + (1|race:educ:division) + white + hispanic + degree + evangelical + dem16,df,family=binomial)
summary(res.sanders)
census$sanders <- predict(res.sanders,census,allow.new.levels=T,type='response')
save.image('fitted_models.RData')

census %>%
  group_by(state.name) %>%
  summarise(biden = wtd.mean(biden,n*turnout),
            sanders = wtd.mean(sanders,n*turnout),
            turnout = wtd.mean(turnout,n)) %>%
  print(n=51)
