library(tidyverse)
library(haven)
library(Hmisc)
library(reldist)

df <- read_dta('cps_00161.dta')

df %>%
  filter(classwly==22,incwage!=99999999,incwage>0,uhrsworkly!=999) %>%
  mutate(race = factor(recode(as.numeric(race),`100`='White',`200`='Black',.default='Other'),levels=c('White','Black','Other'))) %>%
  mutate(hs = as.numeric(educ<80),col = as.numeric(educ>=80 & educ<110), deg = as.numeric(educ>100), educ = factor(recode(hs+2*col+3*deg,`1`='High school or less',`2`='Some college',`3`='Degree',.default=NA_character_),levels=c('Some college','High school or less','Degree'))) %>%
  mutate(sex = as_factor(sex)) %>%
  mutate(incearn = incwage + incbus + incfarm) %>%
  mutate(hour_earn = incwage/(wkswork1*uhrsworkly)*cpi99) %>%
  select(-workly) -> data

data %>%
  filter(year==1980) %>%
  group_by(occ10ly) %>%
  summarise(wage = wtd.mean(incwage)) %>%
  mutate(occscore = percent_rank(wage)) %>%
  select(-wage) -> occ_crosswalk

data <- inner_join(data,occ_crosswalk)

res <- lm(log(incwage) ~ occscore + educ + age + I(age^2) + log(uhrsworkly) + log(wkswork1) + as_factor(year),data)
summary(res)

data$residuals <- residuals.lm(res)

data %>%
  group_by(year) %>%
  summarise(total = gini(incwage,asecwt), unexplained = gini(exp(residuals),asecwt), explained = total-within) %>%
  select(-total) %>%
  gather(inequality,gini,c(within,between)) %>%
  ggplot(aes(x=year,y=gini,color=inequality)) +
  geom_point() +
  geom_line()
    
