library(tidyverse)
library(haven)
library(lme4)

cpds <- read_dta('CPDS_1960-2018_Update_2020.dta') %>%
  select(country,year,adjcov_ipol,gov_left1,gov_cent1,gov_right1,unemp)
epl_temp <- read_csv('epl_temporary.csv')
epl_reg <- read_csv('epl_regular.csv')
cwed <- read_csv('cwed_gen.csv')
tud <- read_csv('trade_union_density.csv') %>%
  spread(source,tud) %>%
  group_by(country,year) %>%
  summarise(tud = coalesce(`Administrative data`,`Survey data`))
pwt <- read_dta('pwt91.dta') %>%
  group_by(country,year) %>%
  summarise(gdppc = rgdpe/pop,productivity = rgdpe/(emp*avh)) %>%
  filter(!is.na(gdppc))


oecd <- inner_join(inner_join(epl_temp,epl_reg),tud)
econ <- inner_join(oecd,pwt)
data <- inner_join(econ,cpds)

data %>%
  group_by(country) %>%
  mutate(growth = log(productivity/lag(productivity))) -> reg_data

res <- lmer(growth ~ log(productivity) + gov_left1 + tud + adjcov_ipol + tud*adjcov_ipol + (1|year) + (1|country),reg_data)
summary(res)
