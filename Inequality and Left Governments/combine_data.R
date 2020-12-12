library(tidyverse)

gini <- read_csv('swiid8_3_summary.csv')
sied <- read_csv('sied_gen.csv')
gov <- read_dta('CPDS_1960-2018_Update_2020.dta') %>%
  mutate(country = ifelse(country=='USA','United States',country),
         gov_centleft = gov_left1 + as.numeric(country %in% c('United States','Canada','Japan'))*gov_cent1) %>%
  select(country,year,gov_left1,gov_cent1,gov_centleft)

df <- left_join(inner_join(gini,gov),sied)

df %>%
  write_csv('inequality_social_insurance_gov.csv')

res <- lm(gini_disp ~ gen,df)
summary(res)

df %>%
  group_by(country) %>%
  filter(sum(!is.na(gen))>3) %>%
  mutate_at(vars(gen,pension,unempl,sick),~approx(year,.x,year)$y) %>%
  mutate(gen_chg = (lead(gen) - gen)/(lead(year)-year)) %>%
  select(country,year,gen,gen_chg,gov_centleft) %>%
  mutate(after1990 = as.numeric(year>=1990)) -> data

gov_gen <- lm(gen_chg ~ I(gov_centleft/100)*after1990,data)
summary(gov_gen)

df %>%
  mutate(gini_chg = lead(gini_disp) - gini_disp,
         after1980 = as.numeric(year>=1980)) -> data1

gov_ineq <- lm(gini_chg ~ gov_centleft*after1980,data1)
summary(gov_ineq)
