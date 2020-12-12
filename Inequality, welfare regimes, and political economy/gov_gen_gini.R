library(tidyverse)
library(haven)

gini <- read_csv('swiid8_3_summary.csv')
cwed <- read_csv('cwed_gen.csv')
cpds <- read_dta('CPDS_1960-2018_Update_2020.dta') %>%
  select(year,country,gov_left1,gov_cent1)

df <- inner_join(left_join(gini,cwed),cpds)

df %>%
  write_csv('gini_cwed_cpds.csv')

df %>%
  ggplot(aes(x=year,y=totgen,color=gov_left1)) +
  geom_line() +
  facet_wrap(~country) +
  scale_color_gradient(low='blue',high='red')

df %>%
  ggplot(aes(x=year,y=gini_disp,color=gov_left1)) +
  geom_line() +
  facet_wrap(~country) +
  scale_color_gradient(low='blue',high='red')

gini_gen <- lm(gini_disp ~ totgen,df)
summary(gini_gen)

gini_gov <- lm(gini_disp ~ gov_left1*I(year>=1980),df)
summary(gini_gov)

gen_gov <- lm(totgen ~ gov_left1*I(year>=1980),df)
summary(gen_gov)
