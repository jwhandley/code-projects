library(tidyverse)
library(haven)

gini <- read_csv('swiid8_3_summary.csv') %>%
  select(country,year,gini_disp,gini_mkt,abs_red,rel_red)

gov <- read_dta('CPDS_1960-2018_Update_2020.dta') %>%
  mutate(country = ifelse(country=='USA','United States',country)) %>%
  select(country,year,gov_left1,gov_cent1,gov_right1,gov_party)

df <- inner_join(gov,gini)

ggplot(df,aes(x=year,y=gini_disp,color=gov_left1)) +
  geom_line() +
  facet_wrap(~country) + 
  scale_color_gradient(low='blue',high='red')

df %>%
  group_by(country) %>%
  filter(n()>45) %>%
  ggplot(aes(x=year,y=gini_disp,color=gov_left1 + gov_cent1*as.numeric(country %in% c('United States','Canada')))) +
  geom_line() +
  facet_wrap(~country) + 
  scale_color_gradient(low='blue',high='red') +
  labs(color='Left cabinet share',
       x='Year',
       y='Gini coefficient for disposable income',
       title='Inequality and government composition') +
  theme(panel.spacing.x = unit(5, "mm")) +
  ggsave('ineq_left_gov.png',width=8*1.2,height=5*1.2,type='cairo')
