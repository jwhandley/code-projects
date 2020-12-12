library(tidyverse)
library(glue)
library(scales)

cpi <- read_csv('uk_cpi.csv')
income <- read_csv('uk_quantiles.csv')

df <- inner_join(income,cpi,by='year')

df %>%
  mutate(real_inc = income/cpi*100) %>%
  group_by(quantile) %>%
  mutate(growth = (log(real_inc) - lag(log(real_inc)))/(year-lag(year)),
         period = glue("{lag(year)}-{year}")) %>%
  filter(!is.na(growth)) %>%
  ggplot(aes(x=quantile,y=growth)) +
  geom_line() +
  geom_line(aes(x=quantile,y=0),color='blue') +
  facet_wrap(~period) +
  labs(x='Percentile of equivalised disposable household income',
       y='Compound annual growth rate',
       title='Profile of income growth in the United Kingdom') +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) +
  ggsave('uk_income_growth_profile.png',width=8,height=5,type='cairo')
  

