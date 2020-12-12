library(tidyverse)
library(haven)
library(Hmisc)
library(glue)
library(scales)

df <- read_dta('cps_00171.dta')
cpi <- read_csv('chained_cpi.csv')

p <- seq(0,0.99,0.01)

df %>%
  mutate(income = spmtotres/spmeqscale) %>%
  filter(income > 0) %>%
  group_by(year) %>%
  mutate(quantile = cut(income,wtd.quantile(income,spmwt,seq(0,1,0.01)),include.lowest = T,labels=c(glue('{p*100}-{p*100+1}')))) %>%
  ungroup() %>%
  group_by(year,quantile) %>%
  summarise(income = wtd.mean(income,spmwt,na.rm=T)) -> data

data <- inner_join(data,cpi)

data %>%
  group_by(quantile) %>%
  mutate(g = log(income/cpi*100) - lag(log(income/cpi*100))) %>%
  filter(!is.na(g)) %>%
  summarise(g = mean(g,na.rm=T)) %>%
  ggplot(aes(x=as.numeric(quantile),y=g)) +
  geom_line() +
  labs(x='Percentile of family economic resources',
       y='Compound annual growth rate',
       title='Profile of income growth in the US 2009-2019',
       subtitle='US Census Supplemental Poverty Measure',
       caption='John Handley') +
  scale_y_continuous(labels=percent)
  
  
