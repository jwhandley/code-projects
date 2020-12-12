library(tidyverse)

quantiles <- read_csv('quantiles.csv')
cpi <- read_csv('cpi_long.csv')
ppp <- read_csv('ppp_2015.csv')

df <- inner_join(inner_join(quantiles,cpi,by=c('country','year')),ppp,by='country')

df %>%
  mutate(realinc = income/cpi*100/ppp) %>%
  group_by(country,quantile) %>%
  mutate(growth = (log(realinc) - lag(log(realinc)))/(year - lag(year))) %>%
  write_csv('data.csv')
  