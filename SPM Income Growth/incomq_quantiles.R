library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00170.dta')

df %>%
  mutate(income = spmtotres/spmeqscale*spmgeoadj) %>%
  group_by(year) %>%
  summarise(avg = wtd.quantile(income,spmwt,probs=seq(0,1,0.01))) %>%
  write_csv('quantiles.csv')
