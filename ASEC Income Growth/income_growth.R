library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00181.dta')

df %>%
  mutate(ftotval = na_if(ftotval,999999999),
         famsize = na_if(famsize,0)) %>%
  filter_at(vars(famsize,ftotval),~!is.na(.x)) %>%
  mutate(faminc = ftotval/sqrt(famsize)) %>%
  group_by(year) %>%
  filter(ftotval != max(ftotval)) %>%
  mutate(quantile = cut(faminc,c(-Inf,wtd.quantile(faminc,asecwt,probs=c(.16,.33,.67,.95)),Inf),include.lowest=T,labels=c('0-16','17-33','34-67','68-95','96-100'))) %>%
  group_by(year,quantile) %>%
  summarise(faminc = wtd.mean(faminc,asecwt,na.rm=T)) %>%
  spread(quantile,faminc) %>%
  write_csv('income_growth.csv')
