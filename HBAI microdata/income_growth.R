library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('UKDA-5828-stata/stata/stata11_se/h1819_all.dta')

df %>%
  mutate(year = as_factor(year)) %>%
  group_by(year,obhcdec) %>%
  summarise(earn = wtd.mean(egrernbu,gs_newbu,na.rm=T),bhc = wtd.mean(s_oe_bhc,gs_newbu,na.rm=T)) -> data

write_csv(data,'income_growth.csv')
