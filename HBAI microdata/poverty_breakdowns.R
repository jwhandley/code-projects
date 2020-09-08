library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('UKDA-5828-stata/stata/stata11_se/h1819.dta')

df %>%
  mutate_at(vars(ecobu,numbkids),~as_factor(.x)) %>%
  group_by(ecobu,numbkids) %>%
  summarise(pov = wtd.mean(low60ahc,gs_newpp,na.rm=T)) %>%
  spread(numbkids,pov) -> pov_breakdown

write_excel_csv(pov_breakdown,'poverty_breakdown.csv')


df %>%
  mutate_at(vars(ecobu,tenhbai),~as_factor(.x)) %>%
  group_by(ecobu,tenhbai) %>%
  summarise(pov = wtd.mean(low60ahc,gs_newpp,na.rm=T)) %>%
  spread(tenhbai,pov) -> pov_breakdown_tenure

write_excel_csv(pov_breakdown_tenure,'poverty_breakdown_tenure.csv')