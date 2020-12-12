library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00173.dta')

df %>%
  mutate(weight = ifelse(is.na(hwtfinl),asecwth,hwtfinl), emp = as.numeric(empstat==10), lf = as.numeric(labforce==2), hours = ifelse(ahrsworkt==999,0,ahrsworkt)) %>%
  group_by(year,month,serial) %>%
  summarise(nearn = sum(emp), hours = sum(hours)) %>%
  ungroup() %>%
  group_by(year,month) %>%
  summarise_at(vars(nearn,hours),~wtd.mean(.x,weight,na.rm=T)) -> data
