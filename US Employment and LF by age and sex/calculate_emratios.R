library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00180.dta')

gc()

df %>%
  filter(age>15) %>%
  mutate(emp = as.numeric(empstat==10),
         age_group = cut(age,breaks=c(-Inf,24,34,44,54,64,74,Inf),labels=c('16-24','24-34','35-44','45-54','55-64','65-74','75+'))) %>%
  group_by(year,month,age_group,sex) %>%
  summarise(emratio = wtd.mean(emp,wtfinl,na.rm-T)) %>%
  write_csv('emratios.csv')