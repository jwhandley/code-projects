library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta("UKDA-8598-stata/stata/stata13/apsp_oct18sep19_eul_pwta18.dta")

df %>%
  filter(NSECMJ10!=-9) %>%
  group_by(GOR9D) %>%
  mutate(pop=sum(PWTA18)) %>%
  ungroup() %>%
  group_by(GOR9D,NSECMJ10) %>%
  summarise(share = sum(PWTA18)/mean(pop)) %>%
  spread(key=NSECMJ10,value=share) -> data
  