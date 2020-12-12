library(tidyverse)
library(haven)
library(Hmisc)

mpd <- read_dta('MPDataset_MPDS2020a_stata14.dta')
cpds <- read_dta('CPDS_1960-2018_Update_2020.dta')
swiid <- read_csv('swiid8_3_summary.csv')

mpd %>%
  select(-country) %>%
  rename(country = countryname) %>%
  mutate(year = round(date/100,0)) %>%
  group_by(country,year) %>%
  summarise(rile = wtd.mean(rile,pervote,na.rm=T)) -> rile

cpds %>%
  select(country,year,gov_left1,gov_cent1,gov_party) -> gov

swiid %>%
  select(country,year,gini_disp) -> gini

data <- inner_join(gini,inner_join(gov,rile))

write_csv(data,'gov_rile_gini1.csv')
