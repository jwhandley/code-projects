library(tidyverse)
library(haven)
library(plm)
library(scales)

df <- read_dta('10SD_jan15.dta') %>%
  filter(Variable == 'VA') %>%
  mutate(mfg = MAN/SUM,countrycode=Country,year=Year,region=Region) %>%
  select(mfg,countrycode,year,region) %>%
  filter(!is.na(mfg))

gdppc <- read_dta('pwt91.dta') %>%
  mutate(rgdppc = rgdpna/pop,cgdppc = rgdpe/pop) %>%
  select(countrycode,year,rgdppc,cgdppc,csh_x,csh_i) %>%
  filter(!is.na(rgdppc))

data <- inner_join(df,gdppc,by=c('countrycode','year')) %>%
  group_by(countrycode) %>%
  mutate(growth = (dplyr::lead(log(rgdppc)) - log(rgdppc))) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(frontier = quantile(cgdppc,0.9),frontier_na = quantile(rgdppc,0.9)) %>%
  ungroup() %>%
  mutate(frontier_g = dplyr::lead(log(frontier_na))-log(frontier_na)) %>%
  mutate(level = log(cgdppc/frontier),g = growth - frontier_g)

res <- plm(growth ~ log(cgdppc) + mfg,data,index=c('countrycode','year'),effect='twoway')
summary(res)

res1 <- plm(growth ~ level + mfg,data,index=c('countrycode','year'),effect='time')
summary(res1)

fixef(res)
