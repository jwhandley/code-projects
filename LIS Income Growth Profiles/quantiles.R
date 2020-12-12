library(tidyverse)

dk <- read_csv('Denmark 1987-2016/denmark_quantiles.csv') %>%
  mutate(country = 'Denmark')

fi <- read_csv('Finland 1987-2016/finland_quantiles.csv') %>%
  mutate(country = 'Finland')

de <- read_csv('Germany 1973-2016/germany_quantiles.csv') %>%
  mutate(country = 'Germany')

ie <- read_csv('Ireland 1987-2017/ireland_quantiles.csv') %>%
  mutate(country = 'Ireland')

uk <- read_csv('United Kingdom 1969-2016/uk_quantiles.csv') %>%
  mutate(country = 'United Kingdom')

us <- read_csv('United States 1974-2018/us_quantiles.csv') %>%
  mutate(country = 'United States')

df <- bind_rows(dk,fi,de,ie,uk,us)

write_csv(df,'quantiles.csv')
