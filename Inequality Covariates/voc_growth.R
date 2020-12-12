library(tidyverse)
library(haven)

df <- read_dta('pwt91.dta')

df %>%
  mutate(type = case_when(
    country %in% c('United States','United Kingdom','Australia','New Zealand','Canada') ~ 'Liberal market economy',
    country %in% c('Denmark','Finland','Norway','Sweden') ~ 'Nordic social market economy',
    country %in% c('Austria','Belgium','Germany','Netherlands','Switzerland') ~ 'Continental social market economy',
    country %in% c('France','Italy','Japan') ~ 'Hybrid',
    TRUE ~ NA_character_
  )) %>%
  mutate(productivity = rgdpna/(emp*avh)) %>%
  filter(year>=1990,!is.na(type)) %>%
  group_by(country) %>%
  mutate(productivity = log(productivity/first(productivity)),
         growth = productivity - lag(productivity),
         level = first(log(rgdpe/(emp*avh)))) %>%
  select(country,type,year,productivity,growth,level) -> data

data %>%
  group_by(type,year) %>%
  summarise(growth = median(growth)) %>%
  filter(!is.na(growth)) %>%
  mutate(productivity = cumsum(growth)) %>%
  ggplot(aes(x=year,y=productivity,color=type)) +
  geom_line()

res <- lm(productivity ~ level + type,filter(data,year==2017))
summary(res)
