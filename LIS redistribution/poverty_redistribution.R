library(tidyverse)
library(ggrepel)

df <- read_csv('poverty_redist_all.csv')

df %>%
  mutate(decade = floor(year/10)*10) %>%
  group_by(country,decade) %>%
  filter(if_else(decade == 2010, year == last(year),year == first(year))) %>%
  ungroup() %>%
  filter(decade %in% c(1980,2010)) -> data

data %>%
  filter(decade == 2010) %>%
  ggplot(aes(x=primary,y=redist,label=country)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method='lm') +
  labs(x = 'Poverty rate for primary income',
       y = 'Reduction in poverty by taxes and transfers',
       title = 'Poverty and redistribution',
       caption = 'Caminada and Wang (2019); John Handley')

data %>%
  group_by(country) %>%
  mutate(ch_primary = primary - lag(primary), ch_redist = redist - lag(redist)) %>%
  ungroup() %>%
  filter(!is.na(ch_redist)) %>%
  ggplot(aes(x=ch_primary,y=ch_redist,label=country)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method='lm') +
  labs(x = 'Change in poverty rate for primary income',
       y = 'Change in reduction in poverty by taxes and transfers',
       title = 'Changes in poverty and redistribution from the 1980s to 2010s',
       caption = 'Caminada and Wang (2019); John Handley')
