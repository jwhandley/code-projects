library(tidyverse)

df <- read_csv('1976-2016-president.csv')

df %>%
  mutate(party = ifelse(party=='democratic-farmer-labor','democrat',party)) %>%
  filter(party %in% c('democrat','republican')) %>%
  group_by(party,state,year) %>%
  mutate(vote = mean(voteshare)) %>%
  arrange(state,year,party) %>%
  group_by(state,year) %>%
  mutate(margin = vote - lag(vote)) %>%
  filter(!is.na(margin)) %>%
  select(year,state,margin) %>%
  group_by(state) %>%
  mutate(swing = margin - lag(margin)) %>%
  filter(!is.na(swing)) %>%
  group_by(year) %>%
  summarise(mean = mean(swing),sd = sd(swing))

df %>%
  mutate(party = ifelse(party=='democratic-farmer-labor','democrat',party)) %>%
  filter(party %in% c('democrat','republican')) %>%
  group_by(party,state,year) %>%
  mutate(vote = mean(voteshare)) %>%
  arrange(state,year,party) %>%
  group_by(state,year) %>%
  mutate(margin = vote - lag(vote)) %>%
  filter(!is.na(margin)) %>%
  select(year,state,margin) %>%
  mutate(winner = ifelse(margin>0,'republican','democrat'))
  