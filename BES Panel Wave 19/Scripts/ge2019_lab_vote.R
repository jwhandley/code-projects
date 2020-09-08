library(tidyverse)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  filter(profile_past_vote_2017==2) %>%
  mutate(brexit = as_factor(na_if(profile_eurefvote,9999)),vote2019 = as_factor(na_if(na_if(pastvote_ge_2019,98),99))) %>%
  filter(!is.na(vote2019),!is.na(brexit)) %>%
  mutate(total = sum(wt)) %>%
  group_by(brexit,vote2019) %>%
  summarise(n = sum(wt)/mean(total)) %>%
  spread(brexit,n) -> data

write.csv(data,'lab17_vote.csv')
