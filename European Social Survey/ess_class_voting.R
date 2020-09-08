library(tidyverse)
library(essurvey)
library(RCurl)

set_email('john.handley@merton.ox.ac.uk')

df <- import_country(country='United Kingdom',rounds=9)
df <- recode_missings(df)

esec <- read_csv('esec_isco08.csv')

esec_crosswalk <- esec %>%
  mutate(esec_class = emp)

df <- df %>%
  mutate(isco08_3dig = round(isco08/10))

data <- inner_join(df,esec_crosswalk)

data %>%
  mutate(party = as_factor(prtvtcgb)) %>%
  filter(party %in% c('Conservative','Labour','Liberal Democrat','Scottish National Party')) %>%
  group_by(esec_class) %>%
  mutate(tot = sum(dweight)) %>%
  ungroup() %>%
  group_by(esec_class,party) %>%
  summarise(vote = sum(dweight)/mean(tot)) %>%
  ggplot(aes(x=party,y=vote,fill=party)) +
  geom_bar(stat='identity') +
  facet_wrap(~esec_class) + 
  scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','#FDF38E'))
