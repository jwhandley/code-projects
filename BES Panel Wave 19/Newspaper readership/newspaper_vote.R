library(tidyverse)
library(haven)
library(labelled)
library(Hmisc)

df <- read_sav("~/Code Projects/BES Panel Wave 19/BES2019_W19_v0.5.sav")

df %>%
  filter_at(vars(p_paper_read,p_past_vote_2019),~!is.na(.x)) %>%
  filter(p_past_vote_2019!=9999) %>%
  group_by(p_paper_read) %>%
  mutate(pop = sum(wt)) %>%
  ungroup() %>%
  group_by(p_paper_read,p_past_vote_2019) %>%
  summarise(share = sum(wt)/mean(pop)) %>%
  ggplot(aes(x=reorder(as_factor(p_past_vote_2019),share),y=share,fill=as_factor(p_past_vote_2019))) +
  geom_bar(stat='identity') +
  facet_wrap(~as_factor(p_paper_read)) +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c('#0087DC','#E4003B','#FAA61A','#FDF38E','#008142','#6AB023','grey','#12B6CF')) +
  labs(x='Party',
       y='Vote share',
       fill='Party',
       title='2019 general election vote by preferred daily newspaper',
       subtitle='British Election Study Panel Wave 19 (post-election wave)',
       caption='John Handley') +
  ggsave('party_newspaper_vote.png',width=8*1.2,height=5*1.2,type='cairo')

df %>%
  filter_at(vars(p_paper_read,p_past_vote_2019),~!is.na(.x)) %>%
  filter(p_past_vote_2019!=9999) %>%
  mutate(pop = sum(wt)) %>%
  group_by(p_paper_read) %>%
  summarise(share = sum(wt)/mean(pop),n=n())
