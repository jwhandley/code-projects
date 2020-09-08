library(tidyverse)
library(lubridate)

ge15 <- read_csv('ge2015.csv') %>%
  filter(country_name == 'England') %>%
  mutate(lab = lab/valid_votes, con = con/valid_votes, ukip = ukip/valid_votes, libdem = ld/valid_votes, winner = first_party,year = 2015) %>%
  select(ons_id,constituency_name,lab,con,ukip,libdem,winner,year)

ge19 <- read_csv('ge2019.csv') %>%
  filter(country_name == 'England') %>%
  mutate(lab = lab/valid_votes, con = con/valid_votes, bxp = brexit/valid_votes, libdem = ld/valid_votes, winner = first_party,year = 2019) %>%
  select(ons_id,constituency_name,lab,con,bxp,libdem,winner,year)
  
imd <- read_csv('imd_2015_2019.csv')

data <- inner_join(bind_rows(ge19,ge15),imd,by=c('ons_id','year'))


ggplot(data,aes(x=imd_score,y=lab)) +
  geom_point(aes(color=winner)) +
  geom_smooth(method='lm') +
  facet_wrap(~year)

data %>%
  arrange(year) %>%
  group_by(ons_id) %>%
  mutate(lab_swing = lab - lag(lab),
         con_swing = con - lag(con),
         score_chg = imd_score - lag(imd_score),
         rank_chg = imd_rank - lag(imd_rank)) %>%
  ungroup() %>%
  filter(year==2019) -> data19

res <- lm(con_swing ~ I(rank_chg/533) + I(imd_rank/533),data19)
summary(res)

res.2019 <- lm(lab ~ I(imd_rank/533),data19)
summary(res.2019)

ggplot(data19,aes(x=rank_chg/533,y=con_swing)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x='Change in English IMD percentile 2015-2019',
       y='Change in Conservative vote share 2015-2019') +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::percent)

data %>%
  filter(year==2019) %>%
  mutate(imd_decile = cut(imd_rank/533,10,labels=c('Most deprived','2','3','4','5','6','7','8','9','Least deprived'))) %>%
  group_by(imd_decile) %>%
  mutate(imd_sub_rank = imd_rank - min(imd_rank)) %>%
  ungroup() %>%
  ggplot(aes(x=imd_decile,y=imd_sub_rank,fill=winner)) +
  geom_tile(aes(width=0.7,height=0.7),color='white',size=1/2) +
  scale_fill_manual(values=c('blue','green','red','orange','grey')) +
  coord_fixed(1/5) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  labs(x='Index of Multiple Deprivation Decile',
       fill='Winner',
       title='2019 general election result by level of deprivation',
       subtitle='for English constituencies') +
  ggsave('imd_res_2019.png',width=8,height=8,type='cairo')


data %>%
  filter(year==2015) %>%
  mutate(imd_decile = cut(imd_rank/533,10,labels=c('Most deprived','2','3','4','5','6','7','8','9','Least deprived'))) %>%
  group_by(imd_decile) %>%
  mutate(imd_sub_rank = imd_rank - min(imd_rank)) %>%
  ungroup() %>%
  ggplot(aes(x=imd_decile,y=imd_sub_rank,fill=winner)) +
  geom_tile(aes(width=0.7,height=0.7),color='white',size=1/2) +
  scale_fill_manual(values=c('blue','green','red','orange','grey','purple')) +
  coord_fixed(1/5) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  labs(x='Index of Multiple Deprivation Decile',
       fill='Winner',
       title='2015 general election result by level of deprivation',
       subtitle='for English constituencies') +
  ggsave('imd_res_2015.png',width=8,height=8,type='cairo')
