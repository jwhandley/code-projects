library(tidyverse)

df <- read_csv('swiid8_3_summary.csv')

df %>%
  mutate(type = case_when(country %in% c('Denmark','Finland','Norway','Sweden') ~ 'Nordic SMEs',
                          country %in% c('Austria','Belgium','Germany','Netherlands','Switzerland') ~ 'Continental SMEs',
                          country %in% c('Australia','Canada','Ireland','New Zealand','United Kingdom','United States') ~ 'Liberal market economies')) -> data

data %>%
  filter(!is.na(type),
         year >= 1980,
         year <= 2017) %>%
  group_by(year,type) %>%
  summarise(gini = mean(gini_disp), se = sqrt(sum(gini_disp_se^2/n()^2))) %>%
  ggplot(aes(x=year,y=gini)) +
  geom_line(aes(color=type),size=1) +
  geom_ribbon(aes(ymin=gini-1.96*se,ymax=gini+1.96*se,fill=type),alpha=0.25) +
  labs(x='year',
       y='Gini coefficient on disposable income',
       title='Inequality by economy type',
       fill='Economy type',
       color='Economy type')

data %>%
  filter(!is.na(type),
         year >= 1980,
         year <= 2017) %>%
  group_by(year,type) %>%
  summarise(gini = mean(gini_mkt), se = sqrt(sum(gini_mkt_se^2/n()^2))) %>%
  ggplot(aes(x=year,y=gini)) +
  geom_line(aes(color=type),size=1) +
  geom_ribbon(aes(ymin=gini-1.96*se,ymax=gini+1.96*se,fill=type),alpha=0.25) +
  labs(x='year',
       y='Gini coefficient on market income',
       title='Inequality by economy type',
       fill='Economy type',
       color='Economy type')

data %>%
  filter(!is.na(type),
         year >= 1980,
         year <= 2017) %>%
  mutate(redist = gini_mkt - gini_disp) %>%
  group_by(year,type) %>%
  summarise(redist = mean(redist), se = sqrt(sum(gini_disp_se^2/4+gini_mkt_se^2/4)/sum(n()^2))) %>%
  ggplot(aes(x=year,y=redist)) +
  geom_line(aes(color=type),size=1) +
  geom_ribbon(aes(ymin=redist-1.96*se,ymax=redist+1.96*se,fill=type),alpha=0.25) +
  labs(x='year',
       y='Absolute redistribution',
       title='Redistribution by economy type',
       fill='Economy type',
       color='Economy type')
