library(tidyverse)
library(haven)

df <- read_sav('2005ates0801171.sav',encoding = 'shift-jis')

df %>%
  group_by(partycod) %>%
  summarise(keynes = mean(keynes,na.rm=T), defence = mean(defence,na.rm=T), smallgov = mean(smallgov,na.rm=T))

df %>%
  filter(!is.na(ideology)) %>%
  select(ideology,keynes,smallgov,publecen,lifetime) %>%
  mutate_at(vars(keynes,smallgov,publecen,lifetime),~6-.x) %>%
  rename(`Fiscal stimulus over austerity` = keynes, `Small government over good services` = smallgov, `Public work to protect jobs` = publecen, `Lifetime employment system` = lifetime) %>%
  gather('policy','position',-ideology) %>%
  ggplot(aes(x=ideology,y=position)) +
  geom_smooth(color='#1f77b4') +
  facet_wrap(~policy) +
  labs(x = 'Subjective ideology (0 = Progressive, 10 = Conservative)',
       y = 'Average agreement (1 = Strongly disagree, 5 = Strongly agree)',
       title = 'Economic policy views of candidates',
       subtitle = '2005 Japanese general election',
       caption = 'UTokyo-Asahi Survey; John Handley') +
  theme_bw() +
  ggsave('econ_policy_2005.png',width=8*1.2,height=5*1.2,type='cairo')

df %>%
  filter(!is.na(ideology)) %>%
  select(ideology,nenkin,usetax,yusei,usecontx) %>%
  mutate(yusei = as.numeric(yusei == 1), nenkin = as.numeric(nenkin == 1), usetax = as.numeric(usetax == 2), usecontx = as.numeric(usecontx == 1) + as.numeric(usecontx == 2)) %>%
  gather('policy','agree',-ideology) %>%
  ggplot(aes(x=ideology,y=agree)) +
  geom_smooth() +
  facet_wrap(~policy)

df %>%
  filter(!is.na(ideology)) %>%
  ggplot(aes(x=ideology)) +
  geom_density() +
  facet_wrap(~as_factor(partycod),scales='free_y')
