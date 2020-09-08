library(tidyverse)
library(ggrepel)
library(MASS)

mfg <- read_csv('mfg_share.csv')
housing <- read_csv('housing_pl.csv')
gdppc <- read_csv('gdppc.csv')

mfg %<>%
  group_by(iso) %>%
  filter(year == max(year)) %>%
  ungroup()

df <- inner_join(mfg,housing,by='country')
data <- inner_join(df,gdppc,by='country') %>%
  select(iso,country,mfg_share,housing_pl,gdppc)

df %>%
  ggplot(aes(x=mfg_share,y=housing_pl,label=country)) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Manufacturing share of value added',
       y = 'Purchasing power parity for housing',
       title = 'House prices and manufacturing output in the OECD',
       caption = '@jwhandley17',
       subtitle = 'OECD 2017 PPP benchmark results and National Accounts at a glance') +
  ggsave('mfg_housing.png',width=8,height=5,type='cairo')

res <- rlm(log(housing_pl) ~ log(gdppc) + log(mfg_share),data)
summary(res)

data %>%
  ggplot(aes(x=mfg_share,y=housing_pl,label=country,color=log(gdppc))) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Manufacturing share of value added',
       y = 'Purchasing power parity for housing',
       color = 'Log GDP per capita',
       title = 'House prices and manufacturing output in the OECD',
       caption = '@jwhandley17',
       subtitle = 'OECD 2017 PPP benchmark results and National Accounts at a glance')
