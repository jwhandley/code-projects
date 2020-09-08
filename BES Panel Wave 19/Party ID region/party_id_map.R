library(tidyverse)
library(haven)
library(Hmisc)
library(labelled)
library(sf)
library(ggthemes)
library(cowplot)

df <- read_sav('~/Code Projects/BES Panel Wave 19/BES2019_W19_v0.5.sav')
shp <- read_sf('NUTS1/NUTS_Level_1__January_2018__Boundaries.shp')

df %>%
  filter(partyId != 9999,gor!=13,partyId %in% c(1,2,3)) %>%
  group_by(gor) %>%
  mutate(pop = sum(wt)) %>%
  ungroup() %>%
  group_by(gor,partyId) %>%
  summarise(share = sum(wt)/mean(pop),objectid = max(gor)) -> data


shp1 <- inner_join(shp,data)

con <- ggplot(filter(shp1,partyId==1)) + 
  geom_sf(aes(fill=share)) +
  scale_fill_gradient(low='white',high='blue') +
  labs(fill='Conservative ID',
       title='Conservative Party')

lab <- ggplot(filter(shp1,partyId==2)) + 
  geom_sf(aes(fill=share)) +
  scale_fill_gradient(low='white',high='red') +
  labs(fill='Labour ID',
       title='Labour Party')

lib <- ggplot(filter(shp1,partyId==3)) + 
  geom_sf(aes(fill=share)) +
  scale_fill_gradient(low='white',high='orange') +
  labs(fill='Lib Dem ID',
       title='Liberal Democrats')

plot_grid(con,lab,lib,align='h',scale=1)

ggplot(shp1,aes(fill=share)) +
  geom_sf(size=0.5) +
  facet_wrap(~as_factor(partyId)) +
  scale_fill_gradient(low='white',high='black') +
  ggthemes::theme_map() +
  ggsave('party_id_region.png',width=16,height=9,type='cairo')


data %>%
  select(-objectid) %>%
  mutate(party = as_factor(partyId),gor = as_factor(gor)) %>%
  select(-partyId) %>%
  spread(party,share) %>%
  write_csv('party_id_region.csv')
