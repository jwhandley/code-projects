library(tidyverse)
library(scales)
library(ggrepel)

df <- read_csv('swiid8_3_summary.csv')

countries <- c('Australia',
               'Austria',
               'Belgium',
               'Canada',
               'Denmark',
               'Finland',
               'France',
               'Germany',
               'Ireland',
               'Italy',
               'Netherlands',
               'New Zealand',
               'Sweden',
               'Spain',
               'United Kingdom',
               'United States')

df %>%
  filter(country %in% countries) -> data

data %>%
  select(country,year,gini_disp,gini_disp_se,gini_mkt,gini_mkt_se) %>%
  gather(key='variable',value='value',-country, -year) %>%
  separate(variable,sep='_',into=c('variable','income_def','type')) %>%
  spread(type,value) %>%
  rename(gini = `<NA>`) %>%
  select(-variable) -> data1

ggplot(data1,aes(x=year,y=gini,color=income_def)) +
  geom_line() +
  geom_ribbon(aes(ymin = gini-1.96*se,
                  ymax = gini+1.96*se,
                  linetype=NA), alpha = .25) +
  facet_wrap(~country) +
  labs(x='Year',
       y='Gini',
       color='Income Definition') +
  scale_color_discrete(labels=c('Disposable','Market'))
