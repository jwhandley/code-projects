library(tidyverse)
library(readxl)
library(maps)
library(Hmisc)
library(cowplot)

df <- read_excel('pvi_trend.xlsx') %>%
  group_by(state) %>%
  mutate(swing = pvi - lag(pvi)) %>%
  group_by(year) %>%
  mutate(rel_swing = scale(swing)[,1])

us_map <- map_data('state')

data <- left_join(us_map,mutate(df,region=tolower(state)))

data %>%
  filter(year>=1988) %>%
  ggplot(aes(x=long,y=lat,fill=pvi,group=state)) +
  geom_polygon(color='grey',size=0.25) +
  coord_map('albers',39,45) +
  theme_map() +
  scale_fill_gradient2(limits=c(-.55,0.55)) +
  facet_wrap(~year)
