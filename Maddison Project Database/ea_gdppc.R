library(tidyverse)
library(haven)
library(scales)

df <- read_dta('mpd2018.dta')

df %>%
  filter(countrycode %in% c('JPN','KOR','TWN','USA'),year>=1950) %>%
  ggplot(aes(x=year,y=cgdppc,color=country)) +
  geom_line(size=1) +
  scale_y_log10(labels = scales::dollar) +
  labs(y = 'US dollars at purchasing power parity (multi-benchmark)',
       title = 'GDP per capita in Japan, Korea, Taiwan, and the US since 1950',
       subtitle = 'Maddison Project Database 2018') +
  ggsave('ea_gdppc.png',width=8,height=5,type='cairo')
