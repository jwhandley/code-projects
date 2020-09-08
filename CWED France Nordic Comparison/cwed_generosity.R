library(tidyverse)

df <- read_csv('cwed-subset.csv')

df %>%
  rename(`Pension` = PGEN, `Unemployment benefit` = UEGEN, `Sickness benefit` = SKGEN, `Total` = TOTGEN) %>%
  select(COUNTRY,YEAR,`Pension`,`Unemployment benefit`,`Sickness benefit`,`Total`) %>%
  gather(key='Benefit',value='Generosity',c(`Pension`,`Unemployment benefit`,`Sickness benefit`,`Total`)) %>%
  ggplot(aes(x=YEAR,y=Generosity,color=COUNTRY)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Benefit, scales = 'free_y') +
  labs(x = 'year',
       color = 'Country',
       title='Benefit generosity in Denmark, Finland, France, Norway and Sweden 1970-2012',
       subtitle='Comparative welfare entitlements dataset (CWED)',
       caption='@jwhandley17') +
  ggsave('cwed-generosity.png',width=8,height=5,type='cairo')
