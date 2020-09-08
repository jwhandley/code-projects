library(tidyverse)

df <- read_csv('lis_gini_pov.csv')

df %>%
  mutate(`Gini coefficient`=gini,`Poverty rate`=poorAll6) %>%
  gather(key='measure',value='value',c(`Gini coefficient`,`Poverty rate`)) %>%
  ggplot(aes(x=year,y=value,color=country)) +
  geom_line(size=1) +
  facet_wrap(~measure,scale='free_y')
