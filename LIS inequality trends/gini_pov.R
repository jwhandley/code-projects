library(tidyverse)
library(ggrepel)
library(plm)

df <- read_csv('gini_pov.csv')

ggplot(df,aes(x=gini,y=poverty)) +
  geom_smooth(method='lm',color='black') +
  geom_point(aes(alpha=year,color=country),size=2) +
  coord_fixed(1/80) +
  labs(y = 'Poverty Rate',
       x = 'Gini coefficient',
       alpha = 'Year',
       title = 'Inequality and poverty across time and space',
       subtitle = 'Luxembourg Income Study',
       caption = '@jwhandley17') +
  ggsave('inequality_poverty.png',width = 16,height = 10,type='cairo')

res <- plm(poverty ~ gini + 1,df,index = c('country','year'), effect = 'twoways')
summary(res)
