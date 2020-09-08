library(tidyverse)
library(Hmisc)
library(ggrepel)

df <- read_csv('ge2019_wave.csv') %>%
  mutate(turnout = na_if(turnout,9999))

df %>%
  group_by(region) %>%
  summarise(al = wtd.mean(al_scale, wt, na.rm = T),lr = wtd.mean(lr_scale, wt, na.rm = T)) %>%
  ggplot(aes(x = lr, y = al, label = region)) +
  geom_point() +
  geom_text_repel() +
  coord_fixed() +
  labs(x = 'Left-right scale',
       y = 'Libertarian-Authoritarian scale',
       title = 'Average left-right and libertarian-authoritarian values by region',
       subtitle = 'British Election Study Internet Panel Wave 19',
       caption = '@jwhandley17') +
  ggsave('region_values.png',width = 8, height = 8, type = 'cairo')

df %>%
  mutate(lab = as.numeric(vote == 'Labour')) -> data

res <- glm(lab ~ as_factor(tenure) + hhincome + educ,data,family=binomial)
summary(res)
