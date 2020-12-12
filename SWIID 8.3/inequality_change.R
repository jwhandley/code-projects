library(tidyverse)
library(haven)
library(ggrepel)
library(scales)

df <- swiid_summary

df %>%
  filter(!is.na(rel_red)) %>%
  group_by(country) %>%
  filter(year %in% c(1985,2015)) %>%
  mutate(disp = log(gini_disp) - lag(log(gini_disp)), mkt = log(gini_mkt) - lag(log(gini_mkt))) %>%
  filter(!is.na(disp)) %>%
  mutate(red = mkt - disp) %>%
  ungroup() -> data


data %>%
  ggplot(aes(x=mkt,y=disp,label=country)) +
  geom_smooth(method='lm') +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels=percent) +
  scale_y_continuous(labels=percent) +
  labs(x='Percent change in market Gini',
       y='Percent change in disposable Gini',
       title='Change in inequality 1985-2015')

res <- lm(disp ~ mkt,data)
summary(res)
