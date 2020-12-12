library(tidyverse)
library(MASS)
library(ggrepel)

gini <- read_csv('swiid8_3_summary.csv')
sied <- read_csv('sied_gen.csv')
pmr <- read_csv('oecd_pmr.csv')
regime <- read_csv('welfare_regime.csv')

df <- left_join(left_join(inner_join(gini,pmr),sied),regime)

reg_gen <- inner_join(sied,pmr) %>%
  group_by(country) %>%
  filter(year==max(year))

df %>%
  group_by(country) %>%
  filter(year==max(year)) -> data

res <- lm(gini_disp ~ pmr,data)
summary(res)

res1 <- lm(log(gini_mkt/gini_disp) ~ pmr,data)
summary(res1)

res2 <- lm(gen ~ pmr,reg_gen)
summary(res2)

df %>%
  filter(!is.na(regime),year>=2000) %>%
  group_by(regime) %>%
  summarise(pmr = mean(pmr), disp = mean(gini_disp,na.rm=T), mkt = mean(gini_mkt,na.rm=T), gen = mean(gen,na.rm=T))


df %>%
  filter(!is.na(regime),year>=1980,year<=2017) %>%
  group_by(regime,year) %>%
  summarise(gini = median(gini_disp,na.rm=T)) %>%
  ggplot(aes(x=year,y=gini,color=regime)) +
  geom_line() +
  labs(x='Year',
       y='Median gini coefficient for disposable income',
       color='Esping-Andersen welfare regime',
       title='Changes in inequality by welfare regime since 1980',
       caption="Source: SWIID 8.3, author's calculations") +
  scale_color_manual(values=c('black','orange','red')) +
  ggsave('inequality_regime.png',width=8,height=5,type='cairo')

df %>%
  filter(!is.na(regime),!is.na(gen)) %>%
  group_by(regime,year) %>%
  summarise(gen = mean(gen,na.rm=T)) %>%
  ggplot(aes(x=year,y=gen,color=regime)) +
  geom_point() +
  geom_line() +
  labs(x='Year',
       y='Average social insurance generosity index',
       color='Esping-Andersen welfare regime',
       title='Changes in welfare generosity by welfare regime since 1980',
       caption="Source: University of Stockholm SIED, author's calculations") +
  scale_color_manual(values=c('black','orange','red')) +
  ggsave('welfare_gen_regime.png',width=8,height=5,type='cairo')

  
