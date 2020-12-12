library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('usa_00063.dta')

df %>%
  filter(ftotinc < 9999999,
         ftotinc > 0,
         age < 86,
         year == 2018) %>%
  mutate(faminc = ftotinc/famsize) %>%
  group_by(age) %>%
  summarise(`Median` = wtd.quantile(faminc,perwt,0.5,na.rm=T),
            `25th percentile` = wtd.quantile(faminc,perwt,0.25,na.rm=T),
            `75th percentile` = wtd.quantile(faminc,perwt,0.75,na.rm=T)) %>%
  gather(key='Quantile',value='Family Income',c(`25th percentile`,`Median`,`75th percentile`)) %>%
  ggplot(aes(x=age,y=`Family Income`,color=Quantile)) +
  geom_line() +
  labs(x='Age',
       y='Family Income Per Capita',
       color='Income Quantile',
       title='Age Profile of Family Income in 2018',
       subtitle='American Community Survey',
       caption='John Handley') +
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) +
  ggsave('faminc_age_profile_2018.png',width=8,height=5,type='cairo')
  
df %>%
  filter(ftotinc < 9999999,
         ftotinc > 0,
         age < 86,
         year != 2018) %>%
  mutate(faminc = ftotinc/famsize) %>%
  group_by(year,age) %>%
  summarise(`Median` = wtd.quantile(faminc,perwt,0.5,na.rm=T),
            `25th percentile` = wtd.quantile(faminc,perwt,0.25,na.rm=T),
            `75th percentile` = wtd.quantile(faminc,perwt,0.75,na.rm=T)) %>%
  gather(key='Quantile',value='Family Income',c(`25th percentile`,`Median`,`75th percentile`)) %>%
  ggplot(aes(x=age,y=`Family Income`,color=Quantile)) +
  geom_line() +
  facet_wrap(~year,scales='free_y') +
  labs(x='Age',
       y='Family Income Per Capita',
       color='Income Quantile',
       title='Age Profile of Family Income',
       subtitle='American Community Survey',
       caption='John Handley') +
  theme_bw() +
  scale_y_continuous(labels=scales::dollar) +
  ggsave('faminc_age_profiles.png',width=8,height=5,type='cairo')

df %>%
  filter(ftotinc < 9999999,
         ftotinc > 0,
         age < 86,
         year != 2018) %>%
  mutate(faminc = ftotinc/famsize) -> data

reg <- lm(faminc ~ as_factor(age) + as_factor(year) + as_factor(sex),data)
summary(reg)