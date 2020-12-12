library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('cps_00174.dta')

df %>%
  mutate(status = case_when(
    empstat == 10 ~ 'Employed',
    empstat == 12 ~ 'Employed',
    empstat == 20 ~ 'Unemployed',
    empstat == 21 ~ 'Unemployed',
    empstat == 22 ~ 'Unemployed',
    empstat == 32 ~ 'Unable to work',
    empstat == 36 ~ 'Retired',
    age < 16 ~ 'Child',
    TRUE ~ 'Not in the Labor Force'
  ),
  faminc = ftotval/sqrt(famsize)) %>%
  mutate(decile = cut(faminc,wtd.quantile(faminc,asecwt,seq(0,1,0.1)),include.lowest=T,seq(1,10,1))) %>%
  group_by(decile,status) %>%
  summarise(n = sum(asecwt)) %>%
  group_by(decile) %>%
  mutate(share = n/sum(n)) %>%
  select(decile,status,share) %>%
  ggplot(aes(x=decile,y=share,fill=status)) +
  geom_bar(stat='identity') +
  labs(title='Labor market status by position in the income distribution',
       x='Decile of equivalised gross family income',
       y='Share of population',
       fill='Labor market status') +
  scale_y_continuous(labels=percent)
  
df %>%
  group_by(empstat) %>%
  summarise(n())
