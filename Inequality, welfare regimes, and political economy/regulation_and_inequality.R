library(tidyverse)
library(haven)

gini <- read_csv('swiid8_3_summary.csv')
cwed <- read_csv('cwed_gen.csv')
etcreg <- read_csv('etcreg.csv')
pmreg <- read_csv('pmreg.csv') %>%
  spread(ind,reg)
regime <- read_csv('welfare_regime.csv')

df <- inner_join(left_join(left_join(inner_join(gini,cwed),etcreg),pmreg),regime)
reg <- inner_join(pmreg,etcreg)

reg_validity <- lm(PMR ~ etcreg,reg)
summary(reg_validity)

pmr_test <- lm(gini_mkt ~ PMR,df)
summary(pmr_test)

etc_test <- lm(gini_mkt ~ etcreg,df)
summary(etc_test)


res <- lm(gini_disp ~ totgen + etcreg,df)
summary(res)

res_pmr <- lm(gini_disp ~ totgen + PMR,df)
summary(res_pmr)

df %>%
  filter(!is.na(etcreg)) %>%
  gather(key='Indicator',value='Value',c(etcreg,gini_mkt)) %>%
  ggplot(aes(x=year,y=Value,color=country)) +
  geom_line() +
  facet_wrap(~Indicator,scales='free_y')

df %>%
  mutate(decade = floor(year/10)*10) %>%
  filter(!is.na(etcreg),decade>=1980) %>%
  group_by(regime,decade) %>%
  summarise(disp = mean(gini_disp,na.rm=T), mkt = mean(gini_mkt,na.rm=T), reg = mean(etcreg,na.rm=T), gen = mean(totgen,na.rm=T)) %>%
  write_csv('results_decade_summary.csv')

df %>%
  filter(country %in% c('United Kingdom','Sweden')) %>%
  ggplot(aes(x=year,y=gini_disp,color=country)) +
  geom_line() +
  geom_hline(yintercept = 25.5)
