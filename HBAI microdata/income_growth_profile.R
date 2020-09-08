require(Hmisc)
library(tidyverse)
library(haven)
library(scales)

df <- read_dta('UKDA-5828-stata/stata/stata11_se/h1819_all.dta')

data <- df %>%
  filter(s_oe_ahc>0) %>%
  group_by(year) %>%
  summarise(enframe(wtd.quantile(s_oe_ahc,gs_newpp,na.rm=T,seq(0.05,0.95,0.01)),'quantile','income')) %>%
  mutate(year = as_factor(year)) %>%
  spread(quantile,income)

write_csv(data,'income_quantiles.csv')

df %>%
  filter(s_oe_ahc>0) %>%
  mutate(famtype = recode(as.numeric(newfambu),`1` = "Pensioner couple", `2` = "Single pensioner", `3` = "Single pensioner", `4` = "Working age with children",`5` = "Working age with children", `6` = "Working age without children", `7` = "Working age without children", `8` = "Working age without children")) %>%
  group_by(year,famtype) %>%
  summarise(enframe(wtd.quantile(s_oe_ahc,gs_newpp,na.rm=T,seq(0.05,0.95,0.01)),'quantile','income')) %>%
  mutate(year = as_factor(year)) %>%
  spread(year,income) %>%
  mutate(growth = log(`2018/19`/`2011/12`)) %>%
  mutate(quantile = as.numeric(sub("%", "", quantile))) %>%
  select(famtype,quantile,growth) %>%
  ggplot(aes(x=quantile,y=growth,color=famtype)) +
  geom_line() +
  labs(x = 'Quantile of equivalised disposable household income after housing costs',
       y = 'Compound growth rate 2011/12-2018/19',
       color = 'Family type',
       title = 'Profile of income growth in the UK 2011/12-2018/19 by family type',
       subtitle = "Department for Work and Pensions; author's calculations",
       caption = '@jwhandley17') +
  scale_y_continuous(labels = percent_format()) +
  ggsave('growth_famtype.png',width=8,height=5,type='cairo')
