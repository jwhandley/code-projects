library(tidyverse)
library(Hmisc)
library(haven)

acs <- read_dta('usa_00065.dta')
gss <- read_csv('sub-data.csv') %>%
  rename(occ2010=OCC10)

acs %>%
  filter(wkswork2 == 6) %>%
  group_by(occ2010) %>%
  summarise(wage = wtd.mean(incwage/uhrswork,perwt,na.rm=T), unemp = wtd.mean(as.numeric(empstat==2),perwt,na.rm=T)) -> occ_stat
  

df <- inner_join(gss,occ_stat)

occ_stat %>%
  ggplot(aes(x=log(wage),y=log(unemp))) +
  geom_point() +
  labs(x='Log average occupational wage',
       y='Log average unemployment rate')

res <- lm(I(8-EQWLTH) ~ log(wage) + log(unemp),filter(df,EQWLTH>0,EQWLTH<8,unemp!=0))
summary(res)

res1 <- lm(I(4-NATSOC) ~ log(wage) + log(unemp),filter(df,NATSOC>0,NATSOC<8,unemp!=0))
summary(res1)

res2 <- lm(I(4-NATFARE) ~ log(wage) + log(unemp),filter(df,NATFARE>0,NATFARE<8,unemp!=0))
summary(res2)

res3 <- lm(I(4-NATHEAL) ~ log(wage) + log(unemp),filter(df,NATHEAL>0,NATHEAL<8,unemp!=0))
summary(res3)
