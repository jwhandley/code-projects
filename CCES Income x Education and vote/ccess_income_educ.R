library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cces18_common_vv.dta')

df %>%
  mutate(educ = as_factor(educ)) %>%
  filter(!is.na(faminc_new),faminc_new<97,CC18_327a<8) %>%
  group_by(faminc_new,educ) %>%
  summarise(m4a = wtd.mean(as.numeric(CC18_327a==1),commonweight,na.rm=T)) %>%
  spread(educ,m4a)

res <- glm(I(as.numeric(CC18_327a==1)) ~ as_factor(educ) + faminc_new,filter(df,!is.na(faminc_new),faminc_new<97,CC18_327a<8),family=binomial)
summary(res)
