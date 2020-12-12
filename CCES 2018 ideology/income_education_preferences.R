library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(scales)

df <- read_dta('cces18_common_vv.dta') %>%
  mutate(m4a=as.numeric(CC18_327a==1),
         wall = as.numeric(CC18_322a==1))

res.m4a <- glmer(m4a ~ (1|educ) + (1|faminc_new),df,family=binomial)
summary(res.m4a)

res.wall <- glmer(wall ~ (1|educ) + (1|faminc_new) + (1|educ:faminc_new),df,family=binomial)
summary(res.wall)

output <- expand_grid(faminc_new = seq(1,16,1),
                      educ = seq(1,6,1))

output$m4a <- predict(res.m4a,output,allow.new.levels=T,type='response')
output$wall <- predict(res.wall,output,allow.new.levels=T,type='response')

output %>%
  ggplot(aes(x=factor(faminc_new),y=m4a,color=factor(educ),group=factor(educ))) +
  geom_point() +
  geom_line() +
  scale_x_discrete(labels=c('<10k','10-20k','20-30k','30-40k','40-50k','50-60k','60-70k','70-80k','80-100k','100-120k','120-150k','150-200k','200-250k','250-350k','350-500k','>500k')) +
  scale_color_discrete(labels=levels(as_factor(df$educ))[1:6]) +
  scale_y_continuous(labels=percent) +
  labs(x='Family Income',
       y='Support Medicare for All',
       color='Education',
       title='Support for Medicare for All by Income and Education')

output %>%
  ggplot(aes(x=factor(faminc_new),y=wall,color=factor(educ),group=factor(educ))) +
  geom_point() +
  geom_line() +
  scale_x_discrete(labels=c('<10k','10-20k','20-30k','30-40k','40-50k','50-60k','60-70k','70-80k','80-100k','100-120k','120-150k','150-200k','200-250k','250-350k','350-500k','>500k')) +
  scale_color_discrete(labels=levels(as_factor(df$educ))[1:6]) +
  scale_y_continuous(labels=percent) +
  labs(x='Family Income',
       y='Support Border Wall',
       color='Education',
       title='Support for Border Wall by Income and Education')
