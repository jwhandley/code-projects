library(tidyverse)
library(haven)
library(Hmisc)
library(glue)
library(scales)
library(lme4)

df <- read_dta('cumulative_2006-2019.dta') %>%
  filter(!is.na(educ)) %>%
  mutate(dem = as.numeric(voted_rep_party==1)) %>%
  mutate(faminc = as_factor(faminc),
         educ = as_factor(educ))

res <- glmer(dem ~ (1|faminc:year) + (1|educ:year) + (1|faminc:educ:year) + (1|year),df,family=binomial)
summary(res)


output <- expand_grid(educ = factor(levels(df$educ)[2:7],levels=levels(df$educ)[2:7]),faminc = factor(levels(df$faminc)[1:12],levels=levels(df$faminc)[1:12]),year=seq(2006,2018,2))
output$dem <- predict(res,output,type='response',allow.new.levels=T)

output %>%
  ggplot(aes(x=faminc,y=dem,color=educ,group=educ)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  scale_y_continuous(labels=percent) +
  labs(x='Family Income',
       y='Democrat vote share',
       color='Education',
       title='Voting in House Elections by Income and Education') +
  theme(axis.text.x = element_text(angle=90)) +
  ggsave('income_educ_vote.png',width=16,height=10,type='cairo')


