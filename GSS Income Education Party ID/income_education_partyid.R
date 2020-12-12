library(tidyverse)
library(Hmisc)
library(scales)
library(lme4)

df <- read_csv('gss_extract.csv') %>%
  mutate(faminc = ifelse(faminc > 0 & faminc < 999997,faminc,NA_real_),
         educ = ifelse(educ > 0 & educ < 97,educ,NA_real_),
         partyid = ifelse(partyid < 8,partyid+1,NA_real_)) %>%
  mutate(faminc = cut(faminc,breaks=c(-Inf,25000,50000,75000,100000,Inf),labels = c('<25k','25-50k','50-75k','75-100k','>100k'),include.lowest=T),
         educ = cut(educ,breaks=c(-Inf,12,16,Inf),labels=c('High school','Some college','Degree'),include.lowest = T)) %>%
  filter_at(vars(faminc,educ,partyid),~!is.na(.x))

res <- lmer(partyid ~ (1|educ:year) + (1|faminc:year) + (1|faminc:educ:year),df)
summary(res)

output <- expand_grid(faminc = factor(levels(df$faminc),levels=levels(df$faminc)),educ = factor(levels(df$educ),levels=levels(df$educ)),year=unique(df$year))
output$partyid <- predict(res,output,allow.new.levels=T)

output %>%
  ggplot(aes(x=faminc,y=partyid,color=educ,group=educ)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  labs(x='Family Income',
       y='Party ID (low = Democrat, high = Republican)',
       title='Party ID by Income and Education',
       color='Education') +
  ggsave('income_education_partyid.png',width=16,height=10,type='cairo')
