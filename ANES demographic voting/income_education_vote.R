library(tidyverse)
library(Hmisc)
library(scales)
library(lme4)

df <- read_csv('anes_extract.csv') %>%
  mutate(gender = as.numeric(gender==2),
         race = recode(race,`1`='White',`2`='Black',`3`='Hispanic',.default='Other'),
         educ = recode(educ,`1`='High school',`2`='High school',`3`='Some college',`4`='Degree',.default=NA_character_),
         faminc = recode(faminc,`1`='0-16',`2`='17-33',`3`='34-67',`4`='68-95',`5`='96-100',.default=NA_character_),
         turnout = as.numeric(turnout==2),
         pres = ifelse(pres %in% c(1,2),as.numeric(pres==1),NA_real_),
         house = ifelse(house %in% c(1,2),as.numeric(house==1),NA_real_),
         region = recode(region,`1`='Northeast',`2`='Midwest',`3`='South',`4`='West')) %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic','Other')),
         educ = factor(educ,levels=c('High school','Some college','Degree')),
         faminc = factor(faminc,levels=c('0-16','17-33','34-67','68-95','96-100'))) %>%
  filter(!is.na(pres),!is.na(region))

res <- glmer(pres ~ (1|faminc:year) + (1|educ:year) + (1|educ:faminc:year),df,family=binomial)
summary(res)


output <- expand_grid(educ=factor(levels(df$educ),levels=levels(df$educ)),faminc=factor(levels(df$faminc),levels=levels(df$faminc)),year=unique(df$year))
output$dem <- predict(res,output,allow.new.levels=T,type='response')

output %>%
  ggplot(aes(x=faminc,y=dem,color=educ,group=educ)) +
  geom_line() +
  geom_point() +
  facet_wrap(~year) +
  scale_y_continuous(labels=percent) +
  labs(x='Family Income Percentile',
       y='Democrat Vote Share',
       color='Education',
       title='Vote for president by income and education') +
  ggsave('income_education_vote_anes.png',width=16,height=10,type='cairo')
