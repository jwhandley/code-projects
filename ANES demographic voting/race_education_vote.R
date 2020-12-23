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

res <- glmer(pres ~ (1|race) + (1|educ) + (1|region) + (1|year) + (1|race:educ) + (1|race:region) + (1|educ:region) + (1|race:educ:region) + (1|race:year) + (1|educ:year) + (1|race:educ:year) + (1|race:region:year),df,family=binomial)
summary(res)

output <- expand_grid(race = factor(levels(df$race),levels=levels(df$race)),
                      educ = factor(levels(df$educ),levels=levels(df$educ)),
                      region = unique(df$region),
                      year = unique(df$year))

output$vote <- predict(res,output,allow.new.levels=T,type='response')

output %>%
  filter(race=='White') %>%
  ggplot(aes(x=year,y=vote,color=educ)) +
  geom_point() +
  geom_line() +
  facet_wrap(~region) +
  labs(title='Democratic vote share among white voters by education and region')

df %>%
  group_by(year) %>%
  summarise(nat_vote = wtd.mean(pres,wgt,na.rm=T)) -> nat.vote

output %>%
  inner_join(nat.vote) %>%
  mutate(lean = vote - nat_vote) %>%
  filter(race=='White') %>%
  ggplot(aes(x=year,y=lean,color=educ)) +
  geom_point() +
  geom_line(size=1) +
  facet_wrap(~region) +
  labs(title='Partisan lean among white voters by education and region',
       y='% voting Democratic mins national Democratic vote share',
       fill='Education',
       x='Year') +
  scale_y_continuous(labels=percent)
