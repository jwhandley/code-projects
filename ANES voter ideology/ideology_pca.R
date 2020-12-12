library(tidyverse)
library(Hmisc)
library(lme4)
library(scales)

df <- read_csv('anes_extract.csv')

df %>%
  mutate(gender = as.numeric(gender==2),
         race = recode(race,`1`='White',`2`='Black',`3`='Hispanic',`4`='Other',.default=NA_character_),
         educ = recode(educ,`1`='High school',`2`='High school',`3`='Some college',`4`='Degree',.default=NA_character_),
         region = recode(region,`1`='Northeast',`2`='Midwest',`3`='South',`4`='West',.default=NA_character_),
         south = as.numeric(south==1),
         faminc = recode(faminc,`1`='0-16',`2`='17-33',`3`='34-67',`4`='68-95',`5`='96-100',.default=NA_character_),
         partyid = ifelse(partyid > 0,partyid,NA_real_),
         turnout = as.numeric(turnout==2),
         president = recode(president,`1`='Democrat',`2`='Republican',`3`='Other',`4`='Other',`7`="Didn't vote",.default=NA_character_),
         pres2p = ifelse(president %in% c('Democrat','Republican'),as.numeric(president=='Democrat'),NA_real_),
         house = ifelse(house>0,as.numeric(house==1),NA_real_),
         affirmative_action = ifelse(affirmative_action<8,as.numeric(affirmative_action==1),NA_real_),
         gay_discrimination = ifelse(gay_discrimination<8,as.numeric(affirmative_action==1),NA_real_)) %>%
  mutate_at(vars(polviews,health,jobs_income,aid_blacks,abortion,bible,immigration),~ifelse(.x>0 & .x<8,.x,NA_real_)) %>%
  filter_at(vars(health,jobs_income,aid_blacks,abortion,bible,affirmative_action,gay_discrimination,immigration),~!is.na(.x)) -> data

pca <- prcomp(select(data,health,jobs_income,aid_blacks,abortion,bible,affirmative_action,gay_discrimination,immigration),scale.=T)
pca$rotation

data$econ <- pca$x[,1]
data$social <- pca$x[,2]

res.econ <- lmer(econ ~ (1|faminc) + (1|educ) + (1|faminc:educ) + (1|year) + (1|faminc:year) + (1|educ:year) + (1|faminc:educ:year),data)
summary(res.econ)

res.social <- lmer(social ~ (1|faminc) + (1|educ) + (1|faminc:educ) + (1|faminc:year) + (1|educ:year),data)
summary(res.social)

res.vote.demog <- glmer(pres2p ~ (1|faminc) + (1|educ) + (1|faminc:educ) + (1|faminc:year) + (1|educ:year),data,family=binomial)

output <- expand_grid(educ = unique(data$educ),
                      faminc = unique(data$faminc),
                      year = unique(data$year)) %>%
  mutate(educ = factor(educ,levels=c('High school','Some college','Degree')),
         faminc = factor(faminc,levels=c('0-16','17-33','34-67','68-95','96-100'))) %>%
  filter_at(vars(educ,faminc),~!is.na(.x))

output$econ <- predict(res.econ,output,allow.new.levels=T)
output$social <- predict(res.social,output,allow.new.levels=T)
output$vote <- predict(res.vote.demog,output,allow.new.levels=T,type='response')

output %>%
  ggplot(aes(x=faminc,y=econ,color=educ,group=educ)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~year) +
  labs(x='Family Income Percentile',
       y='Economic left (+)/right (-)',
       color='Education',
       title='Predicted economic left-right values by income and education') +
  ggsave('lrecon.png',width=8,height=5,type='cairo')

output %>%
  ggplot(aes(x=faminc,y=social,color=educ,group=educ)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~year) +
  labs(x='Family Income Percentile',
       y='Social liberal (+)/conservative (-)',
       color='Education',
       title='Predicted social liberal-conservative values by income and education') +
  ggsave('libconsocial.png',width=8,height=5,type='cairo')

output %>%
  ggplot(aes(x=faminc,y=vote,color=educ,group=educ)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~year) +
  scale_y_continuous(labels=percent) +
  labs(x='Family Income Percentile',
       y='Democrat Vote Share',
       color='Education',
       title='Predicted Democrat vote by income and education') +
  ggsave('demogvote.png',width=8,height=5,type='cairo')


res.vote <- glmer(pres2p ~ (econ|year) + (social-1|year),data,family=binomial)
summary(res.vote)
ranef(res.vote)


output.vote <- expand_grid(econ = seq(-1.5,1.5,0.01),
                           social = c(-1.5,0,1.5),
                           year = unique(data$year))
output.vote$pred <- predict(res.vote,output.vote,allow.new.levels=T,type='response')

output.vote %>%
  ggplot(aes(x=econ,y=pred,color=factor(social))) +
  geom_line() +
  facet_wrap(~year) +
  scale_color_discrete(labels=c('Conservative','Moderate','Liberal')) +
  scale_x_continuous(breaks=c(-1,0,1),labels=c('Right','Moderate','Left')) +
  scale_y_continuous(labels=percent) +
  labs(x='Economic left-right',
       y='Predicted Democrat Vote Share',
       color='Social liberal-conservative',
       title='Predicted Two-Party Presidential Vote by Economic and Social Values') +
  ggsave('econsocvote.png',width=8,height=5,type='cairo')
