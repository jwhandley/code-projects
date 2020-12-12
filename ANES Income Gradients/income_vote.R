library(tidyverse)
library(scales)
library(lme4)

df <- read_csv('income_vote_anes.csv')

df %>%
  gather(key='percentile',value='share',-year,-vote) %>%
  ggplot(aes(x=percentile,y=share,color=vote)) +
  geom_point() +
  facet_wrap(~year) +
  scale_color_manual(values=c('blue','orange','red'))

df %>%
  gather(key='percentile',value='share',-year,-vote) %>%
  spread(vote,share) %>%
  mutate(inccat = case_when(percentile == '0-16' ~ 16,
                            percentile == '17-33' ~ 33,
                            percentile == '34-67' ~ 67,
                            percentile == '68-95' ~ 95,
                            percentile == '95-100' ~ 100),
         inccode = case_when(percentile == '0-16' ~ 1,
                             percentile == '17-33' ~ 2,
                             percentile == '34-67' ~ 3,
                             percentile == '68-95' ~ 4,
                             percentile == '95-100' ~ 5)) -> data

res.dem <- lm(dem ~ inccode + I(inccode^2) + as_factor(year),data)
summary(res.dem)

res.rep <- lm(rep ~ inccode + as_factor(year),data)
summary(res.rep)

res.dnv <- lm(dnv ~ inccode + as_factor(year),data)
summary(res.dnv)

data %>%
  gather(key='vote',value='share',c(dem,rep,dnv)) %>%
  ggplot(aes(x=inccat,y=share,color=vote)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~year) + 
  scale_color_manual(values=c('blue','orange','red'),labels=c('Democrat',"Didn't vote",'Republican')) +
  labs(x='Percentile of family income',
       y='Share of population',
       color='Vote for president',
       title='Income and presidential vote 1948-2016',
       subtitle='American National Election Study') +
  ggsave('income_vote.png',width=8*1.2,height=5*1.2,type='cairo')

data %>%
  write_csv('date_cleaned.csv')
