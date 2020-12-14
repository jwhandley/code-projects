library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(mapproj)
library(scales)
library(cowplot)

context16 <- read_csv('context16.csv')
df16 <- read_spss('31116396_National2016.por') %>%
  mutate(educ = as_factor(EDUC12R),
         race = as_factor(RACE),
         income = recode(as.numeric(INCOME16),`1`='Under $30,000',`2`='$30,000-$49,999',`3`='$50,000-$99,999',`4`='$100,000-$199,999',`5`='$200,000-$249,999',`6`='$250,000 or more',.default=NA_character_),
         state = str_trim(as_factor(STANUM)),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  inner_join(context16) %>%
  filter(!is.na(race),!is.na(income)) %>%
  mutate(income = factor(income,levels=c('Under $30,000','$30,000-$49,999','$50,000-$99,999','$100,000-$199,999','$200,000-$249,999','$250,000 or more'))) %>%
  select(vote,educ,race,income,state,degree,black,hispanic,dem16,median_hh_income)

context12 <- read_csv('context12.csv')
df12 <- read_spss('usmi2012-natelec.por') %>%
  mutate(educ = case_when(EDUC10 <=2 ~ 'High school or less',
                          EDUC10 == 3 ~ 'Some college/assoc. degree',
                          EDUC10 == 4 ~ 'College graduate',
                          EDUC10 == 5 ~ 'Postgraduate study'),
         race = as_factor(RACE),
         income = recode(as.numeric(INCOME12),`1`='Under $30,000',`2`='$30,000-$49,999',`3`='$50,000-$99,999',`4`='$100,000-$199,999',`5`='$200,000-$249,999',`6`='$250,000 or more'),
         state = str_trim(as_factor(STANUM)),
         vote = ifelse(PRES %in% c(1,2),as.numeric(PRES==1),NA_real_)) %>%
  mutate(educ = factor(educ,levels=levels(df16$educ))) %>%
  inner_join(context12) %>%
  filter(!is.na(race),!is.na(income)) %>%
  mutate(income = factor(income,levels=c('Under $30,000','$30,000-$49,999','$50,000-$99,999','$100,000-$199,999','$200,000-$249,999','$250,000 or more'))) %>%
  select(vote,educ,race,income,state,degree,black,hispanic,dem12,median_hh_income)

res.16 <- glmer(vote ~ (1|educ) + (1|income) + (1|state:educ) + (1|educ:income) + (1|state:income) + (1|state:educ:income) + degree + black + hispanic + dem16,df16,family=binomial(link='logit'))
summary(res.16)

res.12 <- glmer(vote ~ (1|educ) + (1|income) + (1|state:educ) + (1|educ:income) + (1|state:income) + (1|state:educ:income) + degree + black + hispanic + dem12,df12,family=binomial(link='logit'))
summary(res.12)

output.16 <- expand_grid(state=unique(df16$state),income=levels(df16$income),educ=levels(df16$educ)) %>%
  inner_join(context16)
output.16$clinton <- predict(res.16,output.16,allow.new.levels=T,type='response')

output.12 <- expand_grid(state=unique(df12$state),income=levels(df12$income),educ=levels(df12$educ)) %>%
  inner_join(context12)
output.12$obama <- predict(res.12,output.12,allow.new.levels=T,type='response')

output <- expand_grid(state=unique(df16$state),income=levels(df16$income),educ=levels(df16$educ)) %>%
  inner_join(select(output.12,state,income,educ,obama)) %>%
  inner_join(select(output.16,state,income,educ,clinton)) %>%
  mutate(swing = clinton - obama)

data <- left_join(map_data('state'),mutate(output,region=tolower(state)))

data %>%
  mutate(educ = factor(educ,levels=c('High school or less','Some college/assoc. degree','College graduate','Postgraduate study')),
         income = factor(income,levels=c('Under $30,000','$30,000-$49,999','$50,000-$99,999','$100,000-$199,999','$200,000-$249,999','$250,000 or more'))) %>%
  filter(!is.na(income),!is.na(educ)) %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=swing)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='#E81B23',mid='white',high='#3333FF',midpoint=0,labels=percent) +
  theme_map() +
  facet_grid(rows=vars(educ),cols=vars(income),switch='y') +
  labs(title='Swing between 2012 and 2016 presidential elections by education and income',
       fill='Swing') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('educ_income_swing.png',width=16*1.5,height=7*1.5,type='cairo')
