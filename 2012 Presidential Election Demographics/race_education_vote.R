library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)
library(maps)
library(mapproj)

test <- read_spss('usmi2012-natelec.por')

context <- read_csv('context.csv')
df <- read_spss('usmi2012-natelec.por') %>%
  mutate(state = str_trim(as_factor(STANUM)),
         race = as_factor(RACE),
         educ = as_factor(EDUC10)) %>%
  inner_join(context) %>%
  mutate(vote = ifelse(PRES %in% c(1,2,9),as.numeric(PRES==1),NA_real_)) %>%
  select(vote,state,race,educ,degree,median_hh_income,dem12,black,hispanic,evangelical,STANUM)

res <- glmer(vote ~ (1|race) + (1|race:educ) + (1|race:state) + (1|race:educ:state) + degree + dem12 + black + hispanic,df,family=binomial)
summary(res)

output <- expand_grid(state = unique(df$state),race=levels(df$race),educ=levels(df$educ)) %>%
  inner_join(context)
  
output$vote <- predict(res,output,allow.new.levels=T,type='response')

us.map <- map_data('state')

data <- left_join(us.map,mutate(output,region=tolower(state)))

data %>%
  mutate(race = factor(race,levels=c('White','Black','Hispanic/Latino','Asian','Other')),
         educ = factor(educ,levels=c('No high school diploma','High school graduate','Some college/assoc. degree','College graduate','Postgraduate study'))) %>%
  ggplot(mapping=aes(x=long,y=lat,group=group,fill=vote)) +
  geom_polygon(color='grey') +
  coord_map(projection='albers',lat0=39,lat1=45) +
  scale_fill_gradient2(low='red',mid='white',high='blue',guide=FALSE,midpoint=0.5) +
  theme_map() +
  facet_grid(rows=vars(race),cols=vars(educ),switch='y') +
  theme(strip.placement = 'outside')
