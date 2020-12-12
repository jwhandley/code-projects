library(tidyverse)
library(Hmisc)
library(scales)
library(cowplot)
library(stargazer)

df <- read_csv('gss_extract.csv')

df %>%
  filter(EQWLTH > 0, EQWLTH < 8,
         EDUC < 97,
         RACE != 0,
         CLASS > 0, CLASS < 8,
         CONINC > 0, CONINC < 999998,
         HOMPOP < 98) %>%
  mutate(faminc = CONINC/sqrt(HOMPOP),
         class = case_when(CLASS == 1 ~ 'Lower class',
                           CLASS == 2 ~ 'Working class',
                           CLASS == 3 ~ 'Middle class',
                           CLASS == 4 ~ 'Upper class',
                           TRUE ~ NA_character_),
         educ = case_when(EDUC <= 12 ~ 'High school',
                          EDUC > 12 & EDUC < 16 ~ 'Some college',
                          EDUC >= 16 ~ 'Degree'),
         race = case_when(RACE == 1 ~ 'White',
                          RACE == 2 ~ 'Black',
                          RACE == 3 ~ 'Other'),
         redist = as.numeric(EQWLTH <= 3),
         year = YEAR,
         wgt = COMPWT,
         id = ID) %>%
  mutate(race = factor(race,levels=c('White','Black','Other')),
         educ = factor(educ,levels=c('Some college','High school','Degree')),
         class = factor(class,levels=c('Working class','Lower class','Middle class','Upper class'))) %>%
  group_by(year) %>%
  mutate(quintile = cut(faminc,wtd.quantile(faminc,wgt,seq(0,1,0.2)),include.lowest=T,c('Bottom quintile','Second quintile','Middle quintile','Fourth quintile','Top quintile'))) %>%
  ungroup() %>%
  filter(!is.na(quintile)) %>%
  select(id,year,wgt,class,race,educ,faminc,quintile,redist) -> data
  

data %>%
  group_by(year,educ) %>%
  summarise(redist = wtd.mean(redist,wgt,na.rm=T)) %>%
  ggplot(aes(x=year,y=redist,color=educ)) +
  geom_line() +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x='Year',
       y='Share supporting redistribution',
       color='Education level',
       title='Opinions on redistribution by education',
       subtitle='GSS 1978-2018') +
  scale_y_continuous(labels=percent) -> educ

data %>%
  group_by(year,quintile) %>%
  summarise(redist = wtd.mean(redist,wgt,na.rm=T)) %>%
  ggplot(aes(x=year,y=redist,color=quintile)) +
  geom_line() +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x='Year',
       y='Share supporting redistribution',
       color='Family income quintile',
       title='Opinions on redistribution by income',
       subtitle='GSS 1978-2018') +
  scale_y_continuous(labels=percent) -> income

data %>%
  group_by(year,race) %>%
  summarise(redist = wtd.mean(redist,wgt,na.rm=T)) %>%
  ggplot(aes(x=year,y=redist,color=race)) +
  geom_line() +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x='Year',
       y='Share supporting redistribution',
       color='Race',
       title='Opinions on redistribution by race',
       subtitle='GSS 1978-2018') +
  scale_y_continuous(labels=percent) -> race

data %>%
  group_by(year,class) %>%
  summarise(redist = wtd.mean(redist,wgt,na.rm=T)) %>%
  ggplot(aes(x=year,y=redist,color=class)) +
  geom_line() +
  geom_point() +
  geom_smooth(method='lm') +
  labs(x='Year',
       y='Share supporting redistribution',
       color='Subjective class',
       title='Opinions on redistribution by class',
       subtitle='GSS 1978-2018') +
  scale_y_continuous(labels=percent) -> class

save_plot('plot_grid.png',plot_grid(educ,income,race,class,align='hv'),base_width=12, base_height = 12/1.5)

res <- glm(redist ~ class*I(year-1980) + quintile*I(year-1980) + educ*I(year-1980) + race*I(year-1980),data,family=binomial)
summary(res)

stargazer(res,covariate.labels = c('Intercept','Lower class','Middle class','Upper class','Time trend','Second quintile','Middle quintile','Fourth quintile','Top quintile','High school','Degree','Black','Other','Lower class trend','Middle class trend','Upper class trend','Second quintile trend','Middle quintile trend','Fourth quintile trend','Top quintile trend','High school trend','Degree trend','Black trend','Other trend'),out='output.html')
