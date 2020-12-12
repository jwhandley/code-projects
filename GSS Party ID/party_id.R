library(tidyverse)
library(Hmisc)
library(scales)
library(cowplot)

df <- read_csv('gss_extract.csv')

df %>%
  filter(RELIG > 0, RELIG < 98,
         FUND > 0,
         AGE < 98,
         RACE > 0,
         EDUC < 97,
         HOMPOP < 98,
         CONINC > 0, CONINC < 999998,
         PARTYID < 7) %>%
  mutate(evangelical = as.numeric(RACE==1)*as.numeric(RELIG==1)*as.numeric(FUND==1),
         race = case_when(RACE == 1 ~ 'White',
                          RACE == 2 ~ 'Black',
                          RACE == 3 ~ 'Other'),
         educ = case_when(EDUC <= 12 ~ 'High school',
                          EDUC > 12 & EDUC < 16 ~ 'Some college',
                          EDUC >= 16 ~ 'Degree'),
         faminc = CONINC/sqrt(HOMPOP),
         democrat = ifelse(PARTYID < 3,1,0),
         age = AGE,
         year = YEAR,
         id = ID,
         wgt = COMPWT) %>%
  group_by(year) %>%
  mutate(income = cut(faminc,wtd.quantile(faminc,wgt,seq(0,1,1/3)),include.lowest = T,c('Low income','Middle income','High income'))) %>%
  ungroup() %>%
  filter(!is.na(income)) %>%
  select(id,year,age,race,educ,income,evangelical,faminc,democrat,wgt) -> data


data %>%
  write_csv('cleaned_data.csv')

data %>%
  group_by(year,evangelical) %>%
  summarise(dem = wtd.mean(democrat,wgt,na.rm=T),se = sqrt(dem*(1-dem)/n())) %>%
  ggplot(aes(x=year,y=dem,fill=evangelical)) +
  geom_line(aes(color=evangelical)) +
  geom_point(aes(color=evangelical)) +
  geom_ribbon(aes(ymin=dem-1.96*se,ymax=dem+1.96*se),alpha=0.15) +
  labs(x='Year',
       y='Democrat party ID',
       title='Religion',
       color='Religious group',
       fill='Religious group') +
  scale_y_continuous(labels=percent) -> relig

data %>%
  group_by(year,income) %>%
  summarise(dem = wtd.mean(democrat,wgt,na.rm=T),se = sqrt(dem*(1-dem)/n())) %>%
  ggplot(aes(x=year,y=dem,fill=income)) +
  geom_line(aes(color=income)) +
  geom_point(aes(color=income)) +
  geom_ribbon(aes(ymin=dem-1.96*se,ymax=dem+1.96*se),alpha=0.15) +
  labs(x='Year',
       y='Democrat party ID',
       title='Income',
       color='Income group',
       fill='Income group') +
  scale_y_continuous(labels=percent) -> income

data %>%
  group_by(year,educ) %>%
  summarise(dem = wtd.mean(democrat,wgt,na.rm=T),se = sqrt(dem*(1-dem)/n())) %>%
  ggplot(aes(x=year,y=dem,fill=educ)) +
  geom_line(aes(color=educ)) +
  geom_point(aes(color=educ)) +
  geom_ribbon(aes(ymin=dem-1.96*se,ymax=dem+1.96*se),alpha=0.15) +
  labs(x='Year',
       y='Democrat party ID',
       title='Education',
       color='Educational level',
       fill='Educational level') +
  scale_y_continuous(labels=percent) -> educ

data %>%
  group_by(year,race) %>%
  summarise(dem = wtd.mean(democrat,wgt,na.rm=T),se = sqrt(dem*(1-dem)/n())) %>%
  ggplot(aes(x=year,y=dem,fill=race)) +
  geom_line(aes(color=race)) +
  geom_point(aes(color=race)) +
  geom_ribbon(aes(ymin=dem-1.96*se,ymax=dem+1.96*se),alpha=0.15) +
  labs(x='Year',
       y='Democrat party ID',
       title='Race',
       color='Racial group',
       fill='Racial group') +
  scale_y_continuous(labels=percent) -> race

save_plot('plot_grid.png',plot_grid(relig,income,educ,race,align='hv'),base_width=12, base_height = 12/1.5)

data %>%
  group_by(income,educ) %>%
  summarise(dem = wtd.mean(democrat,wgt,na.rm=T)) %>%
  spread(income,dem)
