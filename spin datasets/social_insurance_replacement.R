library(tidyverse)
library(readxl)

df <- read_excel("SIED/SIED 1930-2015 190320.xlsx")
crosswalk <- read_csv('SIED/country_lookup.csv')

inner_join(df,crosswalk) -> data


data %>%
  filter(country %in% seq(1,18,1)) %>%
  mutate(pension=prtstwco,unemployment=urtsw26f,sickness=srtstw1f) %>%
  group_by(year) %>%
  summarise(pension=mean(pension,na.rm=T),unemployment=mean(unemployment,na.rm=T),sickness=mean(sickness,na.rm=T)) %>%
  gather(key='benefit',value='replacement',c(pension,unemployment,sickness)) %>%
  ggplot(aes(x=year,y=replacement,color=benefit)) + geom_line(size=1) + geom_point(size=1) +
  labs(y='Average replacement rate',color='Benefit type',title='Social insurance expansion and retrenchment in 18 OECD countries',subtitle='Stockholm Univeristy Social Insurance Entitlements Dataset',caption='@jwhandley17') +
  ggsave('rep_rate.png',width=8,height=5,type='cairo')

data %>%
  filter(country_name=='Finland') %>%
  mutate(pension=prtaverp,unemployment=urtsw26f,sickness=srtstw1f) %>%
  gather(key='benefit',value='replacement',c(pension,unemployment,sickness)) %>%
  ggplot(aes(x=year,y=replacement,color=benefit)) + geom_line(size=1) + geom_point(size=1)
