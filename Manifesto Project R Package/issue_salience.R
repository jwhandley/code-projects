library(tidyverse)
library(manifestoR)
library(Hmisc)
mp_setapikey("manifesto_apikey.txt")

mp_maindataset() -> df

df %>%
  mutate(year = round(date/100)) %>%
  filter(pervote>10,year>1945,parfam %in% c(30,50,60)) %>%
  ggplot(aes(x=year,y=rile,color=factor(parfam))) + geom_smooth()

df %>%
  mutate(year = round(date/100)) %>%
  group_by(year,countryname) %>%
  mutate(mean_rile = wtd.mean(rile,pervote,na.rm=T)) %>%
  ungroup() %>%
  mutate(rel_rile = rile - mean_rile) %>%
  filter(parfam %in% c(30,40,50,60),year>=1945) %>%
  ggplot(aes(x=year,y=rel_rile,color=factor(parfam))) + geom_smooth() + labs(y='Left-Right position relative to the average voter in country',color='Party family',title='Relative Left-Right Positions by Party Family',subtitle='Comparative Manifesto Project 2019b',caption='@jwhandley17') + scale_color_manual(values=c('red','orange','black','blue'),labels=c('Social democratic','Liberal','Christian Democratic','Conservative')) + ggsave('rel_rile_parfam.png',width=8,height=5,type='cairo')

df %>%
  mutate(year=round(date/100)) %>%
  filter(parfam==30,year>=1945) %>%
  mutate(`Market Regulation` = per403, `Planning` = per404, `Corporatism` = per405, `Technology and Infrastructure` = per411, `Economic Control` = per412, `Nationalisation` = per413) %>%
  gather(key='Dimension',value='Frequency',c(`Market Regulation`,`Planning`,`Corporatism`,`Technology and Infrastructure`,`Economic Control`,`Nationalisation`)) %>%
  ggplot(aes(x=year,y=Frequency,color=Dimension)) + geom_smooth() + labs(title='Characteristics of Social Democratic Party Manifestos Since 1945',subtitle='Comparative Manifesto Project 2019b',caption='@jwhandley17') + ggsave('dimension_freq.png',width=8,height=5,type='cairo')
  
df %>%
  mutate(year=round(date/100)) %>%
  filter(parfam==30,year>=1945) %>%
  mutate(`Welfare state` = per504-per505, `Education` = per506-per507,`Environment`=per501,`Equality`=per503) %>%
  gather(key='Dimension',value='Frequency',c(`Welfare state`,`Education`,`Environment`,`Equality`)) %>%
  ggplot(aes(x=year,y=Frequency,color=Dimension)) + geom_smooth() + labs(title='More Characteristics of Social Democratic Party Manifestos Since 1945',subtitle='Comparative Manifesto Project 2019b',caption='@jwhandley17') + ggsave('welfare_dimension_freq.png',width=8,height=5,type='cairo')
