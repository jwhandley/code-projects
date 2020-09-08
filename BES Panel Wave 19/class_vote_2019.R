library(tidyverse)
library(Hmisc)
library(ggrepel)

df <- read_csv('ge2019_wave.csv') %>%
  mutate(turnout = na_if(turnout,9999),lab = as.numeric(vote=='Labour'),con = as.numeric(vote=='Conservative'),labcon = as.numeric(lab+con==1),rel_age = scale(age)[,1]) %>%
  mutate_at(vars(lr_scale,al_scale),~(.x-wtd.mean(.x,wt,na.rm=T))/sqrt(wtd.var(.x,wt,na.rm=T)))

test <- read_dta('BES2019_W19_Panel_v0.2.dta')

data <- filter(df,labcon==1)

df %>%
  filter(!is.na(ns_sec)) %>%
  mutate(class = recode(ns_sec,`1.1`='Higher managers',`1.2`='Higher professionals',`2`='Lower managers',`3`='Intermediate occupations',`4`='Self-employed',`5`='Lower supervisory and technical',`6` = 'Semi-routine',`7`='Routine')) %>%
  group_by(`class`) %>%
  summarise(lr = wtd.mean(lr_scale,wt,na.rm=T),al = wtd.mean(al_scale,wt,na.rm=T)) %>%
  ggplot(aes(x=lr,y=al,color=class,label=class)) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Left-right values',
       y = 'Libertarian-Authoritarian values',
       color = 'Occupational class',
       title='Average economic and social values by occupation',
       subtitle='British Election Study Internet Panel Wave 19') +
  scale_x_continuous(limits=c(-0.6,0.6)) +
  scale_y_continuous(limits=c(-0.6,0.6)) +
  coord_fixed() +
  theme_bw() -> p1

df %>%
  filter(!is.na(vote)) %>%
  mutate(vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Green','Plaid Cymru','Other'))) %>%
  group_by(vote) %>%
  summarise(lr = wtd.mean(lr_scale,wt,na.rm=T),al = wtd.mean(al_scale,wt,na.rm=T)) %>%
  ggplot(aes(x=lr,y=al,color=vote,label=vote)) +
  geom_point() +
  geom_text_repel() +
  labs(x = 'Left-right values',
       y = 'Libertarian-Authoritarian values',
       color = 'GE2019 vote',
       title='Average economic and social values by GE2019 vote',
       subtitle='British Election Study Internet Panel Wave 19') +
  scale_x_continuous(limits=c(-0.6,0.6)) +
  scale_y_continuous(limits=c(-0.6,0.6)) +
  coord_fixed() +
  scale_color_manual(values = c('#0087DC','#DC241f','#FAA61A','yellow','#6AB023','#008142','grey')) + 
  theme_bw() -> p2

df %>%
  filter(!is.na(vote)) %>%
  mutate(vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Green','Plaid Cymru','Other'))) %>%
  group_by(vote) %>%
  summarise(lr = wtd.mean(lr_scale,wt,na.rm=T),al = wtd.mean(al_scale,wt,na.rm=T))

library(cowplot)

plot_grid(p1,p2,align='hv')

df %>%
  filter(!is.na(tenure)) %>%
  mutate(tenure = recode(tenure,`1` = 'Own outright', `2` = 'Own with mortage', `3` = 'Own other', `4` = 'Private rent', `5` = 'Social rent', `6` = 'Social rent', `7` = 'Other', `8` = 'Other', `9` = 'Other')) %>%
  group_by(tenure) %>%
  summarise(lr = wtd.mean(lr_scale,wt,na.rm=T),al = wtd.mean(al_scale,wt,na.rm=T)) %>%
  ggplot(aes(x=lr,y=al,color=tenure,label=tenure)) + 
  geom_point() +
  geom_text_repel() +
  labs(x = 'Left-right values',
       y = 'Libertarian-Authoritarian values',
       color = 'House tenure',
       title='Average economic and social values by house tenure',
       subtitle='British Election Study Internet Panel Wave 19') +
  scale_x_continuous(limits=c(-0.6,0.6)) +
  scale_y_continuous(limits=c(-0.6,0.6)) +
  coord_fixed() +
  theme_bw() + 
  ggsave('tenure_values.png',width=8,height=6,type='cairo')

df %>%
  filter(!is.na(educ)) %>%
  mutate(educ = recode(educ,`1`='No qualifications', `2` = ))

p2 + ggsave('ge19vote_values.png',width=8,height=6,type='cairo')
