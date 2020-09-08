library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_Panel_v0.2.dta')
df[] <- lapply(df, unclass)


df %>%
  mutate(p_past_vote_2019 = na_if(p_past_vote_2019,9999)) %>%
  filter(wave19==1) %>%
  mutate(lr_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(lr_scaleW17,lr_scaleW16),lr_scaleW14W15),lr_scaleW13),lr_scaleW10_W12),lr_scaleW7_W9),lr_scaleW6),lr_scaleW1_W5)) %>%
  mutate(al_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(al_scaleW17,al_scaleW16),al_scaleW14W15),al_scaleW13),al_scaleW10_W12),al_scaleW7_W9),al_scaleW6),al_scaleW1_W5)) %>%
  mutate(vote = p_past_vote_2019, turnout = p_turnout_2019, wt = wt_new_W19_result, vote_2010=p_past_vote_2010, age = ageW19, class_id = na_if(na_if(na_if(subjClassW19,9999),998),999), ns_sec = ns_sec_analyticW19, hhincome = ifelse(p_gross_householdW19>15,NA_integer_,as.numeric(p_gross_householdW19)), tenure = p_housingW19, educ = p_edlevelW19, region = gorW19, englishness = na_if(englishnessW17,9999),scottishness = na_if(scottishnessW17,9999), welshness = na_if(welshnessW17,9999), britishness = na_if(britishnessW17,9999)) %>%
  mutate(ns_sec = recode(ns_sec,`11`='1.1',`12`='1.2',`20`='2',`30`='3',`40`='4',`50`='5',`60`='6',`70`='7')) %>%
  mutate(ns_sec = factor(ns_sec,levels=c('1.1','1.2','2','3','4','5','6','7'))) %>%
  mutate(vote = recode(vote,`1` = 'Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`5`='Plaid Cymru',`6`='Other',`7`='Green',`8`='Other',`9`='Other',`11`='Other',`12`='Brexit party',`13`='Other')) %>%
  mutate(vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Plaid Cymru','Green','Brexit','Other'))) %>%
  mutate(vote_2010 = recode(vote_2010,`1` = 'Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`5`='Plaid Cymru',`6`='Other',`7`='Green',`8`='Other',`9`='Other',`11`='Other',`12`='Brexit party',`13`='Other',`9999`=NA_character_)) %>%
  mutate(vote_2010 = factor(vote_2010,levels=c('Conservative','Labour','Liberal Democrat','SNP','Plaid Cymru','Green','Brexit','Other'))) %>%
  filter(region<12) %>%
  mutate(region = recode(region,`1`='North East', `2` = 'North West', `3` = 'Yorkshire and the Humber', `4` = 'East Midlands', `5` = 'West Midlands', `6` ='East of England', `7` = 'London', `8` = 'South East', `9` = 'South West', `10` = 'Wales', `11` = 'Scotland')) %>%
  select(id,lr_scale,al_scale,vote,turnout,wt,vote_2010,age,class_id,ns_sec,hhincome,tenure,educ,region,englishness,scottishness,welshness,britishness) -> data

write_csv(data,'ge2019_wave.csv')

data %>%
  mutate(lab = as.numeric(vote=='Labour'),degree=as.numeric(educ>=4)) -> data1

ggplot(data1,aes(x=age,y=lab,weight=wt)) +
  geom_smooth() +
  facet_wrap(~region) -> age

data1 %>%
  gather(key='axis',value='position',c(lr_scale,al_scale)) %>%
  ggplot(aes(x=position,color=axis,y=lab,weight=wt)) +
  geom_smooth() +
  facet_wrap(~region) +
  scale_color_manual(values=c('#0087DC','#DC241f')) +
  theme(legend.position = 'bottom') -> values


data1 %>%
  filter(!is.na(ns_sec)) %>%
  group_by(ns_sec,region) %>%
  summarise(lab = wtd.mean(lab,wt,na.rm=T)) %>%
  ggplot(aes(x=ns_sec,y=lab)) +
  geom_bar(stat='identity',fill='#DC241f') +
  facet_wrap(~region) -> class

income_reg <- lm(hhincome ~ age + I(age*age),data1)
summary(income_reg)
data1[(!is.na(data$hhincome) & !is.na(data$age)),'hhincome_age'] <- residuals(income_reg)

data1 %>%
  group_by(educ,region) %>%
  summarise(lab = wtd.mean(lab,wt,na.rm=T)) %>%
  ggplot(aes(x=educ,y=lab)) +
  geom_bar(stat='identity') +
  facet_wrap(~region) -> educ

library(cowplot)

plot_grid(age,values,class,educ)

reg <- glm(lab ~ hhincome + age + degree + lr_scale + al_scale,data1,family = binomial)
summary(reg)

ggplot(data1,aes(x=lr_scale,y=scottishness)) +
  geom_smooth()
