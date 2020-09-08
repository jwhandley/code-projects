library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('UKDA-5828-stata/stata/stata11_se/h1819.dta')

df %>%
  group_by(oahcdec) %>%
  summarise(`Child tax credit` = wtd.mean(ctcbu,gs_newch,na.rm=T),`Working tax credit` = wtd.mean(wtcbu,gs_newch,na.rm=T)) %>%
  gather(key='Tax credit',value='£pw',c(`Child tax credit`,`Working tax credit`)) %>%
  ggplot(aes(x=factor(oahcdec),y=`£pw`,fill=`Tax credit`)) +
  geom_bar(stat='identity') +
  labs(x='Decile of equivalised disposable income after housing costs',
       title='Tax credit payments to families with children by income decile',
       subtitle="HBAI 2018/19 microdata") +
  ggsave('taxcred_decile.png',width=8,height=5)

df %>%
  group_by(oahcdec) %>%
  summarise(`Child tax credit` = wtd.mean(ctcbu,gs_newch,na.rm=T),`Working tax credit` = wtd.mean(wtcbu,gs_newch,na.rm=T))

head(df$chbenbu)

df$childben <- 21.05*as.numeric(df$DEPCHLDB>0) + 13.95*(df$DEPCHLDB-1)*as.numeric(df$DEPCHLDB>1)

df %>%
  group_by(oahcdec) %>%
  summarise(`Child tax credit` = wtd.mean(ctcbu,gs_newch,na.rm=T),`Child benefit` = wtd.mean(childben,gs_newch,na.rm=T)) %>%
  gather(key='Tax credit',value='£pw',c(`Child tax credit`,`Child benefit`)) %>%
  ggplot(aes(x=factor(oahcdec),y=`£pw`,fill=`Tax credit`)) +
  geom_bar(stat='identity') +
  labs(x='Decile of equivalised disposable income after housing costs',
       title='Child benefit payments to families with children by income decile',
       subtitle="HBAI 2018/19 microdata") +
  ggsave('childben_decile.png',width=8,height=5)

df %>%
  group_by(oahcdec) %>%
  summarise(`Child tax credit` = wtd.mean(ctcbu,gs_newch,na.rm=T),`Child benefit` = wtd.mean(childben,gs_newch,na.rm=T),`Number of children` = wtd.mean(DEPCHLDB,gs_newch,na.rm=T)) %>%
  mutate(`Total child benefits` = `Child tax credit` + `Child benefit`,`Benefit per child` = `Total child benefits`/`Number of children`)

df %>%
  mutate(change = 90*DEPCHLDB-ctcbu-childben-0.0375*egrernbu) %>%
  group_by(oahcdec) %>%
  summarise(Effect = wtd.mean(change,gs_newbu,na.rm=T),Legacy = wtd.mean(ctcbu+childben,gs_newbu,na.rm=T),New = wtd.mean(90*DEPCHLDB-0.0375*egrernbu,gs_newbu,na.rm=T)) %>%
  gather(key='System',value='£pw',c(`Legacy`,`New`)) %>%
  ggplot(aes(x=factor(oahcdec),y=`£pw`,fill=System)) +
  geom_bar(stat='identity',position='dodge') +
  labs(x='Decile of equivalised disposable income after housing costs',
       title='Effect of replacing child tax credit and child benefit with a universal £90pw child benefit',
       subtitle="HBAI 2018/19 microdata") +
  ggsave('new_chben_decile.png',width=10,height=8)

df %>%
  mutate(change = 90*DEPCHLDB-ctcbu-childben-0.0375*egrernbu) %>%
  mutate(incahc = s_oe_ahc + change) %>%
  mutate(pov = as.numeric(incahc<0.6*mdoeahc)) %>%
  filter(kidecobu!=0) %>%
  group_by(kidecobu) %>%
  summarise(New = wtd.mean(pov,gs_newch,na.rm=T),Legacy = wtd.mean(low60ahc,gs_newch,na.rm=T)) %>%
  gather(key='System',value='Poverty rate',c(`New`,`Legacy`)) %>%
  ggplot(aes(x=as_factor(kidecobu),y=`Poverty rate`,fill=System)) +
  geom_bar(stat='identity',position='dodge') +
  coord_flip() +
  labs(x="Economic stats of child's family by family type",
       title='Effect of new child benefit on child poverty',
       subtitle='HBAI 2018/19 microdata') +
  ggsave('new_chben_childpoverty.png',width=10,height=5,type='cairo')

df %>%
  mutate(change = 90*DEPCHLDB-ctcbu-childben-0.0375*egrernbu) %>%
  mutate(incahc = s_oe_ahc + change) %>%
  mutate(pov = as.numeric(incahc<0.6*mdoeahc)) %>%
  summarise(New = wtd.mean(pov,gs_newpp,na.rm=T),Legacy = wtd.mean(low60ahc,gs_newpp,na.rm=T))

df %>%
  mutate(kidecobu = as_factor(kidecobu)) %>%
  mutate(change = 90*DEPCHLDB-ctcbu-childben-0.0375*egrernbu) %>%
  mutate(incahc = s_oe_ahc + change) %>%
  mutate(pov = as.numeric(incahc<0.6*mdoeahc)) %>%
  filter(kidecobu!=0) %>%
  group_by(kidecobu) %>%
  summarise(New = wtd.mean(pov,gs_newch,na.rm=T),Legacy = wtd.mean(low60ahc,gs_newch,na.rm=T)) -> new_effect

write_csv(new_effect,'new_child_benefit_effect.csv')
