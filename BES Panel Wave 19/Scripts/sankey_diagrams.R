library(tidyverse)
library(haven)
library(Hmisc)
library(ggalluvial)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(profile_past_vote_2015=replace_na(profile_past_vote_2015,5000),profile_past_vote_2017=replace_na(profile_past_vote_2017,5000),pastvote_ge_2019=replace_na(pastvote_ge_2019,5000)) %>%
  mutate(vote15 = recode(as.numeric(profile_past_vote_2015),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`6`='UKIP',`12`='Brexit party',`5000`="Didn't vote",`0`=NA_character_,`9999`=NA_character_,.default='Other'),vote17 = recode(as.numeric(profile_past_vote_2017),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`6`='UKIP',`12`='Brexit party',`5000`="Didn't vote",`0`=NA_character_,`9999`=NA_character_,.default='Other'),vote19 = recode(as.numeric(pastvote_ge_2019),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`6`='UKIP',`12`='Brexit party',`5000`="Didn't vote",`0`=NA_character_,`9999`=NA_character_,.default='Other')) %>%
  filter(!is.na(vote15),!is.na(vote17),!is.na(vote19)) %>%
  group_by(vote15,vote17,vote19) %>%
  summarise(n=sum(wt)) %>%
  gather(key='election',value='party',c(vote15,vote17,vote19)) %>%
  mutate(party = factor(party,levels=c('Conservative','Labour','Liberal Democrat','UKIP','Other',"Didn't vote"))) -> data
  

ggplot(data,aes(y=n,x=election,stratum=party,alluvium=n,fill=party,label=party)) + geom_flow() + geom_stratum() + geom_text(stat='stratum',size=3) + scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','#70147A','lightgrey','darkgrey')) + scale_x_discrete(labels=c('2015','2017','2019')) + theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + ggtitle('Flow of votes between the 2015, 2017 and 2019 general elections') + labs(subtitle='BES Internet Panel Wave 19',caption='@jwhandley17',fill='Party',x='Election') + ggsave('ge15_17_19_sankey.png',width=8,height=5,type='cairo')

df %>%
  filter(profile_past)
  filter(profile_past_vote_2005==2) %>%
  summarise(wtd.mean(as.numeric(profile_eurefvote==1),wt,na.rm=T))

df %>%
  filter(profile_past_vote_2010==2) %>%
  summarise(wtd.mean(as.numeric(profile_eurefvote==1),wt,na.rm=T))

df %>%
  filter(profile_past_vote_2015==2) %>%
  summarise(wtd.mean(as.numeric(profile_eurefvote==1),wt,na.rm=T))
