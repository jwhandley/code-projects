library(tidyverse)
library(Hmisc)
library(haven)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(class = recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7),generalElectionVote = replace_na(generalElectionVote,0)) %>%
  filter(generalElectionVote!=9999,!is.na(class)) %>%
  mutate(vote=recode(as.numeric(generalElectionVote),`0`="Didn't vote",`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`12`='Brexit Party',.default='Other')) %>%
  group_by(class) %>%
  summarise(`Labour`=weighted.mean(as.numeric(vote=='Labour',wt)),`Conservative`=weighted.mean(as.numeric(vote=='Conservative',wt)),`Liberal Democrat`=weighted.mean(as.numeric(vote=='Liberal Democrat',wt)),`Didn't vote`=weighted.mean(as.numeric(vote=="Didn't vote"),wt)) %>%
  gather(key='Party',value='Vote share',c(`Labour`,`Conservative`,`Liberal Democrat`,`Didn't vote`)) %>%
  mutate(Party = factor(Party,levels=c('Conservative','Labour','Liberal Democrat',"Didn't vote"))) -> data

ggplot(data,aes(x=factor(class),y=`Vote share`,fill=Party)) + geom_bar(stat='identity',position='dodge') + xlab('NS-SEC Analytic class') + ggtitle('Class voting in the 2019 general election') + labs(caption='British Election Study Panel Wave 19') + scale_fill_manual(values=c('#0087DC','#DC241f','#FAA61A','darkgrey')) + ggsave('class_vote_2019.png',width=8,height=,type='cairo')

df %>%
  mutate(class = recode(as.numeric(ns_sec_analytic),`11`=1.1,`12`=1.2,`20`=2,`30`=3,`40`=4,`50`=5,`60`=6,`70`=7)) %>%
  mutate(class = factor(class)) %>%
  filter(genElecTurnoutRetro!=9999,!is.na(class)) %>%
  mutate(turnout=as.numeric(genElecTurnoutRetro==1)) %>%
  group_by(class) %>%
  summarise(turnout=weighted.mean(turnout,wt)) %>%
  ggplot(aes(x=factor(class),y=turnout)) + geom_bar(stat='identity') + labs(caption='British Election Study Internet Panel Wave 19') + ggtitle('Turnout in the 2019 general election by occupational class') + xlab('NS-SEC occupational class') + ylab('Self-reported Turnout Rate') + ggsave('ge19_turnout.png',width=8,height=5,type='cairo')
  