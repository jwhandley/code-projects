library(tidyverse)

df <- read_csv('2014UTASP20150910.csv',locale=locale(encoding='cp932'))
voters <- read_csv('2014_2016UTASV20161004.csv',locale=locale(encoding='cp932'))

df %>%
  mutate_at(vars(Q7_1,Q7_2,Q7_3,Q7_4,Q7_5,Q7_6,Q7_7,Q7_8,Q7_9,Q7_10),~na_if(.x,99)) %>%
  mutate(econ=Q7_2-Q7_3-Q7_4-Q7_8+Q7_10,social=Q7_1+Q7_5+Q7_7+Q7_9-Q7_6) %>%
  mutate(econ=-(econ-mean(econ,na.rm=T))/sd(econ,na.rm=T),social=-(social-mean(social,na.rm=T))/sd(social,na.rm=T)) %>%
  mutate(party=recode(PARTY,`1`='LDP',`2`='DPJ',`3`='JRP',`4`='Komeito',`6`='JCP',`8`='SDP',.default='Other')) %>%
  mutate(party=factor(party,levels=c('LDP','DPJ','Komeito','JCP','JRP','SDP','Other')))-> data

voters %>%
  mutate_at(vars(W1Q17_1,W1Q17_2,W1Q17_3,W1Q17_4,W1Q17_5,W1Q17_6,W1Q17_7,W1Q17_8,W1Q17_9,W1Q17_10),~na_if(.x,99)) %>%
  mutate(econ=W1Q17_2-W1Q17_3-W1Q17_4-W1Q17_8+W1Q17_10,social=W1Q17_1+W1Q17_5+W1Q17_7+W1Q17_9-W1Q17_6) %>%
  mutate(econ=-(econ-mean(econ,na.rm=T))/sd(econ,na.rm=T),social=-(social-mean(social,na.rm=T))/sd(social,na.rm=T)) %>%
  mutate(vote=recode(W1Q2,`1`='LDP',`2`='DPJ',`3`='JRP',`4`='Komeito',`6`='JCP',`8`='SDP',.default='Other')) %>%
  mutate(party=recode(W1Q18_1,`1`='LDP',`2`='DPJ',`3`='JRP',`4`='Komeito',`6`='JCP',`8`='SDP',.default='Other')) %>%
  mutate(vote=factor(vote,levels=c('LDP','DPJ','Komeito','JCP','JRP','SDP','Other'))) %>%
  mutate(party=factor(party,levels=c('LDP','DPJ','Komeito','JCP','JRP','SDP','Other'))) -> data_voters

data %>%
  group_by(party) %>%
  summarise(econ=mean(econ,na.rm=T),social=mean(social,na.rm=T)) -> party_means

data_voters %>%
  group_by(party) %>%
  summarise(econ=mean(econ,na.rm=T),social=mean(social,na.rm=T)) -> party_means_voters

data_voters %>%
  group_by(vote) %>%
  summarise(econ=mean(econ,na.rm=T),social=mean(social,na.rm=T)) -> party_means_voters1


ggplot(data,aes(x=econ,y=social,color=party)) + geom_jitter(alpha=0.5) + scale_color_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + geom_point(data=party_means,aes(x=econ,y=social,fill=party),color='black',size=4,shape=21) + scale_fill_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + xlab('Economic left-right') + ylab('Social progressive-conservative') + ggtitle('Ideology of Japanese general election candidates in 2014') + theme(aspect.ratio=1) + ggsave('japan_2014_leg_spectrum.png',type='cairo',dpi=300)

ggplot(data_voters,aes(x=econ,y=social,color=party)) + geom_jitter(alpha=0.5) + scale_color_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + geom_point(data=party_means_voters,aes(x=econ,y=social,fill=party),color='black',size=4,shape=21) + scale_fill_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + xlab('Economic left-right') + ylab('Social progressive-conservative') + ggtitle('Ideology of the Japanese electorate by party ID in 2014') + theme(aspect.ratio=1) + ggsave('japan_2014_vot_id_spectrum.png',type='cairo',dpi=300)

ggplot(data_voters,aes(x=econ,y=social,color=vote)) + geom_jitter(alpha=0.5) + scale_color_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + geom_point(data=party_means_voters1,aes(x=econ,y=social,fill=vote),color='black',size=4,shape=21) + scale_fill_manual(values=c('green','blue','orange','red','yellow','pink','grey')) + xlab('Economic left-right') + ylab('Social progressive-conservative') + ggtitle('Ideology of the Japanese electorate by PR block vote in 2014') + theme(aspect.ratio=1) + ggsave('japan_2014_vote_spectrum.png',type='cairo',dpi=300)

library(nnet)

res <- multinom('vote~econ+social',data=data_voters)
summary(res)

library(mlogit)

res1 <- mlogit(formula='vote~econ+social',data=data_voters)
summary(res1)
