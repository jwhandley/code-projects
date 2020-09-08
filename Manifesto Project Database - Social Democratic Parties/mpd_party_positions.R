library(tidyverse)
library(readr)

mpd_dataset <- read_csv('mpd_dataset.csv')

mpd_dataset %>%
  filter(countryname=='New Zealand',partyabbrev %in% c('Labour','National','Greens','NZF')) %>%
  mutate(party=factor(partyabbrev,levels=c('Labour','National','Greens','NZF'))) %>%
  group_by(party) %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> nz

mpd_dataset %>%
  filter(countryname=='Canada',partyabbrev %in% c('LP','CP','RPC','PCP','NDP','CCF','BQ')) %>%
  mutate(partyabbrev=recode(partyabbrev,'LP'='Liberal Party','CP'='Conservatives','RPC'='Conservatives','PCP'='Conservatives','CCF'='NDP','BQ'='Bloc Quebecois')) %>%
  mutate(party=factor(partyabbrev,levels=c('Liberal Party','Conservatives','NDP','Bloc Quebecois'))) %>%
  group_by(party) %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> ca

mpd_dataset %>%
  filter(countryname=='United States',!is.na(partyabbrev)) %>%
  mutate(party=factor(partyabbrev,levels=c('Democrats','Republicans'))) %>%
  group_by(party) %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> us

mpd_dataset %>%
  filter(countryname=='Australia',partyname %in% c('Australian Labor Party','Australian Greens','Liberal Party of Australia','National Party of Australia')) %>%
  mutate(partyname=recode(partyname,'Australian Labor Party'='Labor','Australian Greens'='Greens','Liberal Party of Australia'='Liberal Party','National Party of Australia'='National Party')) %>%
  mutate(party=factor(partyname,levels=c('Labor','Greens','Liberal Party','National Party'))) %>%
  group_by(party) %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> au

mpd_dataset %>%
  filter(countryname=='United Kingdom',partyabbrev %in% c('Labour','Conservatives')) %>%
  mutate(party=factor(partyabbrev,levels=c('Labour','Conservatives'))) %>%
  group_by(party) %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> uk


ggplot(ca,aes(x=year,y=position,colour=party)) + geom_line(size=1) + geom_point() + scale_color_manual(values=c('#EA6D6A','#6495ED','#F4A460','#87CEFA')) + facet_grid(~dimension) + ggtitle('Issue positions of major Canadian political parties') + ggsave('CA_parties.png',width=8,height=5,type='cairo')

ggplot(us,aes(x=year,y=position,colour=party)) + geom_line(size=1) + geom_point() + scale_color_manual(values=c('#3333FF','#E81B23')) + facet_grid(~dimension) + ggtitle('Issue positions of major US political parties') + ggsave('US_parties.png',width=8,height=5,type='cairo')

ggplot(au,aes(x=year,y=position,colour=party)) + geom_line(size=1) + geom_point() + scale_color_manual(values=c('#DE3533','#39b54a','#1456F1','#0047AB','#006644')) + facet_grid(~dimension) + ggtitle('Issue positions of major Australian political parties') + ggsave('AU_parties.png',width=8,height=5,type='cairo')

mpd_dataset %>%
  filter(countryname=='Denmark',partyname=='Social Democratic Party') %>%
  gather('dimension','position',c(lr,al)) %>%
  mutate(dimension=factor(dimension,levels=c('lr','al'))) %>%
  select(year,dimension,position) -> dk

ggplot(dk,aes(x=year,y=position,colour=dimension)) + geom_line(size=1) + geom_point()

res <- lmer('pervote ~ lr + al + lr*al + (1|year) + (1|countryname)',data=filter(mpd_dataset,parfam==30))
summary(res)

mpd_dataset %>%
  filter(parfam==30) %>%
  group_by(,countryname,year) %>%
  summarise(lr=weighted.mean(lr,pervote),al=weighted.mean(al,pervote),vote=sum(pervote)) -> data


res1 <- plm('vote ~ lr + al + lr*al',data=data,index=c('countryname','year'),model='within')
summary(res1)


ggplot(uk,aes(x=year,y=position,colour=party)) + geom_line(size=1) + geom_point() + scale_color_manual(values=c('#E81B23','#3333FF')) + facet_grid(~dimension) + ggtitle('Issue positions of major UK political parties') + ggsave('UK_parties.png',width=8,height=5,type='cairo')
