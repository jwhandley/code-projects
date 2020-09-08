library(tidyverse)
library(haven)
library(stargazer)
df <- read_sav("2013/1119.sav",encoding='cp932')
View(df)

df %>%
  mutate(jimin=as.numeric(Q42==1),none=as.numeric(Q42==10)) %>%
  mutate(occ=as_factor(P15)) %>%
  group_by(occ) %>%
  summarise(LDP=mean(jimin),None=mean(none)) %>%
  gather('Party','Share',LDP,None) -> data

ggplot(data,aes(x=occ,y=Share,fill=Party)) + geom_bar(stat='identity',position='dodge') + theme(axis.text.x = element_text(angle = 30, hjust = 1)) + xlab('Occupation') + ggtitle('Japanese Party Support by Occupation in 2013') + ggsave('ldp_support_occupation.png',width=16,height=10,type='cairo')

ggplot(filter(data,party=='jimin'),aes(x=KAISU,y=share,colour=occ)) + geom_line(width=1) + geom_point() + xlab('Survey wave') + ylab('Share') + ggtitle('LDP support by occupation')
