library(tidyverse)
library(haven)
library(stargazer)
df <- read_sav("1973~2008/0814.sav",encoding='cp932')
View(df)

df %>%
  mutate(jimin=as.numeric(Q42==1),none=as.numeric(Q42==9)) %>%
  mutate(occ=as_factor(Q54)) %>%
  group_by(KAISU,occ) %>%
  summarise(jimin=mean(jimin),none=mean(none)) %>%
  gather('party','share',jimin,none) -> data

ggplot(data,aes(x=occ,y=share,fill=party)) + geom_bar(stat='identity',position='dodge') + facet_grid(KAISU~.) + theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(filter(data,party=='jimin'),aes(x=KAISU,y=share,colour=occ)) + geom_line(width=1) + geom_point() + xlab('Survey wave') + ylab('Share') + ggtitle('LDP support by occupation')
