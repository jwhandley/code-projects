library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('2019 Canadian Election Study - Phone Survey v1.0.dta')

df %>%
  filter(q70r > 0,
         vote > 0,
         vote < 8) %>%
  mutate(income = recode(as.numeric(q70r),`1`='<$30k',`2`='<$30k',`3`='$30k-$60k',`4`='$60k-$90k',`5`='$90k-$110k',`6`='$110k-$150k',`7`='$150k-$200k',`8`='>$200k',.default=NA_character_),
         income = factor(income,levels=c('<$30k','$30k-$60k','$60k-$90k','$90k-$110k','$110k-$150k','$150k-$200k','>$200k')),
         vote = recode(as.numeric(vote),`1`='Liberal',`2`='Conservative',`3`='NDP',`4`='BQ',.default='Other'),
         vote = factor(vote,levels=c('Liberal','Conservative','NDP','BQ','Other'))) %>%
  group_by(income) %>%
  mutate(pop = sum(weight_PES,na.rm=T)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(weight_PES,na.rm=T)/mean(pop,na.rm=T),
            n = sum(weight_PES,na.rm=T)) %>%
  mutate(se = share*(1-share)/sqrt(n)) %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote),size=0.75) +
  geom_ribbon(aes(ymin=share-2*se,ymax=share+2*se,fill=vote),alpha=0.3) + 
  scale_color_manual(values=c('#EA6D6A','#6495ED','#F4A460','#87CEFA','grey')) +
  scale_fill_manual(values=c('#EA6D6A','#6495ED','#F4A460','#87CEFA','grey')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household Income',
       y='Vote share',
       title='Income and voting in the 2019 Canadian federal election',
       color = 'Party',
       fill = 'Party') +
  ggsave('ces19_phone_income.png',width = 8,height = 5,type = 'cairo')
