library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('NZES2017Release14-07-19.dta')

df %>%
  filter(rvptyvote != 0) %>%
  mutate(income = recode(as.numeric(rhhincome),`0`='<$23.8k',`1`='<$23.8k',`2`='$23.8k-$35.7k',`3`='$35.7k-$62.2k',`4`='$62.2k-$77k',`5`='$77k-$93.6k',`6`='$93.6k-$136.6k',`7`='$136.6k-$180.2k',`8`='>$180.2k',.default=NA_character_),
         income = factor(income,levels=c('<$23.8k','$23.8k-$35.7k','$35.7k-$62.2k','$62.2k-$77k','$77k-$93.6k','$93.6k-$136.6k','$136.6k-$180.2k','>$180.2k')),
         vote = as_factor(rvptyvote),
         wgt = rwt) %>%
  select(income,vote,wgt) %>%
  filter(!is.na(income),!is.na(vote)) %>%
  group_by(income) %>%
  mutate(pop = sum(wgt)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(wgt)/mean(pop),
            n = sum(wgt)) %>%
  mutate(se = sqrt(share*(1-share)/n)) %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote),size=0.75) +
  geom_ribbon(aes(ymin=share-se,ymax=share+se,fill=vote),alpha=0.3) +
  scale_color_manual(values=c('#D82A20','#00529F','#098137','#000000','grey')) +
  scale_fill_manual(values=c('#D82A20','#00529F','#098137','#000000','grey')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Vote share',
       title='Income and voting in the 2017 New Zealand general election',
       fill='Party',
       color='Party') +
  ggsave('income_vote_nz2017.png',width=16,height=10,type='cairo')
