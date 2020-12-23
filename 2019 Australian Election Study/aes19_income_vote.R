library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('aes19_unrestricted.dta')

df %>%
  mutate(income = case_when(J6 <= 5 ~ '<$30k',
                            J6 > 5 & J6 <= 9 ~ '$30k-$50k',
                            J6 > 9 & J6 <= 14 ~ '$50k-$100k',
                            J6 > 14 & J6 <= 19 ~ '$100k-$150k',
                            J6 > 19 & J6 <= 23 ~ '>$150k',
                            TRUE ~ NA_character_),
         income = factor(income,levels=c('<$30k','$30k-$50k','$50k-$100k','$100k-$150k','>$150k')),
         vote = as_factor(dem_vo4_16),
         wgt = wt_pooled) %>%
  select(income,vote,wgt) %>%
  filter(!is.na(income),!is.na(vote)) %>%
  group_by(income) %>%
  mutate(pop = sum(wgt)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(wgt)/mean(pop),
            n = sum(wgt)) %>%
  mutate(se = (share*(1-share)/sqrt(n))) %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote)) +
  geom_ribbon(aes(ymin=share-2*se,ymax=share+2*se,fill=vote),alpha=0.3) +
  scale_color_manual(values=c('#0033CC','#DE3533','#10C25B','grey')) +
  scale_fill_manual(values=c('#0033CC','#DE3533','#10C25B','grey')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Vote share',
       fill='Party',
       color='Party',
       title='Income and voting in the 2019 Australian federal election') +
  ggsave('aes19_income_vote.png',width=8,height=5,type='cairo')
