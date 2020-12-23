library(tidyverse)
library(haven)
library(Hmisc)
library(scales)

df <- read_dta('2019 Canadian Election Study - Online Survey v1.0.dta')

df %>%
  filter(!is.na(pes19_votechoice2019),
         pes19_votechoice2019 < 8) %>%
  mutate(inccat = as.numeric(cut(cps19_income_number,c(-Inf,1,30000,60000,90000,110000,150000,200000,Inf),seq(1,8))),
         inncat = replace_na(9),
         inccat = coalesce(inccat,cps19_income_cat)) %>%
  mutate(vote = as_factor(pes19_votechoice2019),
         income = as_factor(inccat),
         income.z = as.numeric(inccat),
         wgt = pes19_weight_general_restricted) %>%
  select(vote,income,income.z,wgt) %>%
  filter(!is.na(wgt)) -> data

data %>%
  group_by(income) %>%
  mutate(pop = sum(wgt)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(wgt)/mean(pop),
            n = sum(wgt)) %>%
  mutate(se = sqrt(share*(1-share)/n)) %>%
  filter(income != 'No income',income != "Don't know/ Prefer not to answer") %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote),size=0.75) +
  geom_ribbon(aes(ymin=share-se,ymax=share+se,fill=vote),alpha=0.3) +
  scale_color_manual(values=c('#EA6D6A','#6495ED','#F4A460','#87CEFA','#99C955','#83789E','grey')) +
  scale_fill_manual(values=c('#EA6D6A','#6495ED','#F4A460','#87CEFA','#99C955','#83789E','grey')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household Income',
       y='Vote share',
       title='Income and voting in the 2019 Canadian federal election',
       color = 'Party',
       fill = 'Party',
       caption = 'Source: 2019 Canadian Election Study Online Survey') +
  ggsave('ces19_internet_income.png',width = 16,height = 10,type = 'cairo')
