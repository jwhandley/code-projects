library(tidyverse)
library(readxl)

df <- read_excel('PLB 150617.xlsx')

df %>%
  group_by(COUNTRY) %>%
  filter(length(YEAR)>12) %>%
  ungroup() %>%
  filter(PINRRWK>0) %>%
  group_by(YEAR) %>%
  summarise(`Weekly`= mean(PINRRWK),`1st Year`=mean(PINRRYR)) %>%
  gather(key='Time period',value='Net replacement rate',c(`Weekly`,`1st Year`)) %>%
  ggplot(aes(x=YEAR,y=`Net replacement rate`,color=`Time period`)) +
  geom_point(size=1) +
  geom_line(size=1) +
  labs(x='Year',title='Average parental leave replacement rates in 18 OECD countries',caption="Source: University of Stockholm Parental Leave Benefit dataset, author's calculations")
  #ggsave('plb_average_rr.png',width=8,height=5,type='cairo')

df %>%
  group_by(COUNTRY) %>%
  filter(length(YEAR)>12) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  summarise(`Mother`= mean(PIDRMATFU),`Father`=mean(PIDRPATFU),`Dual`=mean(PIDRDUFU),`Total`=mean(PIDRTOFU,na.rm=T)) %>%
  gather(key='Target group',value='Average weeks of leave',c(Mother,Father,Dual,Total)) %>%
  ggplot(aes(x=YEAR,y=`Average weeks of leave`,color=`Target group`)) +
  geom_line(size=1) +
  geom_point(size=1) +
  labs(x='Year',title='Average weeks of parental leave in 18 OECD countries',caption="Source: University of Stockholm Parental Leave Benefit dataset, author's calculations") +
  ggsave('pld_average_weeks.png',width=8,height=5)
  