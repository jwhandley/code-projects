library(haven)
library(tidyverse)

df <- read_sav("1973~2008/0814.sav",encoding='cp932')
View(df)

df %>%
  mutate(sex1=as.numeric(Q11),sex2=as.numeric(Q12),sex3=as.numeric(Q24)-as.numeric(Q25)) %>%
  select(sex1,sex2,sex3) -> sex

mod <- factanal(sex,1)

df %>%
  mutate(year=recode(factor(KAISU),`1`=1973,`2`=1978,`3`=1983,`4`=1988,`5`=1993,`6`=1998,`7`=2003,`8`=2008)) %>%
  mutate(home=factor(Q8)) %>%
  group_by(year) %>%
  summarise(respect=mean(as.numeric(home==1)),independence=mean(as.numeric(home==2)),gender_roles=mean(as.numeric(home==3)),coop=mean(as.numeric(home==4)))
