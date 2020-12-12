library(tidyverse)
library(haven)
library(Hmisc)
library(lme4)

marist <- read_dta('marist_feb.dta')

marist <- read_dta('marist_feb.dta') %>%
  mutate(wgt = wtfactor,
         educ = ifelse(collegep < 7,as.numeric(collegep==2),NA_real_),
         race = recode(as.numeric(racet),`100`='White',`200`='Black',`300`='Hispanic',.default='Other'),
         income = recode(as.numeric(INC15WT),`1`='<$50k',`2`='Under $50k',`3`='Under $50k',`4`='$50k-$75k',`5`='$75k-$100k',`6`='>$100k',.default='NA/Refused'),
         gender = as.numeric(gender==2),
         age = cut(ager,breaks=c(-Inf,30,45,55,65,Inf),labels=c('18-29','30-44','45-54','55-64','65+')),
         sanders = ifelse(BSDT2020<97,as.numeric(BSDT2020==1),NA_real_),
         biden = ifelse(JBDT2020<97,as.numeric(JBDT2020==1),NA_real_)) %>%
  select(wgt,age,educ,race,gender,income,sanders,biden) %>%
  mutate(wave='marist')

abc <- read_dta('abc_jan.dta')
abc <- read_dta('abc_jan.dta') %>%
  mutate(wgt = weight,
         educ = ifelse(colleduc<8,as.numeric(colleduc==2),NA_real_),
         race = recode(as.numeric(racenet),`1`='White',`2`='Black',`3`='Hispanic',.default='Other'),
         income = recode(as.numeric(income),`1`='<$50k',`2`='<$50k',`3`='<$50k',`4`='$50k-$75k',`5`='$75k-$100k',`6`='>$100k',.default='NA/Refused'),
         gender = as.numeric(Q924NET==2),
         age = cut(Q910,breaks=c(-Inf,30,45,55,65,Inf),labels=c('18-29','30-44','45-54','55-64','65+')),
         sanders = ifelse(Q13_2<3,as.numeric(Q13_2==2),NA_real_),
         biden = ifelse(Q13_1<3,as.numeric(Q13_1==2),NA_real_)) %>%
  select(wgt,age,educ,race,gender,income,sanders,biden) %>%
  mutate(wave='abc')

df <- bind_rows(marist,abc) %>%
  mutate(income = factor(income,levels=c('<$50k','$50-$75k','$75k-$100k','>$100k','NA/Refused')),
         race = factor(race,levels=c('White','Black','Hispanic','Other')),
         age = factor(age,levels=c('18-29','30-44','45-54','55-64','65+')))

res.sanders <- glmer(sanders ~ 1 + (1|educ:income),df,family=binomial)
summary(res.sanders)
ranef(res.sanders)

res.biden <- glmer(biden ~ 1 + (1|educ) + (1|educ:income),df,family=binomial)
summary(res.biden)
ranef(res.biden)

output <- expand_grid(educ=c(0,1),income=factor(levels(df$income),levels=levels(df$income)))
output$biden <- predict(res.biden,output,type='response',allow.new.levels=T)
output$sanders <- predict(res.sanders,output,type='response',allow.new.levels=T)

output <- output %>%
  group_by(educ,income) %>%
  summarise_at(vars(biden,sanders),~mean(.x)) %>%
  mutate(educ = recode(educ,`0`='No degree',`1`='Degree')) %>%
  filter(income!='NA/Refused')

output %>%
  gather(key='candidate',value='share',c(biden,sanders)) %>%
  ggplot(aes(x=income,y=share,color=educ,group=educ)) +
  geom_line() +
  geom_point() +
  facet_wrap(~candidate)

df %>%
  summarise_at(vars(biden,sanders),~wtd.mean(.x,wgt))
