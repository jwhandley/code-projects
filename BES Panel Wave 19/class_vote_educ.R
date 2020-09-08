library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_Panel_v0.2.dta')

df %>%
  mutate(p_past_vote_2019 = na_if(p_past_vote_2019,9999),p_turnout_2019 = na_if(p_turnout_2019,9999)) %>%
  filter(wave19==1) %>%
  mutate(lr_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(lr_scaleW17,lr_scaleW16),lr_scaleW14W15),lr_scaleW13),lr_scaleW10_W12),lr_scaleW7_W9),lr_scaleW6),lr_scaleW1_W5)) %>%
  mutate(al_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(al_scaleW17,al_scaleW16),al_scaleW14W15),al_scaleW13),al_scaleW10_W12),al_scaleW7_W9),al_scaleW6),al_scaleW1_W5)) %>%
  mutate(vote = as.numeric(p_past_vote_2019), turnout = p_turnout_2019, wt = wt_new_W19_result, age = ageW19, class_id = na_if(na_if(na_if(subjClassW19,9999),998),999), ns_sec = as.numeric(ns_sec_analyticW19), hhincome = ifelse(p_gross_householdW19>15,NA_integer_,as.numeric(p_gross_householdW19)), tenure = p_housingW19, educ = p_edlevelW19) %>%
  mutate(ns_sec = recode(ns_sec,`11`='1.1',`12`='1.2',`20`='2',`30`='3',`40`='4',`50`='5',`60`='6',`70`='7')) %>%
  mutate(ns_sec = factor(ns_sec,levels=c('1.1','1.2','2','3','4','5','6','7'))) %>%
  mutate(vote = recode(vote,`1` = 'Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`5`='Plaid Cymru',`6`='Other',`7`='Green',`8`='Other',`9`='Other',`11`='Other',`12`='Brexit party',`13`='Other')) %>%
  mutate(vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Plaid Cymru','Green','Brexit','Other'))) %>%
  mutate(union = currentUnionMemberW19) %>%
  select(id,lr_scale,al_scale,vote,turnout,wt,age,class_id,ns_sec,hhincome,tenure,educ,union) -> data

data %>%
  mutate(lab = as.numeric(vote == 'Labour')) %>%
  filter(turnout == 1,!is.na(educ),!is.na(ns_sec)) %>%
  mutate(educ = as_factor(educ)) %>%
  group_by(ns_sec,educ) %>%
  summarise(lab = wtd.mean(lab,wt,na.rm=T)) %>%
  spread(educ,lab) -> data1

write_csv(data1,'class_vote_educ.csv')

res <- glm(lab ~ ns_sec + as_factor(educ) - 1,data,family=binomial)
summary(res)

exp(res$coefficients[1:8])
