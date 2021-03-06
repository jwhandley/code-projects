library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('BES2019_W19_Panel_v0.2.dta')
df[] <- lapply(df, unclass)


df %>%
  mutate(p_past_vote_2019 = na_if(p_past_vote_2019,9999)) %>%
  filter(wave19==1) %>%
  mutate(lr_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(lr_scaleW17,lr_scaleW16),lr_scaleW14W15),lr_scaleW13),lr_scaleW10_W12),lr_scaleW7_W9),lr_scaleW6),lr_scaleW1_W5)) %>%
  mutate(al_scale = coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(coalesce(al_scaleW17,al_scaleW16),al_scaleW14W15),al_scaleW13),al_scaleW10_W12),al_scaleW7_W9),al_scaleW6),al_scaleW1_W5)) %>%
  mutate(vote = p_past_vote_2019, turnout = p_turnout_2019, wt = wt_new_W19_result, vote_2010=p_past_vote_2010, age = ageW19, class_id = na_if(na_if(na_if(subjClassW19,9999),998),999), ns_sec = ns_sec_analyticW19, hhincome = ifelse(p_gross_householdW19>15,NA_integer_,as.numeric(p_gross_householdW19)), tenure = p_housingW19, educ = p_edlevelW19) %>%
  mutate(ns_sec = recode(ns_sec,`11`='1.1',`12`='1.2',`20`='2',`30`='3',`40`='4',`50`='5',`60`='6',`70`='7')) %>%
  mutate(ns_sec = factor(ns_sec,levels=c('1.1','1.2','2','3','4','5','6','7'))) %>%
  mutate(vote = recode(vote,`1` = 'Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`5`='Plaid Cymru',`6`='Other',`7`='Green',`8`='Other',`9`='Other',`11`='Other',`12`='Brexit party',`13`='Other')) %>%
  mutate(vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Plaid Cymru','Green','Brexit','Other'))) %>%
  mutate(vote_2010 = recode(vote_2010,`1` = 'Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`5`='Plaid Cymru',`6`='Other',`7`='Green',`8`='Other',`9`='Other',`11`='Other',`12`='Brexit party',`13`='Other',`9999`=NA_character_)) %>%
  mutate(vote_2010 = factor(vote_2010,levels=c('Conservative','Labour','Liberal Democrat','SNP','Plaid Cymru','Green','Brexit','Other'))) %>%
  mutate(union = currentUnionMemberW19) %>%
  mutate(sector = recode(sectorW19,`1`='Private sector',`2`='Nationalised industry',`3`='Government',`4`='Voluntary sector',`5` = 'Other', `8` = 'Self-employed',`9`='Self-employed',.defualt = NA_character_)) %>%
  mutate(sector = factor(sector,levels=c('Private sector','Nationalised industry','Government','Voluntary sector','Other','Self-employed'))) %>%
  mutate(public = as.numeric(sector == 'Government') + as.numeric(sector == 'Nationalised industry')) %>%
  select(id,lr_scale,al_scale,vote,turnout,wt,vote_2010,age,class_id,ns_sec,hhincome,tenure,educ,union,sector,public) -> data

data %>%
  mutate(turnout = na_if(turnout,9999),lab = as.numeric(vote=='Labour'),con = as.numeric(vote=='Conservative'),labcon = as.numeric(lab+con==1)) %>%
  mutate_at(vars(lr_scale,al_scale),~(.x-wtd.mean(.x,wt,na.rm=T))/sqrt(wtd.var(.x,wt,na.rm=T))) %>%
  mutate(public = replace_na(public,0)) %>%
  filter(turnout==1,labcon==1) -> data1


res <- glm(lab ~ as_factor(ns_sec) + union + public - 1,data1,family=binomial)
summary(res)

exp(res$coefficients[1:8])
