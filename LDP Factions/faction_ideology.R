library(tidyverse)
library(readxl)
library(ggrepel)

df <- read_excel("taniguchi_2017_leg.xlsx")

df %>%
  filter(RESULT>1) %>%
  mutate_at(vars(c(starts_with('Q4_'))), ~na_if(.x,99)) %>%
  mutate(free_educ = Q4_9, prog_tax = Q4_10, fiscal_stimulus = Q4_8, public_works = Q4_7, small_gov = Q4_6, cons_tax = na_if(Q3,99)) %>%
  mutate(defence = Q4_1, preemtive_strike = Q4_2, nk_pressure = Q4_3, non_prolif = Q4_4, yasukuni = Q4_5, safety_privacy = Q4_11) %>%
  mutate(migrant_worker = Q4_12, restart_nuclear = Q4_13, maiden_name = Q4_14, gay_marriage = Q4_15, pol_age = Q4_16, unicam = Q4_17) %>%
  mutate(econ = free_educ + prog_tax + fiscal_stimulus + public_works - small_gov) %>%
  mutate(security = defence + preemtive_strike + nk_pressure - non_prolif + yasukuni + safety_privacy) %>%
  mutate(social = -migrant_worker + restart_nuclear - maiden_name - gay_marriage - pol_age) %>%
  mutate_at(vars(econ,security,social), ~scale(.x)) %>%
  mutate(party=recode(PARTY,`1`='é©ñØì}',`2`='åˆñæì}',`3`='ã§éYì}',`4`='ì˙ñ{à€êVÇÃâÔ',`5`='é–ñØì}',`7`='äÛñ]ÇÃì}',`8`='óßåõñØéÂì}',.default='ÇªÇÃëº')) %>%
  mutate(party = factor(party,levels=c('é©ñØì}','åˆñæì}','óßåõñØéÂì}','äÛñ]ÇÃì}','ã§éYì}','é–ñØì}','ì˙ñ{à€êVÇÃâÔ','ÇªÇÃëº'))) %>%
  mutate(party_faction = coalesce(na_if(party,'é©ñØì}'),HABATSU)) %>%
  filter(!is.na(party_faction)) %>%
  group_by(party_faction,party) %>%
  summarise_at(vars(econ,security,social),~mean(.x,na.rm=T)) -> data

ggplot(data,aes(x=econ,y=security,label=party_faction,color=party)) +
  geom_point() +
  geom_text_repel() +
  labs(x='åoçœé≤',
       y='à¿ï€é≤',
       color='ì}îh')

ggplot(filter(data,party_faction!=party),aes(x=econ,y=security,color=social,label=party_faction)) +
  geom_point() +
  geom_text_repel() +
  labs(x='åoçœé≤',
       y='à¿ï€é≤',
       color='é–âÔé≤')
