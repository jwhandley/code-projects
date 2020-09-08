library(tidyverse)
library(haven)
library(Hmisc)
require(fitdistrplus)
library(actuar)

df <- read_dta('cps_00162.dta')

df %>%
  filter(union!=0) %>%
  mutate(uhrswork1 = ifelse(uhrswork1>996,NA_integer_,as.numeric(uhrswork1)),
         hours = coalesce(uhrsworkorg, uhrswork1)) %>%
  mutate(hourwage = na_if(hourwage,999.99),
         earnweek = na_if(earnweek,9999.99)) %>%
  group_by(year) %>%
  mutate(topcode = max(earnweek,na.rm=T),
         is_topcode = as.numeric(earnweek == topcode),
         topcode_share = wtd.mean(is_topcode,earnwt,na.rm=T),
         threshold = wtd.quantile(earnweek,earnwt,probs=1-2*topcode_share)) %>%
  ungroup() %>%
  mutate(educ = case_when(educ < 80 ~ 'High school or less', educ < 110 ~ 'Some college', educ < 123 ~ 'Undergraduate degree', educ > 122 ~ 'Postgraduate degree'),
         educ = factor(educ,levels=c('High school or less','Some college','Undergraduate degree','Postgraduate degree'))) %>%
  mutate(age = as.numeric(age),sex = as_factor(sex),occ_codes = as.numeric(occ2010)) %>%
  dplyr::select(year,month,age,sex,educ,occ2010,occ_codes,hours,earnwt,hourwage,paidhour,union,earnweek,topcode,is_topcode,topcode_share,threshold) -> data

write_dta(data,'earnings_org.dta')
