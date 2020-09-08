library(tidyverse)
library(haven)
library(Hmisc)
library(stargazer)

df <- read_dta('BES2019_W19_v0.1.dta')

df %>%
  mutate(occ_class = as_factor(ns_sec_analytic), turnout = as_factor(na_if(genElecTurnoutRetro,9999)), redist = na_if(redistSelf,9999)) %>%
  filter(!is.na(occ_class),!is.na(turnout)) %>%
  group_by(occ_class,turnout) %>%
  summarise(redist = wtd.mean(redist,wt)) %>%
  ggplot(aes(x=occ_class,y=redist,fill=turnout)) + geom_bar(stat='identity',position='dodge') + labs(x='NS-SEC Analytic Class',y='Redistribution (0 = More, 10 = Less)',title='Attitudes towards redistribution by social class and GE2019 turnout',subtitle='BES Panel Wave 19',caption='@jwhandley17',fill = 'Turnout') + scale_x_discrete(labels=c('1.1','1.2','2','3','4','5','6','7')) + ggsave('occupation_redist.png',width=8,height=5,type='cairo')

df %>%
  filter(pastvote_ge_2019 %in% c(1,2)) %>%
  mutate(occ_class = as_factor(ns_sec_analytic),educ = as_factor(edlevel),income=ifelse(profile_gross_household<16,profile_gross_household,NA),lab = as.numeric(pastvote_ge_2019==2),female=as.numeric(gender==2)) %>%
  filter(!is.na(occ_class)) %>%
  select(occ_class,educ,income,age,female,lab,wt) -> data

res <- glm('lab ~ occ_class + educ + income + age + female',data=data,family=binomial)
summary(res)

stargazer(res,out='res_vote.html',dep.var.labels = 'Voting Labour rather than Conservative',covariate.labels = c('Higher professional (1.2)','Lower professional and managerial (2)','Intermediate occupations (3)','Small employers and own account workers (4)','Lower supervisory and technical (5)','Semi-routine (6)','Routine (7)','Below GCSE','A-level','Undergraduate','Post-graduate','Household Income','Age','Constant'))

pred <- data.frame(occ_class=unique(data$occ_class),income=wtd.mean(data$income,data$wt),educ='A-level',age=wtd.mean(data$age,data$wt),female=0.5)

pred$lab <- predict.glm(res,newdata=pred,type='response')

as_tibble(pred) %>%
  mutate(occ_class = factor(occ_class,levels=levels(data$occ_class))) %>%
  ggplot(aes(x=occ_class,y=lab)) + geom_bar(stat='identity',fill='#DC241f') + labs(x='NS-SEC Analytic Class',y='Predicted Labour Vote Share',title='Class voting after controls for age, education, gender and household income',subtitle='BES Panel Wave 19',caption='@jwhandley17') + scale_x_discrete(labels=c('1.1','1.2','2','3','4','5','6','7')) + ggsave('class_vote_controls.png',width=8,height=5,type='cairo')
