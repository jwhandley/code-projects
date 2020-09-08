library(tidyverse)
library(haven)
library(Hmisc)
library(stargazer)
library(reldist)

df <- read_dta("UKDA-8598-stata/stata/stata13/apsp_oct18sep19_eul_pwta18.dta")

df %>%
  mutate(wage = log(na_if(na_if(HOURPAY,-9),-8)),class = as_factor(na_if(na_if(na_if(NSECMJ10,-9),-8),8)),wt = PWTA18) %>%
  filter(!is.na(class)) %>%
  group_by(class) %>%
  summarise(mean = wtd.mean(wage,wt,na.rm=T),sd = sqrt(wtd.var(wage,wt,na.rm=T))) %>%
  ggplot(aes(x=class,y=mean)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),width=0.2) + scale_x_discrete(labels=c('1','2','3','4','5','6','7'))

df %>%
  mutate(wage = log(na_if(na_if(HOURPAY,-9),-8)),class = as_factor(na_if(na_if(na_if(NSECMJ10,-9),-8),8)),wt = PWTA18) %>%
  filter(!is.na(class)) %>%
  ggplot(aes(x=wage,color=class,weight=wt)) + geom_density() + labs(x='Log hourly wage',y='Density',color='Occupation',title='Distribution of wages by occupation in the UK',subtitle='Annual Population Survey: October 2018 - Septempber 2019',caption='@jwhandley17') + scale_x_continuous(limits=c(1,5)) + ggsave('occ_logwage_dist.png',width=8,height=5,type='cairo')

df %>%
  mutate(wage = log(na_if(na_if(HOURPAY,-9),-8)),class = as_factor(na_if(na_if(na_if(NSECMJ10,-9),-8),8)),wt = PWTA18) %>%
  filter(!is.na(class),!is.na(wage)) %>%
  select(wage,class,wt) -> data

res <- lm('wage ~ class - 1',data=data,weight=wt)
summary(res)

stargazer(res,out='class_wage.html',dep.var.labels = 'Log hourly wage',covariate.labels = levels(data$class)[3:9])

data$resid <- resid(res) + wtd.mean(data$wage,data$wt)

ggplot(data,aes(x=resid)) + geom_density()

gini(exp(data$wage),data$wt)
gini(exp(data$resid),data$wt)
log(gini(exp(data$wage),data$wt)/gini(exp(data$resid),data$wt))
