library(tidyverse)
library(haven)
library(Hmisc)
library(scales)
library(lme4)

df <- read_sav('BES2019_W19_v0.5.sav')

df %>%
  mutate(vote = recode(as.numeric(generalElectionVote),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`7`='Green Party',`12`='Brexit Party',.default=NA_character_),
         vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Green Party','Brexit Party')),
         income = case_when(p_gross_household <= 3 ~ '<£15k',
                            p_gross_household > 3 & p_gross_household <= 5 ~ '£15k-£25k',
                            p_gross_household > 5 & p_gross_household <= 8 ~ '£25k-£40k',
                            p_gross_household > 8 & p_gross_household <= 10 ~ '£40k-£50k',
                            p_gross_household > 10 & p_gross_household <= 13 ~ '£50k-£100k',
                            p_gross_household > 13 & p_gross_household <= 15 ~ '>£100k',
                            TRUE ~ NA_character_),
         income = factor(income,levels=c('<£15k','£15k-£25k','£25k-£40k','£40k-£50k','£50k-£100k','>£100k'))) %>%
  filter(!is.na(vote),!is.na(income)) %>%
  select(vote,income,wt) %>%
  group_by(income) %>%
  mutate(pop = sum(wt)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(wt)/mean(pop),
            n = sum(wt)) %>%
  mutate(se = sqrt(share*(1-share)/n)) %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote),size=0.75) +
  geom_ribbon(aes(ymin=share-se,ymax=share+se,fill=vote),alpha=0.3) +
  scale_color_manual(values=c('#0087DC','#E4003B','#FAA61A','#FDF38E','#6AB023','#12B6CF')) +
  scale_fill_manual(values=c('#0087DC','#E4003B','#FAA61A','#FDF38E','#6AB023','#12B6CF')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Vote share',
       fill='Party',
       color='Party',
       title='Income and voting in the 2019 UK general election') +
  ggsave('bes19_income_vote.png',width=16,height=10,type='cairo')

df %>%
  mutate(vote = recode(as.numeric(generalElectionVote),`1`='Conservative',`2`='Labour',`3`='Liberal Democrat',`4`='SNP',`7`='Green Party',`12`='Brexit Party',.default=NA_character_),
         vote = factor(vote,levels=c('Conservative','Labour','Liberal Democrat','SNP','Green Party','Brexit Party')),
         income = case_when(p_gross_household <= 3 ~ '<£15k',
                            p_gross_household > 3 & p_gross_household <= 5 ~ '£15k-£25k',
                            p_gross_household > 5 & p_gross_household <= 8 ~ '£25k-£40k',
                            p_gross_household > 8 & p_gross_household <= 10 ~ '£40k-£50k',
                            p_gross_household > 10 & p_gross_household <= 13 ~ '£50k-£100k',
                            p_gross_household > 13 & p_gross_household <= 15 ~ '>£100k',
                            TRUE ~ NA_character_),
         income = factor(income,levels=c('<£15k','£15k-£25k','£25k-£40k','£40k-£50k','£50k-£100k','>£100k')),
         educ = as_factor(p_edlevelUni),
         age = as_factor(ageGroup),
         euself = na_if(EUIntegrationSelf,9999),
         eucon = na_if(EUIntegrationCon,9999),
         eulab = na_if(EUIntegrationLab,9999),
         eulib = na_if(EUIntegrationLD,9999)) %>%
  filter_at(vars(vote,income,educ,age,euself,eucon,eulab,eulib),~!is.na(.x)) %>%
  select(income,educ,age,vote,euself,eucon,eulab,eulib,wt) %>%
  mutate(con = as.numeric(vote=='Conservative'),
         lab = as.numeric(vote=='Labour'),
         lib = as.numeric(vote=='Liberal Democrat')) -> data

res.con <- glmer(con ~ (1|income) + (1|educ) + (1|age) + (1|income:educ) + (1|income:age:educ),data,family=binomial)
summary(res.con)

res.lab <- glmer(lab ~ (1|income) + (1|educ) + (1|age) + (1|income:age:educ),data,family=binomial)
summary(res.lab)

res.lib <- glmer(lib ~ (1|income) + (1|educ) + (1|age) + (1|income:educ) + (1|income:age) + (1|age:educ) + (1|income:age:educ),data,family=binomial)
summary(res.lib)

output <- expand_grid(income = factor(levels(data$income),levels=levels(data$income)),
                      educ = factor(levels(data$educ),levels=levels(data$educ)),
                      age = factor(levels(data$age),levels=levels(data$age)))

output$con <- predict(res.con,output,allow.new.levels=T,type='response')
output$lab <- predict(res.lab,output,allow.new.levels=T,type='response')
output$lib <- predict(res.lib,output,allow.new.levels=T,type='response')

output %>%
  filter(age!='Under 18') %>%
  ggplot(aes(x=income,y=lab,color=educ,group=educ)) +
  geom_point() +
  geom_line() +
  facet_wrap(~age,scales='free_y') +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Predicted share voting Labour',
       color='Education',
       title='Labour vote in GE2019 by income, education and age') +
  ggsave('bes19_lab_income_educ_age.png',width=16,height=10,type='cairo')

output %>%
  filter(age!='Under 18') %>%
  ggplot(aes(x=income,y=con,color=educ,group=educ)) +
  geom_point() +
  geom_line() +
  facet_wrap(~age,scales='free_y') +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Predicted share voting Conservative',
       color='Education',
       title='Conservative vote in GE2019 by income, education and age') +
  ggsave('bes19_con_income_educ_age.png',width = 16,height = 10,type='cairo')

res.con.eu <- glmer(con ~ (1|income) + (1|educ) + (1|age) + euself + eucon,data,family=binomial)
summary(res.con.eu)

res.lab.eu <- glmer(lab ~ (1|income) + (1|educ) + (1|age) + (1|income:age) + (1|income:educ) + (1|educ:age) + (1|income:educ:age) + euself + eulab,data,family=binomial)
summary(res.lab.eu)
