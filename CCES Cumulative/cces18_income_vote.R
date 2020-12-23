library(tidyverse)
library(haven)
library(Hmisc)
library(scales)
library(lme4)

df <- read_dta('cumulative_2006-2019.dta') %>%
  filter(year == 2018)

df %>%
  mutate(dem = as.numeric(voted_rep_party==1),
         vote = case_when(dem == 1 ~ 'Democrat',
                          dem == 0 ~ 'Republican',
                          TRUE ~ 'Other/DNV'),
         vote = factor(vote,levels=c('Democrat','Republican','Other/DNV')),
         income = as_factor(faminc),
         wgt = weight) %>%
  filter(vote %in% c('Democrat','Republican'),
         !is.na(income),
         income != 'Prefer not to say',
         income != 'Skipped') %>%
  select(income,vote,wgt) %>%
  group_by(income) %>%
  mutate(pop = sum(wgt)) %>%
  group_by(income,vote) %>%
  summarise(share = sum(wgt)/mean(pop),
            n = sum(wgt)) %>%
  mutate(se = sqrt(share*(1-share)/n)) %>%
  ggplot(aes(x=income,y=share,group=vote)) +
  geom_line(aes(color=vote),size=0.75) +
  geom_ribbon(aes(ymin=share-se,ymax=share+se,fill=vote),alpha=0.3) +
  scale_fill_manual(values=c('#3333FF','#E81B23')) + 
  scale_color_manual(values=c('#3333FF','#E81B23')) +
  scale_y_continuous(labels=percent) +
  labs(x='Household income',
       y='Vote share',
       fill='Party',
       color='Party',
       title='Income and voting in the 2018 United States House elections') +
  ggsave('cces18_income_vote.png',width=8,height=5,type='cairo')

df %>%
  mutate(income = as_factor(faminc),
         educ = as_factor(educ),
         dem = as.numeric(voted_rep_party==1)) %>%
  select(income,educ,dem,weight) -> data
  
res <- glmer(dem ~ (1|income) + (1|educ) + (1|income:educ),data,family=binomial)
summary(res)


output <- expand_grid(educ = factor(levels(data$educ)[2:7],levels=levels(data$educ)[2:7]),
                      income = factor(levels(data$income)[1:12],levels=levels(data$income)[1:12]))
output$dem <- predict(res,output,allow.new.levels=T,type='response')

output %>%
  ggplot(aes(x=income,y=dem,color=educ,group=educ)) +
  geom_line() +
  geom_point()
