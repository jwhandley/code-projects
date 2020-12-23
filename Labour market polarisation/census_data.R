library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('usa_00082.dta')

df %>%
  mutate(hourwage = incwage) %>%
  group_by(year) %>%
  mutate(pop = sum(perwt)) %>%
  group_by(year,occ2010) %>%
  summarise(educ = wtd.mean(as.numeric(educ>=10),perwt,na.rm=T),
            wage = wtd.mean(hourwage,perwt,na.rm=T),
            n = sum(perwt)) %>%
  group_by(occ2010) %>%
  mutate(init.wage = first(wage),
         job.growth = log(n) - lag(log(n)),
         wage.growth = log(wage) - lag(log(wage))) %>%
  arrange(desc(year)) -> data
  

res <- loess(job.growth ~ log(init.wage),span=0.4,data)
summary(res)

data$pred <- predict(res,data)

data %>%
  ggplot(aes(x=log(init.wage),y=pred)) +
  geom_line() +
  geom_jitter(aes(y=job.growth),alpha=0.5,color='blue')
