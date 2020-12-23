library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00192.dta') %>%
  mutate(occ2010 = round(occ2010/100)) %>%
  group_by(year,occ2010) %>%
  summarise(wage = wtd.quantile(earnweek,earnwt,na.rm=T,probs=0.5),
            n = sum(earnwt)) %>%
  group_by(occ2010) %>%
  mutate(job.growth = log(n) - lag(log(n)),
         wage.growth = log(wage) - lag(log(wage)),
         init.wage = first(wage)) %>%
  arrange(desc(year))

res <- loess(job.growth ~ log(init.wage),span=0.75,df)

df$pred <- predict(res,df)

df %>%
  ggplot(aes(x=log(init.wage),y=pred)) +
  geom_line()
