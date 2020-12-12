library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('cps_00185.dta') %>%
  mutate(educ = case_when(educ <= 73 ~ 0,
                          educ > 73 & educ <= 100 ~ 1,
                          educ > 100 & educ <= 111 ~ 2,
                          educ > 111 & educ < 999 ~ 3,
                          TRUE ~ NA_real_)) %>%
  filter(earnweek != 9999.99,earnweek > 0.01,!is.na(educ))

pca <- prcomp(select(df,educ,earnweek),scale.=T)

df %>%
  mutate(educ.mean = wtd.mean(educ,earnwt,na.rm=T),
         educ.sd = sqrt(wtd.var(educ,earnwt,na.rm=T)),
         earn.mean = wtd.mean(earnweek,earnwt,na.rm=T),
         earn.sd = sqrt(wtd.var(earnweek,earnwt,na.rm=T)),
         educ.score = (educ-educ.mean)/educ.sd,
         earn.score = (earnweek-earn.mean)/earn.sd) %>%
  group_by(occ2010) %>%
  summarise(educ = wtd.mean(educ.score,earnwt,na.rm=T),
            earn = wtd.mean(earn.score,na.rm=T)) -> data

cluster <- kmeans(select(data,educ,earn),centers=5,nstart=25)

data$cluster <- cluster$cluster

data %>%
  ggplot(aes(x=earn,y=educ,color=factor(cluster))) +
  geom_point()

df %>%
  inner_join(select(data,occ2010,cluster)) -> reg.data

res.micro <- lm(log(earnweek) ~ as_factor(occ2010),reg.data)
summary(res.micro)

res.macro <- lm(log(earnweek) ~ factor(cluster),reg.data)
summary(res.macro)

reg.data %>%
  group_by(cluster,occ2010) %>%
  summarise(n=sum(earnwt)) %>%
  group_by(cluster) %>%
  top_n(3)
  
