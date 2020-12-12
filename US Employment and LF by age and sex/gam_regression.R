library(tidyverse)
library(haven)
library(Hmisc)
library(mgcv)


df <- read_dta('cps_00180.dta')

df %>%
  filter(labforce %in% c(1,2)) %>%
  mutate(age = as.numeric(age),sex = as_factor(sex),labforce = as.numeric(labforce==2)) -> data

res <- gam(labforce ~ age + sex,data,family=binomial)
summary(res)

data %>%
  group_by(age,sex,year,month) %>%
  summarise(n = sum(wtfinl))