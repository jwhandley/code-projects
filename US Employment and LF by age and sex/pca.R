library(tidyverse)

df <- read_csv('lfpr.csv')

df %>%
  mutate(age.sex = interaction(age_group,sex)) %>%
  select(date,age.sex,lfpr) %>%
  spread(age.sex,lfpr) -> data

pca <- prcomp(select(data,-date),scale.=T)

data$pca1 <- pca$x[,1]
data$pca2 <- pca$x[,2]
data$pca3 <- pca$x[,3]
data$pca4 <- pca$x[,4]

data %>%
  select(date,pca2) %>%
  mutate(potential_lfpr = -pca2) %>%
  select(-pca2) %>%
  write_csv('potential_lfpr.csv')
