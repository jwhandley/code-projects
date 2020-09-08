library(tidyverse)

df <- read_csv('death_ts.csv')

df %>%
  filter(age != "110+") %>%
  mutate(age = as.numeric(age)) %>%
  filter(age<101) %>%
  gather(key='sex',value='mort',c(female,male)) %>%
  mutate(sex = recode(sex,`female`=1,`male`=0)) %>%
  write_csv('death_ts_cleaned.csv')
