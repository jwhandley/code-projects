library(tidyverse)

df <- read_csv('hoc_2010_division.csv')

df %>%
  gather(key='mp_id',value='vote',-rowid,-date,-voteno,-Bill) %>%
  write_csv('hoc_divisions_long.csv')
