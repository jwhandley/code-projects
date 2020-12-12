library(tidyverse)

cpi <- read_csv('cpi.csv')

cpi %>%
  gather(key = 'country',value='cpi',-year) %>%
  write_csv('cpi_long.csv')

