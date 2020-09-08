library(tidyverse)

df <- read_csv('oecd_leave.csv')

df %>%
  filter(value>0) %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  spread(indicator,value) %>%
  select(-childcare_enrolment) %>%
  mutate(daddy_leave = replace_na(daddy_leave,0)) %>%
  filter_at(vars(daddy_leave,maternity_leave),~!is.na(.x)) -> data

write_csv(select(data,-year),'parental_leave.csv')
