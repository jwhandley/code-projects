library(tidyverse)
library(readxl)

df <- read_excel('SIED/SIED 1930-2015 190320.xlsx')
crosswalk <- read_csv('SIED/country_lookup.csv')

inner_join(df,crosswalk) -> df


df %>%
  filter(year==2015) %>%
  mutate(replacement = 1/4*(prtstwco + urtsw26f + srtsw26f + artsw26f)) %>%
  select(country_name,replacement) -> data

write.csv(data,'replacement_rates_2015.csv')
