library(tidyverse)

earn <- read_csv('earnings_dispersion.csv')
gini <- read_csv('swiid8_3_summary.csv')


df <- inner_join(earn,gini,by=c('country','year'))

ggplot(df,aes(x=year,y=earnings_90_10)) +
  geom_line() +
  facet_wrap(~country)

write_csv(df,'earnings_gini.csv')
  