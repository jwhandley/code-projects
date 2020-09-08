library(tidyverse)
library(strucchange)

df <- read_csv('us_relig.csv') %>%
  mutate(Religion = factor(Religion,levels=c('Protestant','Catholic','Jewish','Other','None')))

ggplot(df,aes(x=Year,y=Share,color=Religion)) +
  geom_point(size=2,aes(shape=Source)) +
  geom_smooth() +
  labs(title='Religious identficiation in the US')

df %>%
  filter(Religion=='Protestant') -> data

break.test <- sctest(data$Share ~ data$Year,type='Chow',point = 1990)
