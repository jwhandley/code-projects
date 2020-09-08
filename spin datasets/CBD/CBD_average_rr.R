library(tidyverse)
library(readxl)

df <- read_excel('CBD data 190507.xlsx')

df %>%
  group_by(Country) %>%
  filter(length(Year)>11) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarise(Universal = mean(`Rr Universal child benefit (apw)`), `Employment-based` = mean(`Rr Empl.-based child benefit (apw)`), `Income-tested` = mean(`Rr Income-tested child benefit (apw)`), `Tax allowance` = mean(`Rr Child tax allowance (apw)`), `Tax credit` = mean(`Rr Child tax credit (apw)`)) %>%
  gather(key='Type',value='Replacement rate (average production worker)',c(`Universal`,`Employment-based`,`Income-tested`,`Tax allowance`,`Tax credit`)) %>%
  ggplot(aes(x=Year,y=`Replacement rate (average production worker)`,fill=Type)) +
  geom_bar(stat='identity') +
  labs(title='Generosity of child support benefits for 18 OECD countries',caption = "Source: University of Stockholm Child Benefit Dataset, author's calculations",fill='Type of Benefit') +
  ggsave('CBD_average_rr.png',width=8,height=5,type='cairo')
