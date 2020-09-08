library(tidyverse)
library(readr)

df <- read_csv('MPDataset_MPDS2019b.csv')

df %>%
  mutate(year=date/100) %>%
  filter(countryname %in% c('United Kingdom','United States','New Zealand','Sweden','Finland'),partyabbrev %in% c('Labour','Democrats','SAP','SSDP'),year>1945) %>%
  mutate(nationalisation=per413,`economic planning`=per404,`welfare state`=per504,`labour groups`=per701,`market regulation`=per403) %>%
  group_by(year) %>%
  gather(key='category',value='importance',c(nationalisation,`economic planning`,`welfare state`,`labour groups`,`market regulation`)) %>%
  ggplot(aes(x=year,y=importance,color=category)) + geom_line(size=1) + geom_point() + facet_wrap(~countryname) + labs(caption='Source: Manifesto Project Database 2019b')# + ggsave('lab_dem_manifesto.png',width=16,height=10,type='cairo')

