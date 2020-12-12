library(tidyverse)

econ_support <- read_csv('econ_support.csv') %>%
  gather(key='date',value='econ_support',-country,-iso)
stringency <- read_csv('stringency.csv') %>%
  gather(key='date',value='stringency',-country,-iso)
health_containment <- read_csv('health_containment.csv') %>%
  gather(key='date',value='health_containment',-country,-iso)

policy <- inner_join(inner_join(econ_support,stringency),health_containment)
economy <- read_csv('imf_oct20_weo.csv')
cases  <- read_csv('owid-covid-data.csv') %>%
  select(iso,country,date,new_cases_per_million,new_deaths_per_million,total_cases_per_million,total_deaths_per_million)

df <- inner_join(inner_join(economy,policy),cases)

df %>%
  group_by(country) %>%
  summarise_at(vars(growth,gdppc,unemp,output_gap,econ_support,stringency,health_containment,new_cases_per_million),~mean(.x,na.rm=T)) -> data

df %>%
  write_csv('combined_dataset.csv')
