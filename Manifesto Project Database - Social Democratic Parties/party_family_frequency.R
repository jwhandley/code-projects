library(tidyverse)
library(haven)
library(Hmisc)

df <- read_dta('MPDataset_MPDS2019b_stata14.dta')

# Free market economy (per401)
df %>%
  filter(parfam %in% c(30,40,50,60)) %>%
  mutate(year = round(date/100),parfam = as_factor(parfam)) %>%
  mutate(parfam = recode(parfam,`soc social democratic`='Social Democratic',`lib liberal`='Liberal',`chr christian democrat`='Christian Democratic',`con conservative`='Conservative')) %>%
  filter(year>=1950) %>%
  ggplot(aes(x=year,y=per401,colour=parfam)) +
  geom_smooth() +
  scale_colour_manual(values=c('red','orange','black','blue')) +
  labs(y='Free market economy',x='Year',title="Frequency of phrases mentioning a 'free market economy' in party manifestos",subtitle='Comparative Manifesto Project (2019b)',caption='@jwhandley17',colour='Party family') +
  ggsave('free-market-economy.png',width=8,height=5,type='cairo')

# Nationalisation (per413)
df %>%
  filter(parfam %in% c(30,40,50,60)) %>%
  mutate(year = round(date/100),parfam = as_factor(parfam)) %>%
  mutate(parfam = recode(parfam,`soc social democratic`='Social Democratic',`lib liberal`='Liberal',`chr christian democrat`='Christian Democratic',`con conservative`='Conservative')) %>%
  filter(year>=1950) %>%
  ggplot(aes(x=year,y=per413,colour=parfam)) +
  geom_smooth() +
  scale_colour_manual(values=c('red','orange','black','blue')) +
  labs(y='Nationalisation',x='Year',title="Frequency of phrases mentioning a nationalisation in party manifestos",subtitle='Comparative Manifesto Project (2019b)',caption='@jwhandley17',colour='Party family') +
  ggsave('nationalisation.png',width=8,height=5,type='cairo')


# Incentives - positive (per402)
df %>%
  filter(parfam %in% c(30,40,50,60)) %>%
  mutate(year = round(date/100),parfam = as_factor(parfam)) %>%
  mutate(parfam = recode(parfam,`soc social democratic`='Social Democratic',`lib liberal`='Liberal',`chr christian democrat`='Christian Democratic',`con conservative`='Conservative')) %>%
  filter(year>=1950) %>%
  ggplot(aes(x=year,y=per402,colour=parfam)) +
  geom_smooth() +
  scale_colour_manual(values=c('red','orange','black','blue')) +
  labs(y='Incentives - positive',x='Year',title="Frequency of phrases mentioning 'incentives' positively in party manifestos",subtitle='Comparative Manifesto Project (2019b)',caption='@jwhandley17',colour='Party family') +
  ggsave('incentives-positive.png',width=8,height=5,type='cairo')

# Welfare state expansion (per504)
df %>%
  filter(parfam %in% c(30,40,50,60)) %>%
  mutate(year = round(date/100),parfam = as_factor(parfam)) %>%
  mutate(parfam = recode(parfam,`soc social democratic`='Social Democratic',`lib liberal`='Liberal',`chr christian democrat`='Christian Democratic',`con conservative`='Conservative')) %>%
  filter(year>=1950) %>%
  ggplot(aes(x=year,y=per504,colour=parfam)) +
  geom_smooth() +
  scale_colour_manual(values=c('red','orange','black','blue')) +
  labs(y='Welfare state expansion',x='Year',title="Frequency of phrases mentioning welfare state expansion in party manifestos",subtitle='Comparative Manifesto Project (2019b)',caption='@jwhandley17',colour='Party family') +
  ggsave('welfare-state-expansion.png',width=8,height=5,type='cairo')

# Economic planning (per404)
df %>%
  filter(parfam %in% c(30,40,50,60)) %>%
  mutate(year = round(date/100),parfam = as_factor(parfam)) %>%
  mutate(parfam = recode(parfam,`soc social democratic`='Social Democratic',`lib liberal`='Liberal',`chr christian democrat`='Christian Democratic',`con conservative`='Conservative')) %>%
  filter(year>=1950) %>%
  ggplot(aes(x=year,y=per404,colour=parfam)) +
  geom_smooth() +
  scale_colour_manual(values=c('red','orange','black','blue')) +
  labs(y='Economic planning',x='Year',title="Frequency of phrases mentioning economic planning in party manifestos",subtitle='Comparative Manifesto Project (2019b)',caption='@jwhandley17',colour='Party family') +
  ggsave('economic-planning.png',width=8,height=5,type='cairo')
