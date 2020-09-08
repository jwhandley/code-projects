library(tidyverse)
library(manifestoR)
library(Hmisc)
mp_setapikey("manifesto_apikey.txt")

mp_maindataset() -> df

df %>%
  filter(countryname == 'Japan') %>%
  select(partyname,partyabbrev,parfam,edate,rile,pervote) %>%
  filter(partyabbrev %in% c('LDP','JSP','DPJ')) %>%
  ggplot(aes(x=edate,y=rile,color=partyname)) +
  geom_point() + 
  geom_line()

df %>%
  filter(countryname == 'Japan') %>%
  select(partyname,partyabbrev,parfam,edate,rile,pervote) %>%
  filter(pervote>15) %>%
  group_by(edate) %>%
  summarise(polarisation = sqrt(wtd.var(rile,pervote))) %>%
  ggplot(aes(x=edate,y=polarisation)) +
  geom_line() +
  geom_point()


df %>%
  filter(countryname == 'Japan') %>%
  select(partyname,partyabbrev,parfam,edate,rile,pervote) %>%
  filter(partyabbrev %in% c('LDP','JSP','DPJ')) %>%
  select(partyname,edate,rile) %>%
  spread(partyname,rile) %>%
  mutate(`Offical Opposition` = coalesce(`Democratic Party of Japan`,`Japan Socialist Party`)) %>%
  mutate(Gap = `Liberal Democratic Party` - `Offical Opposition`) %>%
  select(edate,Gap) %>%
  write_csv('party_polarisation.csv')
