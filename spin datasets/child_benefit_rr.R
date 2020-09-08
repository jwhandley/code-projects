library(tidyverse)
library(readxl)

# Child benefit data
df <- read_excel('CBD/CBD Data 190507.xlsx')


# Average child benefit replacement rates for APW
df %>%
  filter(Year==2015) %>%
  rename(`Universal child benefit` = `Rr Universal child benefit (apw)`,
         `Employment-based child benefit` = `Rr Empl.-based child benefit (apw)`,
         `Income-tested child benefit` = `Rr Income-tested child benefit (apw)`,
         `Child tax allowance` = `Rr Child tax allowance (apw)`,
         `Child tax credit` = `Rr Child tax credit (apw)`,
         `Child tax rebate` = `Rr Child tax rebate (apw)`) %>%
  gather(key='Benefit',
         value='Replacement Rate',
         c(`Universal child benefit`,`Employment-based child benefit`,`Income-tested child benefit`,`Child tax allowance`,`Child tax credit`,`Child tax rebate`)) %>%
  ggplot(aes(x=reorder(Country,-`Rr Total child benefit (apw)`),y=`Replacement Rate`,fill=Benefit)) +
  geom_bar(stat='identity') +
  labs(x='Country',title='Child benefit replacement rates for the average production worker in 2015 by benefit type',subtitle='Stockholm University Child Benefit Dataset',caption='@jwhandley17') +
  ggsave('child_benefit_rr.png',width=8*1.5,height=5*1.5,type='cairo')


# Same but for half of APW
df %>%
  filter(Year==2015) %>%
  rename(`Universal child benefit` = `Rr     Universal child benefit (0.5*apw)`,
         `Employment-based child benefit` = `Rr Empl.-based child benefit (0.5*apw)`,
         `Income-tested child benefit` = `Rr Income tested child benefit  (0.5*apw)`,
         `Child tax allowance` = `Rr Child tax allowance (0.5*apw)`,
         `Child tax credit` = `Rr Child tax credit (0.5*apw)`,
         `Child tax rebate` = `Rr Child tax rebate (0.5*apw)`) %>%
  gather(key='Benefit',
         value='Replacement Rate',
         c(`Universal child benefit`,`Employment-based child benefit`,`Income-tested child benefit`,`Child tax allowance`,`Child tax credit`,`Child tax rebate`)) %>%
  ggplot(aes(x=reorder(Country,-`Rr Total child benefit (0.5*apw)`),y=`Replacement Rate`,fill=Benefit)) +
  geom_bar(stat='identity') +
  labs(x='Country',title='Child benefit replacement rates for the half the average production wage in 2015 by benefit type',subtitle='Stockholm University Child Benefit Dataset',caption='@jwhandley17') +
  ggsave('child_benefit_rr0.5.png',width=8*1.5,height=5*1.5,type='cairo')

df %>%
  filter(Year==2015) %>%
  rename(`Universal child benefit` = `Rr Universal child benefit (apw)`,
         `Employment-based child benefit` = `Rr Empl.-based child benefit (apw)`,
         `Income-tested child benefit` = `Rr Income-tested child benefit (apw)`,
         `Child tax allowance` = `Rr Child tax allowance (apw)`,
         `Child tax credit` = `Rr Child tax credit (apw)`,
         `Child tax rebate` = `Rr Child tax rebate (apw)`) %>%
  mutate(cben_u = `Universal child benefit`,
         cben_m = `Income-tested child benefit` + `Child tax credit`,
         cben_t = `Child tax allowance` + `Child tax rebate`,
         country = Country) %>%
  select(country,cben_u,cben_m,cben_t) -> data

write_csv(data,'cben_rr.csv')
