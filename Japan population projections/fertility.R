library(tidyverse)

df <- read_csv('zh1-1-02.csv',locale = locale(encoding='cp932'),skip=2)

df %>%
  filter(!is.na(`2015<U+5E74>`)) %>%
  mutate(`<U+5E74><U+9F62>` = as.numeric(`<U+5E74><U+9F62>`)) %>%
  gather(key = 'Year',value = 'Fertility rate',-`<U+5E74><U+9F62>`) %>%
  ggplot(aes(x=`<U+5E74><U+9F62>`,y=`Fertility rate`,color=`Year`)) +
  geom_line()

df %>%
  filter(!is.na(`2015<U+5E74>`)) %>%
  mutate(year = as.numeric(`<U+5E74><U+9F62>`), fert = `2015<U+5E74>`) %>%
  select(year,fert) %>%
  write_csv('fertility_2015.csv')
