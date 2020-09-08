library(tidyverse)
require(plm)

df <- read_csv('gini_data.csv')

df <- df %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1970, year <= 2010) %>%
  mutate(gini = 100*gini) %>%
  group_by(country) %>%
  ungroup() %>%
  select(-dataset)

data <- pdata.frame(df,index=c('country','year'))

data$year <- as.numeric(data$year)

res <- plm(gini ~ year,data,effect = 'individual')
summary(res)

ggplot(df,aes(x=year,y=gini,color=country)) +
  geom_point() +
  geom_line()

res1 <- lm(gini ~ as_factor(country),df)
summary(res1)

df$resid <- residuals.lm(res1)

ggplot(df,aes(x=year,y=resid,color=country)) +
  geom_line() +
  geom_point()
