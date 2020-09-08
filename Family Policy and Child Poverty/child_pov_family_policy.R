library(tidyverse)
library(ggrepel)

ecec <- read_csv('ecec_spend.csv')
famben <- read_csv('famben_spend.csv')
leave <- read_csv('leave_dur.csv')

df <- inner_join(inner_join(ecec,famben),leave)

poverty <- read_csv('child_pov.csv')
poverty %<>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup()

data <- inner_join(df,poverty)

data <- data %>%
  select(-year)

write_csv(data,'family_policy.csv')

data <- data %>%
  mutate_at(vars(ecec_spend,famben_spend,mleave_fte,pleave_fte),~scale(.x)[,1])

res <- lm(ch_pov ~ ecec_spend + famben_spend + mleave_fte + pleave_fte,data)
summary(res)

res1 <- lm(ch_pov ~ famben_spend,data)
summary(res1)

ggplot(data,aes(x=famben_spend,y=ch_pov,label=country)) +
  geom_point() +
  geom_text_repel()
