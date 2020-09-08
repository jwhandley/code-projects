library(tidyverse)
library(ggrepel)

pov <- read_csv('chld_pov.csv')
care <- read_csv('childcare_hours.csv')
parent_care <- read_csv('childcare_parents.csv')
famben_pct <- read_csv('famben_pct.csv')
famben_pps <- read_csv('famben_pps.csv')
leave <- read_csv('parental_leave.csv')

pov %>%
  inner_join(care) %>%
  inner_join(parent_care) %>%
  inner_join(famben_pct) %>%
  inner_join(leave) -> df

res <- lm(chld_pov ~ childcare_30hrs + childcare_parents + famben_pct + daddy_leave + maternity_leave,df)
summary(res)

ggplot(df,aes(x=famben_pct,y=chld_pov,label=country)) +
  geom_point() +
  geom_text_repel()

data <- df %>%
  select(-country,-chld_pov) %>%
  mutate_all(~scale(.x)[,1])

pca <- princomp(data,scores=T)
summary(pca)

df$dim1 = pca$scores[,1]
df$dim2 = pca$scores[,2]

res1 <- lm(chld_pov ~ dim1 + dim2,df)
summary(res1)

ggplot(df,aes(x=dim1,y=dim2,label=country)) +
  geom_point() +
  geom_text_repel()

