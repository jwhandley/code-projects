library(tidyverse)
library(ggrepel)
library(scales)

policy <- read_csv('oecd_family_database.csv')
outcome <- read_csv('oecd_poverty.csv')

outcome %>%
  filter(age_group == 'children') %>%
  group_by(iso) %>%
  filter(year==max(year)) %>%
  select(iso,poverty) -> child_poverty

policy %>%
  filter(!is.na(value)) %>%
  group_by(country,id) %>%
  filter(year==max(year)) %>%
  ungroup() %>%
  select(iso,id,value) %>%
  spread(id,value) %>%
  filter_all(~!is.na(.x)) %>%
  mutate_at(vars(FAM11A,FAM11B,FAM11C,FAM11D,FAM12A,FAM12B,FAM13,FAM14),~scale(.x)[,1]) -> policy_params

pca <- princomp(select(policy_params,-iso),scores=T)

policy_params$pca1 <- pca$scores[,'Comp.1']
policy_params$pca2 <- pca$scores[,'Comp.2']

data <- inner_join(child_poverty,policy_params)


res <- lm(poverty ~ pca1 + pca2,data)
summary(res)

policy %>%
  group_by(id) %>%
  summarise(indicator = max(indicator))

data %>%
  filter(iso != 'TUR') %>%
  ggplot(aes(x=pca1,y=pca2,label=iso,color=poverty)) +
  geom_point() +
  geom_text_repel() +
  labs(x='Component 1 (Focus on services, public childcare, and paternity leave)',
       y='Component 2 (Focus on cash benefits, maternal leave and childcare)',
       color='Child poverty rate',
       title='Causes of child poverty in the OECD') +
  scale_color_gradient(low='steelblue',high='red',labels=percent) +
  ggsave('oecd_child_pov_pca.png',width=8,height=5)
