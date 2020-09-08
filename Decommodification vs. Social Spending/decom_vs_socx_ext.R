library(tidyverse)
library(stargazer)

data <- read_csv('decom_vs_socx_ext.csv')

gini.decom <- lm(`Gini coefficient` ~ Decommodification,data)
summary(gini.decom)

pov.decom <- lm(`Poverty rate` ~ Decommodification,data)
summary(pov.decom)

gini.socx <- lm(`Gini coefficient` ~ `Social spending`,data)
summary(gini.socx)

pov.socx <- lm(`Poverty rate` ~ `Social spending`,data)
summary(pov.socx)

socx.decom <- lm(`Social spending` ~ Decommodification,data)
summary(socx.decom)

stargazer(socx.decom,gini.decom,pov.decom,gini.socx,pov.socx,out='res_ext.html',dep.var.labels = c('Social Spending','Gini','Poverty','Gini','Poverty'),covariate.labels = c('Decommodification','Social Spending','Constant'))

data %>%
  gather(key='measure',value='value',c(`Gini coefficient`,`Poverty rate`)) %>%
  gather(key='covariate',value='index',c(Decommodification,`Social spending`)) %>%
  ggplot(aes(x=index,y=value)) + 
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(covariate ~ measure,scales='free') +
  labs(x='Decommodification/Social Spending',y='Gini coefficient/Poverty rate',caption='@jwhandley17',title='Decommodification vs. Social Spending') +
  ggsave('decom_vs_socx_corr_ext.png',type='cairo')
