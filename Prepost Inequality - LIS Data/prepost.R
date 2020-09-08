library(tidyverse)

df <- read_csv('gini_lis.csv')

ggplot(df,aes(x=year,y=redist,color=country)) + geom_line(size=1) + geom_point() + ylab('% change in gini coefficient due to taxes and transfers') + ggtitle('Redistribution coefficients for select countries') + labs(subtitle="Luxembourg Income Study, author's calculations",caption='@jwhandley17' ) + ggsave('redist.png',width=8,height=5,type='cairo')

df %>%
  gather(key='definition',value='gini',c(pre,post)) -> data

ggplot(data,aes(x=year,y=gini,color=country)) + geom_line(size=1) + geom_point() + facet_wrap(~definition,scales='free_y') + ylab('Gini coefficient') + labs(title='Inequality before and after taxes and transfers for select countries',subtitle="Luxembourg Income Study, author's calculations",caption='@jwhandley17') + ggsave('prepost.png',width=8,height=5,type='cairo')
